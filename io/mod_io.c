/*******************************************************************************
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  - Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 *  - Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS''
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 * Author: Daniel Pocock
 ******************************************************************************/

#include <kstat.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/utsname.h>
#include <strings.h>
#include <sys/types.h>
#include <dirent.h>
#include <procfs.h>
#include <errno.h>

/*
 * used for swap space determination - swapctl()
 * and anon.h has the data structure  needed for swapctl to be useful
 */

#include <sys/stat.h>
#include <sys/swap.h>
#include <vm/anon.h>
#include <fcntl.h>

/*
 * we get the cpu struct from cpuvar, maybe other mojo too
 */

#include <sys/var.h>
#include <sys/cpuvar.h>
#include <sys/time.h>
#include <sys/processor.h>
/*
 * functions spackled in by swagner -- the CPU-percentage-specific code is
 * largely an imitation (if not a shameless copy) of the Solaris-specific
 * code for top.
 */

/*
 * used for disk space determination - getmntent(), statvfs()
 */

#include <sys/mnttab.h>
#include <sys/statvfs.h>

#include <gm_metric.h>

#include <stdio.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>

#include <apr_tables.h>
#include <apr_strings.h>

mmodule io_module;

kstat_ctl_t *kc = NULL;
int first_run = 2;


apr_array_header_t *metric_info = NULL;
apr_array_header_t *devices = NULL;

/* struct k_metrics_struct {
 g_val_t nread;  /* Bytes  * /
 g_val_t read;   /* Requests * /
 g_val_t nwrite; /* Bytes * /
 g_val_t write;  /* Requests * /
 g_val_t rtime;  /* Time spent servicing requests on run queue (nanoseconds) * /
 g_val_t wtime;  /* Time spent servicing requests on wait queue (nanoseconds) * /
 }; */

struct g_metrics_struct {
	g_val_t nread_rate; /* Bytes */
	g_val_t read_rate; /* Requests */
	g_val_t nwrite_rate; /* Bytes */
	g_val_t write_rate; /* Requests */
	g_val_t avg_svc_time; /* Avg time spent servicing requests on run queue (seconds) */
	g_val_t avg_wait_time; /* Avg time spent servicing requests on wait queue (seconds) */
	g_val_t io_time; /* time spent by requests in IO scheduler during sample period (seconds) */
};

typedef struct disk_data {
	kstat_io_t old_values;
	struct g_metrics_struct metriclist;
	struct timeval lasttime;
	unsigned long last_refresh;
	char *module_name;
	int ks_instance;
	kstat_t *ks;
} disk_data_t;

typedef g_val_t (*io_func_t)(disk_data_t *dev);

typedef struct metric_spec {
	io_func_t io_func;
	const char *name;
	const char *units;
	const char *desc;
	const char *fmt;
} metric_spec_t;

u_longlong_t counter_diff(u_longlong_t new_val, u_longlong_t old_val) {
	register u_longlong_t delta = new_val - old_val;
	return delta;
	/* if (delta < 0) {
	 /* this only happens when the counter wraps * /
	 change = (u_longlong_t) ((u_longlong_t) new_val - (u_longlong_t) old_val);
	 }
	 return delta; */
}

double counter_rate(u_longlong_t new_val, u_longlong_t old_val,
		double time_diff) {
	return counter_diff(new_val, old_val) / time_diff;
}

/* time_diff is in seconds */
void update_rates(kstat_io_t *old_values, kstat_io_t *new_values,
		struct g_metrics_struct *rates, double time_diff) {

	uint_t read_count;
	uint_t write_count;
	uint_t io_count;
	hrtime_t rtime_delta; /* ns */
	hrtime_t wtime_delta; /* ns */

	read_count = counter_diff(new_values->reads, old_values->reads);
	write_count = counter_diff(new_values->writes, old_values->writes);
	io_count = read_count + write_count;

	debug_msg("io: read_count = %d, write_count = %d, io_count = %d",
			read_count, write_count, io_count);

	rtime_delta = counter_diff(new_values->rtime, old_values->rtime);
	wtime_delta = counter_diff(new_values->wtime, old_values->wtime);

	rates->nread_rate.f = counter_rate(new_values->nread, old_values->nread,
			time_diff);
	rates->read_rate.f = read_count / time_diff;
	rates->nwrite_rate.f = counter_rate(new_values->nwritten,
			old_values->nwritten, time_diff);
	rates->write_rate.f = write_count / time_diff;
	rates->io_time.f = (rtime_delta + wtime_delta) * 1.0e-9;

	if (io_count > 0) {
		rates->avg_svc_time.f = rates->io_time.f / io_count;
		rates->avg_wait_time.f = 1.0e-9 * (wtime_delta / io_count);
	} else {
		rates->avg_svc_time.f = NAN;
		rates->avg_wait_time.f = NAN;
	}

	bcopy(new_values, old_values, sizeof(kstat_io_t));
}

void clear_ks_pointers() {
	int i;
	disk_data_t *dev;
	dev = (disk_data_t *)devices->elts;
	for(i = 0; i < devices->nelts; i++ ) {
		dev->ks = NULL;
		dev++;
	}
}

int get_kstat_io_values(disk_data_t *dev, kstat_io_t *kio) {

	if (kc == NULL) {
		kc = kstat_open();
		 debug_msg("io: chain found, clearing pointers...");
			clear_ks_pointers();
			debug_msg("io: chain update handled");
	} else {

		 if(kstat_chain_update(kc) != 0) {
			 debug_msg("io: chain update detected, clearing pointers...");
			clear_ks_pointers();
			debug_msg("io: chain update handled");
		}

	}


	if (kc == NULL) {
		debug_msg("io: couldn't open kc...");
		err_ret("kstat_open() error");
		return NULL;
	}

	if(dev->ks == NULL) {
		dev->ks = kstat_lookup(kc, dev->module_name, dev->ks_instance, NULL);

		if (dev->ks == NULL) {
			return -1;
		}

		if (dev->ks->ks_type != KSTAT_TYPE_IO)
			return -1;

	}

	kstat_read(kc, dev->ks, kio);

	return 0;
}

int refresh_device_stats(disk_data_t *dev) {

	struct timeval thistime;
	kstat_io_t new_val;
	double timediff;

	if (dev == NULL)
		return -1;

	gettimeofday(&thistime, NULL);
	if (dev->lasttime.tv_sec)
		timediff = ((double) thistime.tv_sec * 1.0 + ((double) thistime.tv_usec
				* 1.0e-6)) - ((double) dev->lasttime.tv_sec * 1.0
				+ ((double) dev->lasttime.tv_usec * 1.0e-6));
	else
		timediff = 1.0e7;
	if (timediff < 2.0)
		return 0;

	debug_msg("io: getting values for %s %d", dev->module_name,
			dev->ks_instance);
	if (get_kstat_io_values(dev, &new_val) != 0)
		return -1;

	gettimeofday(&thistime, NULL);
	if (dev->lasttime.tv_sec)
		timediff = ((double) thistime.tv_sec * 1.0 + ((double) thistime.tv_usec
				* 1.0e-6)) - ((double) dev->lasttime.tv_sec * 1.0
				+ ((double) dev->lasttime.tv_usec * 1.0e-6));
	else
		timediff = 1.0e7;
	bcopy(&thistime, &dev->lasttime, sizeof(struct timeval));

	debug_msg("io: updating rates for %s %d (timediff=%f)", dev->module_name,
			dev->ks_instance, timediff);
	if(first_run > 0) {
		first_run--;
		dev->metriclist.avg_svc_time.f = NAN;
		dev->metriclist.avg_wait_time.f = NAN;
		dev->metriclist.io_time.f = NAN;
		dev->metriclist.nread_rate.f = NAN;
		dev->metriclist.nwrite_rate.f = NAN;
		dev->metriclist.read_rate.f = NAN;
		dev->metriclist.write_rate.f = NAN;
		bcopy(&new_val, &dev->old_values, sizeof(kstat_io_t));
	} else
		update_rates(&dev->old_values, &new_val, &dev->metriclist, timediff);

	return 0;
}

/*
 * Get the metric name and index from the original
 * metric name
 */
void get_metric_name_cpu(char *metric, char *name, int *index) {
	/* The metric name contains both the name and the cpu id.
	 Split the cpu id from the name so that we can use it to
	 decide which metric handler to call and for which cpu.
	 */
	size_t numIndex = strcspn(metric, "0123456789");

	if (numIndex > 0) {
		strncpy(name, metric, numIndex);
		name[numIndex] = '\0';
		*index = atoi(&metric[numIndex]);
	} else {
		*name = '\0';
		*index = 0;
	}

	return;
}

g_val_t io_nread_func(disk_data_t *dev) {
	g_val_t val;
	refresh_device_stats(dev);
	val.f = dev->metriclist.nread_rate.f;
	return val;
}

g_val_t io_nwrite_func(disk_data_t *dev) {
	g_val_t val;
	refresh_device_stats(dev);
	val.f = dev->metriclist.nwrite_rate.f;
	return val;
}

g_val_t io_reads_func(disk_data_t *dev) {
	g_val_t val;
	refresh_device_stats(dev);
	val.f = dev->metriclist.read_rate.f;
	return val;
}

g_val_t io_writes_func(disk_data_t *dev) {
	g_val_t val;
	refresh_device_stats(dev);
	val.f = dev->metriclist.write_rate.f;
	return val;
}

g_val_t io_avg_svc_time_func(disk_data_t *dev) {
	g_val_t val;
	refresh_device_stats(dev);
	val.f = dev->metriclist.avg_svc_time.f;
	return val;
}

g_val_t io_avg_wait_time_func(disk_data_t *dev) {
	g_val_t val;
	refresh_device_stats(dev);
	val.f = dev->metriclist.avg_wait_time.f;
	return val;
}

g_val_t io_aggregate_time_func(disk_data_t *dev) {
	g_val_t val;
	refresh_device_stats(dev);
	val.f = dev->metriclist.io_time.f;
	return val;
}

metric_spec_t metrics[] = {
		{ io_nread_func, "nread", "bytes/sec", "bytes read", "%.1f" },
		{ io_nwrite_func, "nwrite", "bytes/sec", "bytes written", "%.1f" },
		{ io_reads_func, "reads", "reads/sec",
				"IO read operations", "%.1f" },
		{ io_writes_func, "writes",	"writes/sec",
				"IO write operations", "%.1f" },
		{ io_avg_svc_time_func,
		"avg_svc_time", "s", "Average IO req. service time", "%.6f" },
		{ io_avg_wait_time_func, "avg_wait_time", "s",
				"Average time IO reqs remain in wait queue", "%.6f" },
		{ io_aggregate_time_func, "aggegate_time", "s",
				"Aggregate time spent by all requests in IO", "%.6f" },
		{ NULL, NULL, NULL, NULL, NULL } };

void init_metric_for_device(apr_pool_t *p, apr_array_header_t *ar, kstat_t *ks) {
	metric_spec_t *metric;
	Ganglia_25metric *gmi;
	char *metric_name;

	disk_data_t *dev;

	dev = apr_array_push(devices);
	bzero(dev, sizeof(disk_data_t));

	dev->module_name = apr_pstrdup(p, ks->ks_module);
	dev->ks_instance = ks->ks_instance;
	dev->ks = NULL;

	for (metric = metrics; metric->io_func != NULL; metric++) {
		gmi = apr_array_push(ar);

		/* gmi->key will be automatically assigned by gmond */
		metric_name = apr_psprintf(p, "io_%s_%s%d", metric->name,
				ks->ks_module, ks->ks_instance);
		debug_msg("io: creating metric %s", metric_name);
		gmi->name = metric_name;
		gmi->tmax = 90;
		gmi->type = GANGLIA_VALUE_FLOAT;
		gmi->units = apr_pstrdup(p, metric->units);
		gmi->slope = apr_pstrdup(p, "both");
		gmi->fmt = apr_pstrdup(p, metric->fmt);
		gmi->msg_size = UDP_HEADER_SIZE + 8;
		gmi->desc = apr_pstrdup(p, metric->desc);
	}
}

static int ex_metric_init(apr_pool_t *p) {
	int i;
	Ganglia_25metric *gmi;
	kstat_t *ksp;
	kstat_io_t kio;

	g_val_t val;

	apr_pool_t *pool;

	/* Allocate a pool that will be used by this module */
	apr_pool_create(&pool, p);

	metric_info = apr_array_make(pool, 2, sizeof(Ganglia_25metric));
	devices = apr_array_make(pool, 2, sizeof(disk_data_t));

	if (kc == NULL)
		kc = kstat_open();
	else
		kstat_chain_update(kc);

	if (kc == NULL) {
		debug_msg("io: couldn't open kc...");
		err_ret("kstat_open() error");
		return -1;
	}

	for (ksp = kc->kc_chain; ksp != NULL; ksp = ksp->ks_next) {
		if (ksp->ks_type == KSTAT_TYPE_IO) {
			init_metric_for_device(p, metric_info, ksp);
		}
	}

	/* Add a terminator to the array and replace the empty static metric definition
	 array with the dynamic array that we just created
	 */
	gmi = apr_array_push(metric_info);
	memset(gmi, 0, sizeof(*gmi));

	io_module.metrics_info = (Ganglia_25metric *) metric_info->elts;

	for (i = 0; io_module.metrics_info[i].name != NULL; i++) {
		/* Initialize the metadata storage for each of the metrics and then
		 *  store one or more key/value pairs.  The define MGROUPS defines
		 *  the key for the grouping attribute. */
		MMETRIC_INIT_METADATA(&(io_module.metrics_info[i]), p);
		MMETRIC_ADD_METADATA(&(io_module.metrics_info[i]), MGROUP, "disk");
	}

	debug_msg("modio.c: metric_init() ok.");
	val.int32 = 0;
	/*
	 * We need to make sure that every server thread gets their own copy of "kc".
	 * The next metric that needs a kc-handle will reopen it for the server thread.
	 */
	if (kc) {
		kstat_close(kc);
		kc = NULL;
	}
	return val.int32;

}

static void ex_metric_cleanup(void) {
}

static g_val_t ex_metric_handler(int metric_index) {
	g_val_t val;
	int dev_index;
	int _metric_index;
	char name[64];
	disk_data_t *_devices;
	disk_data_t *dev;

	dev_index = metric_index / 7;
	_metric_index = metric_index % 7;

	_devices = (disk_data_t *) devices->elts;
	dev = &_devices[dev_index];
	debug_msg("io: handling read for metric %d dev %d idx %d name = %s%d",
			metric_index, dev_index, _metric_index, dev->module_name,
			dev->ks_instance);
	val = metrics[_metric_index].io_func(dev);

	return val;
}

mmodule io_module = { STD_MMODULE_STUFF, ex_metric_init, ex_metric_cleanup,
		NULL, /* defined dynamically */
		ex_metric_handler, };
