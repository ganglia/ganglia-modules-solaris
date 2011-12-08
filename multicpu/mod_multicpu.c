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
 * Author: Daniel Pocock (borrowing from various sources)
 ******************************************************************************/



#include <kstat.h>
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

mmodule multicpu_module;

/*  number of seconds to wait before refreshing/recomputing values off kstat */

#define TICK_SECONDS    30

#ifndef FSCALE
#define FSHIFT  8               /* bits to right of fixed binary point */
#define FSCALE  (1<<FSHIFT)
#endif /* FSCALE */

/*  also imported from top, i'm basically using the same CPU cycle summing algo */

#define CPUSTATES       5
#define CPUSTATE_IDLE   0
#define CPUSTATE_USER   1
#define CPUSTATE_KERNEL 2
#define CPUSTATE_IOWAIT 3
#define CPUSTATE_SWAP   4

int first_run = 1;

/* support macros for the percentage computations */

#define loaddouble(la) ((double)(la) / FSCALE)

kstat_ctl_t *kc = NULL;
int g_ncpus;

struct g_metrics_struct {
	g_val_t cpu_num;
	g_val_t sys_clock;
};
struct g_metrics_struct metriclist;

struct g_cpu_metrics_struct {
	g_val_t cpu_wio;
	g_val_t cpu_idle;
	g_val_t cpu_nice;
	g_val_t cpu_system;
	g_val_t cpu_user;
};

typedef struct cpu_data {
	int cpu_states[CPUSTATES];
	unsigned long cpu_old[CPUSTATES];
	struct g_cpu_metrics_struct metriclist;
	struct timeval lasttime;
	time_t last_refresh;
	kstat_t *ks;
	int ks_instance;
} cpu_data_t;

typedef g_val_t (*mcpu_func_t)(cpu_data_t *cpu);


apr_array_header_t *cpus = NULL;

cpu_data_t *get_mcpu(int cpu_index) {
	cpu_data_t *_cpus = (cpu_data_t *) cpus->elts;
	return &_cpus[cpu_index];
}

int mcpu_get_kstat_val(g_val_t *val, char *km_name, char *ks_name, char *name) {
	/* Warning.. always assuming a KSTAT_DATA_ULONG here */
	kstat_t *ks;
	kstat_named_t *kn;

	/*
	 * Get a kstat_ctl handle, or update the kstat chain.
	 */
	if (kc == NULL)
		kc = kstat_open();
	else
		kstat_chain_update(kc);

	if (kc == NULL) {
		debug_msg("multicpu: couldn't open kc...");
		err_ret("mcpu_get_kstat_val() kstat_open() error");
		return -1;
	}

	debug_msg(
			"multicpu: Lookup up kstat:  km (unix?)='%s', ks (system_misc?)='%s',kn (resulting metric?)='%s'",
			km_name, ks_name, name);
	debug_msg("multicpu: %s: kc is %p", name, kc);
	ks = kstat_lookup(kc, km_name, 0, ks_name);
	debug_msg("multicpu: %s: Just did kstat_lookup().", name);

	if (ks == NULL) {
		perror("ks");
	}
	debug_msg("multicpu: %s: Looked up.", name);
	if (kstat_read(kc, ks, 0) == -1) {
		perror("kstat_read");
		return -1;
	}
	kn = kstat_data_lookup(ks, name);
	if (kn == NULL) {
		err_ret("mcpu_get_kstat_val() kstat_data_lookup() kstat_read() error");
		return -1;
	}
	debug_msg("multicpu: %s: Kstat data type:  %d, Value returned: %u, %d %u %d", name,
			(int) kn->data_type, (int) kn->value.ui32, (int) kn->value.l,
			kn->value.ul, kn->value.ui64);
	//    ks = kstat_lookup(kc, "unix", 0, "system_misc");

	if (kn->value.ui32 == 0)
		val->uint32 = (unsigned long) kn->value.ul;
	else
		val->uint32 = (int) kn->value.ui32;
	sleep(0);
	debug_msg("multicpu: %s: Kernel close.  Val returned: %d", name, val->uint32);

	return 0;
}

long mcpu_percentages(int cnt, int *out, register unsigned long *new,
		register unsigned long *old, unsigned long *diffs) {
	register int i;
	register long change;
	register long total_change;
	register unsigned long *dp;
	long half_total;

	/* initialization */
	total_change = 0;
	dp = diffs;

	/* calculate changes for each state and the overall change */
	for (i = 0; i < cnt; i++) {
		if ((change = *new - *old) < 0) {
			/* this only happens when the counter wraps */
			change = (int) ((unsigned long) *new - (unsigned long) *old);
		}
		total_change += (*dp++ = change);
		*old++ = *new++;
	}

	/* avoid divide by zero potential */
	if (total_change == 0) {
		total_change = 1;
	}

	/* calculate percentages based on overall change, rounding up */
	half_total = total_change / 2l;
	for (i = 0; i < cnt; i++) {
		*out++ = (int) ((*diffs++ * 1000 + half_total) / total_change);
	}

	/* return the total in case the caller wants to use it */
	return (total_change);
}

void clear_ks_pointers() {
	int i;
	cpu_data_t *cpu;
	cpu = (cpu_data_t *)cpus->elts;
	for(i = 0; i < cpus->nelts; i++ ) {
		cpu->ks = NULL;
		cpu++;
	}
}

int get_kstat_cpu_values(cpu_data_t *cpu, cpu_stat_t *cpuStats) {

	if (kc == NULL) {
		kc = kstat_open();
		clear_ks_pointers();
	} else {

		 if(kstat_chain_update(kc) != 0) {
			clear_ks_pointers();
		}

	}

	if (kc == NULL) {
		debug_msg("multicpu: couldn't open kc...");
		err_ret("kstat_open() error");
		return NULL;
	}

	if(cpu->ks == NULL) {
		cpu->ks = kstat_lookup(kc, "cpu_stat", cpu->ks_instance, NULL);

		if (cpu->ks == NULL) {
			return -1;
		}

		if (cpu->ks->ks_type != KSTAT_TYPE_RAW)
			return -1;

	}

	kstat_read(kc, cpu->ks, cpuStats);

	return 0;
}

int mcpu_determine_cpu_percentages() {

	/* TODO:
	 * - store an array of cpu_old values
	 * - poll all CPUs together
	 */

	/*
	 * hopefully this doesn't get too confusing.
	 * cpu_snap is a structure from <sys/cpuvar.h> and is the container into which
	 * we read the current CPU metrics.
	 * the static array "cpu_old" contains the last iteration's summed cycle
	 * counts.
	 * the array "cpu_now" contains the current iteration's summed cycle
	 * counts.
	 * "cpu_diff" holds the delta.
	 * across CPUs of course. :)
	 * buffers[0..2] holds past, present and diff info for the "other" CPU stats.
	 */

	struct timeval thistime;
	double timediff;

	unsigned int ncpus;
	unsigned long diff_cycles = 0L;
	unsigned long time_delta = 0L;
	double alpha, beta; // lambda lambda lambda!!!

	unsigned long cpu_now[CPUSTATES];
	unsigned long cpu_diff[CPUSTATES];

	register int j;
	cpu_stat_t cpuKstats;
	processorid_t i;
//	int cpu_id = sysconf(_SC_NPROCESSORS_ONLN);

	cpu_data_t *cd;

	ncpus = metriclist.cpu_num.uint32;

	/*
	 * Modified by Robert Petkus <rpetkus@bnl.gov>
	 * Get stats only for online CPUs. Previously, gmond segfaulted if
	 * the CPUs were not numbered sequentially; i.e., cpu0, cpu2, etc.
	 * Tested on 64 bit Solaris 8 and 9 with GCC 3.3 and 3.3.2
	 */
	//debug_msg("multicpu: processors online = %d", cpu_id);
	for (i = 0; i < cpus->nelts; i++) {

		/*
		 * ripped from top by swagner in the hopes of getting
		 * top-like CPU percentages ...
		 */
		gettimeofday(&thistime, NULL);

		cd = get_mcpu(i);

		if (cd->lasttime.tv_sec)
			timediff = ((double) thistime.tv_sec * 1.0e7
					+ ((double) thistime.tv_usec * 10.0))
					- ((double) cd->lasttime.tv_sec * 1.0e7
							+ ((double) cd->lasttime.tv_usec * 10.0));
		else
			timediff = 1.0e7;

		debug_msg("multicpu: checking cpu %d, timediff = %f", i, timediff);

		/*
		 * constants for exponential average.  avg = alpha * new + beta * avg
		 * The goal is 50% decay in 30 sec.  However if the sample period
		 * is greater than 30 sec, there's not a lot we can do.
		 */
		if (timediff < 30.0e7) {
			alpha = 0.5 * (timediff / 30.0e7);
			beta = 1.0 - alpha;
			debug_msg(
					"multicpu: * * * * Setting alpha to %f and beta to %f because timediff = %d",
					alpha, beta, timediff);
		} else {
			alpha = 0.5;
			beta = 0.5;
		}

		cd->lasttime = thistime;

		/*  END SECTION RIPPED BLATANTLY FROM TOP :) */

		if (first_run == 1) {
			debug_msg("multicpu: Initializing old read/write buffer... ");
			time_delta = 0L;
		}


		/*
		 * Submitted by JB Kim <jbremnant@hotmail.com>
		 * also skip the loop if CPU is "off-line"
		 */
		int n = p_online(i, P_STATUS);
		if (n == 1)
			continue;

		if (n == -1 && errno == EINVAL)
			continue;

		for (j = 0; j < CPUSTATES; j++)
			cpu_now[j] = 0;

		if(get_kstat_cpu_values(cd, &cpuKstats) != 0) {
			perror("kstat_read");
			return -1;
		}

		/* copy up to the wait state counter, the last two we determine ourselves */
		for (j = 0; j < CPU_WAIT; j++) {
			cpu_now[j] = (unsigned long) cpuKstats.cpu_sysinfo.cpu[j];
		}

		cpu_now[CPUSTATE_IOWAIT]
				= (unsigned long) cpuKstats.cpu_sysinfo.wait[W_IO]
						+ (unsigned long) cpuKstats.cpu_sysinfo.wait[W_PIO];
		cpu_now[CPUSTATE_SWAP]
				= (unsigned long) cpuKstats.cpu_sysinfo.wait[W_SWAP];

		time_delta = (time(NULL) - cd->last_refresh);
		if (time_delta == 0)
			time_delta = 1;

		debug_msg("multicpu: checking cpu %d, timediff = %f and time_delta = %ld", i, timediff/1.0e7, time_delta);

		/*
		 * decay stuff
		 * semi-stolen from top.  :)  added by swagner on 8/20/02
		 */
		if (time_delta < 30) {
			alpha = 0.5 * (time_delta / 30);
			beta = 1.0 - alpha;
		} else {
			alpha = 0.5;
			beta = 0.5;
		}

		diff_cycles
				= mcpu_percentages(CPUSTATES, cd->cpu_states, cpu_now, cd->cpu_old, cpu_diff);

		debug_msg("multicpu: diffs are %d:%d %d:%d %d:%d %d:%d (aggregate: %d)",
				CPU_IDLE, cpu_diff[CPU_IDLE],
				CPU_USER, cpu_diff[CPU_USER],
				CPU_KERNEL, cpu_diff[CPU_KERNEL],
				CPU_WAIT, cpu_diff[CPU_WAIT],
				diff_cycles);
		debug_msg(
				"multicpu: ** ** ** ** ** Are percentages electric?  Try %d%%, %d%% , %d%% , %d%% , %d%% (over %d cycles)",
				cd->cpu_states[0], cd->cpu_states[1], cd->cpu_states[2], cd->cpu_states[3],
				cd->cpu_states[4], diff_cycles);


		/*
		 * i don't know how you folks do things in new york city, but around here folks
		 * don't go around dividing by zero.
		 */
		if (diff_cycles < 1) {
			debug_msg("multicpu: diff_cycles < 1 ... == %f %u!", diff_cycles, diff_cycles);
			diff_cycles = 1;
		}

		/*
		 * could this be ANY HARDER TO READ?  sorry.  through hacking around i found
		 * that explicitly casting everything as floats seems to work...
		 */
		cd->metriclist.cpu_idle.f = (float) cd->cpu_states[CPUSTATE_IDLE] / 10;
		cd->metriclist.cpu_user.f = (float) cd->cpu_states[CPUSTATE_USER] / 10;
		cd->metriclist.cpu_system.f = (float) (cd->cpu_states[CPUSTATE_KERNEL]
				+ cd->cpu_states[CPUSTATE_SWAP]) / 10;
		cd->metriclist.cpu_wio.f = (float) cd->cpu_states[CPUSTATE_IOWAIT] / 10;

		time(&cd->last_refresh);
	}

	return (0);
}

g_val_t mcpu_user_func(cpu_data_t *cd) {
	g_val_t val;

	if(cd == get_mcpu(0))
		mcpu_determine_cpu_percentages();
	val.f = cd->metriclist.cpu_user.f;
	return val;
}

/* FIXME: ? */
g_val_t mcpu_nice_func(cpu_data_t *cd) {
	g_val_t val;

	val.f = 0.0; /*  no more mr. nice procs ... */

	return val;
}

g_val_t mcpu_system_func(cpu_data_t *cd) {
	g_val_t val;

	val.f = cd->metriclist.cpu_system.f;
	return val;
}

g_val_t mcpu_idle_func(cpu_data_t *cd) {
	g_val_t val;

	/* mcpu_determine_cpu_percentages(); */
	val.f = cd->metriclist.cpu_idle.f;
	return val;
}

/* FIXME: always 0? */
g_val_t mcpu_wio_func(cpu_data_t *cd) {
	g_val_t val;

	val.f = cd->metriclist.cpu_wio.f;
	return val;
}

apr_array_header_t *metric_info = NULL;

typedef struct metric_spec {
	mcpu_func_t func;
	const char *name;
	const char *units;
	const char *desc;
	const char *fmt;
} metric_spec_t;

metric_spec_t my_metrics[] = {
		{ mcpu_user_func, "multicpu_user", "%",
				"Percentage of CPU utilization that occurred while "
					"executing at the user level", "%.1f" },
		{ mcpu_nice_func, "multicpu_nice", "%",
				"Percentage of CPU utilization that occurred while "
					"executing at the nice level", "%.1f" },
		{ mcpu_system_func, "multicpu_system", "%",
				"Percentage of CPU utilization that occurred while "
					"executing at the system level", "%.1f" },
		{ mcpu_idle_func, "multicpu_idle", "%",
				"Percentage of CPU utilization that occurred while "
					"executing at the idle level", "%.1f" },
		{ mcpu_wio_func, "multicpu_wio", "%",
				"Percentage of CPU utilization that occurred while "
					"executing at the wio level", "%.1f" },
		{ NULL, NULL, NULL, NULL, NULL} };

void init_metrics_for_cpu(apr_pool_t *p, apr_array_header_t *ar,
		kstat_t *ks) {
	metric_spec_t *metric;
	Ganglia_25metric *gmi;

	cpu_data_t *cd;

	cd = apr_array_push(cpus);
	bzero(cd, sizeof(cpu_data_t));
	cd->ks = NULL;
	cd->ks_instance = ks->ks_instance;

	debug_msg("multicpu: init for cpu instance %d", ks->ks_instance);
	for (metric = my_metrics; metric->func != NULL; metric++) {
		gmi = apr_array_push(ar);

		/* gmi->key will be automatically assigned by gmond */
		gmi->name = apr_psprintf(p, "%s%d", metric->name, ks->ks_instance);
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
	kstat_t *ksp;
	Ganglia_25metric *gmi;

	g_val_t val;

	apr_pool_t *pool;

	/* Allocate a pool that will be used by this module */
	apr_pool_create(&pool, p);

	mcpu_get_kstat_val(&metriclist.cpu_num, "unix", "system_misc", "ncpus");
	debug_msg("metric_init: Assigning cpu_num value (%d) to ncpus.",
			(int) metriclist.cpu_num.uint32);
	g_ncpus = metriclist.cpu_num.uint32;

	cpus = apr_array_make(pool, g_ncpus, sizeof(cpu_data_t));
	metric_info = apr_array_make(pool, g_ncpus, sizeof(Ganglia_25metric));

	if (kc == NULL)
		kc = kstat_open();
	else
		kstat_chain_update(kc);

	if (kc == NULL) {
		debug_msg("multicpu: couldn't open kc...");
		err_ret("kstat_open() error");
		return -1;
	}

	for (ksp = kc->kc_chain; ksp != NULL; ksp = ksp->ks_next) {
		if (ksp->ks_type == KSTAT_TYPE_RAW &&
				(strcmp(ksp->ks_module, "cpu_stat") == 0)) {
			init_metrics_for_cpu(p, metric_info, ksp);
		}
	}

	/* Add a terminator to the array and replace the empty static metric definition
	 array with the dynamic array that we just created
	 */
	gmi = apr_array_push(metric_info);
	memset(gmi, 0, sizeof(*gmi));

	multicpu_module.metrics_info = (Ganglia_25metric *) metric_info->elts;

	for (i = 0; multicpu_module.metrics_info[i].name != NULL; i++) {
		/* Initialize the metadata storage for each of the metrics and then
		 *  store one or more key/value pairs.  The define MGROUPS defines
		 *  the key for the grouping attribute. */
		MMETRIC_INIT_METADATA(&(multicpu_module.metrics_info[i]), p);
		MMETRIC_ADD_METADATA(&(multicpu_module.metrics_info[i]), MGROUP, "cpu");
	}

	debug_msg("modmulticpu.c: metric_init() ok.");
	val.int32 = 0;
	first_run = 0;
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
	int cpu_index;
	int _metric_index;
	cpu_data_t *cpu;

	cpu_index = metric_index / 5;
	_metric_index = metric_index % 5;

	cpu = get_mcpu(cpu_index);
	debug_msg("multicpu: handling read for metric %d CPU %d idx %d name = %s%d",
			metric_index, cpu_index, _metric_index, "cpu_stat",
			cpu->ks_instance);
	val = my_metrics[_metric_index].func(cpu);

	return val;
}

mmodule multicpu_module = { STD_MMODULE_STUFF, ex_metric_init,
		ex_metric_cleanup, NULL, /* defined dynamically */
		ex_metric_handler, };
