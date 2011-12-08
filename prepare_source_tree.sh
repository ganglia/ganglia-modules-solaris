#!/bin/sh


# M4=/opt/csw/bin/gm4 MAKE=/opt/csw/bin/gmake autoreconf -i


autoreconf --install

#PREFIX=/usr

#./configure --with-libapr=apr-1-config --with-libconfuse=/opt/confuse-2.6 --with-libganglia=/opt/ganglia-3.1 --enable-shared --disable-static

./configure --enable-shared --disable-static





