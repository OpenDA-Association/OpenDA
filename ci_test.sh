#!/bin/sh
#Note set TZ to force correct date output (otherwise some comparisons will fail!)
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export TZ='Europe/Amsterdam'

export OPENDADIR=$PWD/bin
export LD_LIBRARY_PATH=$OPENDADIR/linux64_gnu/lib:$LD_LIBRARY_PATH

echo LIBDIR
ls -tral "$OPENDADIR/linux64_gnu/lib"

echo "Start of ant test-ci"
ant test-ci
