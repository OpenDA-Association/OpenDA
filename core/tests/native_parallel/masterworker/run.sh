#!/bin/sh

# Fill in the location where the worker process can be found
#BINDIR=/part3/nils/promotie/src/openda_1/public/trunk/core/native_libs/bin
BINDIR="not set"
if [ "$BINDIR" == "not set" ]; then
        echo "Please change script and set BINDIR"
        exit -1
fi

mpiexec -np 2 Application.sh -p PolluteEnKFOpenDaConfigMW.oda : \
        -np 2 $BINDIR/pollute2d_worker pollute2d_worker_config.xml : \
        -np 1 Application.sh -p PolluteEnKFOpenDaConfigMW.oda : \
        -np 2 $BINDIR/pollute2d_worker pollute2d_worker_config.xml

