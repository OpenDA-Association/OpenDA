#!/bin/sh
export OPENDADIR=$PWD/bin
export LD_LIBRARY_PATH=$OPENDADIR/linux64_gnu/lib:$LD_LIBRARY_PATH

echo LIBDIR
ls -tralal $OPENDADIR/linux64_gnu/lib


echo "Start of script travis_test.sh"
ant test
