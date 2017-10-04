#!/bin/sh
echo Current location is $PWD
cd bin
  echo bindir sould be $PWD
  ls -tralal


. ./settings_local.sh linux
cd ..
echo OPENDADIR=$OPENDADIR
echo LD_LIBRARY_PATH=$LD_LIBRARY_PATH

echo "Start of script travis_test.sh"
ant test
