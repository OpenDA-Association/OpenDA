#!/bin/sh
cd bin
. settings_local.sh linux
cd ..
echo LD_LIBRARY_PATH=$LD_LIBRARY_PATH

echo "Start of script travis_test.sh"
ant test
