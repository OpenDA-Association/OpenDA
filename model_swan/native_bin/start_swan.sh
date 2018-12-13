#!/bin/sh
SCRIPT=$(readlink -f "$0")
# Absolute path this script is in
SWANPATH=$(dirname "$SCRIPT")
echo $SWANPATH

export LD_LIBRARY_PATH=$SWANPATH/linux64/libraries

### Set the appropriate path of the SWAN executable.
swan_exe=$SWANPATH/linux64/4120A_3_del_l64_i13_omp.exe

### General, start SWAN.
$swan_exe
