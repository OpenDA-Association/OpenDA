#!/bin/bash
echo Starting step_openda.sh
echo Current working directory is $PWD
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

FILE=$PWD/opa

if [ -f $FILE ];
then
   date
else
   date
   ln -s $DIR/opa .
fi

echo "start running opa. :$PWD/opa"
./opa

echo Done running opa

# remove old restart file
rm restart.nc

# Move the restart file to restart.nc
mv SQB_*_restart.nc restart.nc
