#!/bin/sh
sudo apt-get install gfortran
sudo apt-get install mpi-default-dev
sudo apt-get install libnetcdf-dev
sudo apt-get install liblapack-dev
cd ..
mv OpenDA public
cd public

HIER=$PWD
cd core/native
./autoreconf_fix.sh 
./configure  
make install
cd $HIER

ant build 

echo DONE INSTALLING


#export OPENDADIR=$PWD/bin
#export LD_LYBRARY_PATH=$LD_LIBRARY_PATH:$OPENDADIR/bin/linux64_gnu/lib
