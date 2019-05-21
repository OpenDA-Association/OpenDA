#!/bin/sh
sudo apt-get install gfortran
sudo apt-get install mpi-default-dev
sudo apt-get install libnetcdf-dev
sudo apt-get install liblapack-dev
cd ..
mv OpenDA public
cd public

echo START BUILDING NATIVE CODE
. install_native.sh

echo START BUILDING CASTOR JARs

. install_castor.sh

echo DONE INSTALLING


#export OPENDADIR=$PWD/bin
#export LD_LYBRARY_PATH=$LD_LIBRARY_PATH:$OPENDADIR/bin/linux64_gnu/lib
