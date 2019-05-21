#!/bin/sh
sudo apt-get install gfortran
sudo apt-get install mpi-default-dev
sudo apt-get install libnetcdf-dev
sudo apt-get install liblapack-dev
cd ..
mv OpenDA public
cd public

echo START BUILDING NATIVE CODE
echo "current location $PWD"
ls 
bash  install_native.sh

echo START BUILDING CASTOR JARs

bash install_castor.sh

echo DONE INSTALLING


#export OPENDADIR=$PWD/bin
#export LD_LYBRARY_PATH=$LD_LIBRARY_PATH:$OPENDADIR/bin/linux64_gnu/lib
