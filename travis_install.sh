sudo apt-get install gfortran
sudo apt-get install mpi-devel
sudo apt-get install libnetcdf-dev
sudo apt-get install liblapack-dev
HIER=$PWD
cd core/native
./autoreconf_fix.sh
./configure
make install
cd $HIER
export OPENDADIR=$PWD/bin
export LD_LYBRARY_PATH=$LD_LIBRARY_PATH:$OPENDADIR/bin/linux64_gnu/lib
#ant build
#ant test-travis
