#!/bin/sh
echo INSTALL EXTRA SOFTWARE
bash install_packages_travis.sh
cd ..
mv OpenDA public
cd public

echo START BUILDING NATIVE CODE
ls 
bash  install_native_travis.sh

echo START BUILDING CASTOR JARs

bash install_castor.sh

echo DONE INSTALLING


#export OPENDADIR=$PWD/bin
#export LD_LYBRARY_PATH=$LD_LIBRARY_PATH:$OPENDADIR/bin/linux64_gnu/lib
