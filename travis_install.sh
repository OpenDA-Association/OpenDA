#!/bin/sh
echo INSTALL EXTRA SOFTWARE
#bash install_packages_travis.sh
cd ..
mv OpenDA public
cd public

echo START BUILDING NATIVE CODE
ls
bash github_compile_native.sh

echo START BUILDING CASTOR JARs

bash github_build_castor.sh

echo DONE INSTALLING

#export OPENDADIR=$PWD/bin
#export LD_LYBRARY_PATH=$LD_LIBRARY_PATH:$OPENDADIR/bin/linux64_gnu/lib
