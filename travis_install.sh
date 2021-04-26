#!/bin/sh
echo INSTALL EXTRA SOFTWARE

cd ..
mv OpenDA public
cd public

echo START BUILDING NATIVE CODE

bash ci_compile_native.sh

echo START BUILDING CASTOR JARs

bash ci_build_castor.sh

echo START BUILDING JAVA

bash ci_build.sh

echo DONE INSTALLING

#export OPENDADIR=$PWD/bin
#export LD_LYBRARY_PATH=$LD_LIBRARY_PATH:$OPENDADIR/bin/linux64_gnu/lib
