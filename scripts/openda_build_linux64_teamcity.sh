#! /bin/bash

module(){ eval `/usr/bin/modulecmd bash $*`;};
 
export OPENDASCRIPTROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd ${OPENDASCRIPTROOT}/../..

export GCCDIR=/opt/apps/gcc/9.2.0
module load gcc/9.2.0
module load ant/1.10.9
module load automake/1.16.2_gcc9.2.0
module load autoconf/2.69_gcc9.2.0
module load libtool/2.4.6_gcc9.2.0
rm -rf public/bin/*
rm -rf public/*.tgz
public/scripts/openda_build_linux64_gnu.sh
if [ $? -gt 0 ]; then
        echo "error while building openda native"
        exit 1;
fi
export LIB64=public/core/native_bin/linux64_gnu/lib
cp $GCCDIR/lib64/libgfortran.so.5 $LIB64
cp $GCCDIR/lib64/libquadmath.so.0 $LIB64
cd public
ant tgz-native
