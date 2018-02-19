#! /bin/bash

module(){ eval $(/usr/bin/modulecmd bash $*);};
 
export OPENDASCRIPTROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd ${OPENDASCRIPTROOT}/../..

export GCCDIR=/opt/gcc/4.9.2
#source /usr/share/Modules/init/bash
module load gcc
module load java/jdk_1.7_oracle
module load ant/1.9.4_jdk1.7
module load automake autoconf libtool
rm -rf public/bin/*
rm -rf public/*.tgz
public/scripts/openda_build_linux64_gnu.sh
if [ $? -gt 0 ]; then
        echo "error while building openda native"
        exit 1;
fi
export LIB64=public/core/native_bin/linux64_gnu/lib
cp $GCCDIR/lib64/libgfortran.so.3 $LIB64
cp $GCCDIR/lib64/libquadmath.so.0 $LIB64
cd public
ant tgz-native
