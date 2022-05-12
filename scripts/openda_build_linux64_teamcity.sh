#! /bin/bash

# initialize module system
module(){ eval $(/usr/bin/modulecmd bash $*);};

export OPENDASCRIPTROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd ${OPENDASCRIPTROOT}/../..

# remove old build targets
rm -rf public/bin/*
rm -rf public/*.tgz

# load dev dependencies for native
export GCCDIR=/opt/apps/gcc/9.2.0
module load gcc/9.2.0
module load ant/1.10.9
module load automake/1.16.2_gcc9.2.0
module load autoconf/2.69_gcc9.2.0
module load libtool/2.4.6_gcc9.2.0
# overwrite JAVA_HOME set by module load ant
# reason: incorrect installation of ant/java on TeamCity agents
export JAVA_HOME=/opt/apps/java/amazon-corretto-11.0.7.10.1-linux-x64/amazon-corretto-11.0.7.10.1-linux-x64

# build native
public/scripts/openda_build_linux64_gnu.sh
if [ $? -gt 0 ]; then
        echo "error while building openda native"
        exit 1;
fi

# copy fortran shared objects to core
export LIB64=public/core/native_bin/linux64_gnu/lib
cp $GCCDIR/lib64/libgfortran.so.5 $LIB64
cp $GCCDIR/lib64/libquadmath.so.0 $LIB64

# create tgz
cd public
ant tgz-native
