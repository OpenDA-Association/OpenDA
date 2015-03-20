#! /bin/bash
#
# argument:
# $1 : type of build is one of
# linux32_gnu 
# linux64_gnu
# linux32_ifort 
# linux64_ifort
#
# Compile native code for a linux system
#
# requirements:
# * Current directory should contain a directory public as checked out from the repos!
# - install: (apt-get install <prog> )
#  - g++ gfortan automake
#  - sun-java6-jdk
#  - libxml2-dev
#  - ant subversion
#  - settings
#  - java: export JAVA_HOME=/usr/lib/jvm/java-6-sun
#          export PATH=$JAVA_HOME/bin:$PATH

# argument $1
if [ -z "$1" ]; then
	echo "This stript needs an argument, eg linux64_gnu"
	exit 1
fi

# Get info about this system
export arch=`uname -m`
if [ ! "$arch" == "x86_64" ]; then
	if [ "$1" == "linux64_gnu"]; then
		echo "compilation for 64bit on a 32bit computer is not supported!"
		exit 1
	fi
	if [ "$1" == "linux64_ifort"]; then
		echo "compilation for 64bit on a 32bit computer is not supported!"
		exit 1
	fi
fi
export SYSTEM=$1;
echo "SYSTEM = $SYSTEM"

if [ ! -f $JAVA_HOME/include/jni.h ]; then
   echo You need to set the environment variable JAVA_HOME
   echo so that the interface libraries can be built
   echo Current value: JAVA_HOME = \"$JAVA_HOME\"
   exit
fi

# this dir is the toplevel dir
if [ -d public ]; then
  export TOPDIR=$PWD
fi
if [ -d openda ]; then
  export TOPDIR=$PWD/openda
fi

# create target dir if it does not exist
if [ ! -d $TOPDIR/public/core/native_bin/$SYSTEM/bin ]; then
   rm -rf $TOPDIR/public/core/native_bin/$SYSTEM/*
   mkdir -p $TOPDIR/public/core/native_bin/$SYSTEM/bin
   mkdir -p $TOPDIR/public/core/native_bin/$SYSTEM/sbin
   mkdir -p $TOPDIR/public/core/native_bin/$SYSTEM/etc
   mkdir -p $TOPDIR/public/core/native_bin/$SYSTEM/share
   mkdir -p $TOPDIR/public/core/native_bin/$SYSTEM/lib
   mkdir -p $TOPDIR/public/core/native_bin/$SYSTEM/include
fi

# now compile openda native for linux 
echo "going to dir $TOPDIR/public/core/native"
pushd $TOPDIR/public/core/native
if [ "$SYSTEM" == "linux64_gnu" ]; then
	echo "native compilation for $SYSTEM"
	echo "./linux_install.sh gnu 64 ignorempi ignorenetcdf"
	./linux_install.sh gnu 64 ignorempi ignorenetcdf
fi
if [ "$SYSTEM" == "linux32_gnu" ]; then
	echo "native compilation for $SYSTEM"
	echo "./linux_install.sh gnu 32 ignorempi ignorenetcdf"
	./linux_install.sh gnu 32 ignorempi ignorenetcdf
fi
if [ "$SYSTEM" == "linux64_ifort" ]; then
	echo "native compilation for $SYSTEM"
	echo "./linux_install.sh ifort 64 ignorempi ignorenetcdf"
	./linux_install.sh ifort 64 ignorempi ignorenetcdf
fi
if [ "$SYSTEM" == "linux32_ifort" ]; then
	echo "native compilation for $SYSTEM"
	echo "./linux_install.sh ifort 32 ignorempi ignorenetcdf"
	./linux_install.sh ifort 32 ignorempi ignorenetcdf
fi
popd

# and the Java libraries
#pushd $TOPDIR/public
#ant build
#popd

# create a suitable local settings script
if [ ! -f $TOPDIR/public/core/bin_external/settings_local_`hostname -s`.sh ];then
   sed -e "s/SYSTEM/$SYSTEM/" $TOPDIR/public/core/bin_external/settings_local_base.sh > $TOPDIR/public/core/bin_external/settings_local_`hostname -s`.sh
fi

# just let the user know we are done
echo ---------------------
echo Native build complete
echo ---------------------
echo Post-build actions:
echo Edit the file settings_local_`hostname`.sh for any additional
echo settings you need
echo It is located in the $TOPDIR/openda/public/core/bin_external directory
echo Run ant build to compile java part
