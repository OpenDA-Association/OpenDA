#! /bin/bash
#
# Compile OpenDA native libraries for linux or windows&mingw
# - also compiles mpich2 and netcdf by default
#
# arguments:
#   ifort       : force ifort compiler
#   gnu         : force gfortran compiler
#   32          : force 32bit compilation
#   64          : force 64bit compilation
#   ignorempi   : ignore existing mpi settings and compile mpich2 from source
#   noignorempi
#   ignorenetcdf: ignore existing netcdf installation and compile from source 
#   noignorenetcdf
#   debug       : compile for debugging
# Requirements: 
#
# On linux:
# - make sure you have the compilers you want to use (gcc gfortran/ifort) in the calling path
# On windows:
#   NOT TESTED!!!
#
## libxml2 should be installed including headers
## zypper in libxml2-devel
## for cross compilation to 32bit some additional packages are needed
# eg for opensuse11.1: gcc-32bit gcc-c++-32bit gcc-fortran-32bit libxml2-32bit
# libxml2-devel-32bit

#
#=============================================================================================================
#
#defaults for fortran compiler (=prefer ifort over gfortran)
export FORT=""
hash ifort >/dev/null 2>&1
if [ $? -eq 0 ]; then
	FORT=ifort
else 
	hash gfortran >/dev/null 2>&1
	if [ $? -eq 0 ]; then
		FORT=gnu
	else
		echo "No fortran compiler found"
		return 1
        fi
fi	
#defaults for 64/32bit (=look at architecture of this computer)
export RAWARCH=`uname -m`
if [ "$RAWARCH" == "x86_64" ]; then
   export ARCH=64
else
   export ARCH=32
fi
#defaults for mpi and netcdf support
export IGNOREMPI="no"
export IGNORENETCDF="no"

#test for windows mingw
export OSTYPE="unknown"
export TMPVAR=`uname -s|grep -i mingw`
if [ ! -z  "$TMPVAR" ]; then
	OSTYPE="win"
fi
export TMPVAR=`uname -s|grep -i linux`
if [ ! -z  "$TMPVAR" ]; then
	OSTYPE="linux"
fi

# debug flag
export DEBUG="no"
# parse arguments
for arg in "$@"
do
    case "$arg" in
    64*)    ARCH="64"
            ;;
    32*)    ARCH="32"
            ;;
    ifort*)    FORT="ifort"
            ;;
    gnu*)    FORT="gnu"
            ;;
    ignorempi*)    IGNOREMPI="yes"
            ;;
    ignorenetcdf*)    IGNORENETCDF="yes"
            ;;
    noignorempi*)    IGNOREMPI="no"
            ;;
    noignorenetcdf*)    IGNORENETCDF="no"
            ;;
    debug*)    DEBUG="yes"
            ;;
    esac
done
#recheck compilers
if [ "$FORT" == "ifort" ];then
	export IFORTPATH=$(which ifort 2>/dev/null)
	if [ ! -z "$IFORTPATH" ]; then
		export MYFORT="$IFORTPATH"
	else
		echo "No ifort compiler found"
		exit 1
	fi
fi
if [ "$FORT" == "gnu" ];then
	export GFORTRANPATH=`which gfortran 2>/dev/null`
	if [ ! -z "$GFORTRANPATH" ]; then
		export MYFORT="$GFORTRANPATH"
	else
		echo "No gfortran compiler found"
		exit 1
	fi
fi

export SYSTEM="${OSTYPE}${ARCH}_${FORT}"
echo "Architecture OS=$OSTYPE ARCH=$ARCH on system RAWARCH=$RAWARCH"
echo "Fortran compiler FORT=$FORT"
echo "path to compiler is MYFORT=$MYFORT"
echo "SYSTEM=$SYSTEM"
export BASE=$PWD
#
#=============================================================================================================
#

#
# NETCDF
# 
## netcdf when not available as binary
# go to external/netcdf to compile
if [ -z "$NETCDF_ROOT" ]; then
	echo "Using Netcdf provided with OpenDA."
	export NETCDF_ROOT=$BASE/external/netcdf/$SYSTEM
	if [ -d "$NETCDF_ROOT" ]; then	
		echo "Netcdf already compiled"
	else
		echo "Starting compilation of Netcdf"
		pushd $BASE/external/netcdf
		./netcdf_install.sh $ARCH $FORT shared | tee netcdf_install.log
		popd
	fi
	export PATH=$NETCDF_ROOT/bin:$PATH
else
	echo "Using existing netcdf: $NETCDF_ROOT"
fi

#
# MPI
#
## compile MPICH2 for this system if needed
# go to external/mpi so compile if this has not been done yet.
#
# here just refer to existing libraries
if [ -z "$MPI_ROOT" ]; then
	echo "Using mpich2 provided with OpenDA."
	export MPI_ROOT="$BASE/external/mpi/$SYSTEM"
	if [ -d "$MPI_ROOT" ]; then	
		echo "Mpich2 already compiled"
	else
		echo "Starting compilation of mpich2"
		pushd $BASE/external/mpi
		./mpi_install.sh $ARCH $FORT shared | tee mpi_install.log
		popd
	fi
	export MPI_LIBS="-L${MPI_ROOT}/lib -lmpich -lpthread -lrt -i-static"
	export MPI_RSH="rsh"
	export PATH=$MPI_ROOT/bin:$PATH
	export LD_LIBRARY_PATH=$MPI_ROOT/lib:$LD_LIBRARY_PATH
else
	echo "Using existing mpi: $MPI_ROOT"
fi


#
# JAVA
# 
if [ "$JAVA_HOME" == "" ]; then
    if [ -f /opt/java/include/jni.h ]; then
        export JAVA_HOME=/opt/java
    elif [ -f /usr/local/include/jni.h ]; then
        export JAVA_HOME=/usr/local
    else
        echo "Please set the environment variable JAVA_HOME to the directory that"
        echo "contains the Java development kit"
        exit
    fi

    echo "JAVA_HOME set to $JAVA_HOME"
fi

#
# OpenDA settings
#
pushd $BASE
$BASE/autoreconf_fix.sh
popd

# compile in a temporary direcory to avoid mixing sources and objects
export TEMPDIR="openda_${SYSTEM}_temp"
rm -rf $TEMPDIR
mkdir $TEMPDIR
pushd $TEMPDIR

if [ "$ARCH" == "64" ]; then
	export ARCHFLAG="-m64"
elif [ "$ARCH" == "32" ]; then
	export ARCHFLAG="-m32"
else
	echo "unknown architecture ARCH=$ARCH"
	exit 1
fi
if [ "$DEBUG" == "no" ]; then
	export OPTFLAG=""
else
	export OPTFLAG="-g"
fi

if [ -d "$BASE/$SYSTEM" ];then
	rm -rf $BASE/$SYSTEM
fi


# mpi compiler wrappers are the prefered option for configure and we have added them to the PATH above
$BASE/configure --prefix=$BASE/$SYSTEM --libdir=$BASE/$SYSTEM/lib --disable-system-lapack --disable-system-blas --disable-system-sqlite3 --with-netcdf=$NETCDF_ROOT --with-jdk=$JAVA_HOME CFLAGS="$ARCHFLAG $OPTFLAG" FFLAGS="$ARCHFLAG $OPTFLAG" LDFLAGS="$ARCHFLAG" CPPFLAGS="$ARCHFLAG $OPTFLAG" >myconfig.log 2>&1
if [ $? -gt 0 ]; then
	echo "error in configure openda"
	exit 1;
fi

make >mymake.log 2>&1
if [ $? -gt 0 ]; then
	echo "error in make openda"
	exit 1;
fi
#make -j4 |tee mymake.log
#make testing #optional
make install > mymakeinstall.log 2>&1
if [ $? -gt 0 ]; then
	echo "error in install openda"
	exit 1;
fi

popd

#
#collect output in native_bin directory
#
if [ ! -d "$BASE/../native_bin/$SYSTEM" ]; then
	mkdir -p $BASE/../native_bin/$SYSTEM
fi
#mpi
rsync -ruav $BASE/external/mpi/$SYSTEM/ $BASE/../native_bin/$SYSTEM/
#workaround: !!! teamcity does not like +'s and mpicxx is indentical
rm -f $BASE/../native_bin/$SYSTEM/bin/mpic++
#netcdf
rsync -ruav $BASE/external/netcdf/$SYSTEM/ $BASE/../native_bin/$SYSTEM/
#openda
rsync -ruav $BASE/$SYSTEM/ $BASE/../native_bin/$SYSTEM/
