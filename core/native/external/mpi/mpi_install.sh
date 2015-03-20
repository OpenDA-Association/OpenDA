#! /bin/bash
#
# Compile mpich libraries for linux or windows&mingw
#
# arguments:
#   ifort    : force ifort compiler
#   gnu      : force gfortran compiler
#   32       : force 32bit compilation
#   64       : force 64bit compilation
#   shared   : create shared libs
#   noshared : do not create shared libs
#
# Requirements: 
#
# On linux:
# - make sure you have the compilers you want to use (gcc gfortran/ifort) in the calling path
# On windows:
#   NOT TESTED!!!
#

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
#default for shared-libs support
export USESHARED="yes"

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
    shared*)    USESHARED="yes"
            ;;
    noshared*)    USESHARED="no"
            ;;
    esac
done
#recheck compilers
if [ "$FORT" == "ifort" ];then
	export IFORTPATH=`which ifort 2>/dev/null`
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
# MPICH
#
#export MPIVERSION='1.3.2p1'
export MPIVERSION='1.4.1'
export EXTRACTDIR="$BASE/mpich2-$MPIVERSION"
export MPIFILE="mpich2-${MPIVERSION}.tar.gz"
export MPIURL="http://www.mcs.anl.gov/research/projects/mpich2/downloads/tarballs/$MPIVERSION/mpich2-$MPIVERSION.tar.gz"
if [ ! -f "$MPIFILE" ]; then
	curl -o "$MPIFILE"  "$MPIURL"
	if [ $? -gt 0 ]; then
		echo "could not download mpich2"
		exit 1
	fi
fi

#
# Here we assume the tarball is already there.
#
export EXTRACTDIR="$BASE/mpich2-$MPIVERSION"
rm -rf $EXTRACTDIR
tar -xzf $MPIFILE


# compile in a temporary direcory to avoid mixing sources and objects
export TEMPDIR="mpich2_${SYSTEM}_temp"
rm -rf $TEMPDIR
mkdir $TEMPDIR
pushd $TEMPDIR


# we are going to build a shared library later so the option -fPIC is needed and
# --enable-shared triggers this option.
if [ "$ARCH" == "64" ]; then
	export archflag="-m64"
elif [ "$ARCH" == "32" ]; then
	export archflag="-m32"
else
	echo "unknown architecture ARCH=$ARCH"
	exit 1
fi

export SHAREDFLAGS=""
if [ "$USESHARED" == "yes" ]; then
	export SHAREDFLAGS="--enable-shared"
	if [ "$OSTYPE" == "win" ]; then
		export SHAREDFLAGS="--enable-shared --enable-dll"
	fi
else 
	export SHAREDFLAGS="--disable-shared"
fi
# install to dirs like linux32_ifort
# use lib also for 64bit
$EXTRACTDIR/configure --prefix=$BASE/$SYSTEM --libdir=$BASE/$SYSTEM/lib $SHAREDFLAGS --enable-cxx FFLAGS="-fPIC $archflag" CFLAGS="-fPIC $archflag" CXXFLAGS="-fPIC $archflag" FCFLAGS="-fPIC $archflag" FC="$MYFORT" F77="$MYFORT"

make
#make check #optional tests
make install

popd

#
# give info
# 
echo ""
echo "============================================================================"
echo "The mpi libs and tools were installed at $BASE/$SYSTEM."
echo "The other directories and files can be removed. To do this type:"
echo "rm -rf  *_temp *.tar.gz"
echo "============================================================================"
echo ""
