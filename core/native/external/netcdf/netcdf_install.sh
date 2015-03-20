#! /bin/bash
#
# Compile netcdf libraries for linux or windows&mingw
#
# arguments:
#   ifort    : force ifort compiler
#   gnu      : force gfortran compiler
#   32       : force 32bit compilation
#   64       : force 64bit compilation
#   netcdf4  : include netcdf4 support
#   dap      : include opendap support
#
# Requirements: 
#
# On linux:
# - make sure you have the compilers you want to use (gcc gfortran/ifort) in the calling path
# - for dap: have curl installed or sources available in current directory (typically with your package manager)
# - for netcdf4: have zlib installed inluding the header files (typically with your package manager)
# On windows:
# - Install gnu compiler through mingw : 
#   http://sourceforge.net/projects/mingw/files/Installer/mingw-get-inst/
#   make sure to include msys in the installation (safest is to select install all components)
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
#default for netcdf4 support (=no)
export USENETCDF4="no"
export USEDAP="no"
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
    netcdf4*)    USENETCDF4="yes"
            ;;
    dap*)    USEDAP="yes"
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
# curl (for opendap and to download sources)
#
# http://curl.haxx.se/download/curl-7.25.0.tar.gz

#
# HDF5
# 
if [ "$USENETCDF4" == "yes" ]; then
	export HDFVERSION='1.8.7'
	export HDFFILE="hdf5-${HDFVERSION}.tar.gz"
	export HDFURL="http://www.hdfgroup.org/ftp/HDF5/hdf5-1.8.7/src/hdf5-${HDFVERSION}.tar.gz"
	if [ ! -f "$HDFFILE" ]; then
		curl -o $HDFFILE $HDFURL
		if [ $? -gt 0 ]; then
			echo "could not download hdf"
			exit 1
		fi
	fi
	#
	# Here we assume the tarball is already there.
	#
	export EXTRACTDIR="$BASE/hdf5-$HDFVERSION"
	rm -rf $EXTRACTDIR
	tar -xzf $HDFFILE
	
	# compile in a temporary direcory to avoid mixing sources and objects
	export HDFTEMPDIR="hdf_${SYSTEM}_temp"
	rm -rf $HDFTEMPDIR
	mkdir $HDFTEMPDIR
	pushd $HDFTEMPDIR
	
	if [ "$ARCH" == "64" ]; then
		export archflag="-m64"
	elif [ "$ARCH" == "32" ]; then
		export archflag="-m32"
	else
		echo "unknown architecture ARCH=$ARCH"
		exit 1
	fi
	   $EXTRACTDIR/configure --prefix=$BASE/hdf_${SYSTEM} --libdir=$BASE/hdf_${SYSTEM}/lib  CFLAGS="-fPIC $archflag" CXXFLAGS="-fPIC $archflag" 
	
	make
	#make check #optional tests
	make install
	
	popd
fi

#
# NETCDF
#
export NETCDFVERSION='4.1.1'
export NETCDFFILE="netcdf-${NETCDFVERSION}.tar.gz"
export NETCDFURL="http://www.unidata.ucar.edu/downloads/netcdf/ftp/netcdf-${NETCDFVERSION}.tar.gz"
if [ ! -f "$NETCDFFILE" ]; then
	curl -o "$NETCDFFILE"  "$NETCDFURL"
	if [ $? -gt 0 ]; then
		echo "could not download netcdf"
		exit 1
	fi
fi

#
# Here we assume the tarball is already there.
#
export EXTRACTDIR="$BASE/netcdf-$NETCDFVERSION"
rm -rf $EXTRACTDIR
tar -xzf $NETCDFFILE

#
# download and apply for windows patch if on windows
#
if [ "$OSTYPE" == "win" ]; then
	export NETCDFPATCHURL=http://www.paratools.com/Azure/NetCDF?action=AttachFile&do=get&target=paratools-netcdf-4.1.3.patch""
	# this patch was edited manualy so it can be applied to 4.1.1
	export NETCDFPATCHFILE="paratools-netcdf-4.1.1.patch"
	#TODO download does not work
	if [ ! -f "$NETCDFPATCHFILE" ]; then
		echo "Download of netcdf patch file does not work yet, sorry."
		exit 1
		curl -o "$NETCDFFILE"  "$NETCDFURL"
		if [ $? -gt 0 ]; then
			echo "could not download netcdf"
			exit 1
		fi
	fi
	pushd $EXTRACTDIR
	patch -p1 < ../${NETCDFPATCHFILE}
	popd
fi

# compile in a temporary direcory to avoid mixing sources and objects
export TEMPDIR="netcdf_${SYSTEM}_temp"
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
export NETCDF4FLAGS="--disable-netcdf4"
if [ "$USENETCDF4" == "yes" ]; then
	export NETCDF4FLAGS="--enable-netcdf4 --with-hdf5=$BASE/hdf_${SYSTEM} "
fi
export DAPFLAGS=""
if [ "$USEDAP" == "yes" ]; then
	export DAPFLAGS="--enable-dap"
else 
	export DAPFLAGS="--disable-dap"
	#TODO??? --disable-cdmremote
fi
export sharedlibflag="--enable-shared"
if [ "$OSTYPE" == "win" ]; then
	sharedlibflag="--enable-shared --enable-dll"
	#sharedlibflag=""
fi
# install to dirs like linux32_ifort
# use lib also for 64bit
$EXTRACTDIR/configure --prefix=$BASE/$SYSTEM --libdir=$BASE/$SYSTEM/lib $NETCDF4FLAGS $DAPFLAGS $sharedlibflag --with-pic --disable-cxx FFLAGS="-fPIC $archflag" CFLAGS="-fPIC $archflag" CXXFLAGS="-fPIC $archflag" FCFLAGS="-fPIC $archflag" FC="$MYFORT"

make
#make check #optional tests
make install

popd

#
# copy hdf5 libs to netcdf and delete hdf dirs
#
if [ "$USENETCDF4" == "yes" ]; then
	if [ -d  $BASE/$SYSTEM/lib ]; then
		rsync -ruav $BASE/hdf_${SYSTEM}/lib/ $BASE/$SYSTEM/lib/
	else
		rsync -ruav $BASE/hdf_${SYSTEM}/lib/ $BASE/$SYSTEM/lib64/
	fi
fi

#
# give info
# 
echo ""
echo "============================================================================"
echo "The netcdf libs and tools were installed at $BASE/$SYSTEM."
echo "The other directories and files can be removed. To do this type:"
echo "rm -rf hdf5* *_temp *.tar.gz"
echo "============================================================================"
echo ""
