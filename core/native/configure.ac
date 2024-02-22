#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#
# $URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/configure.ac $
# $Revision: 4362 $, $Date: 2014-03-05 10:55:58 +0100 (Wed, 05 Mar 2014) $

AC_PREREQ(2.59)
AC_INIT(costa, 0.1.0, bugs@costapse.org)
AM_INIT_AUTOMAKE

#set main directory
AC_CONFIG_SRCDIR(src)
AC_MSG_NOTICE([srcdir=${srcdir}])
# starting point for source files
relpath=`dirname $0`
OPENDA_NATIVE_DIR=`cd $relpath; pwd`
AC_SUBST(OPENDA_NATIVE_DIR)
# starting point for targets (objects, makefiles, etc)
OPENDA_BUILD_DIR=${PWD}

# Check what operating system we are running
os_arch="`uname`"
echo "Configuring for operating system: $os_arch"

# Determine the operating system for which we are compiling
#
# -This will influence the default installation location.
# -The user can set the OSTYPE using the variable OPENDA_NATIVE (only for LINUX)
#  TODO make nice configure flag for "OPENDA_NATIVE"
#
OSTYPE="UNKNOWN_OS"
if test "$os_arch" = "Darwin"; then
   CPPFLAGS="$CPPFLAGS -DMAC_OS_X"
   OSTYPE="darwin"
   ac_default_prefix="`pwd`/../native_bin/darwin"
else
   # Default is to install inside the installation
   if test "$OPENDA_NATIVE" != ""; then
      OSTYPE="$OPENDA_NATIVE"
      ac_default_prefix="`pwd`/../native_bin/$OPENDA_NATIVE"
   else
      if test "$HOSTTYPE" = "x86_64"; then
         OSTYPE="linux64_gnu"
         ac_default_prefix="`pwd`/../native_bin/linux64_gnu"
      else
         OSTYPE="linux32_gnu"
         ac_default_prefix="`pwd`/../native_bin/linux32_gnu"
      fi
   fi
fi
AC_PREFIX_DEFAULT([$default_prefix])


# Checks for programs:

AC_PROG_CC(mpicc gcc icc cc)
AM_PROG_CC_STDC
AC_C_CONST       #we use const in the code

AC_PROG_CXX(mpic++,g++, icc)

AC_PROG_FC(mpif90 pgf95 ifort mpif90 pgf90 lf95 f90 gfortran f95 )

# We allow all fortran compilers
#AC_PROG_FC

# now it is important that the f77 compiler is the same!
#BASE_FC=naamfc=`echo $FC  | sed "s/^.*\///"   `
BASE_FC=`basename $FC`
#FC=$naamfc
F77=$FC
AC_PROG_F77($BASE_FC)

AC_PROG_LIBTOOL

# There are some problems recognizing the correct flags for LIBTOOL when mpiwrappers are used.
# For now we only add extra detection for the Intel Fortran compiler but if similar probles are found
# for other compilers as well we can always extend this section.
# We have only tested this detection method for MPICH2 and OpenMPI
if test "$BASE_FC" = "mpif90"; then
   AC_MSG_RESULT([MPI wrapper used for fortran. Checking whether it wraps the Intel Fortran compiler])
   output=`$FC -show`
   compiler=`echo $output | cut -d ' ' -f1 `
   compiler=`basename $compiler`
   if test "$compiler" = "ifort"; then
      AC_MSG_RESULT([Intel Fortran detected set correct libtool flags])
      lt_prog_compiler_wl_FC='-Wl,' 
      lt_prog_compiler_wl_F77='-Wl,'
      AC_SUBST(lt_prog_compiler_wl_FC)
      AC_SUBST(lt_prog_compiler_wl_F77)

      lt_prog_compiler_pic_FC='-fPIC' 
      lt_prog_compiler_pic_F77='-fPIC'
      AC_SUBST(lt_prog_compiler_pic_FC)
      AC_SUBST(lt_prog_compiler_pic_F77)

      lt_prog_compiler_static_FC='-static' 
      lt_prog_compiler_static_F77='-static'
      AC_SUBST(lt_prog_compiler_static_FC)
      AC_SUBST(lt_prog_compiler_static_F77)
  else
      AC_MSG_RESULT([Intel Fortran is not detected])
  fi
fi

AC_CONFIG_MACRO_DIR([m4])

# Add default library paths
#LDFLAGS="$LDFLAGS -L$OPENDA_NATIVE_DIR/../external/lib"
#MVL LDFLAGS="$LDFLAGS -L$OPENDA_NATIVE_DIR/src/cta"
LDFLAGS="$LDFLAGS -L$OPENDA_BUILD_DIR/src/cta"

# Add default include paths
# MVL TODO no such directory!
#MVL CPPFLAGS="$CPPFLAGS -I`pwd`/../external/include" 
CPPFLAGS="$CPPFLAGS -I$OPENDA_NATIVE_DIR/include" 


# Check header for dynamic loading of functions
AC_CHECK_HEADERS(dlfcn.h,[],[
      AC_MSG_RESULT([Header file dlfcn.h is not found. Dynamic loading of
      functions is disabled supported])])

AC_CHECK_LIB([dl],[dlopen],,AC_MSG_ERROR([cannot find libdl]))

#
# BLAS
#
# Note: there are several options
# - user pointed at a specific library
# - search system for installed libraries
# - use version distributed with openda
AC_ARG_ENABLE(system-blas,  [  --disable-system-blas   compile without system blas, use OpenDA BLAS],[disable_system_blas="yes"],[disable_system_blas="no"] )

if test "x$disable_system_blas" = "xyes"; then
   BLAS_DIR="blas"; #used to select for compilation in external dir
   INT_BLAS_VERSION="NON-OPTIMIZED OpenDA BLAS"
   #MVL BLAS_LIBS=
   LTL_BLAS_ADD="../../external/blas/libblas.la"
   #BLAS_LIBS="-lblas" #MVL experiment
   BLAS_LIBS=""
else
   ACX_BLAS([INT_BLAS_VERSION="$BLAS_LIBS"],
   [INT_BLAS_VERSION="NO BLAS FOUND"])
   if test "$INT_LAPACK_VERSION" = "NO BLAS FOUND"; then
      AC_MSG_ERROR(
[No BLAS version can be found on your system that can be used.
There are a number of ways to fix this:
1) When BLAS is installed on your system but could not be found by configure you can specify the full path to your BLAS (see ./configure --help for details).
2) When BLAS is not installed on your system try to find and install an optimized(!) BLAS library. E.g. ATLAS (free) or a commercial BLAS distribution like intel MKL. Alternatively you can used the reference BLAS code that is part of the OpenDA distribution. Using the reference BLAS is not recommended because it might not be the latest version and the performance is poor compared to an optimized BLAS library. You can use the reference BLAS of OpenDA by specifying --disable-system-blas])
   fi
fi

AC_MSG_NOTICE([BLAS_LIBS = $BLAS_LIBS])
AC_MSG_NOTICE([LTL_BLAS_ADD = $LTL_BLAS_ADD])
AC_MSG_NOTICE([LIBS = $LIBS])
AC_MSG_NOTICE([DEFS = $DEFS])
AC_SUBST(LTL_BLAS_ADD)
AC_SUBST(BLAS_DIR)

#
# LAPACK
#

AC_ARG_ENABLE(system-lapack,[  --disable-system-lapack compile without system lapack, use OpenDA LAPACK],[disable_system_lapack="yes"],[disable_system_lapack="no"] )

if test "x$disable_system_lapack" = "xyes"; then
   LAPACK_DIR="lapack"; 
   INT_LAPACK_VERSION="NON-OPTIMIZED OpenDA LAPACK"
   LTL_LAPACK_ADD="../../external/lapack/liblapack.la"
   #LAPACK_LIBS="-llapack" #MVL experiment
   LAPACK_LIBS=""
else
   ACX_LAPACK([INT_LAPACK_VERSION="$LAPACK_LIBS"],
     [INT_LAPACK_VERSION="NO LAPACK FOUND"])
   if test "$INT_LAPACK_VERSION" = "NO LAPACK FOUND"; then
      AC_MSG_ERROR(
[No LAPACK version can be found on your system that can be used.
There are a number of ways to fix this:
1) When LAPACK is installed on your system but could not be found by configure you can specify the full path to your LAPACK (see ./configure --help for details).
2) When LAPACK is not installed on your system try to find and install an optimized(!) LAPACK library. Alternatively you can used the LAPACK code that is part of the OpenDA distribution. Using the reference LAPACK is not recommended because it might not be the latest version. You can use the LAPACK of OpenDA by specifying --disable-system-lapack])
   fi
fi

AC_MSG_NOTICE([LAPACK_LIBS = $LAPACK_LIBS])
AC_MSG_NOTICE([LTL_LAPACK_ADD = $LTL_LAPACK_ADD])
AC_MSG_NOTICE([LIBS = $LIBS])
AC_MSG_NOTICE([DEFS = $DEFS])
AC_SUBST(LTL_LAPACK_ADD)
AC_SUBST(LAPACK_DIR)
#MVL AC_MSG_ERROR([stop here for debugging])

#
# Look for Tcl scripting language
# 
AC_MSG_CHECKING([for tcl])
AC_CHECK_LIB([tcl],Tcl_Init,[use_tcl=yes],[use_tcl=no])
if test "x$use_tcl" = "xyes"; then
   AC_MSG_RESULT([tcl is found])
   AC_CHECK_LIB([tcl],Tcl_Init,[],[])
fi

AC_MSG_NOTICE([CPPFLAGS = $CPPFLAGS])
AC_MSG_NOTICE([LDFLAGS = $LDFLAGS])
AC_MSG_NOTICE([LIBS = $LIBS])
AC_MSG_NOTICE([DEFS = $DEFS])
AC_MSG_NOTICE([LIBS = $LIBS])
AC_MSG_NOTICE([LIBS_EXT = $LIBS_EXT])

#
# Check for libxml2
#

AC_PATH_PROG([xml2config],[xml2-config])
if test "x$xml2config" = "x"; then
   AC_MSG_ERROR([cannot find program xml2-config])
fi
CPPFLAGS="$CPPFLAGS `$xml2config --cflags`"
LIBS="`$xml2config --libs` $LIBS"
# End of libxml2 checks

# Handle MPI-disto

# User can disable use of MPI
AC_ARG_ENABLE(mpi,[  --disable-mpi           compile without mpi],[disable_mpi="yes"],[disable_mpi="no"] )



#User option: Compile with or without examples
AC_ARG_ENABLE(models,  [  --disable-models   compile without the native example models. The models are not needed but some of
the tests will fail when the example models are not compiled],[disable_models="yes"],[disable_models="no"] )
if test "x$disable_models" == "xno"; then
   INT_EXAMPLE_MODELS="YES"
   EXAMPLE_DIR=models
   AC_SUBST(EXAMPLE_DIR)
else
   INT_EXAMPLE_MODELS="NO"
fi



# User does not want to disable MPI
if test "x$disable_mpi" == "xno"; then
   INT_USE_MPI="YES"
   AC_DEFINE(USE_MPI)
# compile pollution model as well
   POLLUTE2D_DIR=pollute2d
   AC_SUBST(POLLUTE2D_DIR)


if test "x$FC" = "xmpif90" -a "x$CC" = "xmpicc" ; then
   AC_MSG_RESULT([Using MPI-COMPILERS no need to configure MPI libraries])
else        
   AC_MSG_CHECKING([for system variable \$MPI_ROOT])
   if test "x$MPI_ROOT" != "x"; then
      AC_SUBST(MPI_ROOT)
      CPPFLAGS="-I$MPI_ROOT/include $CPPFLAGS"
      AC_SUBST(CPPFLAGS)
      LDFLAGS="$LDFLAGS -L$MPI_ROOT/lib $CPPFLAGS"
      AC_SUBST(LDFLAGS)
      FCFLAGS="$FCFLAGS -I$MPI_ROOT/include"
      AC_SUBST(FFLAGS)
      FFLAGS="$FFLAGS -I$MPI_ROOT/include"
      AC_SUBST(FFLAGS)
   else
      AC_MSG_RESULT([
      System variable \$MPI_ROOT is not found.
      Try to find MPI libraries and includes on default paths.])
   fi

   # LAM-MPI
   AC_MSG_CHECKING([for lam-mpi])
   AC_CHECK_LIB([lam],[_kio],[use_lam=yes],[use_lam=no])
   if test "x$use_lam" = "xyes"; then
      AC_MSG_RESULT([LAM-MPI is found])
   else
      AC_MSG_RESULT([LAM-MPI is not found assuming MPICH2])
   fi

   AC_CHECK_HEADER(mpi.h,[],[
			  AC_MSG_ERROR([mpi was found, but cannot locate header file mpi.h. Perhaps a mpi-devel package is needed?])])
      
   if test "x$use_lam" = "xyes"; then
      # We are using LAM check add all libs needed to link
      AC_CHECK_LIB([dl],[dlopen],,AC_MSG_ERROR([cannot find libdl.a]))
      AC_CHECK_LIB([pthread],[pthread_create],,AC_MSG_ERROR([cannot find libpthread.a]))

      AC_CHECK_LIB([lam],[_kio],,AC_MSG_ERROR([cannot find liblam.a]))
      AC_CHECK_LIB([mpi],[MPI_Init],,AC_MSG_ERROR([cannot find libmpi.a]))
      AC_CHECK_LIB([lammpio],[MPI_File_read],,AC_MSG_ERROR([cannot find liblammpio.a]))
      AC_CHECK_LIB([lammpi++],[MPI_Init],,AC_MSG_ERROR([cannot find liblampi++.a]))
      # !DIT IS WEL WAT SPECIFIEK!!!!
      AC_CHECK_LIB([lamf77mpi],[mpi_init_],,AC_MSG_ERROR([cannot find liblamf77mpi.a]))
   else
      # assume MPICH2
      AC_CHECK_LIB([pthread],[pthread_create],,AC_MSG_ERROR([cannot find libpthread]))
      AC_CHECK_LIB([mpich],[MPI_Init],[use_mpich2=yes],[use_mpich2=no])
      if test "x$use_mpich2" = "xyes"; then
         AC_CHECK_LIB([mpich],[MPI_Init],,AC_MSG_ERROR([cannot find libmpich]))
      else
         AC_CHECK_LIB([mpi],[MPI_Init],,AC_MSG_ERROR([cannot find lam mpich or libmpi.a]))
      fi
   fi
fi
else
   INT_USE_MPI="NO"
   AC_MSG_WARN(MPI is disabled. Parallel model builder does not work!)
fi

#
# Checks for NETCDF
#
# User gives path to NETCDF distro using --with-netcdf (overrules --disable-netcdf)
INT_NETCDF="todo";
INT_NETCDF_VERSION="NO"
AC_ARG_WITH(netcdf,
              AS_HELP_STRING([--with-netcdf=PATH],
                             [Try to use 'PATH/include' to find the NETCDF
                              headers and 'PATH/lib' to find the NETCDF libraries.]),
      [
         CPPFLAGS="-I$withval/include $CPPFLAGS"
         AC_SUBST(CPPFLAGS)
         LDFLAGS="$LDFLAGS -L$withval/lib $CPPFLAGS"
         AC_SUBST(LDFLAGS)
         AC_CHECK_LIB([netcdf],[nc_create],[],[])
         INT_NETCDF_VERSION="User (set by --with-netcdf)"
         INT_NETCDF="done";
      ])

# check whether user enables/disables NETCDF
AC_ARG_ENABLE(netcdf,  [  --disable-netcdf   compile without netcdf support. Writing to netcdf will not be supported as a results],[disable_netcdf="yes"],[disable_netcdf="no"] )

echo "test1 $disable_netcdf $INT_NETCDF"
if test "x$disable_netcdf" = "xno" -a "$INT_NETCDF" = "todo"; then
   echo "test2"
   INT_NETCDF_VERSION="system netcdf"

   # The user has set the flag NETCDF_ROOT Add includes and libraries to path
   if test "x$NETCDF_ROOT" != "x"; then
      AC_SUBST(NETCDF_ROOT)
      CPPFLAGS="-I$NETCDF_ROOT/include $CPPFLAGS"
      AC_SUBST(CPPFLAGS)
      LDFLAGS="$LDFLAGS -L$NETCDF_ROOT/lib $CPPFLAGS"
      AC_SUBST(LDFLAGS)
      AC_CHECK_LIB([netcdf],[nc_create],[],[])
   fi

   #check whether netcdf distro can be used
   AC_CHECK_LIB([netcdf],[nc_create],[USE_INT_NETCDF="no"],[USE_INT_NETCDF="yes"])
   if test "$USE_INT_NETCDF" = "no"; then
     AC_CHECK_LIB([netcdf],[nc_create],[],[])
     AC_CHECK_HEADER(netcdf.h,[USE_INT_NETCDF="no"],[USE_INT_NETCDF="yes"])
   fi


   # #use internal netcdf version
   if test "$USE_INT_NETCDF" = "yes"; then
      INT_NETCDF_VERSION="OpenDA precompiled netcdf"
      CPPFLAGS="-I$OPENDA_NATIVE_DIR/external/netcdf/$OSTYPE/include $CPPFLAGS"
      AC_SUBST(CPPFLAGS)
      LDFLAGS="$LDFLAGS -L$OPENDA_NATIVE_DIR/external/netcdf/$OSTYPE/lib $CPPFLAGS"
      AC_SUBST(LDFLAGS)
      #I have no idea how to avoid caching or force a check. Therefore we now test for an alternative function
      AC_CHECK_LIB([netcdf],[nc_open],[],
                   [AC_MSG_ERROR(Cannot link to precompiled OpenDA nedcdf library. It might be possible you are using a 64-bit compiler. Please compile your onwn netcdf libraries and set the variable NETCDF_ROOT accordingly)])
   fi
fi

if test "x$INT_NETCDF_VERSION" != "xNO"; then
   CPPFLAGS="-DHAVE_LIBNETCDF=1 $CPPFLAGS"
   AC_SUBST(CPPFLAGS)
fi
AC_MSG_NOTICE([INT_NETCDF_VERSION=$INT_NETCDF_VERSION])
AC_MSG_NOTICE([LIBS=$LIBS])
AC_MSG_NOTICE([LIBS_EXT=$LIBS_EXT])
AC_MSG_NOTICE([CPPFLAGS=$CPPFLAGS])
AC_MSG_NOTICE([LDFLAGS=$LDFLAGS])
#AC_MSG_ERROR([-- stop here for debugging --])
# end checking for NETCFD


AC_SUBST(LIBS_EXT)

JAVA_OLDEST_WORKING_VER='1.5'
SVN_CHECK_JDK([$JAVA_OLDEST_WORKING_VER])
INT_OPENDA_BRIDGE="YES"
BRIDGE_DIR="bridge"
AC_SUBST(BRIDGE_DIR)
if test "x$JNI_INCLUDES" == "xnone"; then
AC_MSG_WARN([
    Could not the Java Native Interface includes. The bridge with
    OpenDa cannot be compiled. When JDK is installed on your
    system but not found you can set the system variable
    JAVA_HOME to your JDK installation)])
else
CPPFLAGS="$CPPFLAGS $JNI_INCLUDES"
INT_OPENDA_BRIDGE="YES"
fi



# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_CONFIG_FILES([Makefile \
                 external/Makefile \
                 external/blas/Makefile \
                 external/lapack/Makefile \
                 src/Makefile \
                 src/cta/Makefile\
                 src/cta_f90/Makefile\
                 src/cta_f90/generated/Makefile\
                 src/openda/Makefile \
                 src/openda/bridge/Makefile \
                 models/Makefile \
                 models/advec1d/Makefile \
                 models/advec1d/src/Makefile \
                 models/heat_stoch/Makefile \
                 models/heat_stoch/src/Makefile \
                 models/oscill/Makefile \
                 models/oscill/src/Makefile \
                 models/pollute2d/Makefile \
                 models/pollute2d/src/Makefile \
                 models/resultwriter/Makefile \
                 models/resultwriter/src/Makefile \
                 ])
AC_OUTPUT

echo [ ]
echo [Configuration info:]
echo ["C-compiler       :$CC"]
echo ["C++-compiler     :$CXX"]
echo ["Fortran-compiler :$FC"]
echo ["MPI-support      :$INT_USE_MPI"]
echo ["BLAS version     :$INT_BLAS_VERSION"]
echo ["LAPACK version   :$INT_LAPACK_VERSION"]
echo ["NETCDF  version  :$INT_NETCDF_VERSION"]
echo ["OpenDA binding   :$INT_OPENDA_BRIDGE"]
echo ["Example models   :$INT_EXAMPLE_MODELS"]

echo [ ]

echo [Build native part of OpenDA bij typing: \"make\"]
echo [Test your build by typing: \"make check\"]
echo [Install the compiled libraries by typing: \"make install\"]

#AC_MSG_NOTICE([LIBS=$LIBS])
#AC_MSG_NOTICE([LIBS_EXT=$LIBS_EXT])
#AC_MSG_NOTICE([CPPFLAGS=$CPPFLAGS])
#AC_MSG_NOTICE([LDFLAGS=$LDFLAGS])
#AC_MSG_NOTICE([DEFS=$DEFS])
