#! /bin/bash
#
# This is de default settings file for Linux
#
echo "running settings_local_linux.sh"
export arch=`uname -m`

#MPI
#export MPI_ROOT=/opt/mpich2 
#export PATH=$MPI_ROOT/bin:$PATH

# java
#export JAVA_HOME=/opt/jdk
#export PATH=$JAVA_HOME/bin:$PATH

# SIMONA
# No simona installed
#export SIMONADIR='/opt/simona2009'
#. $SIMONADIR/setsimonadir.sh -nodotcheck $SIMONADIR

#openda
export PATH=$OPENDADIR:$PATH
if [ "$(arch)" == "x86_64" ]; then
        echo "64 bit version"
	export OPENDA_NATIVE=linux64_gnu
else
        echo "32 bit version"
	export OPENDA_NATIVE=linux32_gnu
fi
export OPENDALIB=$OPENDADIR/$OPENDA_NATIVE/lib
export LD_LIBRARY_PATH=$OPENDALIB/lib:$LD_LIBRARY_PATH
# There should be no intel fortran active

#tag
export tag="linux"

#eclipse
#export eclipse_exe=`which eclipse`
