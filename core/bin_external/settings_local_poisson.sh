#! /bin/bash
#
# Local settings for poisson
#
echo "running settings_local_poisson.sh"
export arch=`uname -m`
if [ "$arch" == "x86_64" ]; then
	echo "64 bit version"
else
	echo "32 bit version"
fi

#MPI
#export MPI_ROOT=/opt/mpich2 
#export PATH=$MPI_ROOT/bin:$PATH


# SIMONA
# simona changes opendadir!!
export SAVED=$OPENDADIR
export SIMONADIR='/opt64/simona2013'
. $SIMONADIR/setsimonadir.sh -nodotcheck $SIMONADIR
export OPENDADIR=$SAVED

#openda
export PATH=$OPENDADIR:$PATH
echo "OPENDADIR=$OPENDADIR"
if [ "$arch" == "x86_64" ]; then
	export OPENDA_NATIVE=linux64_gnu
else
	export OPENDA_NATIVE=linux32_gnu
fi
export OPENDALIB=$OPENDADIR/$OPENDA_NATIVE/lib
echo "OPENDALIB=$OPENDALIB"
export LD_LIBRARY_PATH=$OPENDALIB:$LD_LIBRARY_PATH

# java
if [ "$arch" == "x86_64" ]; then
	export JAVA_HOME=/opt64/jdk
else
	export JAVA_HOME=/opt/jdk
fi
export PATH=$JAVA_HOME/bin:$PATH

# geen intel fortran geinstalleerd


#tag
export tag="poisson"

#eclipse
if [ "$arch" == "x86_64" ]; then
	export ECLIPSEDIR='/opt64/eclipse'
else
	export ECLIPSEDIR='/opt/eclipse'
fi
export eclipse_exe="$ECLIPSEDIR/eclipse"
alias eclipse="$eclipse_exe"

