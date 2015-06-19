#! /bin/bash
#
# Local settings for winnie
#
echo "running settings_local_winnie.sh"
export arch=`uname -m`
if [ "$arch" == "x86_64" ]; then
	echo "64 bit version"
else
	echo "32 bit version"
fi

#MPI
#export MPI_ROOT=/opt/mpich2 
#export PATH=$MPI_ROOT/bin:$PATH

# java
if [ "$arch" == "x86_64" ]; then
	#export JAVA_HOME=/opt64/jdk
	export JAVA_HOME=/opt64/jdk1.6.0_25
else
	export JAVA_HOME=/opt/jdk
fi
export PATH=$JAVA_HOME/bin:$PATH

# SIMONA
#export BASE=/opt64/openda_simona_2012
#. $BASE/simona/setsimonadir.sh -nodotcheck -linux64 $BASE/simona


#openda
export PATH=$OPENDADIR:$PATH
if [ "$arch" == "x86_64" ]; then
	export OPENDA_NATIVE=linux64_gnu
else
	export OPENDA_NATIVE=linux32_gnu
fi
export OPENDALIB=$OPENDADIR/$OPENDA_NATIVE/lib

# geen intel fortran geinstalleerd


#tag
export tag="winnie"

#eclipse
if [ "$arch" == "x86_64" ]; then
	export ECLIPSEDIR='/opt64/eclipse_indigo'
else
	export ECLIPSEDIR='/opt/eclipse_indigo'
fi
export eclipse_exe="$ECLIPSEDIR/eclipse"
alias eclipse="$eclipse_exe"

