#set -o xtrace
#set -o verbose
# add the COSTAWB program to the path
source /opt/intel_fc_100/bin/ifortvars.sh

export PATH=$( pwd )/src/costawb:$PATH



# set start directory to all model dynamic libraries inside the costa distro
PD=$( pwd )/examples

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$PD/ar1/src/.libs:$PD/heat_stoch/src/.libs:\
$PD/lorenz96/src/.libs:$PD/advec1d/src/.libs:$PD/heat_modelcombiner/src/.libs:\
$PD/lorenz/src/.libs:$PD/oscill/src/.libs:$PD/chimere/src/.libs:$PD/pollute2d/src/.libs:\
$( pwd )/../external/lib

# add the openda wrapper to the JAVA Classpath
CLASSPATH=$CLASSPATH:$PD/src/openda
