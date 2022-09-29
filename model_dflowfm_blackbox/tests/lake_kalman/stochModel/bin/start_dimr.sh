# This script requires one input argument: a file name (typically called dimr_config.xml). 
# To use this script, uncomment the next line and set DFLOWFMDIR to <Your installation base dir>
#export DFLOWFMDIR=
# or create an environment variable DFLOWFMDIR.
if [ -z $1 ]; then
    echo ERROR: no dimr configuration file is specified
    exit 1
fi
if [ -z $DFLOWFMDIR ]; then
    echo ERROR: No installation directory of D-Flow FM specified in ./stochModel/bin/start_dimr.sh.
    exit 1
fi
if [ ! -d $DFLOWFMDIR ]; then
    echo ERROR: D-Flow FM installation directory does not exist: $DFLOWFMDIR
    exit 1
fi
export dimr=$DFLOWFMDIR/lnx64/bin/run_dimr.sh
if [ ! -f $dimr ]; then
    echo ERROR in ./stochModel/bin/start_dimr.sh:
    echo File not found: $dimr
    exit 1
fi
$dimr -m $1
