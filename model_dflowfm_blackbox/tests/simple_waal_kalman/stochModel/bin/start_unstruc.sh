#! /bin/sh
#

if [ -z "$DFLOWFMROOT" ]; then
   echo "ERROR in script ./stochModel/bin/start_dflowfm.sh:"
   echo "environment variable DFLOWFMROOT expected, point DFLOWFMROOT to the installation directory of D-FLOW FM."
   exit 1
fi

cp $DFLOWFMROOT/res/unstruc.ini .
cp $DFLOWFMROOT/bin/unstruc.dia .
$DFLOWFMROOT/bin/dflowfm --autostartstop $1
