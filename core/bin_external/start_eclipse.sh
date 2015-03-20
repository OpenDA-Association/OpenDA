#! /bin/bash
#
# Purpose : set environment for eclipse
# Examples: start_eclipse.sh
# Author  : M. verlaan
# License : GPL

# read local settings
if [ -z "$OPENDADIR" ];then
   echo "OPENDADIR not set! Run settings_local.sh to fix this"
   exit 1;
fi

#overrule with original libs
export OPENDALIB="$OPENDADIR/../core/native_bin/$OPENDA_NATIVE/lib/:$OPENDADIR/../model_example_fortran/bin_external/"
export OPENDALIB=$OPENDALIB:$OPENDADIR/../model_delft3d/native_bin/linux64_gnu/lib
if [ ! -z "$OPENDALIB" ]; then
   echo "setting path for OPENDALIB to $OPENDALIB"
   export LD_LIBRARY_PATH=$OPENDALIB:$LD_LIBRARY_PATH
fi

$eclipse_exe
