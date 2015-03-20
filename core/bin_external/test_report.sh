#! /bin/bash

# read local settings
if [ -z "$OPENDADIR" ];then
   echo "OPENDADIR not set! Run settings_local.sh to fix this"
   exit 1;
fi

echo "============================================================================================"
echo "FAILURES"
echo "============================================================================================"
grep -i failures= $OPENDADIR/../opendaTestReports/TEST* |grep -v 'failures="0"'
echo "============================================================================================"
echo "ERRORS"
echo "============================================================================================"
grep -i failures= $OPENDADIR/../opendaTestReports/TEST* |grep -v 'errors="0"'
echo "============================================================================================"


