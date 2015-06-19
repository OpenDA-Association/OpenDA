#! /bin/bash
#
# Purpose : startup script for Reflection tool. Lists all classes that implement the most important openda interfaces. 
# Args    : (optional) interface name
# Examples: oda_info.sh
# Author  : M. verlaan
# License : GPL

# read local settings
if [ -z "$OPENDADIR" ];then
   echo "OPENDADIR not set! Run settings_local.sh to fix this"
   exit 1;
fi
if [ ! -z "$OPENDALIB" ]; then
   echo "setting path for OPENDALIB"
   export LD_LIBRARY_PATH=$OPENDALIB:$LD_LIBRARY_PATH
fi

# append all jars in opendabindir to java classpath
for file in $OPENDADIR/*.jar ; do
   if [ -f "$file" ] ; then
       export CLASSPATH=$CLASSPATH:$file
   fi
done

#echo "classpath = $CLASSPATH"
java org.openda.utils.Reflection $1

