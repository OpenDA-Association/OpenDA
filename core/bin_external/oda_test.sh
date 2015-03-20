#! /bin/bash
#
# Purpose : startup script for TestRunner
# Args    : use without arguments for info
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

# java options
if [ -z "$ODA_JAVAOPTS" ]; then
	export ODA_JAVAOPTS='-Xmx1024m'
fi

# append all jars in opendabindir to java classpath
for file in $OPENDADIR/*.jar ; do
   if [ -f "$file" ] ; then
       export CLASSPATH=$CLASSPATH:$file
   fi
done

# start java with initial class
java $ODA_JAVAOPTS org.openda.application.TestRunner $*

export errors=$?
if [ $errors -ne 0 ]; then
	exit $errors
fi
