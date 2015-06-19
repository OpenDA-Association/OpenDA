#! /bin/bash
#
# Purpose : startup script for modification of headers
# Args    : directory eg '.' [optional with default .]
#           suffix eg 'java' [optional with default java]
# Author  : M. verlaan
# License : LGPL

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
#echo "CLASSPATH = " $CLASSPATH

# starting from bin-directory is necessary for *.so loading now TODO fix this
if [ $# -eq 0 ] ; then
   java org.openda.tools.HeaderModifier
elif [ $# -eq 1 ]; then
      dirName=$1
      echo "========================================================================="
      echo Starting "java org.openda.tools.HeaderModifier $dirName"

      # run application
      java org.openda.tools.HeaderModifier $dirName

      echo "========================================================================="
elif [ $# -eq 2 ]; then
      dirName=$1
      suffix=$2
      echo "========================================================================="
      echo Starting "java org.openda.tools.HeaderModifier $dirName $suffix"

      # run application
      java org.openda.tools.HeaderModifier $dirName $suffix

      echo "========================================================================="
else
   echo "This scipt accepts no more than two arguments"
fi

