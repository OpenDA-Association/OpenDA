#! /bin/bash
#
# Purpose : startup script for oda_dumpio tool 
# Args    : filename
#           optional classname of ioObject -> if you omit the class name a list of possible names is given
# Examples: oda_dumpio.sh bodem.inc        : gives a list of possible ioObjects
#           oda_dumpio.sh hoekvhld.noos NoosTimeSeriesIoObject : give summary of contents
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
java org.openda.exchange.iotools.ioDumper $*

