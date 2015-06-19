#! /bin/bash
#
# Local settings depending on hostname.
# It is possible to provide the hostname as argument to this script.
# Alternately, it it possible to set the $OPENDA_SETTINGS_LOCAL_SERVER
# variable. If both of these options aren't used, $HOSTNAME is used.
#
# The environment variable $OPENDADIR should be set to the directory
# with the OpenDA binaries. If it isn't set, a check will be performed
# whether the current work directory is suitable. If it is,
# this directory is used for $OPENDADIR and a warning is printed.
#

#check for OPENDADIR
#if it isn't set, try to use the present work directory
if [ -z "$OPENDADIR" ]; then
   here=`pwd`
   if [ -f "$here/settings_local.sh" ]; then
      echo "Setting OPENDADIR to $here"
      export OPENDADIR="$here"
   else
      echo "Environment variable OPENDADIR not set."
      echo "Please point it to the location with your openda binaries."
      if [ "$0" = "/bin/bash" -o "$0" = "/bin/sh" -o "$0" = "/bin/ksh" ]; then
         #return causes an error message if this script isn't sourced
         return 1
      else
         #exit is insanely annoying in case of a sourced script
         exit 1
      fi
   fi
fi

#determine host: on a cluster we use the name of the head-node
if [ "$HOSTNAME" = "" ]; then
   export HOSTNAME=`hostname`
fi

export server="$1" ; shift
if [ "$server" = "" ]; then
     export server=$OPENDA_SETTINGS_LOCAL_SERVER
     if [ "$server" = "" ]; then
        export server=$HOSTNAME
     fi
fi
export host=`echo $server|sed -e "s/\..*//"` #remove bit after dot
#echo "server = $server";
#echo "host   = $host  ";
export scriptname="$OPENDADIR/settings_local_$host.sh"
#echo "scriptname  = $scriptname  ";

#run appropriate settings for this server
if [ -f $scriptname ]; then
    #echo "starting $scriptname"
    . $scriptname
else
   echo "script not found: $scriptname"
fi

