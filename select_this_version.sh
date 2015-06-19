#! /bin/sh
#
# This script activates the OpenDA release in this directory. 
# For easy switching between multiple versions this can be convenient.
# If you have one installed version, you can add similar lines to your .bashrc

export OPENDADIR=$PWD/bin
. $OPENDADIR/settings_local.sh $HOSTNAME
