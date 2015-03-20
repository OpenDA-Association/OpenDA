#! /bin/bash
#
# Base your own local settings file on this file:
# copy this file to settings_local_<hostname>.sh
# where <hostname> is obtained using the hostname command.
#

# Put any settings you need here (for instance, the
# setup for a numerical that you use)

# -- do not change anything below this line
# OpenDA
export PATH=$OPENDADIR:$PATH
export OPENDA_NATIVE=SYSTEM        
export OPENDALIB=$OPENDADIR/$OPENDA_NATIVE/lib
export eclipse_exe=eclipse
