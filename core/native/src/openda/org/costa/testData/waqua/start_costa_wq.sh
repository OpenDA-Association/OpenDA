#!/bin/sh

#
# A script to start a WAQUA/TRIWAQ calculation in COSTA or in OpenDa
#
###############################################################################
#
# USER SETTINGS: 
#
export SIMONADIR=/part3/nils/promotie/src/simona

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$SIMONADIR/lib/linux/
export PATH=$SIMONADIR/bin:$PATH

#
# Run the Kalman calculation in COSTA
#  
waqpre.pl -runid cta -input siminp.kalman \
          -npart 1 -ndom 1 -back no -debug no -bufsize - 



#waqcosta.pl -runid cta -back no -debug no  \
#               -nmdbg nmdbg.inp -obsfile obs2sds_d97.sql \
#               -tstart 4320 -tstop 4400 -tstep 10 -nmode 20


waqopenda.pl -damethod enkf \
             -obsfile obs2sds_d97.sql -runid cta -back n \
             -nmode 3 












