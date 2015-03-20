#!/usr/bin/env perl

# ===========================================================================
# Name    : runSwanCalib.pl
# Purpose : perl script for starting Application.sh
# Args    : <input>   name of configuration input file to be opened
# Example : runSwanCalib.pl -input main.xml 
# Comments: this script waits for the infile (parrun_started) to exist before
#           commencing with the Application.sh in order to make sure that the
#           job has left the queue and is actually running
# Date    : June 2009
# Author  : SV
# ===========================================================================

use strict;
use warnings;

# MAIN PROGRAM
my $flag=1;
# the following loop waits for checkEvent to return 0, otherwise sleep
while ($flag) {
   my $sleep=checkEvent();
   if (not $sleep) {
     $flag=0;
   } else {
      sleep(1);
   }
}

if (not $flag) {
   my $input;
   my $arg = shift;
   if ($arg =~ /-input/) {
   	$input = shift;
   }
   #start OpenDaApplication
   my $cmd="$ENV{OPENDABINDIR}/Application.sh $input";
   system($cmd);
}
# END MAIN PROGRAM

# SUBROUTINE
# Name    : checkEvent
# Purpose : check if file parrun_started exists and return 0
# Args    : None
# RetVal  : 0 if $infile exists, 1 otherwise

sub checkEvent {
   my $infile="parrun_started";
   if (-e $infile) {
     return 0;
   } else {
     return 1;	
   }
}
# END SUB checkEvent
