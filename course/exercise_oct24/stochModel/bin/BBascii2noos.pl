#!/usr/bin/perl

=head1 NAME

BBascii2noos.pl

=head1 SYNOPSIS

BBascii2noos.pl $filename

=head1 DESCRIPTION

Converts output values in SimpleBBAsciiFile format to files in NOOS format

=head1 AUTHORS

Alja Vrieling

=cut

use strict;
use warnings;

my @labels;
my @locations;
my @values;
my @time;
my $refdate;
my $unit;
my $tstart;
my $deltat;
my $tstop;

if ($#ARGV+1 != 1) {
   print "Usage: BBascii2noos.pl filename \n";
   exit;
}
# open $filename
my $filename = $ARGV[0];

open(IN,$filename);
while (my $line = <IN> ){
   chomp($line);
   if ($line =~ m/^output_labels/) {
      #source_labels=['locA','locB','locC']
      my ($dum,$label) = split(/=/,$line);
      $label =~ s/\[//;
      $label =~ s/\]//;
      $label =~ s/'//g;
      @labels = split(/,/,$label);
   }
   if ($line =~ m/^output_locations/) {
      #source_locations=[10,20,40]
      my ($dum,$loc) = split(/=/,$line);
      $loc =~ s/\[//;
      $loc =~ s/\]//;
      @locations = split(/,/,$loc);
   }
   if ($line =~ m/^output_values/) {
      #source_values['loc'] = [..];
      my ($dum,$line) = split(/=/,$line);
      push(@values,$line);
   }

   if ($line =~ m/^refdate/) {
      #refdate='01 dec 1999'
      (my $dum,$refdate) = split(/=/,$line);
   }
   if ($line =~ m/^unit/) {
      #unit='seconds'
      (my $dum,$unit) = split(/=/,$line);
   }
   if ($line =~ m/^time/) {
      #time=[0,60,18000]
      my ($dum,$time) = split(/=/,$line);
      $time =~ s/\[//;
      $time =~ s/\]//;
      ($tstart, $deltat, $tstop) = split(/,/,$time);
   }
}
close(IN);

# reconstruct time to string format YYYYMMDDHHMM
$refdate =~ s/\s*'\s*//g;
my ($day,$month,$year) = split(/\s/,$refdate);
my @months = ('jan','feb','mrt','apr','mei','jun','jul','aug','sep','okt','nov','dec');
for ( my $i=0; $i<=$#months; $i++) {
   if ($month =~ m/$months[$i]/){
      $month=$i+1;
   }
}

# convert time from seconds to minutes, or die
if ($unit =~ m/'seconds'/ ) {
   my $fac = 1/60;
   $tstart = $fac*$tstart;
   $deltat = $fac*$deltat;
   $tstop  = $fac*$tstop;
} else {
  die "unit <> 'seconds' not yet implemented"
}

my $min = $tstart % 60;
my $hour = $min - $min % 60;
$month = 12;
my $timestring = sprintf("%4d%2d%02d%02d%02d",$year,$month,$day,$hour,$min);
push(@time,$timestring);
while ($tstart+$deltat <= $tstop) {
   use integer;
   $tstart = $tstart+$deltat;
   $min = $tstart % 60;
   $hour = ($tstart - $min) / 60;
   die "periods longer than 24 hours not yet implemented" if ($hour > 24);
   $timestring = sprintf("%4d%2d%02d%02d%02d",$year,$month,$day,$hour,$min);
   push(@time,$timestring);
}

# write output in NOOS format, one location per file
for ( my $i=0; $i<=$#locations; $i++) {
   my $vals = $values[$i];
   $vals =~ s/\[//;
   $vals =~ s/\]//;
   my @vals = split(/,/,$vals);
   open OUTFILE, ">", "$labels[$i].noos" or die $!;
   print OUTFILE "#"."-"x72;
   print OUTFILE "\n";
   print OUTFILE "# Timeseries converted from pollution_model.output \n";
   my ($unit,$loc) = split(/_/,$labels[$i]);
   $unit =~ s/c/concentration/;
   print OUTFILE "# Location      : $loc\n";
   print OUTFILE "# Position      : ($locations[$i],0.0)\n";
   print OUTFILE "# Source        : observed\n";
   print OUTFILE "# Unit          : $unit \n";
   print OUTFILE "# Analysis time : null \n";
   print OUTFILE "# Timezone      : GMT\n";
   print OUTFILE "#"."-"x72;
   print OUTFILE "\n";
   for ( my $j=0; $j<=$#vals; $j++) {
      print OUTFILE "$time[$j]  $vals[$j] \n";
   }
   close(OUTFILE)
}


