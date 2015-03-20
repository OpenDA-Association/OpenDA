#!/usr/bin/perl
use strict;


sub SortUnique{

   my @sorted = sort @_;
   my $last   = $sorted[0];
   my @return;
   foreach my $elt (@sorted){
      if ($last eq $elt){
         push(@return, $elt);
         $last = $elt;
      }
   }
   @return;
}

sub getIndex($$$$){
  my @args= @_;
  my @labels  = @{$args[0]};
  my @methods = @{$args[1]};
  my $label   = $args[2];
  my $method  = $args[3];

  my $index=-1;
  my $i=0;
  foreach my $labList (@labels){
     if ($labList eq $label and $methods[$i] eq $method ){
        $index=$i;
     }
     $i++;
  }
  $index;
}





print "Start of oda_timingrep.pl\n";

# Open Timing result file
#open FILE, "<OpenDATiming_csm8v5.txt" or die $!;
open FILE, "<$ARGV[0]" or die $!;
my @content=<FILE>;
close FILE;

# Split columns into arrays
my @labels;
my @method;
my @num;
my @wall;
my @cpu;

#0         1         2         3         4         5         6         7         8         9
#0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
#                         RMI Model:2              getObservedValues     4      0.035     0.0077
#               Native Model:31                        compute     3     0.0010        0.0

foreach my $line (@content){
   push(@labels, substr($line,0,36));
   push(@method, substr($line,37,30));
   push(@num,    substr($line,68,5));
   push(@wall,   substr($line,74,10));
   push(@cpu,    substr($line,85,10));
}

foreach(@labels){s/^ *//;}
foreach(@method){s/^ *//;}



# Make a list of all id's with removed instance number
my $i=0;
my @labelsNoNum;
foreach my $label (@labels){
   $label =~ s/:[0-9]* *$//;
   @labelsNoNum[$i]=$label; 
   $i++;
}


$i=0;
my @sum_labels;
my @sum_method;
my @sum_num;
my @sum_wall;
my @sum_cpu;

foreach my $label (@labelsNoNum){
   my $index=getIndex(\@sum_labels, \@sum_method, $label, $method[$i]);
   if ($index == -1){
      push (@sum_labels, $label);
      push (@sum_method, $method[$i]);
      push (@sum_num,    $num[$i]);
      push (@sum_wall,   $wall[$i]);
      push (@sum_cpu,    $cpu[$i]);
   } 
   else {
      $sum_num[$index]  +=   $num[$i];
      $sum_wall[$index] +=  $wall[$i];
      $sum_cpu[$index]  +=    $cpu[$i];
   }
   $i++;
}

$i=0;
my @output;
foreach my $label (@sum_labels){
   my $line=sprintf("%36s %30s %5d %10.2g %10.2g\n",$sum_labels[$i], $sum_method[$i], $sum_num[$i], $sum_wall[$i], $sum_cpu[$i]);
   push (@output, $line);
   $i++;
}
@output= sort @output;
print @output;




#We collapse 


#Tel alles bij elkaar
#my $som=0;
#foreach my $wall (@wall){
#print "BLALBBALBA $wall\n";
#   $som=$som+$wall;
#print "$som \n";
#}
#print "BLALBBALBA\n";
#print $som



