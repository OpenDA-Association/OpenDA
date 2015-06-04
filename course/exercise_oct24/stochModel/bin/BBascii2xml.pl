#!/usr/bin/perl

=head1 NAME

BBascii2xml.pl

=head1 SYNOPSIS

BBascii2xml.pl $filename

=head1 DESCRIPTION

Converts output values c1/c2 in SimpleBBAsciiFile file format to XML
TreeVector format

=head1 AUTHORS

Alja Vrieling

=cut

use strict;
use warnings;

my @values;

if ($#ARGV+1 != 1) {
   print "Usage: BBascii2XML.pl filename\n";
   exit;
}
my $filename = $ARGV[0];
my @varnames = ("c1","c2");

 
for ( my $var=0; $var<=$#varnames; $var++) {
   my $varname = $varnames[$var];
   my $write_xml = 0;

   # open $filename
   open(IN,$filename);
   while (my $line = <IN> ){
      chomp($line);
      if ($line =~ m/^$varname/) {
         my ($dum,$vals) = split(/=/,$line);
         $vals =~ s/\[//;
         $vals =~ s/\]//;
         @values = split(/,/,$vals);
         $write_xml = 1;
      }
   }
   close(IN);

   if ($write_xml) {
      # write output in XML TreeVectorIoObject
      open OUTFILE, ">", "$varname.xml" or die $!;
      print OUTFILE "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
      print OUTFILE "<treeVectorFile>\n";
      print OUTFILE "    <treeVector id=\"$varname\">\n";
      for ( my $i=0; $i<=$#values; $i++) {
         print OUTFILE "         <treeVectorLeaf id=\"$i\">\n";
         print OUTFILE "             <vector>$values[$i]</vector>\n";
         print OUTFILE "         </treeVectorLeaf>\n";
      }
      print OUTFILE "    <treeVector>\n";
      print OUTFILE "</treeVectorFile>\n";
      close(OUTFILE)
   }
}
