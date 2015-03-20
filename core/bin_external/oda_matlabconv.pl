#!/usr/bin/perl

# MOD_V2.0
# Copyright (c) 2013 OpenDA Association
# All rights reserved.
#
# This file is part of OpenDA.
#
# OpenDA is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
#
# OpenDA is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
#
# Author Nils van Velzen (VORtech)
#
use strict;
use File::Spec;
use File::Basename;

if ($#ARGV!=0){
   print "Usage oda_matlabconv.pl file\n";
   print " Convert an OpenDA matlab result file into a directory with matlab script files \n";
   print " for each variable. First converting using this script and then reading the\n";
   print " variables that are needed is in general much faster.\n"; 
   print " All files are stored in a directory with the same name as the matlab input file. \n";
   print " The files that are generated have the name f_variable_name.m.\n";
   print " Sourcing such a file in matlab will declare the variable 'variable_name'\n"; 
   exit (1);
}

# Get command like argument and open input file
my $filename=$ARGV[0];
open (FILEIN, "<$filename") or die("Cannot open input file $filename. $!\n");

# Construct the output directory name
my $dirname=basename($filename);
$dirname =~ s/\.m *$//;
my %filehandle;
mkdir $dirname;

#Find all variables and max struct index
while (<FILEIN>){
   chomp;
   my $line = $_;
   $line =~ s/%.*//;
   # do we have a variable
   if ($line =~ m/(.*)\{(.*)\}.*=(.*)/) {
      my $variable=$1;
      my $index=$2;
      my $values=$3;
      $values =~ s/\]; *$/]/;
      # Open file if this is the firt time we read this variable
      if ($index == "1"){
         my $filename=File::Spec->catfile($dirname,"f_$variable.m");
         open ($filehandle{"$variable"}, ">$filename") or die " cannot create file $filename: $!\n";
         print {$filehandle{"$variable"}} "$variable=[";
         print {$filehandle{"$variable"}} "$values";
      }
      # Otherwise just write all values
      else {
         print {$filehandle{"$variable"}} "\n$values";
      }
   }
}
# Close all variables with ];
foreach my $variable (keys %filehandle){
   print {$filehandle{"$variable"}} "];\n";
}
close FILEIN;



# Create a script for reading all variables
open (SCRIPT, ">vars.m");
print SCRIPT "vars=\{";
foreach my $variable (keys %filehandle){
   print SCRIPT "\n\'$variable\'"; 
}
print SCRIPT "\};\n"; 
close SCRIPT;

#openLoadAll script
open (SCRIPT, ">load_all.m");

print SCRIPT "function all=load_$dirname(variables)\n";
print SCRIPT "cd $dirname;\n";
foreach my $variable (keys %filehandle){
   print SCRIPT "disp [\'$variable\'];\n";
   print SCRIPT "f_$variable;\n"; 
   print SCRIPT "all.$variable=$variable;\n"; 
   print SCRIPT "clear $variable;\n"; 
}
print SCRIPT "cd .. \n";
close SCRIPT;

