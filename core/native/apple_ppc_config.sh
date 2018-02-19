#!/bin/sh
# Example configuration setting for compiling native code on Mac OSX.
# Change this script to your own needs!

#Note location of my netcdf and jdk
#Note my mpi distro is installed on my path

./configure  -with-netcdf=/Users/nils/Devel/opt/netcdf-4.0 CFLAGS='-Wall -O0 -g' FCFLAGS='-Wall -O0 -g' CXXFLAGS='-Wall -O0 -g' FFLAGS='-Wall -O0 -g'   CPPFLAGS=-I/usr/local/openjdk7-zerovm-ppc-jyeary-2012-02-16-07-18-b00/include/
