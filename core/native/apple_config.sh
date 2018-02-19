#!/bin/sh
# Example configuration setting for compiling native code on Mac OSX.
# Change this script to your own needs!

#I use macports for installing netcdf, c/fortran compiler and OpenMPI
export NETCDF_ROOT=/opt/local

./configure FC=mpif90-openmpi-gcc5 F77=mpif77-openmpi-gcc5 CC=mpicc-openmpi-gcc5 CXX=mpicxx-openmpi-gcc5 CPPFLAGS='-I/System/Library/Frameworks/JavaVM.framework/Versions/A/Headers' FFLAGS="-fcheck=all"  FCFLAGS="-fcheck=all"

