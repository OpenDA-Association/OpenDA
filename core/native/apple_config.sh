# Example configuration setting for compiling native code on Mac OSX.
# Change this script to your own needs!

#I use macports for installing netcdf, c/fortran compiler and OpenMPI
export NETCDF_ROOT=/opt/local

./configure FC=mpif90-openmpi-mp F77=mpif77-openmpi-mp CC=mpicc-openmpi-mp CXX=mpicxx-openmpi-mp CPPFLAGS='-I/System/Library/Frameworks/JavaVM.framework/Versions/A/Headers' FFLAGS="-fcheck=all"  FCFLAGS="-fcheck=all"

