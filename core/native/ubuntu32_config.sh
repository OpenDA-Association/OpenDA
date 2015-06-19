# Example configuration setting for compiling native code on 32 bit Ubuntu using OpenJDK 7.
#
# all packages are installed using the ubuntu relases.
# e.g. netcdf, openmpi, gfortran etc
#
# Change this script to your own needs!
#
# Author: Nils van Velzen

export JAVA_HOME=/usr/lib/jvm/java-7-openjdk-i386
./configure2 --disable-system-lapack
