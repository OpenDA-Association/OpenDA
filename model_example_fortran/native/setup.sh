#
# Setup for building simple_fortran_dll and simple_fortran_dll_test
#

# Add the bin directory to the load library path

BIN_DIR=`pwd`/../bin

export PATH=$PATH:$BIN_DIR
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$BIN_DIR

