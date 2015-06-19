# Setup bin. dir for openda.
# Script has to be run from the dir. containing the script.
export OPENDA_BINDIR=`pwd`
export PATH=$OPENDA_BINDIR:$PATH
export LD_LIBRARY_PATH=$OPENDA_BINDIR/../core/native_bin/linux32_gnu/lib
#Internal netcdf installation (put at end of paths)
export PATH=$PATH:=$OPENDA_BINDIR/../core/native/external/linux32_gnu/bin
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$OPENDA_BINDIR/../core/native/external/linux32_gnu/bin

