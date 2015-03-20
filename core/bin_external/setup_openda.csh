# Setup bin. dir for openda.
# Script has to be run from the dir. containing the script.
#ADJUSTED!
setenv OPENDADIR `pwd`
setenv OPENDA_BINDIR `pwd`
setenv PATH ${OPENDA_BINDIR}:$PATH
setenv LD_LIBRARY_PATH ${OPENDA_BINDIR}/../core/native_bin/linux32_gnu/lib
#Internal netcdf installation (put at end of paths)
setenv PATH ${PATH}:${OPENDA_BINDIR}/../core/native/external/linux32_gnu/bin
setenv LD_LIBRARY_PATH /v3/E09i_simona_bo/c3258-waqua-da-noos/openmpi/lib:${LD_LIBRARY_PATH}:${OPENDA_BINDIR}/../core/native/external/linux32_gnu/bin

