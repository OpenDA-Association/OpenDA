#!/bin/sh

# build all castor stuff
#
# Building the castor jar-files (xml parser based on schema files) failed in
# the past when it was included in a "normal" ant build
#therefore it has been removed from autmatic build

#NOTE: this script must be started in the root dir of OpenDA

for DIRBUILD in model_delft3d core model_wflow model_efdc_dll model_bmi observers model_external_socket
do
   pushd $DIRBUILD
   ant -f build_castor.xml build
   popd
done
echo DONE BUILDING CASTOR JARs
