#!/bin/bash

#
# These tests remain still to be included
#
rm -f OpenDATimings*
rm -f *.log

pushd calibration_discharge_dependent_river_roughness
./clean.sh
popd

pushd calibration_river_roughness
./clean.sh
popd

pushd dcsmv5_kalman
./clean.sh
popd

pushd estuary_calibration
./clean.sh
popd

pushd estuary_kalman
./clean.sh
popd

pushd estuary_kalman_FMSuite2019.01
./clean.sh
popd

pushd estuary_kalman_FMSuite2019.01_bcfile
./clean.sh
popd

pushd lake_kalman
./clean.sh
popd

pushd simple_waal_calibration_roughness
./clean.sh
popd

pushd simple_waal_kalman
./clean.sh
popd

pushd simple_waal_salt_kalman
./clean.sh
popd
