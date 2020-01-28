#!/bin/bash
#
# These tests remain still to be included
#
#s00c2_netcdf_notfinished

pushd kalman_twin_windbound
./clean.sh
popd

pushd s00c2
./clean.sh
popd

pushd swan_l21triad
./clean.sh
popd

pushd swan_l21triad_two_twin
./clean.sh
popd

