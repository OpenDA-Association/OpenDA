#!/bin/bash

#
# These tests remain still to be included
#
#native_parallel
#native_parallel_from_examples
#rmi

pushd native_heat
./clean.sh
popd

pushd native_oscillator
./clean.sh
popd

pushd native_advec1d
./clean.sh
popd

pushd simple_lorenz
./clean.sh
popd

pushd simple_lorenz96
./clean.sh
popd

pushd simple_lorenz_transformed_observations
./clean.sh
popd

pushd simple_oscillator
./clean.sh
popd

pushd simple_resultwriters
./clean.sh
popd

pushd simple_two_oscillators
./clean.sh
popd

pushd thread
./clean.sh
popd

pushd simple_river1d
./clean.sh
popd
