#! /bin/sh
set -x 

./clean.sh
rm -f *.log

oda_test.sh simple_lorenz >simple_lorenz.log 2>&1
oda_test.sh simple_lorenz96 >simple_lorenz96.log 2>&1
oda_test.sh simple_lorenz_transformed_observations >simple_lorenz_transformed_observations.log 2>&1
oda_test.sh simple_oscillator >simple_oscillator.log 2>&1
oda_test.sh simple_resultwriters >simple_resultwriters.log 2>&1
oda_test.sh simple_river1d >simple_river1d.log 2>&1
oda_test.sh simple_two_oscillators >simple_two_oscillators.log 2>&1
oda_test.sh native_advec1d >native_advec1d.log 2>&1
oda_test.sh native_heat >native_heat.log 2>&1
oda_test.sh native_oscillator >native_oscillator.log 2>&1
oda_test.sh native_parallel >native_parallel.log 2>&1
oda_test.sh native_parallel_from_examples >native_parallel_from_examples.log 2>&1
oda_test.sh thread >thread.log 2>&1
oda_test.sh rmi >rmi.log 2>&1
