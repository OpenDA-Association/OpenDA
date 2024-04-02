#!/bin/bash

./clean.sh &> /dev/null

is_teamcity=false
if [ "$1" == "--teamcity" ]; then
    is_teamcity=true
fi

tests=(
    'simple_lorenz'
    'simple_lorenz96'
    'simple_lorenz_transformed_observations'
    'simple_oscillator'
    'simple_resultwriters'
    'simple_river1d'
    'simple_two_oscillators'
    'native_advec1d'
    'native_heat'
    'native_oscillator'
    'native_parallel'
    'native_parallel_from_examples'
    'thread'
    'rmi'
)

is_error=0
for test in "${tests[@]}"; do
    # Remove log from previous test.
    rm -f "${test}.log"

    if [ $is_teamcity = true ]; then
        echo "##teamcity[testStarted name='${test}' captureStandardOutput='true']"
        if oda_test.sh "${test}"; then
            echo "##teamcity[testFinished name='${test}']"
        else
            is_error=1
            echo "##teamcity[testFailed name='${test}' message='{$test} failed' details='']"
        fi
    else
        echo -n "Running test $test..."
        if oda_test.sh "${test}" &> "${test}.log"; then
            echo "passed"
        else
            is_error=1
            echo "failed"
        fi
    fi
done

# If any test has failed, exit with error code.
if [ $is_error -ne 0 ]; then
    exit 1
fi
