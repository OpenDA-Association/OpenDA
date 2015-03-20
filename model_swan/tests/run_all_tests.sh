#! /bin/sh
./clean.sh


oda_test.sh swan_l21triad >swan_l21triad.log 2>&1
oda_test.sh swan_l21triad_two_twin >swan_l21triad_two_twin.log 2>&1
oda_test.sh kalman_twin_windbound >kalman_twin_windbound.log 2>&1
#s00c2
#s00c2_netcdf_notfinished
#s00c2_withMissingValues
