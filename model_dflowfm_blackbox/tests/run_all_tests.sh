#! /bin/sh
if [ -z $DFLOWFMDIR ]; then
    echo ERROR: No environment variable \$DFLOWFMDIR available.
    echo use \"export DFLOWFMDIR={installation directory of D-Flow FM}\" before starting this script.
    exit 1
fi

./clean.sh
rm -f *.log

oda_test.sh calibration_discharge_dependent_river_roughness >calibration_discharge_dependent_river_roughness.log 2>&1
oda_test.sh calibration_river_roughness >calibration_river_roughness.log 2>&1
oda_test.sh dcsmv5_kalman >dcsmv5_kalman.log 2>&1
oda_test.sh estuary_calibration >estuary_calibration.log 2>&1
oda_test.sh estuary_kalman_pli-file >estuary_kalman_pli-file.01.log 2>&1
oda_test.sh estuary_kalman_bc-file >estuary_kalman_bc-file.log 2>&1
oda_test.sh lake_kalman >lake_kalman.log 2>&1
oda_test.sh simple_waal_calibration_roughness >simple_waal_calibration_roughness.log 2>&1
oda_test.sh simple_waal_kalman >simple_waal_kalman.log 2>&1
oda_test.sh simple_waal_salt_kalman >simple_waal_salt_kalman.log 2>&1
