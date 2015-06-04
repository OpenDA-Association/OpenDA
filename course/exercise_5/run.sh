#! /bin/sh
#
# This script runs the standard tests
#
# It is assumed that SIMONA is set before running this script!

#
# Kalman filtering  
#

oda_run.sh Simulation.oda
oda_run.sh Dud.oda
