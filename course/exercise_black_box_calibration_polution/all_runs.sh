#! /bin/sh
#
# This script runs the standard tests
#

#
# Simulation and Dud
#

oda_run.sh Simulation.oda
oda_run.sh Dud.oda

#ipython --matplotlib=gtk all_runs.py
ipython --matplotlib=gtk all_runs.py


