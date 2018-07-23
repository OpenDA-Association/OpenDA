#!/bin/sh

#generate the ensembles
python generate_ensemble.py 25
python generate_ensemble.py 50
python generate_ensemble.py 100

#do all runs
oda_run.sh enkf_25.oda
oda_run.sh enkf_50.oda
oda_run.sh enkf_100.oda

oda_run.sh enkf_25_loc.oda
