#! /bin/sh


rm -rf stochModel/ens*
oda_run.sh simulation.oda

rm -rf stochModel/ens*
#oda_run.sh enkf_wind.oda
oda_run.sh enkf_wind_bound.oda
