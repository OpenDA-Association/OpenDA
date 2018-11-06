#!/bin/bash
# Srcipt to do all runs to prepare output for plotting

#
# compile wrapper
#
pushd simple_advection_model
ant build
popd

# add jar for model to openda
cp simple_advection_model/bin/simple_advection_model.jar ../../bin/

oda_run.sh advectionSimulation.oda 
tail openda_logfile.txt

oda_run.sh advectionEnsembleSimulation.oda 
tail openda_logfile.txt

oda_run.sh advectionEnkf.oda 
tail openda_logfile.txt

#make figures with python
#ipython --matplotlib=gtk all_runs.py
