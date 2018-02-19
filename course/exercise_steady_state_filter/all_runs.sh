#!/bin/sh
# Srcipt to do all runs to prepare output for plotting

# add jar for model to openda
cp simple_wave_model/bin/simple_wave_model.jar ../../bin/

#oda_run.sh waveSimulation_noise.oda
#tail openda_logfile.txt

oda_run.sh waveSimulation.oda 
tail openda_logfile.txt

oda_run.sh waveEnkf.oda 
tail openda_logfile.txt

oda_run.sh waveSteadystate.oda
tail openda_logfile.txt

#make figures with python
ipython --matplotlib=gtk all_runs.py
