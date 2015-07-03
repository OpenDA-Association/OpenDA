# Srcipt to do all runs to prepare output for plotting

#oda_run.sh waveSimulation_noise.oda
#tail openda_logfile.txt

oda_run.sh waveSimulation.oda 
tail openda_logfile.txt

oda_run.sh waveEnkf.oda 
tail openda_logfile.txt

oda_run.sh waveSteadystate.oda
tail openda_logfile.txt

