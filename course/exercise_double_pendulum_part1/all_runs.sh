# Srcipt to do all runs to prepare output for plotting
oda_run.sh simulation_unperturbed.oda
tail  openda_logfile.txt
oda_run.sh simulation.perturbed.oda
tail  openda_logfile.txt
oda_run.sh simulation_ensemble.oda
tail  openda_logfile.txt


