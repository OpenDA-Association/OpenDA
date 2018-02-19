#!/bin/sh
# Srcipt to do all runs to prepare output for plotting
oda_run.sh simulation_initial.oda
tail  openda_logfile.txt
oda_run.sh simulation_initial.oda
oda_run.sh simulation_truth.oda
tail  openda_logfile.txt
oda_run.sh enkf.oda
tail  openda_logfile.txt
oda_run.sh enkf_stdobs2.oda
tail  openda_logfile.txt
oda_run.sh enkf_ens10.oda
tail  openda_logfile.txt
oda_run.sh enkf_ens6.oda
tail  openda_logfile.txt
oda_run.sh enkf_seed31415.oda
tail  openda_logfile.txt
