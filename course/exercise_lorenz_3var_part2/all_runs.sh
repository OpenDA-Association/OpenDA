# Srcipt to do all runs to prepare output for plotting
oda_run.sh EnkF_std1_ens10.oda
tail  openda_logfile.txt
oda_run.sh EnkF_std10_ens10.oda
tail  openda_logfile.txt
oda_run.sh EnkF_std5_ens5.oda
tail  openda_logfile.txt
oda_run.sh EnkF_std5_ens10.oda
tail  openda_logfile.txt
oda_run.sh EnkF_std5_ens100.oda
tail  openda_logfile.txt


