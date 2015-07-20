# Srcipt to do all runs to prepare output for plotting

oda_run.sh simulation_unperturbed.oda
tail  openda_logfile.txt
oda_run.sh simulation_perturbed.oda
tail  openda_logfile.txt
oda_run.sh simulation_ensemble.oda
tail  openda_logfile.txt

echo "To make plots with python try:"
echo "ipython"
echo "and then enter:"
echo "import make_all_plots"

echo "Now try automated version"
ipython --matplotlib=gtk all_runs.py
