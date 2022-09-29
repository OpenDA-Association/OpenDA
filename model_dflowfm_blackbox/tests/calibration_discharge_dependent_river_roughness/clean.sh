#! /bin/sh
#
# Removes all output files
#

echo "Removing working directories"
rm -rf stochModel/work[0-9]*
rm -f results_*.m
rm -f results_*.csv
rm -f openda_logfile.txt
rm -f *.orp
rm -f OpenDATimings_*.txt
