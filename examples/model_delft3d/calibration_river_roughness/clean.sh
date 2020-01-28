#! /bin/sh
#
# Remove all output files
#

echo "Removing working directories"
rm -rf work[0-9]*
rm -rf openda_logfile.txt
rm -f \.*-results.txt
rm -f results_*.m
rm -f results_*.csv
rm -f \.*.cases
rm -f \.*.orp
