#! /bin/sh

echo "Removing working directories"
rm -rf swanModel/work[0-9]*
rm -f results_*.m
rm -f results_*.csv
rm -f openda_logfile.txt
rm -rf register_cases
rm -rf register_presentations
rm -rf ./*.orp
rm -f ./*.log
