#! /bin/sh
#
# Removes all output files
#

echo "Removing working directories"
rm -rf stochModel/output/work[0-9]*
rm -rf stochModelKalman/output/work[0-9]*
rm -f *_results.m
rm -f openda_logfile.txt
rm -f *.orp
rm -f *.log
rm -f OpenDATimings_*
