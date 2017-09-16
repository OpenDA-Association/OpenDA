#! /bin/sh
#
# Removes all output files
#

echo "Removing working directories"
rm -rf stochModel/output/work[0-9]*
rm -f *_results.m
rm -f *_results2.m
rm -f *_results.py
rm -f *_results2.py
rm -f openda_logfile.txt
rm -f *.orp
rm -f *.png
rm -f *.pyc
rm -f OpenDATimings_*
