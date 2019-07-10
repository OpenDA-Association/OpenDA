#! /bin/sh
#
# Removes all output files
#

echo "Removing working directories"
rm -rf stochModel/output/work[0-9]*
rm -f *_results.m
rm -f *_results.py
rm -f openda_logfile.txt
rm -f *.orp
rm -f OpenDATimings_*
rm -f restart_*.xml
rm -f *.png
rm -f *.pyc
