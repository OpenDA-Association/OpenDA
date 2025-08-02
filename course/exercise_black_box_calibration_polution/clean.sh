#! /bin/sh
#
# Removes all output files
#

echo "Removing working directories"
rm -rf work/work[0-9]*
rm -f *_results.m
rm -f *_results.py
rm -f openda_logfile.txt
rm -f *.orp
rm -f OpenDATimings_*
rm -f restart_*.xml
rm -f *.png
rm -rf __pycache__
rm -rf original_model/maps
rm -rf original_model/restart
rm -f original_model/c*_loc*.csv