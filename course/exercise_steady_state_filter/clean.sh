#! /bin/sh
#
# Removes all output files
#

echo "Removing working directories"
rm -f *_results.m
rm -f *_results.py
rm -f openda_logfile.txt
rm -f *.orp
rm -f *.png
rm -f *.pyc
rm -f OpenDATimings_*
rm -rf enkf_wave_*
