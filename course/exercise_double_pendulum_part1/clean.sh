#! /bin/sh
#
# Removes all output files
#

echo "Removing working directories"
rm -f sim*_results*.m
rm -f *_results*.py
rm -f openda_logfile.txt
rm -f *.orp
rm -f OpenDATimings_*
rm -f restart_*.xml
rm -f *.pyc
