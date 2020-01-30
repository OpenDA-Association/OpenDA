#! /bin/sh
#
# Removes all output files
#

echo "Removing working directories"
rm -f ./*_results.m
rm -f ./*_results.py
rm -f openda_logfile.txt
rm -f ./*.orp
rm -f ./*.png
rm -f OpenDATimings_*
rm -rf enkf_wave_*
rm -rf simple_advection_model/MANIFEST.MF
rm -rf simple_advection_model/bin
rm -rf simple_advection_model/build
rm -rf simple_advection_model/javadoc
rm -f ./*.pyc
