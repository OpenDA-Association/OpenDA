#!/bin/sh
mkdir tmp
mkdir tmp/algorithms
hier=$PWD
cp observations.csv tmp/
cp ensembles_25.csv tmp/

for dist in 5 10 25 50 100 200 500 1000;
do
   echo $dist
   sed s/OUTFILE/enkf25_loc_l${dist}_results.py/ <enkf_25_loc_dist.oda > tmp/enkf_25_loc_${dist}.oda
   sed s/"<distance>.*<\/distance>"/"<distance>$dist<\/distance>"/ <algorithms/EnKF25_loc.xml   >tmp/algorithms/EnKF25_loc.gen.xml 
   echo done generating
   cd tmp
   oda_run.sh enkf_25_loc_${dist}.oda
   cd $hier
done

