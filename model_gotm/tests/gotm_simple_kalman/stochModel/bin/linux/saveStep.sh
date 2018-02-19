#!/bin/sh

# create a folder with the input data and the output nc for one gotm run
timestamp=$(date +%Y%m%d_%H%M%S)
namedir="Output_"$timestamp 
directory=$namedir
i=1
while [ -d "$directory" ];
do
  directory=$namedir"_$i"
  let "i +=1"
done
mkdir $directory

cp nns_seasonal.nc $directory
cp *.txt $directory
cp gotmrun.nml $directory
cp gotmturb.nml $directory

