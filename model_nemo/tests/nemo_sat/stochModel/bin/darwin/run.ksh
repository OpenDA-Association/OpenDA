#!/bin/ksh
#

##############################
# PARAMETERS OF THE SIMULATION
##############################

# Name of the experiment
expt="SQB"
# Time parameters (1/4 degree resolution model)
iyear="0001"        # Initial year
lyear="0003"        # Last year
nstep="34560"       # Number of time steps per year

# Result directory
target_dir="Results"   # Where to store results

##############################
# RUN THE MODEL
##############################

# Year loop
let i=$iyear ; let l=$lyear
while [ $i -le $l ] ; do

   year=`echo $i | awk '{printf("%04d", $1)}'`
   echo "Running year: $year"

# Compute intial and final timestep
   nitrst=`echo $nstep $year | awk '{printf("%08d", $1*($2-1))}'`
   nit000=`echo $nstep $year | awk '{printf("%8d", 1+$1*($2-1))}'`
   nitend=`echo $nstep $year | awk '{printf("%8d", $1*$2)}'`

# Get model restart
   if [ $year = '0001' ] ; then
     rstart=".false."
     rstctl="0"
   else
     rstart=".true."
     rstctl="2"
     ln -sf ${expt}_${nitrst}_restart.nc restart.nc
   fi

# Edit model namelist
   sed -e "s/\%EXPT\%/${expt}/" \
       -e "s/\%NIT000\%/${nit000}/" \
       -e "s/\%NITEND\%/${nitend}/" \
       -e "s/\%RSTCTL\%/${rstctl}/" \
       -e "s/\%RSTART\%/${rstart}/" namelist.skel > namelist

# Remove final restart (from previous experiment if any)
   rm -f  ${expt}_${nitend}_restart.nc

# Run model for current year
   ./opa

# Remove initial restart
   rm -f restart.nc

# Save model output files
   mv ${expt}_??_${year}????_${year}????_*.nc ${target_dir}

   let i=$i+1
done
