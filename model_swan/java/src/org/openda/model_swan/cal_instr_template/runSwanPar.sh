#! /bin/sh
. /opt/sge/InitSGE

# Use a swn input file as input argument

export MPI_VER=mpich2

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/intel_fc_90/lib
  
export swn="$1.swn" ; shift
export swanexe="$1" ; shift
export out="$1" ; shift

logfile=$PWD/swan_logfile.txt
echo "$PWD" > $logfile
date "+%F, %H:%M:%S" >> $logfile 2>&1
STARTRUN=`date +%s` 

#doKillMpd=""
doKillMpd="-killmpd /opt/mpich2/bin/mpdallexit"

if [ "$swn" = "" ]; then
   echo 'No swan-runid specified!' >> $logfile
   exit 1
fi

if [ "$swanexe" = "" ]; then
   echo 'No swan-executable specified!' >> $logfile
   exit 1
fi

# Create a directory for output
if [ -n "$out" ]; then
   mkdir -p $out
else
   out='.'
fi

pwd=`pwd`

cd $pwd

echo "Working directory: $PWD" >> $logfile
echo "OPENDABINDIR: $OPENDABINDIR" >> $logfile
echo "SWANEXESDIR: $SWANEXESDIR" >> $logfile
echo "swanexe: $swanexe" >> $logfile
echo "perlSwanPar.pl: `which perlSwanPar.pl`" >> $logfile

export PATH="$SWANEXESDIR/linuxPar":$PATH
perlSwanPar.pl $doKillMpd -input $swn -mpi $npart -swanexe $swanexe -output $out

date "+%F, %H:%M:%S" >> $logfile 2>&1
ENDRUN=`date +%s` 
declare -i DURATION
DURATION=($ENDRUN-$STARTRUN)
echo DURATION, "$DURATION" >> $logfile 2>&1
