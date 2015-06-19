#! /bin/sh
# ===========================================================================
# Name    : waquaQsub.sh
# Purpose : startup script for openDA applications parallel in qsub
# Args    : <input>   name of configuration input file to be opened
#           <jobname> job-name (must be unique!!)
#           <par_env> ser, shared or distrib
#           <npart>   number of processors 
#		      (in case of ser, will be disregarded)
#		      (in case of shared, will always be 2)
#           <optional1> ="-q" for special qsub queue
#           <optional2> value of qsub parameter "-q"
# Examples: swanQsub.sh main.xml myjob shared 2  
#           - starts computations shared on 2 processors
# Comments: swanQsub must be started in the main directory of the openda test 
#           set, in other words the directory where the main.xml file is located
# Date    : June 2009
# Author  : SV
# ===========================================================================

echo "========================================================================="
echo "Starting" $0 $1 $2 $3

# find out where this script is
opendabindir=`(cd \`dirname $0\`; pwd)`
echo "opendabindir = $opendabindir"
. $opendabindir/settings_local.sh $HOSTNAME

# check input
export input="$1" ; shift
export jobname="$1"; shift
export par_env="$1"; shift
export npart="$1"; shift
export optionalParam="$1";
if [ "$optionalParam" = "-q" ]; then  
  export addQparam1="$1"; shift
  export addQparam2="$1"; shift
  if [ "$addQparam2" = "" ]; then
    echo "ERROR: No value is assigned for optional parameter -q !"
  fi
fi

if [ "$input" = "" ]; then
     echo 'No input file specified!';
     echo "Use  waquaQsub.sh <input> <jobname> <par_env> <napart>"
     echo " Args    : <input>   name of configuration input file to be opened"
     echo "           <jobname> job-name (must be unique!!)"
     echo "           <par_env> ser, shared or distrib"
     echo "           <npart>   number of processors "
     echo "		      (in case of ser, will be disregarded)"
     echo "		      (in case of shared, will always be 2)"
     echo " Examples: swanQsub.sh main.oda myjob distrib 2  "
     exit 1
fi
if [ "$jobname" = "" ]; then
     echo 'No jobname specified!';
     exit 1
fi
if [[ ${#jobname} -gt 10 ]]; then
     echo 'jobname length cannot exceed 10 characters!';
     exit 1
fi
if [ "$par_env" = "" ]; then
     echo 'No parallel environment is given: ser, shared or distrib';
     exit 1
fi
if [ "$par_env" = "shared" ]; then
     #shared parallel runs only on two processors
     npart=2
fi
if [ "$par_env" = "shared" -o "$par_env" = "distrib" ]&[ "$npart" = "" ]; then
     echo 'No number of partitions given!';
     exit 1
fi

# location of input/output data
pwd=`pwd`
out=$pwd

#Delete existing output and error files
rm -f $out/$jobname.out
rm -f $out/$jobname.err

if [ "$par_env" = "ser" ]; then
	#Start qsub 
	echo "cd $pwd;
             . $OPENDADIR/settings_local.sh $HOSTNAME
	     Application.sh $input " |\
        if [ "$optionalParam" = "-q" ]; then
	  qsub $addQparam1 $addQparam2 -V -N $jobname
        else
	  qsub -V -N $jobname
        fi
	qstat -s r -u $USER
else
	#Start qsub 
	echo "cd $pwd;
             . $OPENDADIR/settings_local.sh $HOSTNAME
	     export NPART=$npart
	     Application.sh $input " |\
        if [ "$optionalParam" = "-q" ]; then
	  qsub $addQparam1 $addQparam2 -V -N $jobname -pe $par_env $npart -e $out/$jobname.err -o $out/$jobname.out
        else
	  qsub -V -N $jobname -pe $par_env $npart -e $out/$jobname.err -o $out/$jobname.out
        fi
	qstat -s r -u $USER
fi

echo "Waqua job with name $jobname started"
echo "========================================================================="
