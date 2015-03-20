#! /bin/sh
. /opt/sge/InitSGE

# ===========================================================================
# Name    : swanCalibration.sh
# Comments: swanCalibration.sh must be started in the main directory of the
#           openda test, in other words the directory where the
#           main <calibration>.xml file (i.e. <calibration>.oda file) is located
# Date    : June 2009
# Author  : SV
# ===========================================================================


usage()
{
	echo ""
	echo "Usage 1: $0 <input>"
	echo "  \"  2: $0 <input> <jobname> <ser or distrib> <npart>"
	echo "Args   : <input>          name of openda application file to be opened"
	echo "         <jobname>        job-name (must be unique!!)"
    echo "         <ser or distrib> serial or parallel run"
    echo "         <npart>          number of processors"
	echo "Example: swanCalibration.sh myCase.oda myParRun distrib 8"

}


echo "========================================================================="
echo "Starting" $0 $1 $2 $3

export input="$1"
export jobname="$2"
export par_env="$3"
export npart="$4"

#This is the location of the (perl)swanCalibration.(sh|pl) and (perl)swanPar.(sh|pl):
export SWANCALBINDIR=`(cd \`dirname $0\`; pwd)`
#This is the location of the available swan executables
export SWANEXESDIR="$SWANCALBINDIR/swanExes"
#This is the location of the Application.sh (openda):
export OPENDABINDIR="$SWANCALBINDIR/openda"

if [ $# -eq 0 ]; then
     #no input, start openda application
     $OPENDABINDIR/Application.sh -gui
     exit 0
fi

if [ $# -eq 1 ]; then
     #one input argument. if it is a file, start openda application for that file
     if [ "$1" == "-h" -o "$1" == "-help" -o "$1" == "help" ] ; then
		 usage;
		 exit 0
     else
        $OPENDABINDIR/Application.sh -gui $1
     fi
     exit 0
fi

echo "DIRS swanCalibration.sh"
echo "PWD $PWD"
echo "SWANCALBINDIR $SWANCALBINDIR"
echo "SWANEXESDIR $SWANEXESDIR"
echo "OPENDABINDIR $OPENDABINDIR"
echo "ENDDIRS swanCalibration.sh"

if [ "$input" = "" ]; then
     echo 'No input file specified!';
     usage;
     exit 1
fi
if [ "$jobname" = "" ]; then
     echo 'No jobname specified!';
     usage;
     exit 1
fi
if [[ ${#jobname} -gt 10 ]]; then
     echo 'jobname length cannot exceed 10 characters!';
     usage;
     exit 1
fi
if [ "$par_env" = "" ]; then
     echo 'No parallel environment is given: ser, shared or distrib';
     usage;
     exit 1
fi
if [ "$par_env" = "shared" ]; then
     #shared parallel runs only on two processors
     npart=2
fi
if [ "$par_env" = "distrib" ]&[ "$npart" = "" ]; then
     echo 'No number of partitions given!';
     usage;
     exit 1
fi

out='.'

#Delete existing output and error files
rm -f $out/$jobname.out
rm -f $out/$jobname.err
rm -f parrun_started

pwd=`pwd`

QUEUE_NAME=""
if [ "$HOSTNAME" == "h4.deltares.nl" ]; then
	QUEUE_NAME="-q swancal"
fi

if [ "$par_env" = "ser" ]; then
	#Start qsub 
	echo "cd $pwd;
	     export PATH=$SWANCALBINDIR:$SWANEXESDIR:$PATH
	     . /opt/sge/InitSGE
	     perlSwanCalibration.pl -input $input " |\
	qsub -V -N $jobname
	qstat -s r -u $USER
else
	#Start qsub 
	export RUN_SWAN_PARALLEL="true"
	echo "cd $pwd;
	     export PATH=$SWANCALBINDIR:$SWANEXESDIR:$PATH
	     export npart=$npart
	     . /opt/sge/InitSGE
	     perlSwanCalibration.pl -input $input " |\
	qsub $QUEUE_NAME -V -N $jobname -pe $par_env $npart -e $out/$jobname.err -o $out/$jobname.out
	qstat $QUEUE_NAME -s r -u $USER
fi

#Check to see if this job has left the queue and is running
#print $5 is the status of the job (r=running, qw=waiting, Eqw=an error)
qout=`qstat -u $USER|grep $jobname |awk '{print $5}'`
until [ "$qout" == "r" -o "$qout" == "Eqw" ]
do
     qout=`qstat $QUEUE_NAME -u $USER|grep $jobname |awk '{print $5}'`
done

#We now know that the job is running, script perlSwanCalibration.pl is waiting
#for the file parrun_started to exist in order to start Application.sh
qstat $QUEUE_NAME -xml -t -u $USER| grep queue_name > parrun_started 2>&1

echo "Swan job with name $jobname started"
echo "========================================================================="


