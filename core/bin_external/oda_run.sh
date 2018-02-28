#! /bin/bash
#
# Purpose : startup script for openDA applications
# Args    : -gui      to start the gui
#           -p        perform a parallel run
#           -o=<log>  overrule default name of log file (openda_logfile.txt)
#           <file>    configuration file
# Examples: oda_run.sh               : starts gui
#           oda_run.sh -gui          : starts gui
#           oda_run.sh main.oda      : starts computations without gui
#           oda_run.sh -p main.oda   : starts parallel computations without gui
#           oda_run.sh -gui main.oda : starts gui and opens the configuration file main.oda
#           oda_run.sh -o=mylog.txt main.oda
#                                    : start computations without gui, writes log info to mylog.txt.
#           oda_run.sh -o=mylog.txt -p main.oda
#                                    : start parallel computation without gui, writes log info to mylog.txt
# Author  : M. verlaan
# License : GPL

if [ "$#" -eq 0 ]; then
    GUI=true
elif [ "$#" \> 4 ]; then
    echo "Something wrong: check the input parameters of this script."
    exit 1;
else
    # default settings
    LOGFILE="openda_logfile.txt"
    GUI=false
    PARALLEL=""
fi

for i in "$@"
do
    case $i in
        -gui)
        GUI=true
        shift
        ;;
        -o=*|-out=*)
        LOG=true
        LOGFILE="${i#*=}"
        shift
        ;;
        -p)
        PARALLEL="-p"
        shift
        ;;
        *)
        CONFIG="${i#*=}"
        configfile="`dirname $CONFIG`/$CONFIG"
        shift
        ;;
    esac
done

if [ ! -f $configfile ]; then
    echo "OpenDa config file $configfile not found"
    exit 1;
fi

# read local settings
if [ -z "$OPENDADIR" ];then
   echo "OPENDADIR not set! Run settings_local.sh to fix this"
   exit 1;
fi
if [ ! -z "$OPENDALIB" ]; then
   echo "setting path for OPENDALIB"
   export LD_LIBRARY_PATH=$OPENDALIB:$LD_LIBRARY_PATH
fi

# java options
if [ -z "$ODA_JAVAOPTS" ]; then
   export ODA_JAVAOPTS='-Xmx1024m'
fi

# append all jars in opendabindir to java classpath
for file in $OPENDADIR/*.jar ; do
   if [ -f "$file" ] ; then
       export CLASSPATH=$CLASSPATH:$file
   fi
done

# starting from bin-directory is necessary for *.so loading now TODO fix this
if [ $GUI == true ]; then
    if [[ ! -z ${PARALLEL} || ${LOG} = true ]]; then
        echo "Options -p and -o=<log> ignored in combination with -gui."
    fi
    if [ $configfile ]; then
        #echo "start OpenDA with option -gui ${configfile}"
        java $ODA_JAVAOPTS org.openda.application.OpenDaApplication -gui $configfile
    else
        #echo "start OpenDA with option -gui"
        java $ODA_JAVAOPTS org.openda.application.OpenDaApplication -gui
    fi
else
    if [ ! $configfile ]; then
        echo "OpenDA config file missing."
        exit 1;
    fi

    logfile="`dirname $CONFIG`/$LOGFILE"

    echo "========================================================================="
    echo Starting "java org.openda.application.OpenDaApplication ${PARALLEL} > $logfile  2>&1"

    # start timing
    STARTRUN=`date +%s`
    echo "$configFile" > $logfile
    date "+%F, %H:%M:%S" >> $logfile 2>&1

    # run application
    java $ODA_JAVAOPTS org.openda.application.OpenDaApplication $configFile > $logfile  2>&1

    # end timing
    date "+%F, %H:%M:%S" >> $logfile 2>&1
    ENDRUN=`date +%s`
    declare -i DURATION
    DURATION=($ENDRUN-$STARTRUN)
    echo DURATION, "$DURATION" >> $logfile 2>&1

    echo "Run finished"
    echo "========================================================================="
fi
