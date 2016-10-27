#!/bin/bash
usage="Usage:$(basename "$0") [-h] [-c REFERENCE_DATA_OBJECT] [-a ARGUMENTS] REFERENCE_FILE [-c TEST_DATA_OBJECT] [-a ARGUMENTS] TEST_FILE 

Compare Exchange Items from TEST_FILE to REFERENCE_FILE using OpenDA DataObjects

where:
    -a  DataObject arguments
    -c  specify the dataobject to be used    
    -h  show this help text
    -v  verbose
"
# exit on error
set -e

# A POSIX variable
OPTIND=1         # Reset in case getopts has been used previously in the shell.
while getopts "h?a:c:v" opt; do
    case "$opt" in
    h|\?)
        # show_help
        printf "$usage"
        exit 0
        ;;
    a)  nrprocs=$OPTARG
        ;;
    c)  solver=$OPTARG
        ;;
    v) verbose=1
        ;;
    esac
done


# read local settings
if [ -z "$OPENDADIR" ];then
    echo "OPENDADIR not set! Run settings_local.sh to fix this"
    exit 1;
fi
if [ !-z "$OPENDALIB" ]; then
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

# start java with initial class
java $ODA_JAVAOPTS org.openda.exchange.iotools.DataObjectDiffer "$@"
