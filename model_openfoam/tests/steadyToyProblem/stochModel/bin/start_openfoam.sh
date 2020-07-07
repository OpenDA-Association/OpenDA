#!/bin/bash
usage="$(basename "$0") [-h] [-d] [-p nrprocs] [-s solver] [-v]-- script to run OpenFOAM case within OpenDA

where:
    -h  show this help text
    -d  dryrun
    -p  number of parallel processes
    -s  solver
    -v  verbose
"
# exit on error
set -e

# Initialize our own variables:
dryrun=0
verbose=0
nrprocs=1
solver=''

# A POSIX variable
OPTIND=1         # Reset in case getopts has been used previously in the shell.
while getopts "h?dp:s:v" opt; do
    case "$opt" in
    d) dryrun=1;verbose=1;
        ;;
    h|\?)
        # show_help
        echo "$usage"
        exit 0
        ;;
    p)  nrprocs=$OPTARG
        ;;
    s)  solver=$OPTARG
        ;;
    v) verbose=1
        ;;
    esac
done

if [ nrprocs -gt 1 ]; then
    count=0
    count=`find . -maxdepth 1 -type d -name 'processor[0-9]*' | wc -l`

    if [ $verbose -eq 1 ]; then echo "Found ${count} existing processor directories."; fi
    if [ $verbose -eq 1 ]; then echo "Starting job on ${nrprocs} processors."; fi

    if [ "$count" -ne "$nrprocs" ]; then
        if [ "$verbose" -eq 1 ]; then echo "decomposePar"; fi;
        if [ "$dryrun" -eq 0 ]; then decomposePar > decomposePar.log; fi;
    else
        if [ "$verbose" -eq 1 ]; then echo "decomposePar -fields"; fi;
        echo "decomposePar -fields";
        if [ "$dryrun" -eq 0 ]; then decomposePar -fields > decomposePar.log; fi;
        decomposePar --fields
    fi
fi

if [ $nrprocs -eq 1 ]; then
    if [ "$verbose" -eq 1 ]; then echo "${solver}"; fi
    if [ "$dryrun" -eq 0 ]; then ${solver} | tee >(grep '^Time =') > ${solver}.log; fi
else
    if [ "$verbose" -eq 1 ]; then echo "mpirun -np ${nrprocs} ${solver} -parallel"; fi
    if [ "$dryrun" -eq 0 ]; then mpirun -np ${nrprocs} ${solver} | tee >(grep '^Time =') > ${solver}.log; fi
fi

if [ nrprocs -gt 1 ]; then
    if [ "$verbose" -eq 1 ]; then echo "reconstructPar -latestTime"; fi
    if [ "$dryrun" -eq 0 ]; then  reconstructPar -latestTime  | tee >(grep '^Time =') > reconstructPar.log; fi
fi
