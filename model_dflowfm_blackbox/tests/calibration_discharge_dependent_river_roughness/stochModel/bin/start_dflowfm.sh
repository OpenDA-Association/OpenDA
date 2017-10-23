#! /bin/bash
#
echo "script : start_dflowfm.sh"
echo "pwd : $PWD"
echo "args : $1"
run_dflowfm.sh --autostartstop $1
