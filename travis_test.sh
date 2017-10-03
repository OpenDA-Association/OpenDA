#!/bin/sh
echo "Start of script travis_test.sh"
echo "PWD=$PWD"
export OPENDADIR=$PWD/bin
. $OPENDADIR/settings_local.sh linux
env

ant test-travis

