#!/bin/sh
#echo "Start of script travis_test.sh"
export OPENDADIR=$PWD/bin
. bin/settings_local.sh linux

ant test-travis
