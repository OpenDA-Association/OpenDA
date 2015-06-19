#! /bin/sh


# this dir is the toplevel dir
if [ -d public ]; then
  export TOPDIR=$PWD
fi
if [ -d openda ]; then
  export TOPDIR=$PWD/openda
fi
if [ -d core ]; then
  export TOPDIR=$PWD/..
fi

$TOPDIR/public/scripts/openda_build_linux.sh linux64_ifort
