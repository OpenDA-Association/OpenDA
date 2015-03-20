#! /bin/sh
# arg 1) name extension to savedir

export savedir="saved_$1_`date +'%Y%m%d%I%M%S'`"
echo "saving output to $savedir"

mkdir $savedir
cp *.* $savedir
sleep 1
