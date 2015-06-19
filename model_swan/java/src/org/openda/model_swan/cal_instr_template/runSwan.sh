#! /bin/sh

echo "RUNNING SWAN" > runSwan-sh-out.txt

swancalbindir=`(cd \`dirname $0\`; pwd)`
echo "swancalbindir $swancalbindir" >> runSwan-sh-out.txt

if [ $# -eq 2 ] ; then
	# 2 parameter, must be case name and the linux executable
    swanExe=$2
elif [ $# -eq 5 ] ; then
	# 5 parameter, case name and ser./ser-linux-executable, par./par-linux-executable
	if [ "$RUN_SWAN_PARALLEL" == "true" ] ; then
	    swanExe=$5
	else
	    swanExe=$3
	fi
else
	echo "Invalid number of parameters ($#):" >> runSwan-sh-out.txt
	echo "$1 $2 $3 $4 $5 $6 $7 $8 $9 ${10}" >> runSwan-sh-out.txt
	exit 1
fi

echo "swanExe: $swanExe" >> runSwan-sh-out.txt

swanCaseName=$1

if [ "$RUN_SWAN_PARALLEL" == "true" ] ;then
	echo "executing $swancalbindir/runSwanPar.sh $swanCaseName $swanExe" >> runSwan-sh-out.txt
	$swancalbindir/runSwanPar.sh $swanCaseName $swanExe
else
	swanExePath=$swancalbindir/swanExes/linux/$swanExe
	if [ ! -f $swanExePath ] ; then
		echo "swan executable $swanExePath does not exist" >> runSwan-sh-out.txt
		exit 1
	else
		echo "executing $swanExePath" >> runSwan-sh-out.txt
		$swanExePath
	fi
fi

