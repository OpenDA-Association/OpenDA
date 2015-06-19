#! /bin/sh
#
# Create a fresh checkout of openda
#

if [ "$1" == "" ]; then
   export SVNUSER=$USER
else
   export SVNUSER=$1
fi

echo Checking out the repository, using user-ID \"$SVNUSER\"

okay=1
for program in svn javac gcc gfortran g++ xml2-config ant; do
   if [ "`which $program 2>/dev/null`" == "" ]; then
      echo Please install: $program
      okay=0
   fi
done
if [ $okay == 0 ]; then
   echo One or more programs missing, can not continue
   echo Install these first or expand the path!
   exit
fi

if [ ! -d openda ] ; then
   mkdir openda
   export SVNURL='https://openda.svn.sourceforce.net/svnroot/openda'
   svn co $SVNURL openda
else
   echo "Directory openda already exists."
   echo "You can update with subversion 'svn update' or delete it and
rerun."
fi

