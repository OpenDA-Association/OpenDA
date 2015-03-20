#! /bin/bash
#
# Purpose : test script for simple model
# Args    : see org.openda.examples.simplef90model.SimpleModelInstanceTestExe.printUsage()
#           <file>  to open configuration file
# Author  : S. Hummel
# License : GPL


# append all jars in bindir to java classpath
bindir=`(cd \`dirname $0\`; pwd)`
# make sure java can find shared libraries in this directory
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$bindir
# add all jar-files from bin-dir to classpath
for file in $bindir/*.jar ; do
   if [ -f "$file" ] ; then
       export CLASSPATH=$CLASSPATH:$file
   fi
done

#echo "CLASSPATH = " $CLASSPATH

java org.openda.examples.simplef90model.SimpleModelInstanceTestExe $1 $2

