#!/bin/sh

# append all jars in opendabindir to java classpath
for file in $OPENDADIR/*.jar ; do
   if [ -f "$file" ] ; then
       export CLASSPATH=$CLASSPATH:$file
   fi
done

killall -v rmiregistry
rmiregistry &
echo wait 5 seconds for rmiregistry to start and initialize
sleep 5

# start the server with a non-default factory ID
echo starting server
java  -Djava.rmi.server.codebase="file:///$OPENDADIR/" org.openda.models.rmiModel.Server IRmiIStochModel_1 &
java  -Djava.rmi.server.codebase="file:///$OPENDADIR/" org.openda.models.rmiModel.Server IRmiIStochModel_2 &
java  -Djava.rmi.server.codebase="file:///$OPENDADIR/" org.openda.models.rmiModel.Server IRmiIStochModel_3 &
java  -Djava.rmi.server.codebase="file:///$OPENDADIR/" org.openda.models.rmiModel.Server IRmiIStochModel_4 &

