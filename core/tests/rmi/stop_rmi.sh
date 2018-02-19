#!/bin/sh

# kill the rmi registry
killall -v rmiregistry
#kill all java stuff using rmi
kill $(ps -f |grep java| grep rmi|grep -v grep |cut -f 2 -d ' ')

