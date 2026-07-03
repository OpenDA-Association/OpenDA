#!/bin/bash
pushd core/native
./autoreconf_fix.sh 
./configure --with-jdk=/usr/lib/jvm/default-java 
make install
popd
