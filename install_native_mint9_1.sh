HIER=$PWD
# build the native part of OpenDA
# NOTE: this script must be started in the root dir of OpenDA

HIER=$PWD
cd core/native
./autoreconf_fix.sh 
./configure --with-jdk=/usr/lib/jvm/default-java 
make install
cd $HIER
