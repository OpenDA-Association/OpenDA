HIER=$PWD
# build the native part of OpenDA
# NOTE: this script must be started in the root dir of OpenDA

HIER=$PWD
cd core/native
./autoreconf_fix.sh 
./configure  
make install
cd $HIER
