#! /bin/bash

#tests
env >env.txt

#delft3d v6 config
export DELFT3DDIR=/opt64/delft3d_trunk
export LD_LIBRARY_PATH=$DELFT3DDIR/lib
$DELFT3DDIR/bin/d_hydro.exe config_d_hydro.xml
