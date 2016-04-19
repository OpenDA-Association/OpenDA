#! /bin/bash


#delft3d v6 config
export DELFT3DBINDIR=${DELFT3DBINDIR:=/opt64/delft3d_trunk/bin}
$DELFT3DBINDIR/d_hydro.sh config_d_hydro_dd.xml
