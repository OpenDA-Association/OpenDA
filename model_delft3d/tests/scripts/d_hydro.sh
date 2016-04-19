#! /bin/sh
# startup script for delft3d

if [ $# -eq 0 ] ; then
   echo "usage: d_hydro.sh <input-file>> [options]"
fi
export OMP_NUM_THREADS=3
export DELFT3DROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
export DELFT3DROOT="${DELFT3DROOT}/.."

export PATH=$DELFT3DROOT/bin:$PATH
export LD_LIBRARY_PATH=$DELFT3DROOT/lib:$LD_LIBRARY_PATH
ulimit -s unlimited

$DELFT3DROOT/bin/d_hydro.exe $*

