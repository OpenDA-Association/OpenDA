# build the native part of OpenDA
# NOTE: this script must be started in the root dir of OpenDA
set -euo pipefail

pushd core/native
./autoreconf_fix.sh
#Added flags due to stricter gfortran 10
./configure FFLAGS=-fallow-argument-mismatch FCFLAGS=-fallow-argument-mismatch
make install
popd
