# build the native part of OpenDA
# NOTE: this script must be started in the root dir of OpenDA
set -euo pipefail

cmake -S costa/native -B build -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=bin/linux64_gnu
cmake --build build --target install
