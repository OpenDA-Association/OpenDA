

Running tests on linux

To make these tests work on a particular computer, OpenDA needs to find the installation of Delft3D.
The folder with the delft3d-executable d_hydro.exe should be added as 
export DELFT3DBINDIR="<your install dir>"
to settings_local_<your_hostname>.sh in the openda bin-folder. OpenDA will then call
$DELFT3DBINDIR/d_hydro.sh
If d_hydro.sh is missing for you then a copy can be found in the scripts folder.

Running tests on windows

TODO
