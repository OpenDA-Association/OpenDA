D-Flow Flexible Mesh is not part of OpenDA and should be installed separately on your computer. 

All tests start the D-Flow Flexible Mesh executable with a shell or bat script located in directory *./test_dir/stochModel/bin*. There are 3 scripts in this directory:
 
- start_dimr.bat. Use this script if you have installed Delft3D FM Suite where DIMR (Delft Integrated Model Runner)is used to start the dflowfm.dll
- start_dflowfm.bat. Use this script if your installation contains a command-line executable dflowfm(-cli).exe,
- start_dflowfm.sh. Use this script you have a Linux installation with command line executable
 
If D-Flow Flexible Mesh cannot be started by OpenDA, you probably need to check and modify the installation path or other details in one of these scripts. 

The configuration of the examples in this directory was updated and tested on Windows7:

- OpenDA 2.4.4
- Delft3D Flexible Mesh Suite 2017 HMWQ (1.2.2.36603

For other distributions or platforms the configuration might not be correct.

	Status: 
	Tests that are enabled in run_all_tests.bat run correctly. 
	Tests that are commented out do not run correctly. WORK IN PROGRESS.


