D-Flow Flexible Mesh is not part of OpenDA and should be installed separately on your computer. 
Please refer to the D-Flow FM User Manual for instructions on installing and running D-Flow FM on the commandline. 

	https://content.oss.deltares.nl/delft3d/manuals/D-Flow_FM_User_Manual.pdf

All OpenDA tests in this directory start the D-Flow Flexible Mesh executable with either a shell or a bat script located in directory ./test_name/stochModel/bin. More information on the contents of these scripts can be found in chapter "Running a model" in 
the D-Flow FM manual.

If D-Flow Flexible Mesh cannot be started by OpenDA, you probably need to check and modify the installation path or other details in one of these scripts. Please try:

1. start D-Flow FM on the commandline as described in the User Manual. The D-Flow FM Input files are located in ./*name\_of\_test*/stochModel/input_dflowfm.
2. start the example from ./*name\_of\_test*/stochModel/input\_dflowfm by running one of the scripts 
	"..\bin\start\_dimr.{bat/sh} dimrConfig.xml".
3. in each directory *name\_of\_test* there is an OpenDA input file **Simulation.oda**. Start the OpenDA GUI, open one of these files Simulation.oda and try to run it.

		$OPENDADIR/oda_run.gui.bat
		$OPENDADIR/oda_run.sh -gui
	
4. If the previous three steps are OK, it should also be possible to run Dud.oda and/or EnKF.oda. 

The configuration of the examples in this directory was updated and tested for

- OpenDA 3.0.4
- Delft3D Flexible Mesh Suite HM (2021.05)

For other distributions of D-Flow FM the configuration might not be correct. You can help us by leaving a message on the forum
https://sourceforge.net/p/openda/discussion
with your experience (please state clearly the used versions of OpenDA and D-Flow FM).


