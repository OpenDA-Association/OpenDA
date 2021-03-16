This example was created as a ToyProblem for testing using these versions of OpenDA and OpenFOAM: 

* OpenDA 2.4.4.1
* OpenFOAM v1712

It is not tested (yet) for OpenDA 3.0 and/or newer releases of OpenFOAM. 



The OpenFOAM model is a simplified geometry representing a room in a data-center. There is a false floor, a CRAC unit introducing cooled air into the room, an 'alley' with 4 server racks that produce heat and 4 temperature sensors close to the ceiling.  

The OpenDA configuration files are for a Black Box coupling (see Chapter "OpenDA black box wrapper cookbook" in OpenDA_documentation.pdf). Some of the OpenFOAM input files contain lines ending with:

​	//#oda:keyword

OpenDA is able to modify such lines, using the wrapper code as available in ~/model_openfoam/java/src/org/openda/model_openfoam. 

Heat is produced by the server racks, the modeling of this process is in file ~/system/HEATING. We have run OpenFOAM with certain parameters for the heat production and use the output of the temperature sensors for this run as 'measurements'.

If you start the OpenDA-OpenFOAM run with slightly different settings for the heat production (either calibration with DUD.oda or Kalman Filtering with EnKF.oda) you can check that the values in files ~/work\*/\*/HEATING are indeed updated by OpenDA. 