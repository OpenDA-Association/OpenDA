=================================
Continue on a previous OpenDA run
=================================

On operational systems, OpenDA will run repetitively on a scheduled basis, for instance, daily.
For each run, there will be new data for new predictions, for instance, for the next day.
The start time of the predicted period will then most likely be the end time of the previous run.
Therefore OpenDA could just continue on the status of the end of the last run.
This can be done using ``restartIn``- and ``restartOut``-related configuration in the main OpenDA configuration ``.oda`` file.

Saving the output of a certain run is done as follows:: 

    <restartOutFilePrefix>rst_</restartOutFilePrefix>
    <restartOutFileExtension>.zip</restartOutFileExtension>
    <restartOutFileOnlyAtEndOfRun>true</restartOutFileOnlyAtEndOfRun>
	
The option ``<restartOutFileOnlyAtEndOfRun>`` has been added in OpenDA 3.1. When this option is set to true, a restart zip file is written only at the end of the last run (compared to every analysis step). This is very useful in an operational system since the users have no interest in options to restart from intermediate times in the last run. Another advantage is that there is no need to configure explicit times (that need to change each consecutive run).

On the other hand, continuing with a previously written zip file can be configured as follows::

    <restartInFile>rst_202201010000.zip</restartInFile>

When running from an operational system (like Delft-FEWS), the time stamps differ every run as time progresses.
As a result, it is usually impossible to use the same configuration for subsequent runs.
However, zip files can be renamed outside of OpenDA to use a constant time stamp in the file name. For example, ``rst_202212160000.zip`` (related to a previous run that ran until December 16th, 2022) can be renamed to ``rst_202201010000.zip``. 
This way, there is no need to change the OpenDA configuration for subsequent runs.

Full ``.oda`` example file::

     <?xml version="1.0" encoding="UTF-8"?>
     <openDaApplication  xmlns="http://www.openda.org" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.openda.org http://schemas.openda.org/openDaApplication.xsd">
     	<stochObserver className="org.openda.observers.IoObjectStochObserver">
     		<workingDirectory>./stochObserver</workingDirectory>
     		<configFile>stochObsConfig.xml</configFile>
     	</stochObserver>
     	<stochModelFactory className="org.openda.blackbox.wrapper.BBStochModelFactory">
     		<workingDirectory>./stochModel</workingDirectory>
     		<configFile>dflowfmStochModel.xml</configFile>
     	</stochModelFactory>
     	<algorithm className="org.openda.algorithms.kalmanFilter.EnKF">
     		<workingDirectory>./algorithm</workingDirectory>
     		<configString>EnKF.xml</configString>
     	</algorithm>
     	<restartInFile>rst_202201010000.zip</restartInFile>
        <restartOutFilePrefix>rst_</restartOutFilePrefix>
        <restartOutFileExtension>.zip</restartOutFileExtension>
        <restartOutFileOnlyAtEndOfRun>true</restartOutFileOnlyAtEndOfRun>
     </openDaApplication>
