=================
Restarting OpenDA
=================

OpenDA can write zip files in order to continue later on.
It is configured as follows::

    <restartOutFilePrefix>rst_</restartOutFilePrefix>
    <restartOutFileExtension>.zip</restartOutFileExtension>
    <restartOutFileOnlyAtEndOfRun>true</restartOutFileOnlyAtEndOfRun>
	
The option ``<restartOutFileOnlyAtEndOfRun>`` is added in OpenDA 3.1 and when this is set to true only at the end of the run a restart zip file is written instead of at every analysis step. This is very useful in an operational system where you always want to continue at the end of the last run and have no interest in options to restart from intermediate times in the last run. This way it also does not need to have explicit times configured that need to change each consecutive run.

Continuing with a previously written zip file can be configured as follows::

    <restartInFile>rst_202201010000.zip</restartInFile>

When running from an operational system (like Delft-FEWS), zip files can be renamed outside of OpenDA to a constant time stamp in the file name. This way the OpenDA configuration can stay the same even though it will use the restart files from different times. Then a trick is applied where for instance ``rst_202212160000.zip`` is written in a previous run that ran until December 16th, 2022 but it is renamed to ``rst_202201010000.zip`` so the OpenDA configuration can refer to a constant zip file name. 
