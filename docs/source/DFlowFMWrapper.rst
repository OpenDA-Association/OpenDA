=================
D-Flow FM wrapper
=================
Disclaimer: this page has not been finished yet. Current open questions 
are described in this `issue <https://github.com/OpenDA-Association/OpenDA/issues/431>`__.

The D-Flow FM wrapper has been developed and is maintained by our OpenDA
partner Deltares. On this page, we describe the different data objects for the
D-Flow FM wrapper. Examples based on the dcsmv5 (North Sea) model are available
from OpenDA release 3.1 in the directories ``examples\model_dflowfm_blackbox``.

Start and end time of model runs
--------------------------------

For Ensemble Kalman filtering, we always need a data object that reads, edits
and rewrites the start and end times of a model run.  For D-Flow FM this is
stored in the so-called ``.mdu`` file (the input file for the hydrodynamic
simulation program). More information about this file can be found in Section
4.2 of the `D-Flow FM documentation <https://usermanual.wiki/Pdf/DFlowFMUserManual.1771347134/view>`__.

In order to use :ref:`restarts of a model<restart>`, OpenDA should be able to
change the run period of the model.  The data object related to the start and
end times of model runs has been implemented in the Java class
``org.openda.model_dflowfm.DFlowFMTimeInfo``. This data object reads
``TStart``, ``TStop``, ``Tunit`` and ``RefDate`` from the ``[time]`` section in
the ``.mdu`` file to create the OpenDA exchange items with ids ``start_date``
and ``end_date``. The run period of the model (as set in ``TStart`` and
``TStop`` in the ``.mdu`` file) can then be changed by adapting the exchange
items. In the ``dflowfmModel.xml``, the time exchange items are set as
``<timeInfoExchangeItems start="start_time" end="end_time"/>``.

Example configuration for this data object can be found below::

  <dataObject className="org.openda.model_dflowfm.DFlowFMTimeInfo">
      <file>%mdufile%</file>
      <id>mdufile</id>
  </dataObject>

Model state
-----------

In order to change the model state, the Java class
``org.openda.model_dflowfm.DFlowFMRestartFileWrapper`` is used.  It will read,
edit and rewrite either a ``*map.nc`` file or a ``*_rst.nc`` file. These files
contain the model state from which a model run can restart and continue based
on how a previous run ended. More information about the ``_rst.nc`` file can be
found in Section 5.6.3 of the `D-Flow FM documentation <https://usermanual.wiki/Pdf/DFlowFMUserManual.1771347134/view>`__, and
:ref:`below <rst nc>`). More information about map files can be found in
Section 5.6.2 of the same `D-Flow FM documentation <https://usermanual.wiki/Pdf/DFlowFMUserManual.1771347134/view>`__.

The previously mentioned Java class will create exchange items for each
variable with the variable name as "exchange item id".  An example
configuration for this data object is::

        <dataObject className="org.openda.model_dflowfm.DFlowFMRestartFileWrapper">
            <file>%mapfile%</file>
            <id>mapfile</id>
        </dataObject>
		
For reading NetCDF output files the general NetcdfDataObject can be used. More
information can be found on the page about :ref:`data objects <data objects>`. 

Boundary files
--------------

D-Flow FM boundary files are explained in Section 7.4.1 of the `D-Flow FM documentation <https://usermanual.wiki/Pdf/DFlowFMUserManual.1771347134/view>`__. Here, we
describe the approach for ``.tim`` and ``.bc`` files.

- In order to read, edit and rewrite boundary files in the ``.tim`` format,
  the data object with class name
  ``org.openda.model_dflowfm.DFlowFMTimeSeriesDataObject`` should be used.
  This data object will use the file references in the ``.mdu``, ``.ext``, and
  ``.pli`` files to locate the ``.tim`` files (see the `D-Flow FM documentation <https://usermanual.wiki/Pdf/DFlowFMUserManual.1771347134/view>`__). 
  
  An example configuration for this data object is::
  		
          <dataObject className="org.openda.model_dflowfm.DFlowFMTimeSeriesDataObject">
              <file>%mdufile%</file>
              <id>boundaries</id>
          </dataObject>
		
- In order to read, edit and rewrite boundary files in the ``.bc`` format, the
  data object with class name ``org.openda.model_dflowfm.BcFile`` must be used.
  As an extra argument, the file(name) of the ``.bc`` file after editing must
  be provided in ``<arg>``. If this filename is identical to the original, then
  OpenDA will overwrite the original ``.bc`` file. As an example, we provide::
  		
          <dataObject className="org.openda.model_dflowfm.BcFile">
              <file>boundary_conditions/flow/TielNewNoise_0001.bc</file>
              <id>bcfile</id>
              <arg>boundary_conditions/flow/TielNewNoise_0001.bc</arg>
          </dataObject>

NetCDF concatenater
-------------------
OpenDA splits longer runs into smaller parts (as we already explained in the
section on :ref:`restarts of a model<restart>`).  Therefore, lots of smaller
NetCDF output files are written instead of one single NetCDF output file that
contains the whole run.  To prevent them from getting overwritten each time and
in order to merge them together, OpenDA has a NetcdfFileConcatenater. This is
general functionality available in OpenDA and can be used for many different
models that create NetCDF output files.  This concatenater (formally spelled as
concatenator) should be configured as compute action after the compute action
of the model run.  It takes two arguments: the first argument is the name of
the file with all concatenated data and the second argument is the file that
should be concatenated to it. Since the concatenator will run after each small
model run, only 1 NetCDF file has to be concatenated to the main file
containing all concatenated data.  If the concatenated file does not exist yet
it will be created with identical content to the file that it should be
concatenated with. This will happen after the first model run.
 
In the ``dflowfmWrapper.xml``, of the ``model_dflowfm_blackbox`` examples, the
examples ``dcsmv5_kalman_rst`` and ``dcsmv5_kalman_rst_partitioning`` can be
seen. It works for both scalar data, like the ``%hisfile%`` and gridded data
like the ``%mapfile%`` (Sections 5.6.1 and 5.6.2 of the `D-Flow FM documentation <https://usermanual.wiki/Pdf/DFlowFMUserManual.1771347134/view>`__)::

            <action className="org.openda.exchange.dataobjects.NetcdfFileConcatenater" workingDirectory="%instanceDir%/">
                <arg>%concatenated_file%</arg>
                <arg>%outputDir%/%file%</arg>
            </action>

Restarting from file
--------------------

.. _rst nc:            

Since OpenDA 3.1, we support the use of D-Flow FM models that restart from
``*_rst.nc`` files (see the Section on :ref:`restarts of a model<restart>`). 

An example setup can be found in the folder
``examples\model_dflowfm_blackbox\dcsmv5_kalman_rst``.

To let this work, the argument ``useRstForRestart=true`` must be added to the
``org.openda.model_dflowfm.DFlowFMTimeInfo`` data object.  This prevents OpenDA
to change settings in the ``.mdu`` file specific for the map file (namely,
RestartFile and RestartDateTime, located at the ``[restart]`` section of the
``.mdu`` file). An example configuration is::

        <dataObject className="org.openda.model_dflowfm.DFlowFMTimeInfo">
            <file>%mdufile%</file>
            <id>mdufile</id>
            <arg>useRstForRestart=true</arg>
        </dataObject>
		
The ``DFlowFMRestartFileWrapper`` does not need any changes, but we do need to
add a compute action that uses
``org.openda.model_dflowfm.DFlowFMRestartFilePostProcessor`` after running
D-Flow FM.  This class will rename the newest ``*_rst.nc`` file to a constant
file name which will always be used for restarting. 

We will explain how renaming and restarting works by using an example.
Therefore, we investigate a North Sea example (the model has run id ``dcsmv5``)
that runs from January 3 2007 at midnight until January 5 2007 at midnight.
Furthermore, we assume that a restart file is written every day at midnight. At
the end of the run, the following restart files are created::

%workingDirectory%\subdir\dcsmv5_20070103_000000_rst.nc
%workingDirectory%\subdir\dcsmv5_20070104_000000_rst.nc
%workingDirectory%\subdir\dcsmv5_20070105_000000_rst.nc

The ``DFlowFMRestartFilePostProcessor`` will be looking for files with pattern
``<runId>_'yyyyMMdd'_'HHmmss'_rst.nc`` where the ``<runId>`` is passed as an
argument and the ``_'yyyyMMdd'_'HHmmss'_rst.nc`` is hardcoded (as this is
default for D-Flow FM). The ``DFlowFMRestartFilePostProcessor`` will find all
three files and will use the dates from the file names to extract their time
stamps. 

We choose the following configuration for the compute action::

    <action className="org.openda.model_dflowfm.DFlowFMRestartFilePostProcessor" workingDirectory="%instanceDir%">
        <arg>runId=%runid%</arg>
        <arg>sourceRestartFileSubDir=%outputDir%</arg>
        <arg>targetRestartFileNamePostFix=00000000_000000_rst.nc</arg>
        <arg>deleteOlderRstFiles=true</arg>
    </action>

were the following required and optional arguments are related to
``DFlowFMRestartFilePostProcessor``:

- Required arguments for the renaming are ``runId=<runId>`` and
  ``targetRestartFileNamePostFix='yyyyMMdd'_'HHmmss'_rst.nc``.

  Here, ``'yyyyMMdd'_'HHmmss'`` should match the specific time of the
  configured ``*_rst.nc`` file D-Flow FM is configured to restart from.  This
  restart file is configured in the .mdu file (the ``[restart]`` block contains
  a key-value pair ``RestartFile = <fileName>``).  These two required arguments
  are used to identify all restart files, find the newest one and determine the
  file name that the newest ``*_rst.nc`` file should be renamed to. 
  
- The optional argument ``sourceRestartFileSubDir=subdir`` can be provided when
  the restart files written by D-Flow FM are in a different subfolder than the
  ``*_rst.nc`` file that D-Flow FM will restart from. 
- When the optional argument ``deleteOlderRstFiles=true`` is provided, all
  older ``*_rst.nc`` files will be deleted to prevent the accumulation of files
  that are no longer needed.

Let us return to our example. The compute action will copy and rename the file
with the highest time stamp to ``<runId>_<targetRestartFileNamePostFix>`` in
the ``workingDirectory``. This means
``%workingDirectory%\subdir\dcsmv5_20070105_000000_rst.nc`` will be copied and
renamed to ``%workingDirectory%\dcsmv5_00000000_000000_rst.nc``. For this
mechanism to work properly, ``RestartFile = dcsmv5_00000000_000000_rst.nc``
should be configured in the ``[restart]`` section of the ``.mdu`` file.

This compute action is specifically designed for an operational context where
external systems can store restart files of previous runs and reuse it for
future runs. By using a constant filename these external systems can always
work with the same file name without needing complex functionality for renaming
to match the time stamps.
			
Partitioning
------------

Since OpenDA 3.1, support for D-Flow FM models that are partitioned is added.
More information on D-Flow FM partitioning can be found in Section 5.2.2 of the
`D-Flow FM documentation <https://usermanual.wiki/Pdf/DFlowFMUserManual.1771347134/view>`__.  Similar to
the restarting section above, this setup is specifically designed for an
operational context.  An example setup can be found in 

``model_dflowfm_blackbox\tests\dcsmv5_kalman_rst_partitioning\stochModel\dflowfmWrapper.xml``

For this to work, multiple configuration changes are needed:

- For the ``DFlowFMTimeInfo``, an extra argument should be added specifying the
  number of partitions. This way, OpenDA knows it should edit an ``.mdu`` file
  for each partition and the changes are the same for each ``.mdu`` file::

    <dataObject className="org.openda.model_dflowfm.DFlowFMTimeInfo">
        <file>%mdufile%</file>
        <id>mdufile</id>
        <arg>useRstForRestart=true</arg>
        <arg>numberOfPartitions=3</arg>
    </dataObject>
	
- For the restart file, a different data object must be used for partitioning.
  This is because the changes that will be applied to the contents will differ
  for each partitioned restart file. This has been solved with the use of one
  data object that (in the background) will use multiple
  ``DFlowFMRestartFileWrapper`` data objects and collect all exchange items::

    <dataObject className="org.openda.model_dflowfm.DFlowFMPartitionedRestartFilesWrapper">
        <file>%rstfile%</file>
        <id>rstfile</id>
        <arg>runId=%runid%</arg>
        <arg>numberOfPartitions=3</arg>
    </dataObject>
		
  The exchange items that this data object creates are for each variable and
  each partitioning combination. So if the partitioned restart files contain a
  variable called ``"s1"``, and there are three partitions, then the following
  exchange items will be created according to the naming
  ``<variableName>_<partitionNumber>``: ``"s1_0000"``, ``"s1_0001"``,
  ``"s1_0002"``.
  
- For the ``DFlowFMRestartFilePostProcessor``, one extra argument specifying
  the number of partitions will suffice. This argument will be used in the
  naming pattern to find all restart files, keep the newest and delete older
  ones if needed::

	<action className="org.openda.model_dflowfm.DFlowFMRestartFilePostProcessor" workingDirectory="%instanceDir%">
		<arg>runId=%runid%</arg>
		<arg>sourceRestartFileSubDir=%outputDir%</arg>
		<arg>targetRestartFileNamePostFix=00000000_000000_rst.nc</arg>
		<arg>deleteOlderRstFiles=true</arg>
		<arg>numberOfPartitions=3</arg>
	</action>

  As an example, we revisit the same North Sea example again, using three
  partitions. At the end of the run, the following restart files are created::

        %workingDirectory%\subdir\dcsmv5_0000_20070103_000000_rst.nc
        %workingDirectory%\subdir\dcsmv5_0000_20070104_000000_rst.nc
        %workingDirectory%\subdir\dcsmv5_0000_20070105_000000_rst.nc
        %workingDirectory%\subdir\dcsmv5_0001_20070103_000000_rst.nc
        %workingDirectory%\subdir\dcsmv5_0001_20070104_000000_rst.nc
        %workingDirectory%\subdir\dcsmv5_0001_20070105_000000_rst.nc
        %workingDirectory%\subdir\dcsmv5_0002_20070103_000000_rst.nc
        %workingDirectory%\subdir\dcsmv5_0002_20070104_000000_rst.nc
        %workingDirectory%\subdir\dcsmv5_0002_20070105_000000_rst.nc

  The ``DFlowFMRestartFilePostProcessor`` will be looking for files with
  pattern ``<runId>_<partitionNumber>_'yyyyMMdd'_'HHmmss'_rst.nc`` where the
  ``<runId>`` is passed as an argument and the ``_'yyyyMMdd'_'HHmmss'_rst.nc``
  is hardcoded because that is default for D-Flow FM. The
  ``DFlowFMRestartFilePostProcessor`` will find all nine files and will use the
  dates from the file names to extract their time stamps. For each partition
  individually, it will copy and rename the files with the highest time stamp
  to ``<runId>_<partitionNumber>_<targetRestartFileNamePostFix>`` in the
  ``workingDirectory``. In the example above, this means
  ``%workingDirectory%\subdir\dcsmv5_000n_20070105_000000_rst.nc``, n = 0,1,2
  (the partition number), will be copied and renamed to
  ``%workingDirectory%\dcsmv5_000n_00000000_000000_rst.nc``. For this mechanism
  to work properly, ``RestartFile = dcsmv5_000n_00000000_000000_rst.nc`` should
  be configured in the ``[restart]`` section of the partitioned ``.mdu`` file. 
