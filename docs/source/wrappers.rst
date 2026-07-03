========================
Introduction to wrappers
========================
OpenDA model wrappers are used to connect existing models (such as D-Flow FM)
to OpenDA, and call these external models from within an OpenDA run.  
In OpenDA, models are used in *black-box* form, which means that data-assimilation
and model calibration techniques are applied to a model without changing the
existing model code. OpenDA and the black-box wrapper have no knowledge of the
model internals (black box) and only use the input and output files of the
model.  In order to connect such
models to OpenDA, specific functionality needs to be created to supply OpenDA
with information about the model runs.  Examples include run periods, model input
values or files that OpenDA can adjust, and model output values OpenDA can
read.  Note that the file containing the model run time must be rewritten as
OpenDA cuts the whole run period into smaller pieces.  
The existing wrappers can be
found in the ``model_*`` directories in the OpenDA root.

Data objects
------------

For each input or output file, OpenDA will read or write a *data object*,
which needs to be configured in a wrapper configuration XML file.  The wrapper
configuration files are based on the ``blackboxWrapperConfig.xsd`` schema.
In such a wrapper configuration file, we have::

  <dataObject className="[package].[className]">
      <file></file>
      <id></id>
  </dataObject>

The ``dataObject`` contains the following information: 

 - ``className="[package].[className]"``: refers to the main Java code that handles (wraps) the file;
 - ``<file>``: refers to the file that needs to be read / written;
 - ``<id>``: specifies the identifier of the object so it can be referred to at other places in the configuration.

For example, we can construct the following data object::

  <dataObject className="org.openda.model_dflowfm.DFlowFMRestartFileWrapper">
     <file>rstfilename</file>
     <id>rstfile</id>
  </dataObject>

Exchange items
--------------

For each data object, OpenDA will create virtual objects in memory containing
the needed values.  These virtual objects are called `exchange items`, because
OpenDA has the possibility to change these values according to its algorithms.
An exchange item can be seen as a container of values with optionally some
meta information. In the most simple form an exchange item contains a single value or array
and has an identifier. This identifier can be used to refer to from other
parts of the OpenDA configuration.  The identifiers of the exchange items are
determined by the specific data objects and ideally resemble the meaning of
the values they hold in memory.  For instance, a data object created for a
file that contains information about the start and end time of a run will
typically create two exchange items with ``id`` ``start_time`` and
``end_time``.  But a data object for model results would typically create
exchange items with identifiers resembling station and parameter identifiers, like
``waterlevel_delft``, ``temperature_delft``, ``waterlevel_rotterdam``
etc.  Since each data object is model and file specific and there are
no strict rules about or automatic functionality for the exact identifiers,
they will differ a lot between models. 

In the easiest case, all exchange items of a data object are used with the
identifiers the data object has created for them::

 <vector id="allElementsFromDataObject" dataObjectId="rstfile"/>

Here, the ``dataObjectId`` refers to the ``rstfile`` identifier that was chosen
in the example above.

It is also possible to explicitly configure which exchange items are used and
which identifiers they have throughout the code.  For example:: 


  <vector id="start_time" dataObjectId="mdufile" elementId="start_time_data_object_name" />
  <vector id="end_time" dataObjectId="mdufile" elementId="end_time_data_object_name" />
		
Here,

 - ``id`` determines the identifier that can be referenced elsewhere in the configuration; 
 - ``dataObjectId`` refers to the identifier of the data object as specified in the wrapper configuration;
 - ``elementId`` has to match the exchange item identifier that the data object has created for it.

When ``elementId`` is not specified, the ``id`` has to match the exchange item
identifier that the data object has created for it.

Within the model configuration XML file, there is a specific element to
determine which exchange items are used for the model run period::

  <timeInfoExchangeItems start="start_time" end="end_time"/>

These ``start_time`` and ``end_time`` should be the ``elementId`` identifiers of the data object.
  
Aliases
-------

In the OpenDA model configuration (``blackBoxModelConfig``) aliases can be used,
for instance for file references::

  <aliasValues>
      <alias key="mdufile" value="dflowfm/estuary.mdu"/>
      <alias key="hisfile" value="dflowfm/DFM_OUTPUT_estuary/estuary_his.nc"/>
      <alias key="rstfile" value="dflowfm/estuary.mdu"/>
  </aliasValues>
	
These aliases can be used as file references in other parts of the OpenDA
configuration.  Benefit of this is the longer paths do not need to be repeated and
if a file path or name changes, it only needs to be changed in one place.  It
also enables configurators to use names which make more sense to them.
In the wrapper configuration, the following lines are needed to make sure the
aliases can be used::

  <aliasDefinitions defaultKeyPrefix="%" defaultKeySuffix="%">
      <alias key="mdufile"/>
      <alias key="hisfile"/>
      <alias key="rstfile"/>
  </aliasDefinitions>
	
The surrounding ``%`` make sure we refer to tags: its content is
defined elsewhere.  
Below we will see an example where aliases are used in practice.

Compute actions
---------------

.. _compute actions:

Actions are configurations that specify which (external) executables
OpenDA should run. Typically, this would be the model itself. 
This will be configured as ``<computeActions>``::

  <computeActions>
      <action workingDirectory="%instanceDir%" linuxExe="%exedir%/<start_exe.sh>" windowsExe="%exedir%/<start_exe.bat>">
          <arg>%optional_argument_1%</arg>
          <arg>%optional_argument_2%</arg>
          <checkOutput file="%required_output_1%"/>
          <checkOutput file="%required_output_2%"/>
      </action>
  </computeActions>
			
OpenDA will run the model executable matching the operating system.
This would be the main and most important execute action. Here, ``checkOutput`` checks whether a certain output file has been created.

Other actions can include pre- or postprocessing steps. For example, the
:ref:`NetCDF concatenater<NetCDF concatenater>` is a postprocessing step that merges smaller NetCDF result
files from multiple runs into a single NetCDF file.
Other examples include the copy or rename of certain files.
This can be the case when a model uses time stamps in files that OpenDA needs to read or change.
Then a postprocessing step can find the most recent file and locate (and rename) it such that OpenDA can find it or use it for a restart for the next model run.
An example of this is used in ``model_dflowfm_blackbox/dcsmv5_kalman_rst/stochModel/dflowfmWrapper.xml``::

  <action className="org.openda.model_dflowfm.DFlowFMRestartFilePostProcessor" workingDirectory="%instanceDir%">
      <arg>runId=%runid%</arg>
      <arg>sourceRestartFileSubDir=%outputDir%</arg>
      <arg>targetRestartFileNamePostFix=00000000_000000_rst.nc</arg>
      <arg>deleteOlderRstFiles=true</arg>
  </action>

This action does not refer to a Windows or Linux executable but to an executable Java class which is part of the OpenDA source code and in this case also part of the D-Flow FM specific wrapper code. More explanation on the specific contents of this action can be found at the :ref:`DFlow-FM wrapper<rst nc>` page.
