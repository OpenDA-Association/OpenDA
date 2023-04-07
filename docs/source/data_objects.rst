===================
Netcdf data objects
===================
On this page, we describe the use of the netcdf data object. 
This data object is not specific to particular wrappers, but can be used for many different netcdf files. 
Here, the format of the variables should match one of the examples below when reading observations, model results
or even updating states or boundary conditions.
The netcdf data object is very useful since many different model
software packages use netcdf files for storing and reading data.

In ``org.openda.exchange.dataobjects.NetcdfDataObject``, the
implementation of netcdf data objects can be found. It supports many
different variable formats like scalar and gridded data. The most important ones will be described here.

.. _Scalar data:

Scalar data
-----------

The simplest form of scalar data has two dimensions, one for time and
one for location. This will create exchange items for each location in
each variable. The corresponding id is a combination of the variable name 
with the location name, which can be any string. The exchange item will contain the data for all the time steps. As an example, we create the exchange items

``24.waterlevel``, ``25.waterlevel``, ``26.waterlevel``, ``27.waterlevel``,
``28.waterlevel``, ``29.waterlevel``

by using the following variables ``waterlevel`` and ``data``::

  float waterlevel(time=3, stations=6); 
    :standard_name = "water_surface_height_above_reference_datum detection_minimum";
    :long_name = "waterlevel"; 
    :units = "m"; 
    :_FillValue = -9999.0f; // float 
    :coordinates = "lat lon"; 
    :cell_methods = "time: maximum";
  
  data: 
    station_id = "26", "27", "28", "24", "29", "25"
  
with corresponding XML config::

  <dataObject className="org.openda.exchange.dataobjects.NetcdfDataObject">
      <file>scalar.nc</file> 
      <id>dataObjectId</id>
      <arg>true</arg>
      <arg>false</arg>
  </dataObject>

Note: 

 - The options for ``standard_name`` are defined `here <https://cfconventions.org/Data/cf-standard-names/current/build/cf-standard-name-table.html>`__. The ``long_name`` can be chosen.
 - :ref:`Below <3D scalar data>`, an example of 3D scalar data is given. 

Gridded data
------------

Gridded data variables are supported with three dimensions containing time, :math:`x` and :math:`y`:
in this case 1 exchange item will be created for the whole variable with the variable id as a name.

Example of a gridded variable::

   double p(time=49, Y=16, X=27);
     :coordinates = "Time Y X";
     :long_name = "Atmospheric Pressure";
     :standard_name = "air_pressure";
     :units = "Pa";
  
with corresponding XML config (almost the same as for simple scalar, except for the filename)::

  <dataObject className="org.openda.exchange.dataobjects.NetcdfDataObject">
      <file>grid.nc</file> 
      <id>dataObjectId</id> 
      <arg>true</arg>
      <arg>false</arg> 
  </dataObject>

Extra arguments
---------------

In this section, we describe some extra arguments that can be used in the netcdf data object. 

Boolean: Lazy reading and writing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The first two possible extra arguments are boolean values for lazy reading and lazy writing. When lazy
reading is set to true, the data will only be read from the netcdf file
when the data is needed instead of when initializing the data object.
When lazy writing is set to true, the data will be written when the data
object is closed instead of directly when the data is being changed by
OpenDA. 
Depending on how much data will be read and/or written, how many times this will happen and the available memory, users can choose which settings will be most suitable.

These boolean arguments always have to be specified as the first 2 ``<arg>`` elements (lazy reading, lazy writing, respectively) in the XML config for the ``NetcdfDataObject``, see the example :ref:`above<Scalar data>`.

Key-value pair: ``requiredExchangeItemId``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To prevent a long list of arguments with a specific order,
key-value pair arguments have been introduced. Any number of
``requiredExchangeItemId=<ID>`` arguments can be supplied which limits
the exchange items being created to the ones supplied. This can save a
lot of memory and performance for large netcdf files.

If we use the same variables ``waterlevel`` and ``data`` as :ref:`above<Scalar data>`, then
it is possible to limit the creation of exchange items by adding three extra
arguments: ``requiredExchangeItemId=24.waterlevel``,
``requiredExchangeItemId=26.waterlevel``, and 
``requiredExchangeItemId=27.waterlevel``.  This way only the specified
exchange items will be created.

The corresponding XML config looks as follows::

  <dataObject className="org.openda.exchange.dataobjects.NetcdfDataObject">
      <file>scalar.nc</file> 
      <id>dataObjectId</id> 
      <arg>true</arg>
      <arg>false</arg> 
      <arg>requiredExchangeItemId=24.waterlevel</arg>
      <arg>requiredExchangeItemId=26.waterlevel</arg>
      <arg>requiredExchangeItemId=27.waterlevel</arg>
  </dataObject>

.. _3D scalar data:

Key-value pair: ``layerDimensionName``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For scalar data that have multiple layers, a
``layerDimensionName=<name>`` needs to be provided. The size of that
dimension will determine the amount of layer-specific exchange items
that will be created. When this argument is provided then the id of the
exchange items will be constructed as follows:
``<variableName>.<locationName>.layer<layerIndex>``

As an example, we take the next variables ``temperature`` and ``data``::

  double temperature(time=49, stations=3, laydim=20); 
    :coordinates = "station_x_coordinate station_y_coordinate station_name zcoordinate_c";
    :units = "degC"; 
    :geometry = "station_geom"; 
    :_FillValue = -999.0; // double 
    :standard_name = "sea_water_temperature";

  data: 
    station_id = "station01", "station02", "station03"

This will result in 60 exchange items with ids like:``station0i.temperature.layerj``, where i=1,2,3, j=0,...,19.

The corresponding XML config looks as follows::

  <dataObject className="org.openda.exchange.dataobjects.NetcdfDataObject">
    <file>scalarLayers.nc</file> 
    <id>dataObjectId</id> 
    <arg>true</arg>
    <arg>false</arg> 
    <arg>layerDimensionName=laydim</arg> 
  </dataObject>

Key-value pair: ``allowTimeIndependentItems``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The argument ``allowTimeIndependentItems=true/false`` determines whether
time-independent exchange items should be created. Time-independent
exchange items can be created for variables that do not depend on a time
dimension. The default is false.

