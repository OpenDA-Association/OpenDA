.. _data objects:

===================
NetCDF data objects
===================
On this page, we describe the use of the NetCDF data object which can be used to create exchange items for NetCDF files. 
This data object is not specific to particular wrappers but can be used for many NetCDF files as long as the format of the variables matches one of the examples below when reading observations, model results
or even updating states or boundary conditions.
The NetCDF data object is very useful since many different model
software packages use NetCDF files for storing and reading data.


In ``org.openda.exchange.dataobjects.NetcdfDataObject``, the
implementation of NetCDF data objects can be found. It supports many
different variable formats, such as

- Scalar data, which in the NetCDF variables have 2 dimensions one for time and one for stations;
- Three-dimensional scalar data, which in the NetCDF variables have 3 dimensions one for time, one for stations, and one for the vertical layers;
- Gridded data, which in the NetCDF variables have 3 dimensions one for time, one for :math:`y` coordinates, and one for :math:`x` coordinates.

These formats will be described in more detail below. 

Note that the current implementation only applies to files that follow the `CF metadata conventions <https://cfconventions.org/>`__.
If the NetCDF file does not, then its own data object should be implemented in ``org.openda.exchange.dataobjects.NetcdfDataObject``.

Scalar data
-----------

The simplest form of scalar data has two dimensions, one for time and
one for location. This will create exchange items for each measurement location in
each observation variable. The corresponding id is a combination of the variable name 
with the location name which can be any string. The exchange item will contain the data for all the time steps. As an example, we create the exchange items

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

The two arguments for lazy reading and writing listed in this configuration will be explained :ref:`below <Lazy reading writing>`.

Three-dimensional scalar data
-----------------------------

For scalar data that have multiple layers an extra key=value argument needs to be provided:
``layerDimensionName=<name>``. The size of that
dimension will determine the amount of layer-specific exchange items
that will be created. 
When this argument is provided the id of the
exchange items will be
``<variableName>.<locationName>.layer<layerIndex>``.
Examples of layer-specific values are concentration or temperature measurements
at different water depths or atmospheric heights.

Suppose we have the next variables ``temperature`` and ``data``::

  double temperature(time=49, stations=3, laydim=20); 
    :coordinates = "station_x_coordinate station_y_coordinate station_id zcoordinate_c";
    :units = "degC"; 
    :geometry = "station_geom"; 
    :_FillValue = -999.0; // double 
    :standard_name = "sea_water_temperature";

  data: 
    station_id = "station01", "station02", "station03"

This will result in 60 exchange items with ids like:``station0i.temperature.layerj``, where i=1,2,3, j=0,...,19.

The corresponding XML config looks as follows (note that the ``layerDimensionName=laydim`` argument refers to the snippet ``laydim=20`` above)::

  <dataObject className="org.openda.exchange.dataobjects.NetcdfDataObject">
    <file>scalarLayers.nc</file> 
    <id>dataObjectId</id> 
    <arg>true</arg>
    <arg>false</arg> 
    <arg>layerDimensionName=laydim</arg> 
  </dataObject>

Gridded data
------------

Gridded data variables are used to describe model results for a computation on a grid :math:`(x,y)`.
They are supported with three dimensions containing time, :math:`x` and :math:`y`.
In this case, 1 exchange item will be created for the whole variable with the variable id as a name.

Example of a gridded variable::

 double p(time=49, Y=16, X=27);
   :coordinates = "Time Y X";
   :long_name = "Atmospheric Pressure";
   :standard_name = "air_pressure";
   :units = "Pa";
  
with corresponding XML config (same as for simple scalar)::

  <dataObject className="org.openda.exchange.dataobjects.NetcdfDataObject">
      <file>grid.nc</file> 
      <id>dataObjectId</id> 
      <arg>true</arg>
      <arg>false</arg> 
  </dataObject>

Extra arguments
---------------
In this section, we describe some extra arguments that can be used in the NetCDF data object. 

Boolean: Lazy reading and writing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.. _Lazy reading writing:

The first two possible extra arguments are boolean values for lazy reading and lazy writing. When lazy
reading is set to true, the data will only be read from the NetCDF file
when the data is needed instead of when initializing the data object.
When lazy writing is set to true, the data will be written when the data
object is closed instead of directly when the data is being changed by
OpenDA. Depending on how much data will be read and or written, how many times this will happen and available memory, users can choose which settings will be most suitable.

These boolean arguments always have to be specified as the first two ``<arg>`` elements in the XML config for the NetCDF data object::

  <dataObject className="org.openda.exchange.dataobjects.NetcdfDataObject">
      <file>scalar.nc</file> 
      <id>dataObjectId</id> 
      <arg>true</arg>
      <arg>false</arg> 
  </dataObject>



Key-value pair: ``requiredExchangeItemId``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To prevent a long list of arguments with a specific order,
key-value pair arguments have been introduced. Any number of
``requiredExchangeItemId=<ID>`` arguments can be supplied which limits
the exchange items being created to the ones supplied. This can save a
lot of memory and performance for large NetCDF files.

If we have the following variables ``waterlevel`` and ``data``::

  float waterlevel(time=3, stations=6); 
    :standard_name =
    "water_surface_height_above_reference_datum detection_minimum";
    :long_name = "waterlevel"; 
    :units = "m"; 
    :_FillValue = -9999.0f; // float 
    :coordinates = "lat lon";
    :cell_methods = "time: maximum";
  
  data: 
    station_id = "26", "27", "28", "24", "29", "25"

it is possible to limit the creation of exchange items by adding three extra
arguments: ``requiredExchangeItemId=24.waterlevel``,
``requiredExchangeItemId=26.waterlevel``, and 
``requiredExchangeItemId=27.waterlevel``.  This way only the specified
exchange items will be created.

The XML config for this will look as follows::

  <dataObject className="org.openda.exchange.dataobjects.NetCDFDataObject">
      <file>scalar.nc</file> 
      <id>dataObjectId</id> 
      <arg>true</arg>
      <arg>false</arg> 
      <arg>requiredExchangeItemId=24.waterlevel</arg>
      <arg>requiredExchangeItemId=26.waterlevel</arg>
      <arg>requiredExchangeItemId=27.waterlevel</arg>
  </dataObject>


Key-value pair: ``allowTimeIndependentItems``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The argument ``allowTimeIndependentItems=true/false`` determines whether
time-independent exchange items should be created. Time-independent
exchange items can be created for variables that do not depend on a time
dimension. Default is false.
