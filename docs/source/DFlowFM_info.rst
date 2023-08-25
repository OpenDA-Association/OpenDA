=============================
D-Flow FM related information
=============================

Writing Kalman gain
-------------------

Since OpenDA 3.1, the :ref:`Kalman gain<Kalman_gain>` can be written fully
according to the :ref:`NetCDF CF format<data objects>`.

An example can be found in
``model_dflowfm_blackbox\tests\dcsmv5_kalman_rst\algorithm\Enkf.xml``.

The Kalman gain is saved by configuring:: 

    <saveGain> 
        <times type="fixed" timeFormat="dateTimeString" >202201010100,202201010110,...,202401010400</times>
        <file dirPrefix="kgStorage_" fileName="kalmanGainStorage.nc" fileType="netcdf_cf"/>
    </saveGain>
	
OpenDA will use the first two explicit times to determine the time step and
will fill all times according to the time step until the end time which is
defined after the dots.  For each configured time, a directory is created with
the prefix ``kgStorage_`` and the time stamp according to the date format
``yyyyMMDDHHmm``. In these directories, the file with the configured name
``kalmanGainStorage.nc`` is written. 

For the gridded noise, one variable is created where the first argument equals
the station dimension of the state and the other arguments are related to the
noise dimensions::

  double 2DNoise(station_dimension=4, 2DNoise_dimension_0=16, 2DNoise_dimension_1=27)

For the state variables ``s1``,  ``unorm`` (coming from the ``*_rst.nc``
restart file), the first dimension is again the station dimension of the state
and the second dimension according to the number of predictors::

  double s1(station_dimension=4, s1_dimension_0=17843);
  double unorm(station_dimension=4, s1_dimension_0=17843);

Reading Kalman gain
-------------------

When a Kalman gain is available, like from a previous EnKF run, it can be used
in a :ref:`steady-state Kalman filter<Optimal_interpolation>`. An example can
be found in ``model_dflowfm_blackbox\tests\dcsmv5_kalman_rst\SSKF.oda``.

In ``model_dflowfm_blackbox\tests\dcsmv5_kalman_rst\algorithm\SSKF.xml``, a
reference to a previously written Kalman gain is configured as follows::

    <readGain> 
        <dirPrefix>kgStorage_L100km_</dirPrefix>
        <time timeFormat="dateTimeString">202209011200</time>
        <file>kalmanGainStorage.nc</file>
    </readGain>
	
With the above elements the reference to
``kgStorage_L100km_202209011200\kalmanGainStorage.nc`` is constructed and the
Kalman gain read.  This way, there is no need to run multiple ensembles in
order to determine the Kalman gain and the model can just be run once. 


Maps Noise Model
----------------

In
``model_dflowfm_blackbox\tests\dcsmv5_kalman_rst\stochModel\dflowfmStochModel.xml``,
an example of applying noise in grid format is given.

It refers to
``model_dflowfm_blackbox\tests\dcsmv5_kalman_rst\stochModel\2DPressureNoise.xml``
for the definition of the noise.  The noise is applied to the model exchange
item ``p`` which is based on the variable ``p`` in the ``%meteofile_p%`` NetCDF
file that is read and edited via the data object ``<dataObject
className="org.openda.exchange.dataobjects.NetcdfDataObject">`` in
``dflowfmWrapper.xml``.  This way, OpenDA manipulates the air-pressure field in
order to correct the water levels in the model.  In order to overwrite the
forecast noise with analysis noise, the following options should be configured:
``allowOverwriteForecastWithAnalysisNoise="true"`` and
``transformation="set"``. This way, OpenDA will set the value of the calculated
noise directly in the exchange item (and thus file) instead of adding it to or
multiplying it with existing values.  Because OpenDA stores the gridded-noise
values as a plain one-dimensional array of values, some care must be taken with
applying it to two-dimensional data structures (as there is no check on
matching dimensions or matching coordinates). For this reason, the
``airpressure_noise.nc`` variable ``p`` has the following order of dimensions:
``double p(time=49, Y=16, X=27)`` and the y-coordinates are descending: 63.0,
62.0, ..., 49.0, 48.0.
