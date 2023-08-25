=============================
D-Flow FM related information
=============================

Writing Kalman Gain
-------------------

Since OpenDA 3.1, the Kalman gain can be written fully according to the :ref:`NetCDF CF format<data objects>`. (hier nog verwijzen naar meer info over de Kalman gain)

An example can be found in ``model_dflowfm_blackbox\tests\dcsmv5_kalman_rst\algorithm\Enkf.xml``.

The Kalman gain is saved by configuring:: 

    <saveGain> 
        <times type="fixed" timeFormat="dateTimeString" >202201010100,202201010110,...,202401010400</times>
        <file dirPrefix="kgStorage_" fileName="kalmanGainStorage.nc" fileType="netcdf_cf"/>
    </saveGain>
	
OpenDA will use the first two explicit times to determine the time step and will fill all times according to the time step until the end time which is defined after the dots.
For each configured time, a directory is created with the prefix ``kgStorage_`` and the time stamp according to the date format ``yyyyMMDDHHmm``. In these directories, the file with the configured name ``kalmanGainStorage.nc`` is written. 

For the gridded noise (vraag: wat moeten we met noise hier?), one variable is created with the first dimension equal to the station dimension of the state and the other dimensions according to the noise dimensions (vraag: klopt het woordje dimensions hier? Moet dat niet arguments zijn?)::

  double 2DNoise(station_dimension=4, 2DNoise_dimension_0=16, 2DNoise_dimension_1=27)

For the state variables ``s1``,  ``unorm`` (coming from the ``*_rst.nc`` restart file) (vraag: wat zijn s1 en unorm?), the first dimension (vraag: dimension moet argument zijn?) is again the station dimension of the state and the second dimension according to the number of predictors (vraag: number of predictors?)::

  double s1(station_dimension=4, s1_dimension_0=17843);
  double unorm(station_dimension=4, s1_dimension_0=17843);

(vraag: waar horen die blokjes met doubles? Zit dat in de java code? Wat moet een gebruiker ermee?)  

Reading Kalman gain
-------------------

When a Kalman gain is available, like from a previous EnKF run, it can be used in a steady-state Kalman filter. An example can be found in ``model_dflowfm_blackbox\tests\dcsmv5_kalman_rst\SSKF.oda``.

In ``model_dflowfm_blackbox\tests\dcsmv5_kalman_rst\algorithm\SSKF.xml``, a reference to a previously written Kalman gain is configured as follows::

    <readGain> 
        <dirPrefix>kgStorage_L100km_</dirPrefix>
        <time timeFormat="dateTimeString">202209011200</time>
        <file>kalmanGainStorage.nc</file>
    </readGain>
	
With the above elements the reference to ``kgStorage_L100km_202209011200\kalmanGainStorage.nc`` is constructed and the Kalman gain read. (vraag: waarom deze naam van de dirPrefix? Of maakt dat voor de lezers niet echt uit?)
This way, there is no need to run multiple ensembles in order to determine the Kalman gain and the model can just be run once. 


Maps Noise Model
----------------

(Vraag: kunnen we maps noise model duidelijker maken? Is het een noise model voor gridded data?)
In ``model_dflowfm_blackbox\tests\dcsmv5_kalman_rst\stochModel\dflowfmStochModel.xml``, an example of applying noise in grid format is given.

It refers to 
``model_dflowfm_blackbox\tests\dcsmv5_kalman_rst\stochModel\2DPressureNoise.xml`` for the definition of the noise.
The noise is applied to the model exchange item ``p`` which is based on the variable ``p`` in the ``%meteofile_p%`` NetCDF file that is read and edited via the data object ``<dataObject className="org.openda.exchange.dataobjects.NetcdfDataObject">`` in ``dflowfmWrapper.xml``. (vraag: wat is p? Wat is de meteofile_p?)
This way, OpenDA manipulates the air-pressure field in order to correct the water levels in the model. (vraag: zullen we wat extra informatie geven over die air pressure die hier opeens opduikelt?)
In order to overwrite the forecast noise with analysis noise (vraag: forecast noise en analysis noise?), the following options should be configured: ``allowOverwriteForecastWithAnalysisNoise="true"`` and ``transformation="set"`` (vraag: waar?). This way, OpenDA will set the value of the calculated noise directly in the exchange item (and thus file) instead of adding it to or multiplying it with existing values.
Because OpenDA stores the gridded-noise values as a plain one-dimensional array of values, some care must be taken with applying it to two-dimensional data structures (as there is no check on matching dimensions or matching coordinates). For this reason, the ``airpressure_noise.nc`` variable ``p`` has the following order of dimensions: ``double p(time=49, Y=16, X=27)`` and the y-coordinates are descending: 63.0, 62.0, ..., 49.0, 48.0. (vraag: van deze laatste zin maak ik echt niks. Hoe verhouden die y-coordinaten zich tot de p? Hoe weet je dat je start bij 63? Wil je misschien gewoon aangeven dat de volgorde tijd, y, x moet zijn met aflopende y? Loopt x dan wel gewoon op?)
