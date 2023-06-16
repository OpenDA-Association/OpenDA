============
Localization
============

When multiple observation stations are used that are spread over quite some distance, it can be beneficial to apply localization of the Kalman gain.
This way, observations further away will have a smaller effect on the Kalman gain. Localization can also be used to handle situations where the number of observation locations is larger than the number of ensemble members.

Localization has been explained in the :ref:`OpenDA course <course>`: for more information, we refer to that document. A nice introductory paper on localization can be found `here <https://link.springer.com/article/10.1007/s10236-006-0088-8>`__.

In OpenDA, we have two different options for localization: `Hamill localization <https://journals.ametsoc.org/view/journals/mwre/129/11/1520-0493_2001_129_2776_ddfobe_2.0.co_2.xml>`__ (commonly used) and `Zhang localization <https://d-nb.info/1199809977/34>`__ (no need to implement maskers). 

Example configurations can be found in 

- ``model_dflowfm_blackbox\tests\dcsmv5_kalman_rst\algorithm\Enkf_localization.xml``, and
- ``model_dflowfm_blackbox\tests\dcsmv5_kalman_rst_partitioning\algorithm\Enkf_localization.xml``.

They look as follows::

    <localization>Hamill</localization>
    <distance>100000</distance>
	
It depends on the data object whether or not it can read coordinate information, and it also depends on whether or not the data contains coordinate information.
The unit of the distance depends on the data itself. 
It can be in different units, but within an application, all different data objects should then supply it in the same unit.
The distance is determined based on the insight about the processes being modeled. 
This can be considered as a calibration parameter too, to be determined for obtaining an optimal Kalman filter setup.

As a guideline: if there is a correlation between stations over a longer distance, the value for <distance> should be higher.

OpenDA support for localization
-------------------------------

A specific data object supports localization when the related exchange item returns an ``IGeometry`` object.
This object has information about coordinates and can calculate the distance to a certain point via the ``IArray distanceToPoint(double x, double y, double z)`` method.

Since OpenDA 3.1, support for localization has been extended for:

- ``NetcdfDataObject`` with scalar and gridded data
- ``DFlowFMRestartFileWrapper``
- ``IoObjectStochObserver``
