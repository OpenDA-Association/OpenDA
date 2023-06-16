============
Localization
============

When multiple observation stations are used that are spread over quite some distance, it can be beneficial to apply localization of the Kalman gain.
This way, observations further away will have a smaller effect on the Kalman gain. Localization can also be used to handle situations where the number of observation locations is larger than the number of ensemble members.

Localization has been explained in the :ref:`OpenDA course <course>`: for more information, we refer to that document. A nice introductory paper on localization can be found `here <https://link.springer.com/article/10.1007/s10236-006-0088-8>`__.

In OpenDA, we have two different options for localization: `Hamill localization <https://journals.ametsoc.org/view/journals/mwre/129/11/1520-0493_2001_129_2776_ddfobe_2.0.co_2.xml>`__ (commonly used), or `Zhang localization <https://d-nb.info/1199809977/34>`__ (no need to implement maskers). 

Example configurations can be found in 

- ``model_dflowfm_blackbox\tests\dcsmv5_kalman_rst\algorithm\Enkf_localization.xml``, and
- ``model_dflowfm_blackbox\tests\dcsmv5_kalman_rst_partitioning\algorithm\Enkf_localization.xml``.

They look as follows::

    <localization>Hamill</localization>
    <distance>100000</distance>

Since OpenDA 3.1, support for localization has been extended for

- ``NetcdfDataObject`` with scalar and gridded data
- ``DFlowFMRestartFileWrapper``
- ``IoObjectStochObserver``
