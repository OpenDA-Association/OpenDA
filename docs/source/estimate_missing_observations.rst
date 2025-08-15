.. _Estimate missing observations:

=============================
Estimate missing observations
=============================

If some observations are missing, OpenDA has less information to calculate the
innovation at a given location. However, by using the other available
observations, we can estimate the innovation, allowing this location to still
play a role in the data assimilation.

This page describes how missing observations can be calculated using
observations that are available. Before being able to calculate these, 
:math:`H*K` should already be calculated and stored in ``KalmanGainStorage``. 
How this is done is explained :ref:`below<CalculationHK>`.

The vector :math:`d` is called the residual vector or the innovation vector,
and it contains the difference between measurements and predictions.  

The innovation of missing stations can be computed using the following formula: 

.. math:: d_{missing} = (I - M_2)^{-1} * M_1 * d_{available}.

The matrices :math:`M_1` and :math:`M_2` are constructed from the matrix
:math:`H*K`.  We will explain how this is done using an example with 9 stations
where the observations at locations 2, 4 and 5 are missing. The matrix
:math:`H*K` can be colored as follows:

.. role:: orange
   :class: orange-text
.. role:: blue
   :class: blue-text

=============== =============== ============= =============== =============== ============= ============= ============= =============
hk_11           hk_12           hk_13         hk_14           hk_15           hk_16         hk_17         hk_18         hk_19
:blue:`hk_21`   :orange:`hk_22` :blue:`hk_23` :orange:`hk_24` :orange:`hk_25` :blue:`hk_26` :blue:`hk_27` :blue:`hk_28` :blue:`hk_29`
hk_31           hk_32           hk_33         hk_34           hk_35           hk_36         hk_37         hk_38         hk_39
:blue:`hk_41`   :orange:`hk_42` :blue:`hk_43` :orange:`hk_44` :orange:`hk_45` :blue:`hk_46` :blue:`hk_47` :blue:`hk_48` :blue:`hk_49`
:blue:`hk_51`   :orange:`hk_52` :blue:`hk_53` :orange:`hk_54` :orange:`hk_55` :blue:`hk_56` :blue:`hk_57` :blue:`hk_58` :blue:`hk_59`
hk_61           hk_62           hk_63         hk_64           hk_65           hk_66         hk_67         hk_68         hk_69
hk_71           hk_72           hk_73         hk_74           hk_75           hk_76         hk_77         hk_78         hk_79
hk_81           hk_82           hk_83         hk_84           hk_85           hk_86         hk_87         hk_88         hk_89
hk_91           hk_92           hk_93         hk_94           hk_95           hk_96         hk_97         hk_98         hk_99
=============== =============== ============= =============== =============== ============= ============= ============= =============

Then :math:`M_1` is constructed from the blue elements in :math:`H*K`: the rows related
to the missing stations and the columns related to the available stations: 

=============== ============= ============= ============= ============= =============
:blue:`hk_21`   :blue:`hk_23` :blue:`hk_26` :blue:`hk_27` :blue:`hk_28` :blue:`hk_29`
:blue:`hk_41`   :blue:`hk_43` :blue:`hk_46` :blue:`hk_47` :blue:`hk_48` :blue:`hk_49`
:blue:`hk_51`   :blue:`hk_53` :blue:`hk_56` :blue:`hk_57` :blue:`hk_58` :blue:`hk_59`
=============== ============= ============= ============= ============= =============

The number of rows in :math:`M_1` relates to the number of missing stations,
and the number of columns is equal to the number of available stations.

:math:`M_2` is a square matrix with dimensions equal to the number of missing
stations. It is constructed from the orange elements in :math:`H*K`:

=============== =============== ===============
:orange:`hk_22` :orange:`hk_24` :orange:`hk_25`
:orange:`hk_42` :orange:`hk_44` :orange:`hk_45`
:orange:`hk_52` :orange:`hk_54` :orange:`hk_55`
=============== =============== ===============

These matrices :math:`M_1` and :math:`M_2` are created and filled via
``SteadyStateFilter.HKCalculator.createMatrices()``, and :math:`d_{available}`
is filled in ``SteadyStateFilter.HKCalculator.setDAvailableValue()`` during the
loop in ``SteadyStateFilter.analysis()``, where the innovation values of
non-missing observations are processed.  With a filled :math:`d_{available}`
the innovation for missing observations can be calculated and applied.  This
happens in
``SteadyStateFilter.HKCalculator.compensateForMissingObservationsWithHK()``.

.. _CalculationHK:

Calculating and writing :math:`H*K`  
-----------------------------------

In order to use :math:`H*K` in the steady-state filter, the :ref:`Kalman gain<Kalman_gain>` matrix
(saved in ``KalmanGainStorage``) should be available in NetCDF CF format. 
We distinguish two different cases: 

- **Standard case**: :math:`H*K` is calculated in ``EnKF.computeHK()`` and the calculation is based on ``EnKF.computeGainMatrix()`` where :math:`H*K` was already calculated.  In the logging and the code :math:`H*K` is also known as :math:`K_{pred}`.
- **Hamill localization**: When :ref:`Hamill localization<Localization>` is used, :math:`H*K` needs to be element-wise multiplied with the weight factor between observations called ``rho``. ``rho`` will be retrieved via a call to ``IModelInstance.getRhoForLocalization()``. The weight factor ``rho`` is currently only calculated for the black box model in ``BBStochModelInstance.java``. For all other models, a default implementation (``IModelInstance.getDefaultRhoForLocalization()``) is available such that the interface contracts will not be broken. This default implementation will return a rho matrix consisting of only the value 1, so it does not affect :math:`H*K`.
- For all **other localizations**, :math:`H*K` is not computed and the user is informed by a message.

:math:`H*K` will be added to ``KalmanGainStorage`` in NetCDF CF
format in the method ``KalmanGainStorage.writeKalmanGainToNetcdfCF()``.
