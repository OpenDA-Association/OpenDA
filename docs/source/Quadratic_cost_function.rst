=======================
Quadratic cost function
=======================

The quadratic cost function of a simulation is used to evaluate how well a
calibration performs. OpenDA searches for a set of calibration parameters for
which this cost function becomes minimal. 

For every observation location and every time step, we compute the difference
between the model result and the observation, square this, and divide it by the
square of the observation's standard deviations.  Summing over all locations
and times yields: 

.. math::
   L(x) = \sum_{i=1}^I \sum_{t=1}^{T_i} \frac{(y_{t,i} - f_{t,i}(x))^2}{\sigma_i^2},

where :math:`I` is the number of observation locations, :math:`T_i` is the
number of time steps at observation location :math:`i` for which both model
results and observations are available, :math:`y` is the set of observations,
and :math:`f(x)` the corresponding set of model results, and :math:`\sigma_i`
the user-defined standard deviation of the observations at location :math:`i`.

In the Java implementation, this cost function is defined in the class
``SimulationKwadraticCostFunction``.  The default calculation is activated
using the following XML input:: 

  <costFunction weakParameterConstraint="false"
                class="org.openda.algorithms.SimulationKwadraticCostFunction"
                stdRemoval="false" biasRemoval="false" factor="1"/>

If the attribute ``factor`` is omitted, the default value 0.5 is used. 
The computed cost is multiplied by this factor.

OpenDA supports several special options that modify how the cost function is
calculated. 

#. ``stdRemoval="true"``

   With this option, calibration is performed *using only the bias*, meaning
   the standard deviation of the differences between the model results and the
   observations (STD) is removed from the calculation.  This standard deviation STD
   is not the same as the user-defined standard deviation of the observations :math:`\sigma_i`.

   For each station, the bias is computed as the average difference between the
   model results and observations over all times: 

   .. math::
      \mathrm{bias}_i =  \frac{\sum_{t=1}^{T_i} (y_{t,i} - f_{t,i}(x))}{T_i}.

   In the cost function, the model-observation difference is replaced by this
   bias:

   .. math::
     L_{\mathrm{bias}}(x) = \sum_{i=1}^I \sum_{t=1}^{T_i}
     \frac{\mathrm{bias}_i^2}{\sigma_i^2} = \sum_{i=1}^I T_i
     \frac{\mathrm{bias}^2}{\sigma_i^2}.

   The final value is multiplied by the chosen factor.

   Below is an example with five observation stations (:math:`I=5`).

   ========================== ==================== ===================== ===================== =========== ==================== ====================
   ObsID                      RMS                  bias                  STD                   Num of data Min                  Max
   Pannerdense-kop.waterlevel 8.765000563570873E-6 -8.765000563570881E-6 1.705030836774512E-21  78         -8.76500056357088E-6 -8.76500056357088E-6
   Zaltbommel.waterlevel      2.553417424167047E-6  2.553417424167031E-6 5.511568187681813E-21 471          2.55341742416703E-6  2.55341742416703E-6
   Vuren.waterlevel           4.557510343339312E-6 -4.557510343339312E-6 2.374213988539862E-20 471         -4.55751034333928E-6 -4.55751034333928E-6
   Nijmegen-haven.waterlevel  2.171564102226854E-5  2.171564102226849E-5 3.068557404185452E-20  80          2.17156410222685E-5  2.17156410222685E-5
   Tiel-Waal.waterlevel       4.144963131675898E-4 -4.144963131675885E-4 8.729040419079085E-19  79         -4.14496313167589E-4 -4.14496313167589E-4
   ========================== ==================== ===================== ===================== =========== ==================== ====================

   All values in the table are computed per observation location, based on the
   differences between model results and observations over time.  The table
   columns have the following meaning: 

     - *ObsID*: the observation location;
     - *RMS*: the root-mean-square error (cost) of the difference;
     - *bias*: the bias (kind of mean difference);
     - *STD*: the standard deviation of the difference;
     - *Num of data*: the number of data points at the observation location
       that are included in the cost calculation (:math:`T_i`);
     - *Min*: the minimum value of the difference at the location;
     - *Max*: the maximum value of the difference at the location.

   OpenDA uses a data-assimilation algorithm to minimize the cost function. In
   this example, the algorithm  `Dud
   <https://www.tandfonline.com/doi/abs/10.1080/00401706.1978.10489610>`__ has
   been used.  It needs 9 iterations to arrive at a minimum cost, generating the
   following output::

      % costObserved{9}   =6.814670088199292E-6;
      % costTotal{9}      =6.814670088199292E-6;
      % SimulationKwadraticCostFunction: evaluation 9 : cost = 6.815E-6

   Note: ``costObserved`` and ``costTotal`` are equal due to the default
   value ``weakParameterConstraint="false"``. Below we will see an example
   for which the total cost will differ from the observed cost.

#. ``biasRemoval="true"``
   
   With this option, calibration uses only the *standard deviation* (not the
   bias).  First, the bias is computed for each station using the same formula
   as above. Then, for each time step, this bias is subtracted from the
   model-observation difference:
   
   .. math::
      L_{\mathrm{std}}(x) = \sum_{i=1}^I \sum_{t=1}^{T_i} \frac{(y_{t,i} -
      f_{t,i}(x) - \mathrm{bias}_i)^2}{\sigma_i^2},

   again multiplied by the chosen factor.


#. ``weakParameterConstraint="true"``

   This option adds an extra term to the cost function to penalize deviations
   between the current and initial estimates of the calibration parameters.
   This helps stabilize calibration when parameters drift too far from their
   initial values.
   
   This resulting cost function is: 

   .. math::
      L_{\mathrm{weakParam}}(x) = \sum_{i=1}^I \sum_{t=1}^{T_i} \frac{( y_{t,i} -
      f_{t,i}(x))^2}{\sigma_i^2} + \sum_{p=1}^P \frac{(x_p -
      x_{p,0})^2}{\tilde{\sigma}_p^2},

   where :math:`P` is the number of parameters, :math:`\tilde{\sigma}_p` is a 
   user-defined standard deviation per parameter, :math:`x_{p,0}` is the initial
   value of the parameter, and :math:`x_p` is the deviation of this parameter.
   This expression is also multiplied by the chosen factor.
