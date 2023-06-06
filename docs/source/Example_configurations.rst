.. _Example configurations:

======================
Example configurations
======================
On this page, we discuss some example configurations. They can be found in the directory ``<path_to_openda_release>/examples``.

A first example that can easily be executed is found in ``model_example_blackbox/blackbox_example_calibration/Dud.oda``.
After opening this file in the :ref:`GUI <OpenDA installation>`, you can run the simulation. 

Note: for the examples ``model_dflowfm_blackbox`` and ``model_delft3d``, it is necessary to connect to existing models.

Below we explain some more examples in detail.

Example 1: Oscillator-Dud application
-------------------------------------

 
In this example, we will learn how to use OpenDA for model
calibration. To do so, we use an Oscillator model, which is one of the
OpenDA internal/toy models. The algorithm used in this example is the
`Dud <https://www.tandfonline.com/doi/abs/10.1080/00401706.1978.10489610>`__ (which stands for **D** oesn't **U** se **D** erivative).

#. Check and examine the ``core/simple_oscillator`` directory. The main components in this directory are
   a main configuration file ``Dud.oda`` and three subdirectories that each contain a data-assimilation component: algorithm, model, and
   stochobserver. Each directory contains a configuration file for the
   respective component: ``dudAlgorithm.xml``, ``OscillatorStochModel.xml``, and
   ``observations_oscillator_generated_for_calibration.csv``.
#. Run the OpenDA application with ``Dud.oda`` as the main configuration
   file in the mode that you prefer (with or without GUI). In GUI mode,
   you can get a real-time update of the execution by checking either
   Control, Output, or Cost Function tabs.
#. Check the results. Upon completion a new file ``dud_results.m`` is
   created. This file contains the results of OpenDA-Dud application,
   which are written in Matlab format. If you have no access to Matlab,
   then `Octave <https://www.gnu.org/software/octave/>`__ can be used as
   an alternative.
#. Play around with the stopping criteria in ``dudAlgorithm.xml`` and see if
   the results are different from the previous ones. See the `XML
   documentation <https://olddocs.openda.org/xmlSchemasHTML/index.html>`__ for the description
   of each XML attribute.
 

Example 2: Oscillator-Simplex application
-----------------------------------------

To edit the .xml files, the user is advised to use an XML validity
editor, of which many can be found online (for free). In this
example, we are going to use the same set of model and observation as
in the previous one, but use a different algorithm for solving the
parameter estimation problem. This will illustrate how easy it is in
OpenDA to couple different algorithms to the existing model and
observation.

#. Create a new algorithm configuration file: ``simplexAlgorithm.xml``.
   Store it in the ``algorithm`` directory.

#. Edit the ``simplexAlgorithm.xml``. See the `XML
   documentation <https://olddocs.openda.org/xmlSchemasHTML/index.html>`__ for the description
   of each XML attribute and an example of a simplex algorithm configuration
   file.

#. Create a new main configuration file ``Oscillator-Simplex.oda``, by
   copying ``Oscillator-Dud.oda``.

#. Edit ``Oscillator-Simplex.oda``:

   -  set the algorithm ``className`` to the one of simplex.
   -  set another name for the ``resultWriter`` ``configFile`` to be different
      from the previous one to avoid overwriting the previous result.

#. Run the OpenDA application with ``Oscillator-Simplex.oda`` as input.

#. Check the results and compare them to the ones obtained using the Dud
   algorithm.

 

Example 3: Oscillator-Powell application
----------------------------------------

Follow the same procedure as in Example 2, but this time we use the
Powell algorithm.

Example 4: Oscillator-GriddedFullSearch application
---------------------------------------------------

Follow the same procedure as in Example 2, but this time we use the
Gridded Full Search algorithm.

Example 5: Oscillator-EnKF application
--------------------------------------

In the previous examples, we learned how to use OpenDA for model
calibration or parameter estimation. In this example, we will learn how
to use OpenDA for Kalman filtering. In this particular example, we use
the Ensemble Kalman Filter (EnKF) algorithm. Follow the same procedure
as in Example 2, but this time we use the EnKF algorithm.
Check the results in ``Enkf_results.m``. It contains several variables.
For this tutorial, the following variables are of importance:

-  ``x_f{time}``: model state before data assimilation (forecast state)
-  ``x_a{time}``: model state after data assimilation (analysis state)
-  ``obs{time}``: observation data. Note that only one state variable is
   observed.
-  ``pred_f{time}``: forecast state variable which corresponds to the
   observed variable.
-  ``pred_a{time}``: analysis state variable which corresponds to the
   observed variable.

Notice that ``pred_a`` is much closer to the ``obs`` at each time than ``pred_f``.
This illustrates how data assimilation improves the model accuracy.

Example 6: Oscillator-EnSR application
--------------------------------------

Follow the same procedure in Example 6 for the Ensemble Square Root filter
(EnSR).

Example 7: Lorenz-EnKF application
----------------------------------

In the previous examples, we learned how to use different algorithms for
the same set of model and observation. In this example, we will learn to
couple another set of model and observation to the existing algorithms.
We use the Lorenz model, which is another toy model available in OpenDA.
Adjust the relevant XML attributes and elements in the main
configuration files created in the previous examples to work with the
Lorenz model and its available observation data
``observations_lorenz_generated.csv``.
