
Small description of the model

Simplified linear response of a river with two sections. This model does not perform
any real hydrodynamics computation, but only mimics the behaviour with a linear regression
formula. To add some time behaviour the model is started from the equilibrium for alpha_a=0
and alpha_b=0 and it performs an exponential relaxation towards the final equilibrium:
x0_equilibrium = xa0+ 0.1 * alpha_a
x1_equilibrium = xb0+ 0.15 * alpha_b + 0.03 * alpha_a
 
section a is the downstream section of the two and x[0] models the waterlevel at the upstream end
of section a. alpha_a is the change in friction in section a. Dito for the upstream section b.

The calibration test shows how to adapt the costfunction for a nearly stationary system.
Only the difference of the average per timeseries is then used in the cost-function.
