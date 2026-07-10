===========================
Introduction to calibration
===========================

Calibration starts with the definition of a scalar cost function that measures
the misfit between model predictions and the available observations over the
assimilation interval. The cost function is typically defined as the weighted
sum of squared differences between the observations and the corresponding model
values. The goal of the calibration process is then to find the model solution
that minimizes this cost function.

The minimization of the cost function is often based on quasi-Newton methods.
These methods require computation of the gradient of the cost function. In most
situations, it is impossible to establish explicit analytical expressions for
the gradient. It is possible to numerically and approximately determine the
gradient through explicit finite perturbations of the initial state. But this
would be much too costly for practical implementation since it requires to
compute the cost function, i.e. to effectively integrate the model over the
assimilation period, as many times as there are independent components in the
initial states. Therefore to compute the gradient efficiently an adjoint model
should be used.

In the calibration or parameter-estimation algorithms, the basic idea is
to find the set of model parameters which minimizes the cost function
measuring the distance between the observation and the model prediction.
Two different cost functions have been implemented:

* :math:`J(x_o) = \sum_{k=1}^N (y^o(k)-Hx(k))R^{-1}(y^o(k)-Hx(k))^\top`; and

* :math:`J(x_o) = (x_o-x^b)^\top(P^b)^{-1}(x_o-x^b)+\sum_{k=1}^N (y^o(k)-Hx(k))R^{-1}(y^o(k)-Hx(k))^\top`.

Here, :math:`x_o` is the initial value of the variable to be determined (the
optimal initial condition is the one that minimizes :math:`J`), :math:`x(k)` is
the variable at time :math:`t_k`, :math:`H` is the observation operator,
:math:`y^o(k)` is the observation at time :math:`t_k`, and :math:`R` is the
covariance of the measurement errors.  For the second cost function (the
so-called `weak-constraint variant`), we additionally have :math:`x^b`, the
background or initial estimate of :math:`x_o`, and :math:`P^b`, its covariance.
:math:`P^b` is a measure for how sure you are of your initial parameter values.
This additional component ensures that the solution will not be too far from
the initial guess.

Parameter estimation with Dud
=============================

`Dud <https://www.tandfonline.com/doi/abs/10.1080/00401706.1978.10489610>`_
(Doesn’t Use Derivative) is one of the optimization algorithms that do not
use derivatives of the function being evaluated. It can be seen as a
Gauss-Newton method, in the sense that it transforms the nonlinear least-square
problem into the well-known linear least-square problem. The difference is that
instead of approximating the nonlinear function by its tangent function, the
Dud algorithm uses an affine function for the linearization. For :math:`p`
calibration parameters, Dud requires a set of :math:`p+1` parameter estimates.
The affine function for the linearization is formed through all these
:math:`p+1` guesses. Note that the affine function gives exact values at each
of the :math:`p+1` sample points. The resulting least-square problem is then
solved along the affine function to get a new estimate, whose cost is smaller
than those of all other previous estimates. If it does not produce a better
estimate, the Dud will perform different steps, like searching in opposite
direction and/or decreasing the search step, until a better estimate is found.
Afterwards, the estimate with the largest cost is replaced by the new estimate and
the procedure is repeated for the new set of :math:`p+1` estimates. The
procedure is stopped when one of the stopping criteria is fulfilled.
