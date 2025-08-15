===========================
Introduction to calibration
===========================

For calibration, one first defines a scalar function which, for any model solution over
the assimilation interval, measures the distance or misfit between that
solution and the available observations. The so-called *cost function*
will typically be a sum of squared differences between the observations and the
corresponding model values. One will then look for the model solution that
minimizes the cost function.

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

Here, :math:`x_o` is the initial value of the variable to be determined 
(the optimal initial condition is the one that minimizes :math:`J`), :math:`x(k)` is the
variable at time :math:`t_k`, :math:`H` is the observation operator, :math:`y^o(k)` is the observation
at time :math:`t_k`, and :math:`R` is the representation covariance. 
For the second cost function, we additionally have :math:`x^b`, the background or initial estimate of :math:`x_o`,
and :math:`P^b`, its covariance. This additional
component ensures that the solution will not be too far from the initial
guess.

Parameter estimation with Dud
=============================

Dud (Doesn’t use derivative) is one of the optimization algorithms
which does not use any derivative of the function being evaluated. It can
be seen as a Gauss-Newton method, in the sense that it transforms the
nonlinear least-square problem into the well-known linear square
problem. The difference is that instead of approximating the nonlinear
function by its tangent function, the Dud algorithm uses an affine function for
the linearization. For :math:`p` calibration parameters, Dud requires a set of
:math:`p+1` parameter estimates. The affine function for the
linearization is formed through all these :math:`p+1` guesses. Note
that the affine function gives exact value at each of the :math:`p+1`
points. The resulting least-square problem is then solved along the
affine function to get a new estimate, whose cost is smaller than those
of all other previous estimates. If it does not produce a better
estimate, the Dud will perform different steps, like searching in
opposite direction and/or decreasing the search step, until a better
estimate is found. Afterwards, the estimate with the largest cost is
replaced with the new one and the procedure is repeated for the new set
of :math:`p+1` estimates. The procedure is stopped when one of the
stopping criteria is fulfilled.

Suppose we have a numerical model with a vector of :math:`p` uncertain
parameters, say :math:`\mathbf{x} \in \mathbb{R}^p` . Let
:math:`\mathbf{y} \in \mathbb{R}^n` be a set of :math:`n` data points and let
:math:`f_i : \mathbb{R}^p \rightarrow \mathbb{R}` be the model
prediction corresponding to the :math:`i^{th}` data point. The goal is
to find a vector of parameters :math:`\mathbf{x}^{opt} \in \mathbb{R}^p`
that minimizes the following cost function:

.. math::

   \label{eq:dudcost}
   (\mathbf{x}) = [\mathbf{y} - \mathbf{f(x)}]^T [\mathbf{y} - \mathbf{f(x)}].

The idea of the Dud algorithm is similar to that of the Gauss-Newton
algorithm. The key difference is that the Dud algorithm uses an
approximation of gradients instead of the exact gradients. In short the
Dud algorithm works as follows.

The memory of the algorithm always contains :math:`p + 1` estimations of
the optimal parameter set and their corresponding model predictions.
First the model functions :math:`f_i(\mathbf{x})` are linearized based
on interpolation between the model predictions that are stored in
memory. Then the linearized version of
equation `[eq:dudcost] <#eq:dudcost>`__ is solved, yielding a new
estimation of the optimal parameter set. Assuming the new estimation is
better than all previous ones, it replaces the worst estimation in the
memory of the algorithm. This process is repeated until the best
estimation is ”sufficiently” close to the optimal parameter set.

Let us now formulate the algorithm in more detail. Let
:math:`\mathbf{x}^1 , \ldots
\mathbf{x}^{p+1}` and
:math:`\mathbf{f}(\mathbf{x}^1), \ldots, \mathbf{f}(\mathbf{x}^{p+1})`
be the parameter estimations and corresponding model predictions that
are stored in memory. The first step is to fill this memory with
:math:`p+1` initial guesses. These guesses are sorted according to the
value of their respective cost functions :math:`S(\mathbf{x}^i)`, meaning
:math:`\mathbf{x}^1` is the parameter set with the highest cost function
and :math:`\mathbf{x}^{p+1}` the one with the lowest.

Then the model functions :math:`f_i(\mathbf{x})` are linearized by
interpolation between the model predictions that are stored in memory.
Let us denote the linearized model functions by :math:`l_i(\mathbf{x})`.
Then

.. math:: l_i (\Delta \mathbf{x}) = f_i(\mathbf{x}^{p+1}) + M \Delta \mathbf{x}

with :math:`\Delta \mathbf{x} = \mathbf{x} - \mathbf{x}^{p+1}` and
:math:`M` the approximation of the Jacobian matrix by interpolation.
Note that :math:`M = F P^{-1}` where :math:`F` is the matrix with
:math:`j^{th}` column equal to :math:`\mathbf{f}(\mathbf{x}^j)
- \mathbf{f}(\mathbf{x}^{p+1})` and :math:`P` the matrix with
:math:`j^{th}` column equal to :math:`\mathbf{x}^i - \mathbf{x}^{p+1}`.

Then :math:`\mathbf{f}(\mathbf{x}) = \mathbf{l}(\mathbf{x})` is
substituted into cost equation `[eq:dudcost] <#eq:dudcost>`__. Let us
call the linearized cost function :math:`Q(\mathbf{x})`. By solving
:math:`Q'(\Delta \mathbf{x}) = 0` we find that for the optimal value of
:math:`\Delta \mathbf{x}` holds

.. math:: M^T M \Delta \mathbf{x} = M^T \left[\mathbf{y} - \mathbf{f}(\mathbf{x}^{p+1})\right]

yielding a new parameter estimation
:math:`\mathbf{x}^\ast = \mathbf{x}^{p+1}
+ \Delta \mathbf{x}`. If :math:`\mathbf{x}^\ast` has a lower
cost function than :math:`\mathbf{x}^{p+1}`, the worst estimate
:math:`\mathbf{x}^1` is tossed out of the memory, making place for the
new estimation. The elements in the memory are again sorted according to
their cost functions, so :math:`\mathbf{x}^{p+1}
= \mathbf{x}^\ast` , :math:`\mathbf{x}^p = \mathbf{x}^{p+1}` etc.

It may however happen that the new estimation :math:`\mathbf{x}^\ast` is
not better than one or more of the previous estimations. In this case a
line search is done. A better estimation is searched in the direction
from :math:`\mathbf{x}^{p+1}` to :math:`\mathbf{x}^\ast` , so on the
line

.. math:: \mathbf{r}(\epsilon) = \mathbf{x}^{p+1} + \epsilon (\mathbf{x}^\ast - \mathbf{x}^{p+1} )

The step size :math:`\epsilon \in \mathbb{R}` is iteratively being
reduced until a step size :math:`\epsilon^\ast` is found for which
:math:`\mathbf{r}(\epsilon^\ast)` has a lower cost function than
:math:`\mathbf{x}^{p+1}`. Note that :math:`\epsilon^\ast` may very well
be negative. The new estimation :math:`\mathbf{r}(\epsilon^\ast)` is
then stored into memory as before.
