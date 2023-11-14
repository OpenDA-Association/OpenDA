.. _Introduction da:

=================================
Introduction to data assimilation
=================================

Data assimilation is about the combination of two sources of information
- computational models and observations - to utilize both of their
strengths and compensate for their weaknesses.

Computational models are available nowadays for a wide range of
applications: weather prediction, environmental management, oil
exploration, traffic management and so on. They use knowledge of
different aspects of reality, e.g. laws of physics, empirical relations,
human behavior, etc., to construct a sequence of computational steps, by
which simulations of different aspects of reality can be made.

The strengths of computational models are the ability to
describe/forecast future situations (also to explore what-if scenarios),
in a large amount of spatial and temporal detail. For instance, weather
forecasts are run at ECMWF using a horizontal resolution of about 50 km
for the entire earth and a time step of 12 minutes. This is achieved
with the tremendous computing power of modern-day computers, and with
carefully-designed numerical algorithms.

However, computations are worthless if the system is not initialized
properly: “Garbage in, garbage out”. Furthermore, the “state” of a
computational model may deviate from reality more and more while
running, because of inaccuracies in the model, aspects that are not
considered or not modeled well, inappropriate parameter settings and so
on. Observations or measurements are generally considered to be more
accurate than model results. They always concern the true state of the
physical system under consideration. On the other hand, the number of
observations is often limited in both space and time.

The idea of data assimilation is to combine a model and observations,
and optimally use the information contained in them.

Some theory that might be helpful to understand the basics of data
assimilation includes the distinction between offline and online
methods, the statistical framework used, the notions of deterministic
and stochastic models, noise models, the combination of values (weights
are needed), data assimilation on top of an existing model, and the
general structure of filtering methods.

In this documentation, we give a bit of information about the statistical
framework. We do not aim for completeness but only give a bit of context
to the methods used. An open-source text book considering the fundamentals of data
assimilation can be found
`here <https://library.oapen.org/handle/20.500.12657/54434>`__.

Variational methods
===================

Variational methods aim to adjust a model solution to all the
observations available over the assimilation period. In variational methods, one first
defines a scalar function which, for any model solution over the
assimilation interval, measures the “distance” or “misfit” between that
solution and the available observations. The so-called *cost function*
will typically be a sum of squared differences between the observations
and the corresponding model values. One will then look for the model
solution that minimizes the cost function.

In OpenDA, we have implemented a variational method called 3DVar, but the most
common is to use one of the sequential methods described below. 

Sequential Methods
==================

While variational methods are based on the minimization of the cost function
within a time interval, sequential methods assimilate the data each time
the observation becomes available. 

Let :math:`x^f(k)` represent the forecast state at time :math:`t_k`, while :math:`x^a(k)` is
the analysis state at the same time, also called the adjusted state. 
Furthermore, :math:`y^o(k)` is the observation, and
:math:`H` is the interpolation operator from the model state to the observation space. 
Then, the adjusted model solution can be computed as

.. math:: x^a(k) = x^f(k) + K(k)(y^o(k)-H(k)x^f(k)).

Here, the *Kalman gain matrix*, :math:`K(k)`, determines the change to the model prediction based on the 
difference between the current model forecast, :math:`H(k)x^f(k)`, and the observation, :math:`y^0(k)`.
Various methods are available to determine this Kalman gain.

Optimal interpolation
---------------------
One popular sequential method is related to 
`optimal interpolation <https://www.cambridge.org/nl/universitypress/subjects/earth-and-environmental-science/atmospheric-science-and-meteorology/atmospheric-data-analysis?format=PB&isbn=9780521458252>`__. This method uses a gain matrix based on
an empirical covariance function or matrix. The basic assumption is that
both the forecast and the observation error are normally distributed
with mean zero and known covariances. The idea of optimal interpolation
is to set the analyzed state to the conditional mean of the true state
given the observations: :math:`x^a(k)=E[x^t(k)|y^o(k)]`, where :math:`x^t(k)`
is the true state at time :math:`t_k`. 

OpenDA supports the method of 
steady-state Kalman filtering where the :math:`K` matrix is computed in a previous run
using, for instance, EnKF. This can be considered a special form of optimal interpolation.
However, we mostly use Kalman filtering, which is described below.

Kalman filtering
----------------
In OpenDA, we mostly use Kalman filtering to calculate the adjusted model solution. 
Kalman filtering can be seen as an extension of the optimal interpolation scheme,
accounting for the evolution of errors from previous times. The target
of the Kalman filter is to obtain a distribution for the true state in
terms of a mean :math:`\hat{x}` and covariance matrix of the model uncertainty :math:`P`, given the
model and the measurements. The Kalman filter also assumes normally distributed forecast and
observation errors. The Kalman filter has originally been derived for linear
systems, which in state-space form can be written as:

.. math::

   \begin{align}
   x(k)&= M(k-1) x(k-1) + \eta(k-1) \\
   y(k) &= H(k) x(k) + \nu(k)
   \end{align}

where :math:`x` is the system state, :math:`M` the linear model
operator, :math:`\eta \sim
N(0,Q)` the system noise, :math:`y` the predicted observation, :math:`H`
the observation operator to interpolate from state space to observation space, 
and :math:`\nu \sim N(0,R)` the observation error. 

The algorithmic implementation of this Kalman filter consists of two steps:

#. Forecast step: in this step, we forecast a new model state, :math:`x^f(k)`,
   and a new covariance matrix of the model uncertainty, :math:`P^f(k)`.

   .. math::

      \begin{align}
           x^f(k) &= M(k-1) x^a(k-1); \\
           P^f(k) &= M(k-1) P^a(k-1) M^\top(k-1) + Q(k-1).
      \end{align}

   Here, :math:`Q(k-1)` is the injected state uncertainty at time :math:`t_k`, 
   that is used, for example, when noise at the boundaries is applied. 

#. Assimilation step: here, the adjusted model solution is computed by 
   adding information from the measurements. 
   Similarly, we calculate :math:`P^a(k)`. 

   .. math::

      \begin{align}
           x^a(k)&=x^f(k) + K(k) (y^o(k) - H(k) x^f(k)); \\
           P^a(k)&=(I-K(k) H(k)) P^f(k), \\
      \end{align}

   where the Kalman gain matrix is given by

   .. math::
      
      K(k)  = P^f(k) H(k) (H(k) P^f(k) H^\top(k) + R(k))^{-1}.
   
   Here, :math:`R(k)` is the covariance matrix of the measurement errors. 

   :math:`P^a(k)` should be 'smaller' than :math:`P^f(k)`
   since we are closer to the real state after incorporating the measurements. 
   Note that we need to choose a matrix :math:`P^f(0)` to start with. 
   We can start with an initially-generated ensemble, based on 'engineering knowledge'; 
   or, for models where the main source of uncertainty is the boundary values, 
   we generate the initial ensemble by running the ensemble for some time and injecting
   random noise to the boundary values. 

The Kalman filter gives optimal estimates for :math:`x` and :math:`P` in the case of
linear models. The main problem of applying the Kalman filter directly
to environmental models is the computation of the covariance matrix
:math:`P`. Since such models usually have a large number of states (e.g.
:math:`O(10^4)`), the covariance matrix will also become very big, which causes
very expensive computational costs or even the impossibility of computing it.

Another problem is that the real-life model is usually nonlinear, and the state vector is large.
In OpenDA, we have implemented three popular algorithms that can be used to modify the 
Kalman filter for these situations.

- Reduced-Rank Square-Root Filtering

  The reduced-rank square-root (RRSQRT) filter algorithm is based on a
  factorization of the covariance matrix of the model uncertainty
  according to :math:`P=LL^\top`, where :math:`L` is a matrix with the
  :math:`q` leading eigenvectors :math:`l_i` (scaled by the square root of
  the eigenvalues), :math:`i=1,...,q`, of :math:`P` as columns. 
  All details about this method can be found in the 
  `PhD thesis <https://www.researchgate.net/publication/224840111_Efficient_Kalman_Filtering_Algorithms_for_Hydrodynamic_Models>`__ 
  written by Martin Verlaan.

- Ensemble Kalman filtering

  While the RRSQRT represents the covariance matrix :math:`P` based on the
  first :math:`q` leading eigenvectors, the ensemble Kalman filter (EnKF)
  is based on a representation of the probability density of the state
  estimate by a finite number :math:`N` of randomly generated system
  states. More information about the EnKF method can be found in a 
  `paper <https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/94JC00572>`__
  and a `book <https://link.springer.com/book/10.1007/978-3-540-38301-7>`__, both written by Geir Evensen.

- Ensemble Square-Root Filtering

  There are two fundamental problems associated with the use of EnKF.
  First, the ensemble size is limited by the computational cost of
  applying the forecast model to each ensemble member. The second problem is
  that small ensembles have few degrees of freedom available to represent
  errors and suffer from sampling errors that will further degrade the
  forecast error covariance representation. Sampling errors lead to loss
  of accuracy and underestimation of error covariances. This problem can
  progressively worsen, resulting in filter divergence.
  
  In ensemble square-root filters (ENSRF), the analysis step is done
  deterministically without generating any observation noise realization. Since no random
  sample is generated, this extra source of sampling error is eliminated.
  Therefore, these methods are expected to perform better than the ones
  with perturbed observations for certain types of applications.

  More information about the ENSRF method can be found in the 
  `paper <https://journals.ametsoc.org/view/journals/mwre/130/7/1520-0493_2002_130_1913_edawpo_2.0.co_2.xml>`__ written by Whitaker and Hamill.
