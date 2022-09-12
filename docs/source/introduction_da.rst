***************
Introduction DA
***************

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

On open-source text book considering the fundamentals of data
assimilation can be found
`here <https://library.oapen.org/handle/20.500.12657/54434>`__.
