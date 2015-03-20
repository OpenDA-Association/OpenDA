The lorenz96 model :

This simple model is often used as an example for chaotic behaviour. With the
default settings this model will be extremely sensitive to changes of the initial
conditions.
The background of this model is an extremely simplified meteorological parameter
at a latitude circle

(Lorenz and Emanuel 1998, "optimal sites for supplementary weather observations: Simulation with a small model"
J. Atmos. SCi. 127 2803-28020)

dx(i)/dt = ( x(i+1) - x(i-2) ) x(i-1) - x(i) + F
i=1:1:N N=40 cyclic and F=8.0
