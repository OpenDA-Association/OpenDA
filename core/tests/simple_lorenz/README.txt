

Introduction

The 3 variable lorenz model is a well known testmodel for data-assimilation methods. The model is highly
non-linear (chaotic). The 3 variables represent the coefficient for the first mode of instability when
a horizontal layer of fluid is heated from below. This causes instabilities. Depending on the temperature
the behaviour can be periodic or chaotic. The current settings in this model give chatic behaviour.

  dx = sigma*(y-x)  *dt
  dy = (rho*x-y-x*z)*dt
  dz = (x*y-beta*z) *dt

All variables have been transformed to non-dimensional units, i.e. 1 is typical time-scale.

In our experiments observations of x are taken periodically. The time between observations is
very important in this problem as it controls the non-linearity. For th present setting dt_obs=0.1
the problem is not very hard and all Kalman filters should work.

