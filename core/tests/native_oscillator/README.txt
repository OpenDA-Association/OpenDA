
Introduction

This model implement a simple oscillator model. Because of its small size
(2 state variables) and linear behaviour, this model is ideal as a first
test for assimilation methods. Al should converge quickly to the right 
values.

simple linear oscilator (e.g. mass-spring system with friction)
 d(x)/d(t) = u
 d(u)/d(t) = - omega^2 * x - (2/t_damp) u


The model is implemented in native code (Fortran). Input files are given
for 3 experiments

- enkf_sqlobs.oda  Run with ensemble Kalman filter and
                   assimilate observations from a native
                   implementation of an observer that reads the
                   observations from an sqlite-database file
                   obs_oscill.sql

- simplex.oda      Run with ensemble Kalman filter use
                   observations from a java implementation of
                   an observer that reads NOOS observations
                   from the file obs_oscill.noos

- enkf_javaobs.oda Calibration run using the simplex method

