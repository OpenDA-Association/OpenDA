#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
Plot movie of output of Ensemble Kalman filter.
Uses the output from OpenDA contrary to exercise 4

@author: verlaanm
"""
import os
import numpy as np
import matplotlib.pyplot as plt
from time import sleep
import polution_utils as util


#load data
import sequentialSimulation_results as sim
import enkf_results as enkf
#import enkf_results2 as enkf
c1,c2 = util.read_maps("twinTrue")

# create initial plot
plt.close("all")
f,ax = plt.subplots(2,1)

#offsets for chunks in state vector
ngrid      = 61
no_sources_sim  = len(sim.x_a[0])-2*ngrid
no_sources_enkf = len(enkf.x_a[0])-2*ngrid

#locations of the observations
x_obs = [10, 20, 40]


if True:
   for i in range(len(enkf.analysis_time)):
      time = enkf.analysis_time[i]
      c1_true = c1[i]
      c1_sim  = sim.x_a[i,(no_sources_sim):(no_sources_sim+ngrid)]
      c1_enkf = enkf.x_a[i,(no_sources_enkf):(no_sources_enkf+ngrid)]

      ax[0].clear();
      ax[1].clear();
      ax[0].plot(c1_true,'k')
      ax[0].plot(c1_sim,'b')
      ax[0].plot(c1_enkf,'g')
      
      c2_true = c2[i]
      c2_sim  = sim.x_a[i,(no_sources_sim+ngrid):(no_sources_sim+2*ngrid)]
      c2_enkf = enkf.x_a[i,(no_sources_enkf+ngrid):(no_sources_enkf+2*ngrid)]

      ax[1].plot(c1_true,'k')
      ax[1].plot(c1_sim,'b')
      ax[1].plot(c1_enkf,'g')


      ax[0].plot(x_obs,[0, 0, 0],'*')
      ax[1].plot(x_obs,[0, 0, 0],'*')

      plt.legend(("Truth","First guess","EnKF"))
      plt.draw()
      plt.pause(0.1)

