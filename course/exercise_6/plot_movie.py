#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
Plot movie of output of Ensemble Kalman filter.
Uses the output from OpenDA contrary to exercise 4

@author: verlaanm
"""

import numpy as np
import matplotlib.pyplot as plt
from time import sleep

#load data
import simulation_results as sim
#import enkf_results as enkf

# create initial plot
plt.close("all")
plt.figure()
plt.ion()

#offsets for chunks in state vector
(nTimes,nStates)=np.shape(sim.x_f_central)

for i in range(len(sim.analysis_time)):
   x=sim.x_a[i,1:]
   #x_enkf=enkf.x_a[i,1:]
   plt.clf()
   plt.plot(x,'k')
   #plt.plot(x_enkf,'b')
   plt.xlabel("x [index]")
   plt.ylabel("concentration")
   #plt.legend("First Guess","Enkf")
   plt.draw()
   sleep(0.1)

plt.savefig("figure_1.png")
