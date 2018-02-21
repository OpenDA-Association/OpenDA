#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
Plot movie of model simulation output.
Uses directly the output of the model, not the output from OpenDA

@author: verlaanm
"""

import shutil
import numpy as np
import matplotlib.pyplot as plt
from time import sleep

#load data
import simulation_results as sim
(nTimes,nState)=np.shape(sim.x_a)
nGrid= (nState-1)/2

plt.close("all")

# timeseries of element 3 (=2 in python)
values=sim.x_a[:,2]
t=sim.analysis_time
fig4=plt.figure()
plt.plot(t,values,'b-')
plt.title("Waterlevel near left boundary")
plt.xlabel("time")
plt.ylabel("water level [m]")
plt.savefig("figure_4.png")


f,ax = plt.subplots(2,1)
plt.ion()

for i in range(nTimes):
   ax[0].clear();
   ax[1].clear();
   x=sim.x_a[i,:]
   sep=x[2:nGrid+1]
   u=x[nGrid+1:(2*nGrid)]
   ax[0].plot(sep,'b')
   ax[0].set_ylabel("sealevel [m]")
   ax[1].plot(u,'b')
   ax[1].set_ylabel("velocity [m/s]")
   plt.title("time="+str(sim.analysis_time[i]))
   plt.draw()
   sleep(0.1)

