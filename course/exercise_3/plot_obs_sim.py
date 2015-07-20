#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
Simulation of model:
Make plot to show difference between observations and predicted values

@author: verlaanm
"""
#load numpy and matplotlib if needed
import numpy as np
import matplotlib.pyplot as plt

import simulation_results as sim

diff = sim.obs -sim.pred_a_central

#
#plot residual
#
fig1 = plt.figure()
plt.plot(sim.analysis_time,diff)
plt.title("Difference between model and observations")
plt.xlabel("time")
plt.ylabel("prediction-observation")
plt.savefig("figure_1.png")

#
# Compute RMS of errors
#
(ntimes,nobs)=np.shape(diff)

rms=[]
print("===================================================")
print("RMS errors at observations locations after analysis")
print("Model simulation")
print("===================================================")
for iobs in range(nobs):
    rms.append(  np.sqrt(np.sum(np.square(diff[:,iobs])))  )
    print("  "+str(iobs)+" : "+str(rms[iobs]))
    
