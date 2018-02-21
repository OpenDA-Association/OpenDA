#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
Steady state Kalman filter:
Make plot to show difference between observations and predicted values

@author: verlaanm
"""
#load numpy and matplotlib if needed
import numpy as np
import matplotlib.pyplot as plt

import steadystate_results as steady

diff = steady.obs -steady.pred_a_central

#
#plot residual
#
fig3 = plt.figure()
plt.plot(steady.analysis_time,diff)
plt.title("Difference between Steady State filter and observations")
plt.xlabel("time")
plt.ylabel("prediction-observation")
plt.savefig("figure_3.png")

#
# Compute RMS of errors
#
(ntimes,nobs)=np.shape(diff)

rms_steady=[]
print("===================================================")
print("RMS errors at observations locations after analysis")
print("Steady-state Kalman filter")
print("===================================================")
for iobs in range(nobs):
    rms_steady.append(  np.sqrt(np.sum(np.square(diff[:,iobs])))  )
    print("  "+str(iobs)+" : "+str(rms_steady[iobs]))

