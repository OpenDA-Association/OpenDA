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

import enkf_results as enkf

diff = enkf.obs -enkf.pred_a_central

#
#plot residual
#
fig2 = plt.figure()
plt.plot(enkf.analysis_time,diff)
plt.title("Difference between EnKF and observations")
plt.xlabel("time")
plt.ylabel("prediction-observation")
plt.savefig("figure_2.png")

#
# Compute RMS of errors
#
(ntimes,nobs)=np.shape(diff)

rms_enkf=[]
print("===================================================")
print("RMS errors at observations locations after analysis")
print("Ensemble Kalman filter")
print("===================================================")
for iobs in range(nobs):
    rms_enkf.append(  np.sqrt(np.sum(np.square(diff[:,iobs])))  )
    print("  "+str(iobs)+" : "+str(rms_enkf[iobs]))

