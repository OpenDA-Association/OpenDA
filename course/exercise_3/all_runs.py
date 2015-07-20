#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
Simulation of model:
Make plot to show difference between observations and predicted values
as well as for EnKF and steady state filter

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

#
# EnKF
#
    
import enkf_results as enkf
diff_enkf = enkf.obs -enkf.pred_a_central

#
#plot residual
#
fig2 = plt.figure()
plt.plot(enkf.analysis_time,diff_enkf)
plt.title("Difference between EnKF and observations")
plt.xlabel("time")
plt.ylabel("prediction-observation")
plt.savefig("figure_2.png")

#
# Compute RMS of errors
#
(ntimes,nobs)=np.shape(diff_enkf)

rms_enkf=[]
print("===================================================")
print("RMS errors at observations locations after analysis")
print("Ensemble Kalman filter")
print("===================================================")
for iobs in range(nobs):
    rms_enkf.append(  np.sqrt(np.sum(np.square(diff_enkf[:,iobs])))  )
    print("  "+str(iobs)+" : "+str(rms_enkf[iobs]))

#
# Steady state filter
#
import steadystate_results as steady
diff_steady = steady.obs -steady.pred_a_central

#
#plot residual
#
fig3 = plt.figure()
plt.plot(steady.analysis_time,diff_steady)
plt.title("Difference between Steady State filter and observations")
plt.xlabel("time")
plt.ylabel("prediction-observation")
plt.savefig("figure_3.png")

#
# Compute RMS of errors
#
(ntimes,nobs)=np.shape(diff_steady)

rms_steady=[]
print("===================================================")
print("RMS errors at observations locations after analysis")
print("Steady-state Kalman filter")
print("===================================================")
for iobs in range(nobs):
    rms_steady.append(  np.sqrt(np.sum(np.square(diff_steady[:,iobs])))  )
    print("  "+str(iobs)+" : "+str(rms_steady[iobs]))
    
#
# plot Kalman gains
#
import gain

dirnames =["enkf_wave_185811190000","enkf_wave_185811210000",\
   "enkf_wave_185811230000","enkf_wave_185811250000","enkf_wave_185811270000"]

k_all=[]
for dir in dirnames:
    k_all.append(gain.read(dir));

(nObs,nState)=np.shape(k_all[0])
nGrid= (nState-1)/2

plt.close("all")

ifig=5 #start figures at 5
for i in range(len(k_all)):
    f,ax = plt.subplots(2,1)
    k_sep=k_all[i][:,2:nGrid+1].transpose()
    k_u=k_all[i][:,nGrid+1:(2*nGrid)].transpose()
    ax[0].plot(k_sep)
    ax[0].set_ylabel("sealevel [m]")
    ax[0].set_xlabel("x [index]")
    ax[1].plot(k_u)
    ax[1].set_xlabel("x [index]")
    ax[1].set_ylabel("velocity [m/s]")
    plt.title("Kalman gain "+dirnames[i])
    plt.legend(("station 1","station 2", "station 3"))
    plt.savefig("figure_"+str(ifig))
    ifig+=1
