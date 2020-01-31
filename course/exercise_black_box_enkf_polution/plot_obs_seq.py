#! /usr/bin/env python3
#import sequentialSimulation_results as sim
import enkf_results as sim
import matplotlib.pyplot as plt


obs = sim.obs
nloc=obs.shape[1]//2 #number of observation locations included in OpenDA run
pred = sim.pred_f_central
time = sim.analysis_time

f,ax = plt.subplots(3,2)
for i in range(nloc):
   ax[i,0].plot(time,obs[:,i],'r')
   ax[i,0].plot(time,pred[:,i],'b')
   ax[i,1].plot(time,obs[:,i+nloc],'r')
   ax[i,1].plot(time,pred[:,i+nloc],'b')
ax[0,0].set_title("c1")
ax[0,1].set_title("c2")
plt.show()
