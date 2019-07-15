#! /usr/bin/env python3
import sequentialSimulation_results as seq_sim
import matplotlib.pyplot as plt

obs = seq_sim.obs
pred = seq_sim.pred_f_central
time = seq_sim.analysis_time

f,ax = plt.subplots(3,2)
for i in range(3):
   ax[i,0].plot(time,obs[:,i],'r')
   ax[i,0].plot(time,pred[:,i],'b')
   ax[i,1].plot(time,obs[:,i+3],'r')
   ax[i,1].plot(time,pred[:,i+3],'b')
ax[0,0].set_title("c1")
ax[0,1].set_title("c2")
plt.show()
