#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
   Sequential simulation:
   Make plot to show difference between observations and predicted values
"""

import SequentialSimulation_results as seqsim
import matplotlib.pyplot as plt

nobs = len(seqsim.obs)
obs = seqsim.obs
pred_f = seqsim.pred_f_central

fig1 = plt.figure()
plt.plot(pred_f[:,0])
plt.plot(obs[:,0])
plt.title('Obs01')
plt.xlabel('time')
plt.legend(['prediction','observation'])

fig2 = plt.figure()
plt.plot(pred_f[:,1])
plt.plot(obs[:,1])
plt.title('Obs02')
plt.xlabel('time')
plt.legend(['prediction','observation'])

fig3 = plt.figure()
plt.plot(pred_f[:,2])
plt.plot(obs[:,2])
plt.title('Obs03')
plt.xlabel('time')
plt.legend(['prediction','observation'])

diff = pred_f - obs
fig4 = plt.figure()
plt.plot(diff)
plt.title('Difference between model and observations')
plt.ylabel('prediction-observation')
plt.xlabel('time')

plt.show()


