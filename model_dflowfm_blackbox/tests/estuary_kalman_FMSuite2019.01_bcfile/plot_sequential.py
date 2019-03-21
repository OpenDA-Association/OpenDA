#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
   Sequential simulation:
   Make plot to show difference between observations and predicted values
"""

import SequentialSimulation_results as seqsim
import matplotlib.pyplot as plt

obs = seqsim.obs
pred_f = seqsim.pred_f_central
nobs = obs.shape[0]
nstat = obs.shape[1]

for i in range(nstat):
    fig = plt.figure()
    plt.plot(pred_f[:,i])
    plt.plot(obs[:,i])
    plt.title('Station0'+str(i+1))
    plt.xlabel('time')
    plt.legend(['prediction','observation'])

diff = pred_f - obs
fig4 = plt.figure()
plt.plot(diff)
plt.title('Difference between model and observations')
plt.ylabel('prediction-observation')
plt.xlabel('time')
plt.legend(['Station 1', 'Station 2', 'Station 3'])

plt.show()
print("hou plotjes in beeld")

