#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
    Plot results of EnKF run
"""

import matplotlib.pyplot as plt

import Enkf_results as Enkf

t = Enkf.analysis_time
enkf = Enkf.pred_f
t = (t - min(t))*24

obs_stat = Enkf.obs
nstations = obs_stat.shape[1]

for i in range(nstations):
    # plot for station i
    fig = plt.figure()
    plt.plot(t, enkf[:,i])
    plt.plot(obs_stat[:,i])
    plt.legend(['enkf','observation'])
    plt.title("station "+str(i+1))

plt.show()
print("debugging")
