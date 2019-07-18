#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
    Plot results after running Simulation.oda
"""
import matplotlib.pyplot as plt
import simulation_results as sim

obs = sim.obs[0]
pred_f = sim.pred[0]

fig1 = plt.figure()
plt.plot(pred_f)
plt.plot(obs)
plt.title('Obs01')
plt.xlabel('time')
plt.legend(['prediction','observation'])

plt.show()
