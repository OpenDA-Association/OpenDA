#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
Created on Fri Jul  3 16:38:17 2015

@author: verlaanm
"""

# ex1 

#load numpy and matplotlib if needed
import numpy as np
import matplotlib.pyplot as plt

#load data
import simulation_unperturbed_results as sim

# make 3d line plot
from mpl_toolkits.mplot3d import Axes3D
fig1 = plt.figure()
ax = fig1.add_subplot(111, projection='3d')
Axes3D.plot(ax,sim.x[:,0],sim.x[:,1],sim.x[:,2])

#plot of variable 1 (that is o here)
fig2 = plt.figure()
plt.plot(sim.model_time,sim.x[:,0])
plt.plot(sim.analysis_time,sim.obs,'r*')

#load unperturbed and perturbed results
import simulation_unperturbed_results as sim
import simulation_perturbed_results as simp
fig3 = plt.figure()
ax = fig3.add_subplot(111, projection='3d')
Axes3D.plot(ax,sim.x[:,0],sim.x[:,1],sim.x[:,2],'b')
Axes3D.plot(ax,simp.x[:,0],simp.x[:,1],simp.x[:,2],'r')

fig4 = plt.figure()
plt.plot(sim.model_time,sim.x[:,0])
plt.plot(simp.model_time,simp.x[:,0],'r')

#load ensemble and plot
import ensemble
(t,ens)=ensemble.load_ensemble()
ens1=ens[:,0,:] #note we start counting at 0
fig5 = plt.figure()
plt.plot(t,ens1)
fig6 = plt.figure()
plt.plot(t,np.mean(ens1,1))