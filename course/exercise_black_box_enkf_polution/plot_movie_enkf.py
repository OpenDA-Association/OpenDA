#! /usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Plot movie of output of Ensemble Kalman filter.
Uses the output from OpenDA contrary to exercise 4

@author: verlaanm
"""
import os
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import polution_utils as util


#load data
import sequentialSimulation_results as sim
import enkf_results as enkf
#import enkf_results2 as enkf
c1,c2 = util.read_maps("twinTrue")

# create initial plot
plt.close("all")
f,ax = plt.subplots(2,1)

#offsets for chunks in state vector
ngrid      = 61
no_sources_sim  = len(sim.x_a[0])-2*ngrid
no_sources_enkf = len(enkf.x_a[0])-2*ngrid

#locations of the observations
x_obs = [10, 20, 40]
v_obs = [0, 0, 0]

# create initial plot
plt.close("all")
fig,ax = plt.subplots(2,1)
xdata, ydata = [], []
ln, = plt.plot([], [], 'ro')


def init():
    ax[0].set_ylim([0, 200])
    ax[1].set_ylim([0, 250])
    return ln,


def update(frame):
    i=frame
    time = enkf.analysis_time[i]
    c1_true = c1[i]
    c1_sim  = sim.x_a[i,(no_sources_sim):(no_sources_sim+ngrid)]
    c1_enkf = enkf.x_a[i,(no_sources_enkf):(no_sources_enkf+ngrid)]

    #Ugly only picks right observations in most cases
    if (len( enkf.obs[i,:]) == 6):
       obs_c1  = enkf.obs[i,[0,1,2]]
       obs_c2  = enkf.obs[i,[3,4,5]]
    elif (len( enkf.obs[i,:]) == 2):
       obs_c1  = [0,0, enkf.obs[i,0]]
       obs_c2  = [0,0, enkf.obs[i,1]]
    else:
       obs_c1 = [0,0,0]
       obs_c2 = [0,0,0]


    ax[0].clear();
    ax[1].clear();
    ax[0].plot(c1_true,'k')
    ax[0].plot(c1_sim,'b')
    ax[0].plot(c1_enkf,'g')
    
    c2_true = c2[i]
    c2_sim  = sim.x_a[i,(no_sources_sim+ngrid):(no_sources_sim+2*ngrid)]
    c2_enkf = enkf.x_a[i,(no_sources_enkf+ngrid):(no_sources_enkf+2*ngrid)]

    ax[1].plot(c2_true,'k')
    ax[1].plot(c2_sim,'b')
    ax[1].plot(c2_enkf,'g')
    ax[0].set_ylim([0, 200])
    ax[1].set_ylim([0, 250])


    ax[0].plot(x_obs,obs_c1,'*')
    ax[1].plot(x_obs,obs_c2,'*')
    ax[0].set_title("t="+str(int(time)))
    ax[0].legend(("Truth","First guess","EnKF"))
    return ln,

ani = FuncAnimation(fig, update, frames=range(len(c1)),
                    init_func=init, repeat=False, interval=20,blit=True)
plt.show()

