#! /usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Plot movie of model results of the original model

@author: Nils van Velzen
"""

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
from time import sleep
#import polution_utils as util
import sequentialSimulation_results as sim


#offsets for chunks in state vector
ngrid      = 61
no_sources_sim  = len(sim.x_a[0])-2*ngrid


# create initial plot
plt.close("all")
fig,ax = plt.subplots(2,1)
xdata, ydata = [], []
ln, = plt.plot([], [], 'ro')


#for i in range(len(c1)):
#   ax[0].clear();
#   ax[1].clear();
#   ax[0].plot(c1[i],'k')
#   ax[1].plot(c2[i],'k')
#   ax[0].set_ylabel("c_1")
#   ax[1].set_ylabel("c_2")
#   ax[0].set_ylim([0, 200])
#   ax[1].set_ylim([0, 250])
#   ax[0].set_title("t="+str(60*i))
#   plt.draw()
#   plt.pause(0.02)


def init():
    ax[0].set_ylim([0, 200])
    ax[1].set_ylim([0, 250])
    return ln,

def update(frame):
   ax[0].clear();
   ax[1].clear();
   time = sim.analysis_time[frame]
   c1_sim  = sim.x_a[frame,(no_sources_sim):(no_sources_sim+ngrid)]
   c2_sim  = sim.x_a[frame,(no_sources_sim+ngrid):(no_sources_sim+2*ngrid)]


   ax[0].plot(c1_sim,'k')
   ax[1].plot(c2_sim,'k')
   ax[0].set_ylabel("c_1")
   ax[1].set_ylabel("c_2")
   ax[0].set_ylim([0, 200])
   ax[1].set_ylim([0, 250])
   ax[0].set_title("t="+str(int(time)))
   return ln,


ani = FuncAnimation(fig, update, frames=range(len(sim.analysis_time)),
                    init_func=init, repeat=False, interval=20,blit=True)
plt.show()

