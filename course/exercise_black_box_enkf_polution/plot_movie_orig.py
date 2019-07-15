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
import polution_utils as util

#load data
c1,c2 = util.read_maps("original_model")

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
   ax[0].plot(c1[frame],'k')
   ax[1].plot(c2[frame],'k')
   ax[0].set_ylabel("c_1")
   ax[1].set_ylabel("c_2")
   ax[0].set_ylim([0, 200])
   ax[1].set_ylim([0, 250])
   ax[0].set_title("t="+str(60*frame))
   return ln,


ani = FuncAnimation(fig, update, frames=range(len(c1)),
                    init_func=init, repeat=False, interval=20,blit=True)
plt.show()

