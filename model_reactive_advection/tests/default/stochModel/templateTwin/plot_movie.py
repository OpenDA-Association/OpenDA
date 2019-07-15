#! /usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Plot movie of output of Ensemble Kalman filter.
Uses the output from OpenDA contrary to exercise 4

@author: verlaanm
"""

import numpy as np
import matplotlib.pyplot as plt
from time import sleep
import read_map

#load data
c1,c2 = read_map.read_maps("maps")

# create initial plot
plt.close("all")
f,ax = plt.subplots(2,1)


for i in range(len(c1)):
   ax[0].clear();
   ax[1].clear();
   ax[0].plot(c1[i],'k')
   ax[1].plot(c2[i],'k')
   ax[0].set_ylabel("c_1")
   ax[1].set_ylabel("c_2")
   ax[0].set_ylim([0, 200])
   ax[1].set_ylim([0, 250])
   plt.draw()
   plt.pause(0.02)

