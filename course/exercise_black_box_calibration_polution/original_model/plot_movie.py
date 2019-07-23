#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
Plot movie of model simulation output.
Uses directly the output of the model, not the output from OpenDA

@author: verlaanm
"""

import shutil
import numpy as np
import matplotlib.pyplot as plt
from time import sleep

#load data
shutil.copy2("reactive_pollution_model.output", "reactive_pollution_model_output.py")
import reactive_pollution_model_output as sim

plt.close("all")
f,ax = plt.subplots(2,1)

# split sources and outputs based on substance
stypeisone=np.array(sim.source_substance)==1
stypeistwo=np.array(sim.source_substance)==2
sloc1=np.array(sim.source_locations)[stypeisone]
sloc2=np.array(sim.source_locations)[stypeistwo]
otypeisone=np.array(sim.output_substance)==1
otypeistwo=np.array(sim.output_substance)==2
oloc1=np.array(sim.output_locations)[otypeisone]
oloc2=np.array(sim.output_locations)[otypeistwo]

for i in sim.c1_map.keys():
   ax[0].clear();
   ax[1].clear();
   ax[0].plot(sim.c1_map[i])
   ax[0].plot(oloc1,0*oloc1+1,'*')
   ax[0].plot(sloc1,0*sloc1+1,'d')
   ax[0].set_ylabel("c_1")
   ax[1].plot(sim.c2_map[i])
   ax[1].plot(oloc2,0*oloc2+1,'*')
   ax[1].plot(sloc2,0*sloc2+1,'d')
   ax[1].set_ylabel("c_1")
   plt.draw()
   plt.pause(0.1)

