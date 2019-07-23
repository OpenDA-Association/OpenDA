#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
Make plots for exercise 4

@author: verlaanm
"""

#load numpy and matplotlib if needed
import matplotlib.pyplot as plt

#load data
import dud_results as dud

# create plot of cost and parameter
plt.close("all")
f,ax = plt.subplots(2,1)
ax[0].plot(dud.costTotal);
ax[0].set_xlabel("model run");
ax[0].set_ylabel("cost function");
ax[1].plot(dud.evaluatedParameters);
ax[1].set_xlabel('model run');
ax[1].set_ylabel('change of reaction\_time [seconds]');
plt.savefig("figure_1.png")

#plot_movie
import numpy as np
from time import sleep

#load data
import reactive_pollution_model_original as ori
import reactive_pollution_model_truth as truth

f,ax = plt.subplots(2,1)
plt.ion()

# split sources and outputs based on substance
stypeisone=np.array(ori.source_substance)==1
stypeistwo=np.array(ori.source_substance)==2
sloc1=np.array(ori.source_locations)[stypeisone]
sloc2=np.array(ori.source_locations)[stypeistwo]
otypeisone=np.array(ori.output_substance)==1
otypeistwo=np.array(ori.output_substance)==2
oloc1=np.array(ori.output_locations)[otypeisone]
oloc2=np.array(ori.output_locations)[otypeistwo]

for i in ori.c1_map.keys():
   ax[0].clear();
   ax[1].clear();
   ax[0].plot(ori.c1_map[i],'b')
   ax[0].plot(truth.c1_map[i],'k')
   ax[0].plot(oloc1,0*oloc1+1,'*')
   ax[0].plot(sloc1,0*sloc1+1,'d')
   ax[0].set_ylabel("c_1")
   ax[1].plot(ori.c2_map[i],'b')
   ax[1].plot(truth.c2_map[i],'k')
   ax[1].plot(oloc2,0*oloc2+1,'*')
   ax[1].plot(sloc2,0*sloc2+1,'d')
   ax[1].set_ylabel("c_1")
   plt.draw()
   sleep(0.1)

plt.savefig("figure_2.png")
