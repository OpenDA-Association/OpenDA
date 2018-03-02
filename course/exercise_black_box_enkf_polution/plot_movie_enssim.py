#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
Plot movie of output of Ensemble Kalman filter.
Uses the output from OpenDA contrary to exercise 4

@author: verlaanm
"""

import numpy as np
import matplotlib.pyplot as plt

#load data
import reactive_pollution_model_truth as truth
import sequentialSimulation_results as sim
import sequentialEnsembleSimulation_results as enkf
#import enkf_results as enkf
#import enkf_results2 as enkf

# create initial plot
plt.close("all")
f,ax = plt.subplots(2,1)

#offsets for chunks in state vector
ngrid=len(truth.c1)
no_sources=len(truth.source_locations)

# split sources and outputs based on substance
stypeisone=np.array(truth.source_substance)==1
stypeistwo=np.array(truth.source_substance)==2
sloc1=np.array(truth.source_locations)[stypeisone]
sloc2=np.array(truth.source_locations)[stypeistwo]
otypeisone=np.array(truth.output_substance)==1
otypeistwo=np.array(truth.output_substance)==2
oloc1=np.array(truth.output_locations)[otypeisone]
oloc2=np.array(truth.output_locations)[otypeistwo]

ii=[10,20]
for i in range(len(enkf.analysis_time)):
   ax[0].clear();
   ax[1].clear();
   ax[0].plot(truth.c1_map[i],'k')
   c1_sim=sim.x_a[i,(no_sources-1):(no_sources+ngrid)]
   ax[0].plot(c1_sim,'b')
   c1_enkf=enkf.x_a[i,(no_sources-1):(no_sources+ngrid)]
   ax[0].plot(c1_enkf,'g')
   ax[0].plot(oloc1,0*oloc1+1,'*')
   ax[0].plot(sloc1,0*sloc1+1,'d')
   ax[0].set_ylabel("c_1")
   ax[1].plot(truth.c2_map[i],'k')
   c2_sim=sim.x_a[i,(no_sources+ngrid-1):(no_sources+2*ngrid)]
   ax[1].plot(c2_sim,'b')
   c2_enkf=enkf.x_a[i,(no_sources+ngrid-1):(no_sources+2*ngrid)]
   ax[1].plot(c2_enkf,'g')
   ax[1].plot(oloc2,0*oloc2+1,'*')
   ax[1].plot(sloc2,0*sloc2+1,'d')
   ax[1].set_ylabel("c_2")
   plt.legend(("Truth","First guess","EnsSim"))
   plt.draw()
   plt.pause(0.1)

plt.savefig("figure_1.png")
