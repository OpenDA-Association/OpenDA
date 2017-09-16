#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
Created on Fri Jul  3 16:38:17 2015

@author: verlaanm
"""

# ex2 

#load numpy and matplotlib if needed
import numpy as np
import matplotlib.pyplot as plt

#[t,ens]=load_ensemble('enkf_results');
import ensemble
import enkf_results
(t,enkf)=ensemble.reshape_ensemble(enkf_results)

enkf1=enkf[:,0,:] #note we start counting at 0
#plot all ensemble members
fig1 = plt.figure()
plt.plot(t,enkf1)
#plot ensemble mean and observations
fig2 = plt.figure()
plt.plot(t,np.mean(enkf1,1))
plt.plot(t,enkf_results.obs,"r*")
#add observations


