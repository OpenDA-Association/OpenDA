#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
Read Kalman gains from xml-files and make plots

@author: verlaanm
"""
import numpy as np
import matplotlib.pyplot as plt
import gain

dirnames =["enkf_wave_185811190000","enkf_wave_185811210000",\
   "enkf_wave_185811230000","enkf_wave_185811250000","enkf_wave_185811270000"]

k_all=[]
for dir in dirnames:
    k_all.append(gain.read(dir));

(nObs,nState)=np.shape(k_all[0])
nGrid= (nState-1)/2

plt.close("all")

ifig=5 #start figures at 5
for i in range(len(k_all)):
    f,ax = plt.subplots(2,1)
    k_sep=k_all[i][:,2:nGrid+1].transpose()
    k_u=k_all[i][:,nGrid+1:(2*nGrid)].transpose()
    ax[0].plot(k_sep)
    ax[0].set_ylabel("sealevel [m]")
    ax[0].set_xlabel("x [index]")
    ax[1].plot(k_u)
    ax[1].set_xlabel("x [index]")
    ax[1].set_ylabel("velocity [m/s]")
    plt.title("Kalman gain "+dirnames[i])
    plt.legend(("station 1","station 2", "station 3"))
    plt.savefig("figure_"+str(ifig))
    ifig+=1


