#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
Script that will produce the figures of exercise 2

@author: verlaanm
"""

#load numpy and matplotlib if needed
import numpy as np
import matplotlib.pyplot as plt
import ensemble

# laod data fomr enkf_results_estd1_ens10, collect ensemble in one 3d-array
# and compute ensemble mean of first [0] element in statevector (state=[x,y,z])
import enkf_results_std1_ens10 as std1
(t,enkf)=ensemble.reshape_ensemble(std1)
enkf1=enkf[:,0,:] #note we start counting at 0

#
#plot all ensemble members
#
fig1 = plt.figure()
plt.plot(t,enkf1)
plt.title("Lorenz model first variable (EnKF)")
plt.savefig("figure_1.png")

#
#plot ensemble mean and observations
#
fig2 = plt.figure()
plt.plot(t,np.mean(enkf1,1))
plt.plot(t,std1.obs[:,0],"r*")
plt.title("Lorenz model mean and observations of first variable (EnKF)")
plt.legend(("ensemble mean for x, std=1","observations"))
plt.savefig("figure_2.png")

#
#now for std=10
#
import enkf_results_std10_ens10 as std10
(t,enkf)=ensemble.reshape_ensemble(std10)
enkf10=enkf[:,0,:] #note we start counting at 0

#
#plot all ensemble members
#
fig3 = plt.figure()
plt.plot(t,enkf10)
plt.title("Lorenz model first variable (EnKF)")
plt.savefig("figure_3.png")

#
#plot ensemble mean and observations
#
fig4 = plt.figure()
plt.plot(t,np.mean(enkf10,1))
plt.plot(t,std10.obs[:,0],"r*")
plt.title("Lorenz model mean and observations of first variable (EnKF)")
plt.legend(("ensemble mean for x, std=10","observations"))
plt.savefig("figure_4.png")

#
# compare data for different ensemble sizes
#
import enkf_results_std5_ens5 as ens5
(t,ens5.ens)=ensemble.reshape_ensemble(ens5)
ens5.x=np.mean(ens5.ens[:,0,:],1)
import enkf_results_std5_ens10 as ens10
(t,ens10.ens)=ensemble.reshape_ensemble(ens10)
ens10.x=np.mean(ens10.ens[:,0,:],1)
import enkf_results_std5_ens100 as ens100
(t,ens100.ens)=ensemble.reshape_ensemble(ens100)
ens100.x=np.mean(ens100.ens[:,0,:],1)
#make plot
fig5 = plt.figure()
plt.plot(t,ens5.x,'k-')
plt.plot(t,ens10.x,'b-')
plt.plot(t,ens100.x,'g-')
plt.plot(t,ens10.obs[:,0],"r*")
plt.legend(("n=5","n=10","n=100","observations"))
plt.savefig("figure_5.png")

