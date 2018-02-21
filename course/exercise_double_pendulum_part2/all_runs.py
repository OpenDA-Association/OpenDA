#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
Script that will produce the figures of exercise 2

@author: verlaanm
"""

#load numpy and matplotlib if needed
import numpy as np
import matplotlib.pyplot as plt


import simulation_truth_results as truth
import simulation_initial_results as initial
import simulation_enkf_results as enkf

plt.figure()
plt.clf()
plt.subplot(2,1,1)
plt.plot(initial.model_time,initial.x[:,0],'g')
plt.plot(truth.model_time,truth.x[:,0],'k')
plt.plot(enkf.analysis_time,enkf.x_f_central[:,0],'b');
plt.legend(('initial','truth','EnKF'))
plt.ylabel(r'$\theta_1$')
plt.subplot(2,1,2)
plt.plot(initial.model_time,initial.x[:,1],'g')
plt.plot(truth.model_time,truth.x[:,1],'k')
plt.plot(enkf.analysis_time,enkf.x_f_central[:,1],'b');
plt.ylabel(r'$\theta_2$')
plt.xlabel(r'$t$')
plt.savefig('fig_series_enkf.png')
plt.show()


import simulation_enkf_results_seed31415 as enkf2

plt.figure()
plt.clf()
plt.subplot(2,1,1)
plt.plot(initial.model_time,initial.x[:,0],'g')
plt.plot(truth.model_time,truth.x[:,0],'k')
plt.plot(enkf.analysis_time,enkf.x_f_central[:,0],'b');
plt.plot(enkf2.analysis_time,enkf2.x_f_central[:,0],'r');
plt.legend(('initial','truth','EnKF seed=21','EnKF seed=31415'))
plt.ylabel(r'$\theta_1$')
plt.subplot(2,1,2)
plt.plot(initial.model_time,initial.x[:,1],'g')
plt.plot(truth.model_time,truth.x[:,1],'k')
plt.plot(enkf.analysis_time,enkf.x_f_central[:,1],'b');
plt.plot(enkf2.analysis_time,enkf2.x_f_central[:,1],'r');
plt.ylabel(r'$\theta_2$')
plt.xlabel(r'$t$')
plt.savefig('fig_series_enkf_seed31415.png')
plt.show()


import simulation_enkf_results_stdobs2 as enkf2

plt.figure()
plt.clf()
plt.subplot(2,1,1)
plt.plot(initial.model_time,initial.x[:,0],'g')
plt.plot(truth.model_time,truth.x[:,0],'k')
plt.plot(enkf.analysis_time,enkf.x_f_central[:,0],'b');
plt.plot(enkf2.analysis_time,enkf2.x_f_central[:,0],'r');
plt.legend(('initial','truth','EnKF $\sigma_0=0.2$','EnKF $\sigma_o=2.0$'))
plt.ylabel(r'$\theta_1$')
plt.subplot(2,1,2)
plt.plot(initial.model_time,initial.x[:,1],'g')
plt.plot(truth.model_time,truth.x[:,1],'k')
plt.plot(enkf.analysis_time,enkf.x_f_central[:,1],'b');
plt.plot(enkf2.analysis_time,enkf2.x_f_central[:,1],'r');
plt.ylabel(r'$\theta_2$')
plt.xlabel(r'$t$')
plt.savefig('fig_series_enkf_std2.png')
plt.show()


import simulation_enkf_results_ens6 as enkf3
import simulation_enkf_results_ens10 as enkf4

plt.figure()
plt.clf()
plt.subplot(2,1,1)
plt.plot(initial.model_time,initial.x[:,0],'g')
plt.plot(truth.model_time,truth.x[:,0],'k')
plt.plot(enkf.analysis_time,enkf.x_f_central[:,0],'b');
plt.plot(enkf3.analysis_time,enkf3.x_f_central[:,0],'r');
plt.plot(enkf4.analysis_time,enkf4.x_f_central[:,0],'m');
plt.legend(('initial','truth','EnKF n=50','EnKF n=6','Enkf n=10'))
plt.ylabel(r'$\theta_1$')
plt.subplot(2,1,2)
plt.plot(initial.model_time,initial.x[:,1],'g')
plt.plot(truth.model_time,truth.x[:,1],'k')
plt.plot(enkf.analysis_time,enkf.x_f_central[:,1],'b');
plt.plot(enkf3.analysis_time,enkf3.x_f_central[:,1],'r');
plt.plot(enkf4.analysis_time,enkf4.x_f_central[:,1],'m');
plt.ylabel(r'$\theta_2$')
plt.xlabel(r'$t$')
plt.savefig('fig_series_enkf_ens_size.png')
plt.show()

