#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
Plot cost evolution

@author: verlaanm
"""

# ex1

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

