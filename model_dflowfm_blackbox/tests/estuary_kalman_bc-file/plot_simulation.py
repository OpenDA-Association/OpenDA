#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
    Plot results after running Simulation.oda
"""
import matplotlib.pyplot as plt
from netCDF4 import Dataset

from datetime import datetime, timedelta

print("WARNING: results only useful after running Simulation.oda\n")

file = './stochModel/work0/dflowfm/DFM_OUTPUT_estuary/estuary_his.nc'
data = Dataset(file, 'r')

time = data.variables['time'][:]
wl = data.variables['waterlevel'][:]

fig1 = plt.figure()
plt.plot(wl[:, 0])
plt.title('Obs01')
plt.xlabel('time')
plt.legend(['prediction'])

fig2 = plt.figure()
plt.plot(wl[:, 1])
plt.title('Obs02')
plt.xlabel('time')
plt.legend(['prediction'])

fig3 = plt.figure()
plt.plot(wl[:, 2])
plt.title('Obs03')
plt.xlabel('time')
plt.legend(['prediction'])

plt.show()
