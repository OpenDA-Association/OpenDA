#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
    Plot results hisfile
"""
from netCDF4 import Dataset, num2date
import os
import matplotlib.pyplot as plt

# location of input file is defined relative to location of this script
ncfile = os.path.join(os.path.dirname(__file__), 'stochModel/work0/dflowfm/DFM_OUTPUT_estuary/full_estuary_his.nc')

nc_fid = Dataset(ncfile, 'r')

time_work = nc_fid.variables['time'][:]
timeunit = nc_fid['time'].units
time = num2date(time_work, timeunit)
wl = nc_fid.variables['waterlevel'][:]

fig1 = plt.figure()
plt.plot_date(time, wl[:, 0], 'b')
plt.title('station01')
plt.xlabel('time')
plt.ylabel('waterlevel')

fig2 = plt.figure()
plt.plot_date(time, wl[:, 1], 'b')
plt.title('station02')
plt.xlabel('time')
plt.ylabel('waterlevel')

fig3 = plt.figure()
plt.plot_date(time, wl[:, 2], 'b')
plt.title('station03')
plt.xlabel('time')
plt.ylabel('waterlevel')

plt.show()

 
