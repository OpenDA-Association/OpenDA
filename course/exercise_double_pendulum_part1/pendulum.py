# -*- coding: utf-8 -*-
"""
use as: 
from pendulum import *
import simulation_unperturbed_results as sim
plot_movie(sim.model_time,sim,x)

Created on Mon Jul 3 2017

@author: verlaanm
"""
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as patches
import math
import time


#constants in code for now \TODO improve
g=9.81
l=0.3
w=0.02

rad2deg=180./math.pi

def plot_pendulum(ax1,x,color='b'):
    ''' plot one state x=[th1, th2, pth1, pth2] '''
    p1=patches.Rectangle((0.0,0.0),height=l,width=w,angle=rad2deg*x[0]-180.0,facecolor=color)
    ax1.add_patch(p1)
    x1=l*math.sin(x[0])
    y1=-l*math.cos(x[0])
    p2=patches.Rectangle((x1,y1),height=l,width=w,angle=rad2deg*x[1]-180.0,facecolor=color)
    ax1.add_patch(p2)

def plot_movie(times,states,more_states=None):
    ''' plot sequence off plots of a double pendulum like a movie '''
    fig1 = plt.figure()
    ax1 = fig1.add_subplot(111, aspect='equal')
    ax1.set_xlim([-2.0*l,2.0*l])
    ax1.set_ylim([-2.0*l,2.0*l])
    #plt.ion()
    for i in range(len(times)):
	ax1.clear()
        plot_pendulum(ax1,states[i,:])
        if(more_states!=None):
           plot_pendulum(ax1,more_states[i,:],color='g')
        plt.title('time = %.2f'%times[i])
        plt.draw()
        plt.pause(0.2)
    #plt.ioff()
if __name__ == '__main__':
    #only used for testing
    #plot first frame only
    fig1 = plt.figure()
    ax1 = fig1.add_subplot(111, aspect='equal')
    ax1.set_xlim([-2.0*l,2.0*l])
    ax1.set_ylim([-2.0*l,2.0*l])
    plot_pendulum(ax1,[0.25*math.pi,0.5*math.pi,0.0,0.0])
    plt.show()
