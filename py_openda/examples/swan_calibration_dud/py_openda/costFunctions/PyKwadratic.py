#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct 12 15:06:13 2018

@author: hegeman
"""
import os
os.chdir(os.getcwd()+'/py_openda/costFunctions')
from simulation_kwadratic_cost_function import simulation_kwadratic_cost_function
from model_instance import model_instance
from stoch_observer import stoch_observer
os.chdir(os.getcwd())
cost_function = None
model_params = None
model_ini = None
observer = None

def setup():
    global cost_function
    global model_params
    global model_ini
    global observer
  
    model_ini = model_instance()
    observer = stoch_observer()
    cost_function = simulation_kwadratic_cost_function(model_ini, observer)
    p = model_ini.get_parameters()
    model_params = p
    
    return(cost_function, model_params, model_ini, observer)
    
def get_obs():
    """
    Find the observations and corresponding uncertainties at the relevant locations 
    
    :return: a tuple containing a list of the mean of all observations at each locations, followed by their respective standard deviations
    """
    selectionTimes = model_ini.get_time_horizon()
    observer.create_selection(selectionTimes)
    obs_mean = observer.get_expectations()
    obs_std = observer.get_standard_deviations()
    
    return (obs_mean, obs_std)

def object_function(p):
    """
    Compute the predictions of the object function for parameters p
    :param p: parameters
    :return: list of predictions
    """

    # Create an OpenDA TreeVector with parameter value
    model_new = model_ini
    model_new.set_parameters(p)

    prd = cost_function.evaluate(p)
    print("Cost="+str(prd)+" p="+str(p))
    return prd

