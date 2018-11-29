#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct 12 15:06:13 2018

@author: hegeman
"""
from py_openda.costFunctions.simulation_kwadratic_cost_function import simulation_kwadratic_cost_function
from py_openda.costFunctions.model_instance import model_instance
from py_openda.costFunctions.stoch_observer import stoch_observer

class PyKwadratic:
    
    def __init__(self):
        self.model_instance = model_instance()
        self.observer = stoch_observer()
        self.cost_function = simulation_kwadratic_cost_function(self.model_instance, self.observer)
        
    def get_parameters(self):
        p = self.model_instance.get_parameters()
        
        return p
        
    def get_obs(self):
        """
        Find the observations and corresponding uncertainties at the relevant locations 
        
        :return: a tuple containing a list of the mean of all observations at each locations, followed by their respective standard deviations
        """
        selectionTimes = self.model_instance.get_time_horizon()
        self.observer.create_selection(selectionTimes)
        obs_mean = self.observer.get_expectations()
        obs_std = self.observer.get_standard_deviations()
        
        return (obs_mean, obs_std)
    
    def object_function(self, p):
        """
        Compute the predictions of the object function for parameters p
        :param p: parameters
        :return: list of predictions
        """    
        prd = self.cost_function.evaluate(p)
        print("Cost="+str(prd)+" p="+str(p))
        return prd

