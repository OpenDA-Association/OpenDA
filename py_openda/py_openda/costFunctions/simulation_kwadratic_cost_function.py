#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct 12 13:44:45 2018

@author: hegeman
"""


class simulation_kwadratic_cost_function:
    def __init__(self, model_instance, stoch_observer):
        self.model_instance = model_instance
        self.stoch_observer = stoch_observer
        
    def evaluate(self, parameters):
        self.model_instance.set_parameters(parameters)
        sel = self.stoch_observer.get_observation_descriptions()
        test = self.model_instance.get_observed_values(sel)
        return test
