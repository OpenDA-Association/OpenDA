#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct 12 13:48:33 2018

@author: hegeman
"""

class model_instance:
    def __init__(self, parameters = [31,42], time_horizon = [5,7]):
        self.parameters = parameters
        self.time_horizon = time_horizon #Time horizon geeft enkel een begin en eindtijd
    
    def get_parameters(self):
        return self.parameters
    
    def set_parameters(self, parameters):
        self.parameters = parameters
    
    def get_time_horizon(self):
        return self.time_horizon
    
    def get_observed_values(self, selection):
        all_positions = [1, 2, 3, 4, 5, self.parameters[0], self.parameters[1], 8, 9]
        return [all_positions[i] for i in selection]
        