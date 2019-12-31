#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct 12 13:50:20 2018

@author: hegeman
"""

class stoch_observer:
    def __init__(self, expectations = [1, 2, 3, 4, 5, 6, 7, 8, 9], standard_deviations = [1, 1, 1, 1, 1, 1, 1, 1, 1]):
        self.expectations = expectations
        self.standard_deviations = standard_deviations
        self.selection = range(0,len(expectations)) #Geeft indices
        
    def create_selection(self,time_horizon):
        self.selection = range(time_horizon[0], time_horizon[1])
    
    def get_observation_descriptions(self):
        return self.selection
    
    def get_expectations(self):
        return [self.expectations[i] for i in self.selection]
    
    def get_standard_deviations(self):
        return [self.standard_deviations[i] for i in self.selection]