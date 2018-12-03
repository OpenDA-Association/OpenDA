#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Class for using a very simple model and observer in Python with Dud.
Note that the classes used are very simple and not really representative of
what a model and observer would look like for which one would actually use Dud,
but it serves as an example of the minimum requirements for an actual implementation.
Created on Fri Oct 12 15:06:13 2018

@author: hegeman
"""
from py_openda.costFunctions.simulation_kwadratic_cost_function import simulation_kwadratic_cost_function
from py_openda.costFunctions.model_instance import model_instance
from py_openda.costFunctions.stoch_observer import stoch_observer

class PyKwadratic:
    """
    Class for linking a very simple Python model, observer and cost function to the Dud algorithm.
    """
    def __init__(self):
        """
        """
        self.model_instance = model_instance()
        self.observer = stoch_observer()
        self.cost_function = simulation_kwadratic_cost_function(self.model_instance, self.observer)

    def get_parameters(self):
        """
        Extracts the parameters from the main model instance.

        :return: the initial parameters.
        """
        p = self.model_instance.get_parameters()

        return p

    def get_obs(self):
        """
        Find the observations and corresponding uncertainties at the relevant locations.

        :return: a tuple containing a list of the mean of all observations at each locations,
        followed by their respective standard deviations.
        """
        selectionTimes = self.model_instance.get_time_horizon()
        self.observer.create_selection(selectionTimes)
        obs_mean = self.observer.get_expectations()
        obs_std = self.observer.get_standard_deviations()

        return (obs_mean, obs_std)

    def object_function(self, p):
        """
        Compute the predictions of the object function for parameters p.
        :param p: parameters.
        :return: list of predictions.
        """
        prd = self.cost_function.evaluate(p)
        print("Cost="+str(prd)+" p="+str(p))
        return prd

