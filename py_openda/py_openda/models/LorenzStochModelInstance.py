#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Instance of a three varible Lorenz model, usable by the ensemble kalman filter algorithm.
Created on Thu Nov 22 11:32:08 2018

@author: hegeman
"""

from math import sqrt
import numpy as np
from scipy.stats import norm
from scipy.integrate import ode
from py_openda.costFunctions.JObjects import PyTime
import py_openda.utils.py4j_utils as utils
from py_openda.interfaces.IStochModelInstance import IStochModelInstance


class LorenzStochModelInstance(IStochModelInstance):
    """
    Instance of a three point Lorenz model.
    """
    def __init__(self, model_attributes, noise_config, main_or_ens=None):
        """
        :param model_attributes: attributes saved in the model factory.
        :param noise_config: dictionary as given by EnkfAlgorithm.xml for the noise configuration.
        :param main_or_ens: determines the ouput level of the model.
        """
        (self.param, self.param_uncertainty, self.state, self.state_uncertainty, self.sys_mean,
         self.sys_std, self.span) = model_attributes
        
        if noise_config is None:
            if main_or_ens == "main":
                noise_config = {'@stochParameter':False, '@stochForcing':False, '@stochInit':False}
            elif main_or_ens == "ens":
                noise_config = {'@stochParameter':False, '@stochForcing':True, '@stochInit':True}
        if noise_config.get('@stochInit'):
            realizations = [norm(loc=mean, scale=std).rvs() for mean,
                            std in zip(self.state, self.state_uncertainty)]
            self.state = realizations.copy()
        if noise_config.get('@stochParameter'):
            realizations = [norm(loc=mean, scale=std).rvs() for mean,
                            std in zip(list(self.param.values()),
                                       list(self.param_uncertainty.values()))]
            self.param = realizations

        self.auto_noise = noise_config.get('@stochForcing')


        self.current_time = PyTime(self.span[0])
        self.state = np.array(self.state)

    def get_time_horizon(self):
        """
        Get the computational time horizon of the model (begin and end time).

        :return: the time horizon (containing begin and end time).
        """
        return PyTime(self.span[0], self.span[2])

    def get_current_time(self):
        """
        Get the model instance's current simulation time stamp.

        :return: The model's current simulation time stamp.
        """
        return self.current_time

    def announce_observed_values(self, descriptions):
        """
        Tells model that it can expect to be asked for model values corresponding to the observations
        described. The model can make arrangement to save these values. The method compute run over a long
        interval at once, not stopping at each time with observations.
        This is meant to increase the performance especially of calibration algorithms.

        :param descriptions: an ObservationDescriptions object with meta data for the observations.
        :return:
        """
        None

    def compute(self, time):
        """
        Let the stochastic model instance compute to the requested target time stamp.
        This function can not be used to go back in time.

        :param time: Time to compute to.
        :return:
        """
        end_time = time.get_start()
        solver = ode(_lorenz_function_).set_integrator('dopri5').set_f_params(list(self.param.values()))
        solver = solver.set_initial_value(self.state, self.current_time.get_start())
        new_state = self.state.copy()
        t = self.current_time.get_start()
        t_step = self.span[1]
        nsteps = round((end_time-t)/t_step)
        for _ in range(nsteps):
            solver.integrate(solver.t +t_step)
            new_state = solver.y
            if self.auto_noise:
                realizations = [norm(loc=mean, scale=std).rvs() for mean,
                                std in zip(self.sys_mean, self.sys_std)]
                new_state = new_state + sqrt(t_step)*np.array(realizations)
            solver = solver.set_initial_value(new_state, solver.t)
        self.current_time = PyTime(end_time)
        self.state = new_state

    def get_observations(self, descriptions):
        """
        Get model values corresponding to the descriptions.

        :param descriptions: An ObservationDescriptions object with meta data for the observations
        :return: python list with the model values corresponding to the descriptions
        """
        indeces = utils.input_to_py_descriptions(descriptions)
        obs = [None]*len(indeces)
        for (i, index) in enumerate(indeces):
            obs[i] = self.state[int(index)].copy()
        return obs

    def update_state(self, state_array, main_or_ens):
        """
        Update the state vector of the model.

        :param state_array: numpy array used to update the model state.
        :main_or_ens: "main" for updating the main model, "ens" for ensemble members.
        :return:
        """
        if main_or_ens == "ens":
            delta = utils.input_to_np_array(state_array)
            self.state += delta
        elif main_or_ens == "main":
            delta_mean = utils.input_to_np_array(state_array)
            self.state = delta_mean


    def get_state(self):
        """
        Returns the state of the model.

        :return: State vector.
        """
        return self.state

def _lorenz_function_(t, x, params):
    """
    Function which computes the derivative of the current state at time t.

    :param t: time.
    :param x: state.
    :param params: list containing the parameters sigma, rho and beta.

    :return: derivative of the state.
    """
    res = [None]*3
#    beta = 2.6667
#    sigma = 10.0
#    rho = 28.0
    sigma = params[0]
    rho = params[1]
    beta = params[2]
    #   dx = sigma*(y-x)  *dt
    #   dy = (rho*x-y-x*z)*dt
    #   dz = (x*y-beta*z) *dt
    res[0] = sigma*(x[1] - x[0])
    res[1] = rho*x[0] - x[1] - x[0]*x[2]
    res[2] = x[0]*x[1] - beta*x[2]
    return res
