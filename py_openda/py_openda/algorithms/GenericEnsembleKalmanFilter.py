#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Module containing the class which houses the models and observer that will be used for
ensemble kalman filtering.
Created on Tue Nov 20 15:33:06 2018

@author: hegeman
"""

import numpy as np
from py_openda.costFunctions.JObjects import PyTime as Time

import py_openda.utils.py4j_utils as utils



class GenericEnsembleKalmanFilter:
    """
    Class which holds the models and observer that will interact with the Kalman Filter.
    """

    def __init__(self, ensemble_size, alg_config, model_factory, stoch_observer):
        """
        :param ensemble_size: number of ensemble members you wish to use.
        :param alg_config: dictionary decoded from an xml configuration file which adheres to
            http://schemas.openda.org/algorithm/enkf.xsd
        """
        self.model_factory = model_factory

        self.observer = stoch_observer

        self.main_model = self.model_factory.get_instance(alg_config.get('mainModel'),
                                                          main_or_ens="main")

        now = self.main_model.get_current_time()
        self.current_time = Time(now.get_start(), now.get_end())

        model_span = self.main_model.get_time_horizon()
        model_span = Time(model_span.get_start(), model_span.get_end())
        selection = self.observer.create_selection(model_span)
        self.analysis_times = utils.input_to_time_list(selection.get_times(), Time)


        #TODO: This part might not be correct!
        self.this_step = -1
        if alg_config.get('analysisTimes') is not None:
            if alg_config.get('analysisTimes').get('@skipAtInitialTime'):
                self.this_step = 0

        self.selection = None
        self.ensemble_size = ensemble_size
        self.ensemble = [None]*self.ensemble_size
        for i in range(self.ensemble_size):
            self.ensemble[i] = self.model_factory.get_instance(alg_config.get('ensembleModel'),
                                                               main_or_ens="ens")

    def get_n_observations(self):
        """
        Total number of observations.

        :return: the number of observations.
        """
        return self.observer.create_selection(self.analysis_times[self.this_step+1]).get_count()

    def get_n_times(self):
        """
        Returns number of times in analysis_times

        :return: number of available time steps.
        """
        return len(self.analysis_times)

    def get_timeline(self):
        """
        Returns the list of time stamps where measurements took place.

        :return: Python list with all analysis times in MJD.
        """
        return [time.get_mjd() for time in self.analysis_times]

    def get_covariance_matrix(self):
        """
        Create a covariance matrix from the stoch observer.

        :return: numpy array of the covariance matrix.
        """
        return self.selection.get_sqrt_covariance()

    def get_realizations(self):
        """
        Get realizations from the stoch observer.

        :param selection: stoch observer.
        :return: numpy array with realizations.
        """
        return utils.input_to_np_array(self.selection.get_realizations())

    def get_ensemble_vectors_forecast(self):
        """
        Represents the values of the state of each ensemble at the observed locations.

        :return ensemble_predicted_observations: numpy array containing the ensemble states
        at the observed locations, with the mean removed.
        :return mean_observations: ensemble mean at those same locations.
        """

        descriptions = self.selection.get_observation_descriptions()
        ensemble_predicted_observations = [None]*self.ensemble_size
        for i in range(self.ensemble_size):
            ensemble_predicted_observations[i] = utils.input_to_py_list(self.ensemble[i].get_observations(descriptions))
        ensemble_predicted_observations = np.array(ensemble_predicted_observations).transpose()
        mean_observations = np.array([np.mean(ensemble_predicted_observations, axis=1)]).transpose()
        ensemble_predicted_observations -= mean_observations
        return(ensemble_predicted_observations, mean_observations)

    def get_ensemble_vectors_state(self):
        """
        Returns the entire state vector for each ensemble member, as well as the ensemble mean.

        :return ensemble_states: numpy array containing the state vectors, with the mean removed.
        :return ensemble_mean: ensemble mean at those same locations.
        """
        ensemble_states = [None]*self.ensemble_size
        for i in range(self.ensemble_size):
            ensemble_states[i] = utils.input_to_py_list(self.ensemble[i].get_state())
        ensemble_states = np.array(ensemble_states).transpose()
        ensemble_mean = np.array([np.mean(ensemble_states, axis=1)]).transpose()
        ensemble_states -= ensemble_mean
        return(ensemble_states, ensemble_mean)

    def forecast(self, time):
        """
        Runs the model and all ensemble members until the given time.

        :param time: time which indicates the current time.
        :return:
        """
        self.main_model.compute(time)
        for i in range(self.ensemble_size):
            if self.selection.get_count() > 0:
                self.ensemble[i].announce_observed_values(self.selection.get_observation_descriptions())
            self.ensemble[i].compute(time)

    def next_predictions(self):
        """
        Advances the time at the start of the next step in the algorithm
        and advances the model and ensemble members along as well.

        :return:
        """
        self.this_step += 1
        time = self.analysis_times[self.this_step]

        self.selection = self.observer.create_selection(time)
        if time.after(self.current_time):
            if time.get_is_span():
                time = Time(time.get_mjd(), time.get_mjd())
            self.forecast(time)
        self.current_time = time

    def update_state(self, i, state_array):
        """
        Updates the state vector for one of the ensemble members.

        :param i: index of the relevant ensemble member.
        :param state_array: numpy array containing the updated state vector.
        :return:
        """
        self.ensemble[i].update_state(state_array, "ens")

    def update_model(self, mean):
        """
        Updates the state vector for the main model based on the ensemble members.

        :param mean: numpy array containing the new ensemble mean.
        :return:
        """
        self.main_model.update_state(mean, "main")


    def get_results(self):
        """
        Returns the main model state at the observed locations.
        Corresponds to the x_f_central results in Java.

        :return: Python list containing the results.
        """

        descriptions = self.selection.get_observation_descriptions()
        pred = self.main_model.get_observations(descriptions)
        return utils.input_to_py_list(pred)
