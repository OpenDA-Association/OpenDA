#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Stoch observer for reading from .csv files, usable by the ensemble kalman filter algorithm.
Created on Mon Nov 26 10:49:24 2018

@author: hegeman
"""

import os
import numpy as np
from scipy.stats import norm
import pandas as pd

class CsvStochObserver:
    """
    A stochastic observer which uses pandas to read observations from a csv file.
    """
    def __init__(self, config=None, scriptdir=None, clone=None):
        """
        :param config: dictionary used for configuration.
        :param scriptdir: location of the main .oda file.
        :param clone: if None (default), the class will initialize from configuration,
        otherwise the class will be a copy of clone.
        """
        if clone is None:
            observer_file = os.path.join(scriptdir, config.get('workingDirectory'), config.get('configFile'))
            self.data = pd.read_csv(observer_file).set_index('time')
        else:
            self.data = clone

    def create_selection(self, model_span):
        """
        Create a new observer containing a selection of the present observer
        based on the given time span.

        :param model_span: time span with selection.
        :return: stochastic observer containing the required selection.
        """
        time_precision = 1.1574074074074073E-5
        selection = self.data.loc[model_span.get_start()-time_precision : model_span.get_end()+time_precision]
        return CsvStochObserver(clone=selection)

    def get_times(self):
        """
        Get all different times in increasing order. There is at least one observation for each time.

        :return: some type of vector containing the times
        """
        return self.data.index.values

    def get_count(self):
        """
        Total number of observations.

        :return: the number of observations.
        """
        return self.data.count()[0]

    def get_observation_descriptions(self):
        """
        Get the observation descriptions.

        :return: observation descriptions which are compatible with the used model instance
        """
        return self.data

    def get_sqrt_covariance(self):
        """
        Get the covariance matrix for the stochastic observations.

        :return: the covariance matrix as numpy array.
        """
        return np.diag(self.data['std'])

    def get_realizations(self):
        """
        Get realization values for all observations, for one ensemble member.

        :return: the realizations.
        """
        return [norm(loc=mean, scale=std).rvs() for mean,
                std in zip(self.data['value'], self.data['std'])]
