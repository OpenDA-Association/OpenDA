#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Algorithm for data assimilation using an ensemble kalman filter. Uses an
observer and a model factory and algorithm from Python
There is no need to start the server oda_py4j since we are not using any of the java buildin blocks

Created on Thu Feb 11 20202

@author: Nils van Velzen
"""


import os
from time import time as tm
import numpy as np
import matplotlib.pyplot as plt

#Algorithms
from py_openda.algorithms.GenericEnsembleKalmanFilter import GenericEnsembleKalmanFilter
from py_openda.algorithms.ensemble_kalman import kalman_algorithm, no_filter

#Select model factory
from py_openda.costFunctions.JObjects import JModelFactory as ModelFactory
from py_openda.models.LorenzStochModelFactory import LorenzStochModelFactory as PyModelFactory

#Select stochastic observer
from py_openda.costFunctions.JObjects import JStochObserver as StochObserver

#Parser for main configuration file
from py_openda.utils.parse_xml import parse_main

#Our main configuration file on OpenDA xml format
input_string = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'enkf.oda')


def main(use_java_model, input_string, observation_location=0):
    """
    Main function that runs an ensemble kalman filter as described by the .oda file in input_string.
    yields the pred_f_central for a filtered and an unfiltered simulation and plots the
    results at one observation location.

    :param use_java_model: use java or python implementation model of Lorenz model
    :param input_string: absolute file path of the .oda file.
    :param observation_location: index of the observation location you want to plot (default 0).

    :return results: numpy array containing the results for the filtered experiment.
    :return no_results: numpy array containing the results for the unfiltered experiment.
    """

    #Parse configuration files
    main_config, alg_config, config_dir = parse_main(input_string)

    #Setup model and observations
    if (use_java_model):
        #Use java implementation of Lorenz model
        model_factory = ModelFactory(main_config.get('stochModelFactory'), config_dir)
    else:
        #Use Python implementation of Lorenz model
        model_factory = PyModelFactory(main_config.get('stochModelFactory'), config_dir)

    #Setup stoch observer.
    stoch_observer = StochObserver(config=main_config.get('stochObserver'), scriptdir=config_dir)

    #Setup algorithm
    n_ensemble = alg_config.get('ensembleSize')
    main_class = GenericEnsembleKalmanFilter(n_ensemble, alg_config, model_factory, stoch_observer)
    n_obs = main_class.get_n_observations()

    #Timestepping of EnKF and extract intermediate results
    n_steps = main_class.get_n_times()-1
    results = np.zeros((n_steps+1, n_obs))
    for j in range(n_steps):
        print("Step "+str(j))
        results[j, :] = kalman_algorithm(main_class)

    results[-1, :] = no_filter(main_class)
    # fiddle time for ploting
    t = main_class.get_timeline()[:n_steps+1]
    t = [(time - t[0])*24 for time in t]

    plt.plot(t, results[:, observation_location])

    #An other run without assimilation
    compare_class = GenericEnsembleKalmanFilter(1, alg_config, model_factory, stoch_observer)
    no_results = np.zeros((n_steps+1, n_obs))
    for j in range(n_steps+1):
        no_results[j, :] = no_filter(compare_class)

    #Some plotting
    print(results)
    plt.plot(t, no_results[:, observation_location])
    plt.legend(("EnKF", "no_filter"))
    plt.ylabel("x_f_central")
    plt.xlabel("t in hours")
    plt.show()
    return(results, no_results)

if __name__ == "__main__":
    strt = tm()
    (results, no_results) = main(False, input_string)
    print(tm()-strt)
