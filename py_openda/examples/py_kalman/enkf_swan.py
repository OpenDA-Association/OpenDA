#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Algorithm for data assimilation using an ensemble kalman filter. Uses an
observer and a model factory from Java but the algorithm resides in Python.
To run the algorithm remember to first start the server : oda_py4j.sh

Created on Thu Nov  8 15:57:29 2018

@author: hegeman
"""


import os
from time import time as tm
import numpy as np
import matplotlib.pyplot as plt
import xmlschema
from py_openda.costFunctions.GenericEnsembleKalmanFilter import GenericEnsembleKalmanFilter
from py_openda.algorithms.ensemble_kalman import kalman_algorithm, no_filter

#TODO: Some movable parts in Java might still be hard coded in Python
#TODO: Quite a few xml files might not follow the schema as well as xmlschema wants them to

#input_string = '/v3/Stage/Rick/openda/openda_public/course/exercise_lorenz_3var_part1/simulation_ensemble.oda'
input_string = '/v3/Stage/Rick/openda/openda_public/examples/model_swan/kalman_twin_windbound/enkf_wind_bound.oda'
#input_string = '/v3/Stage/Rick/openda/openda_public/course/exercise_double_pendulum_part2/enkf.oda'
#input_string = '/v3/Stage/Rick/openda/openda_public/core/tests/simple_oscillator/Enkf.oda'

def main(input_string, observation_location=0):
    """
    Main function that runs an ensemble kalman filter as described by the .oda file in input_string.
    yields the pred_f_central for a filtered and an unfiltered simulation and plots the
    results at one observation location.

    :param input_string: absolute file path of the .oda file.
    :param observation_location: index of the observation location you want to plot (default 0).

    :return results: numpy array containing the results for the filtered experiment.
    :return no_results: numpy array containing the results for the unfiltered experiment.
    """
    #TODO: Create a py4j JavaGateway and make it a global variable using __builtin__
    #This way you can use a callback server for multiple objects.

    os.chdir(input_string.rsplit('/', 1)[0])

    scriptdir = os.getcwd()

    main_schema = xmlschema.XMLSchema('http://schemas.openda.org/openDaApplication.xsd')
    alg_schema = xmlschema.XMLSchema('http://schemas.openda.org/algorithm/enkf.xsd')
#    alg_schema = xmlschema.XMLSchema('http://schemas.openda.org/algorithm/sequentialEnsembleAlgorithm.xsd')
    main_config = main_schema.decode(input_string.rsplit('/', 1)[1])



    alg_config = alg_schema.decode('%s/%s' % (main_config.get('algorithm').get('workingDirectory'),
                                              main_config.get('algorithm').get('configString')))

# ensembleModel@stochParameter=false
# ensembleModel@stochForcing=true
# ensembleModel@stochInit=true
    

#    n_ensemble = 3
    n_ensemble = alg_config.get('ensembleSize')


    main_class = GenericEnsembleKalmanFilter(n_ensemble, alg_config, main_config, scriptdir)
    n_obs = main_class.get_n_observations()

#    n_steps = main_class.get_n_times()-3
    n_steps = 3
    t = main_class.get_timeline()[:n_steps+1]
    t = [(time - t[0])*24 for time in t]
    results = np.zeros((n_steps+1, n_obs))
    for j in range(n_steps):
        print(j)
        start = tm()
        results[j, :] = kalman_algorithm(main_class)
        end = tm()
        print(end-start)

    results[-1, :] = no_filter(main_class)
    plt.plot(t, results[:, observation_location])

    compare_class = GenericEnsembleKalmanFilter(1, alg_config, main_config, scriptdir)
    no_results = np.zeros((n_steps+1, n_obs))
    for j in range(n_steps+1):
        print(j)
        start = tm()
        no_results[j, :] = no_filter(compare_class)
        end = tm()
        print(end-start)

    plt.plot(t, no_results[:, observation_location])
    plt.legend(("EnKF", "no_filter"))
    plt.ylabel("x_f_central")
    plt.xlabel("t in hours")
    return(results, no_results)

if __name__ == "__main__":
    strt = tm()
    (results, no_results) = main(input_string)
    print(tm()-strt)