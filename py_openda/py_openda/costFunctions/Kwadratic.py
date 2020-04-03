#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Class for letting a Python script use the Java object SimulationKwadraticCostFunction
which is included in OpenDA.

Created on Wed Oct 10 14:39:10 2018

@author: hegeman
"""
import warnings
import os
from py4j.java_gateway import JavaGateway

import py_openda.utils.py4j_utils as utils

try:
    gateway = JavaGateway()   # connect to the JVM
except:
    warnings.warn("Cannot connect to JVM did you start oda_py4j. Java building blocks cannot be used")



#os.chdir('/v3/Stage/Rick/openda/openda_public/py_openda/examples/py_swan_calibration')
scriptdir = os.getcwd()
#scriptdir = '/v3/Stage/Rick/openda/openda_public/py_openda/examples/py_swan_calibration'








class Kwadratic:
    """
    Class for keeping track of the model and the stochastic observer.
    """
    def __init__(self):
        """
        """
        #Initialize the model factory
        #model_input_dir = "/Users/nils/Develop/py4j/swanModel/config"
        model_input_dir = os.path.join(scriptdir, 'swanModel', 'config')
        model_config_xml = "swanStochModelConfig.xml"


        model_factory = gateway.jvm.org.openda.model_swan.SwanCalibStochModelFactory()
        utils.initialize_openda_configurable(model_factory, model_input_dir, model_config_xml)

        #Initialize stoch observer
        #observer_input_dir = "/Users/nils/Develop/py4j/stochObserver"
        observer_input_dir = model_input_dir = os.path.join(scriptdir, 'stochObserver')
        observer_config_xml = "swanStochObsConfig.xml"

        observer = gateway.jvm.org.openda.observers.IoObjectStochObserver()
        utils.initialize_openda_configurable(observer, observer_input_dir, observer_config_xml)


        #Initialize cost function org.openda.algorithms
        self.cost_function = gateway.jvm.org.openda.algorithms.SimulationKwadraticCostFunction(model_factory, observer)

        #Get initial parameter
        outputLevel = gateway.jvm.org.openda.interfaces.IStochModelFactory.OutputLevel.Debug
        self.model_instance = model_factory.getInstance(outputLevel)
        selectionTimes = self.model_instance.getTimeHorizon()
        self.selection = observer.createSelection(selectionTimes)
        self.target_time = self.model_instance.getTimeHorizon().getEndTime()

    def get_parameters(self):
        """
        Extracts the parameters from the main model instance.

        :return: the initial parameters.
        """


        p = self.model_instance.getParameters()
        return p

    def get_obs(self):
        """
        Find the observations and corresponding uncertainties at the relevant locations.

        :return: a tuple containing a list of the mean of all observations at each locations,
        followed by their respective standard deviations.
        """

        #TODO: announceObservedValues is missing here!
        obs_mean = self.selection.getExpectations()
        obs_mean = utils.input_to_py_list(obs_mean.getValues())
        obs_std = self.selection.getStandardDeviations()
        obs_std = utils.input_to_py_list(obs_std.getValues())

        return (obs_mean, obs_std)

    def object_function(self, p):
        """
        Compute the predictions of the object function for parameters p.
        :param p: parameters.
        :return: list of predictions.
        """

        # Create an OpenDA TreeVector with parameter value
        p_new = utils.input_to_j_vector(p)
        model_new = self.model_instance
        model_new.setParameters(p_new)
        model_new.compute(self.target_time)

        descr = self.selection.getObservationDescriptions()
        prd = model_new.getObservationOperator().getObservedValues(descr)
        prd = utils.input_to_py_list(prd.getValues())

        val = self.cost_function.evaluate(p_new, "-")
        print("Cost="+str(val)+" p="+str(p))
        return prd
    