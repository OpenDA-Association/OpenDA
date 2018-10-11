#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct 10 14:39:10 2018

@author: hegeman
"""

import os
from py4j.java_gateway import JavaGateway

gateway = JavaGateway()   # connect to the JVM

#os.chdir('/v3/Stage/Rick/openda/openda_public/py_openda/examples/py_swan_calibration')
scriptdir = os.getcwd()
#scriptdir = '/v3/Stage/Rick/openda/openda_public/py_openda/examples/py_swan_calibration'








def py_list_to_j_array(py_x):
    """
    Create a java double array from python list

    :param py_x: python list of doubles
    :return: java array of doubles copy of py_x
    """
    n = len(py_x)
    double_class = gateway.jvm.double
    j_x = gateway.new_array(double_class, n)

    for i in range(n):
        j_x[i] = py_x[i]

    return j_x

def j_array_to_py_list(j_x):
    """
    Create a python list from a java array
    :param j_x: java array
    :return: python list with values of j_x
    """

    n = len(j_x)
    py_x = [None]*n
    for i in range(n):
        py_x[i] = j_x[i]

    return py_x


def initialize_openda_configurable(openda_configurable, input_dir, config_xml):
    """
    Initialise an OpenDA configurable (e.g. ModelFactory, StochObserver)

    :param openda_configurable: OpenDA object implementing IConfigurable
    :param input_dir: Working directory/input directory
    :param config_xml: Configuration file
    :return:
    """

    # Translate the input strings to java objects (File and String array)
    j_input_dir = gateway.jvm.java.io.File(input_dir)
    string_class = gateway.jvm.String
    j_arguments = gateway.new_array(string_class, 1)
    j_arguments[0] = config_xml

    # Initialize ..
    openda_configurable.initialize(j_input_dir, j_arguments)

def setup():
    """
    Setup the OpenDA model factory, stoch observer and object function

    :return:
    """
    global cost_function
    global model_params
    global model_ini
    global observer

    #Initialize the model factory
    #model_input_dir = "/Users/nils/Develop/py4j/swanModel/config"
    model_input_dir = os.path.join(scriptdir, 'swanModel', 'config')
    model_config_xml = "swanStochModelConfig.xml"


    model_factory = gateway.jvm.org.openda.model_swan.SwanCalibStochModelFactory()
    initialize_openda_configurable(model_factory, model_input_dir, model_config_xml)

    #Initialize stoch observer
    #observer_input_dir = "/Users/nils/Develop/py4j/stochObserver"
    observer_input_dir = model_input_dir = os.path.join(scriptdir, 'stochObserver')
    observer_config_xml = "swanStochObsConfig.xml"

    observer = gateway.jvm.org.openda.observers.IoObjectStochObserver()
    initialize_openda_configurable(observer, observer_input_dir, observer_config_xml)

    #Initialize cost function org.openda.algorithms
    cost_function = gateway.jvm.org.openda.algorithms.SimulationKwadraticCostFunction(model_factory, observer)

    #Get initial parameter
    outputLevel = gateway.jvm.org.openda.interfaces.IStochModelFactory.OutputLevel.Debug
    model_ini = model_factory.getInstance(outputLevel)
    p = model_ini.getParameters()
    model_params = p
    
    return(cost_function, model_params, model_ini, observer)

def get_obs():
    selectionTimes = model_ini.getTimeHorizon()
    observerSelection = observer.createSelection(selectionTimes)
    obs_mean = observerSelection.getExpectations()
    obs_mean = j_array_to_py_list(obs_mean.getValues())
    obs_std = observerSelection.getStandardDeviations()
    obs_std = j_array_to_py_list(obs_std.getValues())
    
    return (obs_mean, obs_std)

def object_function(p):
    """
    Compute the object function for parameters p
    :param p: parameters
    :return: value object function
    """

    # Create an OpenDA TreeVector with parameter value
    p_new = model_params.clone()
    j_p = py_list_to_j_array(p)
    p_new.setValues(j_p)
    model_new = model_ini
    model_new.setParameters(p_new)
    #Compute object function
    selectionTimes = model_new.getTimeHorizon()
    observerSelection = observer.createSelection(selectionTimes)
    targetTime = model_new.getTimeHorizon().getEndTime()
    model_new.compute(targetTime)

    descr = observerSelection.getObservationDescriptions()
    prd = model_new.getObservationOperator().getObservedValues(descr)
    prd = j_array_to_py_list(prd.getValues())

    val = cost_function.evaluate(p_new, "-")
    print("Cost="+str(val)+" p="+str(p))
    return prd