#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Module of utility py4j functions for converting python lists, java arrays,
numpy arrays and OpenDA vectors, as well as initializing OpenDA configurables.

Created on Tue Nov 20 15:16:14 2018

@author: hegeman
"""

import warnings
import numpy as np
import pandas as pd
from py4j.java_gateway import JavaGateway
from py4j.java_collections import JavaArray

try:
    gateway = JavaGateway()   # connect to the JVM
    IVector_class = gateway.jvm.java.lang.Class.forName("org.openda.interfaces.IVector")
    ITime_class = gateway.jvm.java.lang.Class.forName("org.openda.interfaces.ITime")
    IObservationDescriptions_class = gateway.jvm.java.lang.Class.forName("org.openda.interfaces.IObservationDescriptions")
except:
    warnings.warn("Cannot connect to JVM did you start oda_py4j. Java building blocks cannot be used")


def initialize_openda_configurable(openda_configurable, input_dir, config_xml):
    """
    Initialise an OpenDA configurable (e.g. ModelFactory, StochObserver).

    :param openda_configurable: OpenDA object implementing IConfigurable.
    :param input_dir: Working directory/input directory.
    :param config_xml: Configuration file.
    :return:
    """

    # Translate the input strings to java objects (File and String array)
    j_input_dir = gateway.jvm.java.io.File(input_dir)
    string_class = gateway.jvm.String
    j_arguments = gateway.new_array(string_class, 1)
    j_arguments[0] = config_xml

    # Initialize ..
    openda_configurable.initialize(j_input_dir, j_arguments)

def py_list_to_j_array(py_x):
    """
    Create a java double array from python list.

    :param py_x: python list of doubles.
    :return: java array of doubles copy of py_x.
    """
    n_elements = len(py_x)
    double_class = gateway.jvm.double
    j_x = gateway.new_array(double_class, n_elements)
    j_x[:] = py_x[:]

    return j_x

def j_array_to_py_list(j_x):
    """
    Create a python list from a java array.
    :param j_x: java array.
    :return: python list with values of j_x.
    """

    n_elements = len(j_x)
    py_x = [None]*n_elements
    py_x[:] = j_x[:]
    return py_x

def j_vector_array_to_np_array(j_vectors):
    """
    Create a numpy array from a java vector array.
    Note that this function also works for different data types,
    as long as the data type has the method getValues().

    :param j_vectors: java vector array.
    :return: numpy array with values of j_vectors.
    """


    vals = [j_array_to_py_list(vec.getValues()) for vec in j_vectors]
    py_array = np.c_[[np.array(val) for val in vals]].transpose()
    return py_array

def np_array_to_j_array(np_array):
    """
    Create a java double array from a 1D numpy array.

    :param np_array: 1D numpy array of doubles.
    :return: java array of doubles.
    """
    np_array = np_array.squeeze().tolist()
    j_array = py_list_to_j_array(np_array)
    return j_array

def np_array_to_j_vector(np_array):
    """
    Create a java Vector from a 1D numpy array.

    :param np_array: 1D numpy array of doubles.
    :return: java Vector.
    """
    j_array = np_array_to_j_array(np_array)
    return gateway.jvm.org.openda.utils.Vector(j_array)

def input_to_j_array(obj):
    """
    Converts a python list, a 1D numpy array, java Array or an OpenDA vector
    to a java array.

    :param obj: object of unknown type.
    :return: obj as java array
    """
    if isinstance(obj, list):
        obj = py_list_to_j_array(obj)
    elif isinstance(obj, np.ndarray):
        obj = np_array_to_j_array(obj)
    elif isinstance(obj, JavaArray):
        None
    elif IVector_class.isInstance(obj):
        obj = obj.getValues()
    return obj

def input_to_py_list(obj):
    """
    Converts a python list, a 1D numpy array, java Array or an OpenDA vector
    to a java array.

    :param obj: object of unknown type.
    :return: obj as java array
    """
    if isinstance(obj, JavaArray):
        obj = j_array_to_py_list(obj)
    elif isinstance(obj, np.ndarray):
        obj = obj.squeeze().tolist()
    elif isinstance(obj, list):
        None
    elif IVector_class.isInstance(obj):
        obj = j_array_to_py_list(obj.getValues())
    return obj

def input_to_np_array(obj):
    """
    Converts a python list, a 1D numpy array, java Array or an OpenDA vector
    to a java array.

    :param obj: object of unknown type.
    :return: obj as java array
    """
    if isinstance(obj, list):
        obj = np.array(obj)
    elif isinstance(obj, JavaArray):
        obj = np.array(j_array_to_py_list(obj))
    elif isinstance(obj, np.ndarray):
        obj = obj.squeeze()
    elif IVector_class.isInstance(obj):
        obj = np.array(j_array_to_py_list(obj.getValues()))
    return obj

def input_to_j_vector(obj):
    """
    Converts a python list, a 1D numpy array, java Array or an OpenDA vector
    to a java array.

    :param obj: object of unknown type.
    :return: obj as java array
    """
    vector_class = gateway.jvm.org.openda.utils.Vector
    if isinstance(obj, list):
        obj = vector_class(py_list_to_j_array(obj))
    elif isinstance(obj, JavaArray):
        obj = vector_class(obj)
    elif isinstance(obj, np.ndarray):
        obj = np_array_to_j_vector(obj)
    return obj

def input_to_time_list(obj, t_class):
    """
    Converts a python list, a 1D numpy array, java Array or an OpenDA vector
    containing Itime or t_class objects to a python list of t_class objects.

    :param obj: object of unknown type.
    :param t_class: Time class of the desired output.
    :return: obj as list of times.
    """
    time_list = input_to_py_list(obj)
    n_times = len(time_list)
    result = [None]*n_times
    for (i, time) in enumerate(time_list):
        if ITime_class.isInstance(time):
            result[i] = t_class(time.getBeginMJD(), time.getEndMJD())
        else:
            result[i] = t_class(time)
    return result

def input_to_py_descriptions(obj):
    """
    Converts a pandas DataFrame or a java observer description to a python list
    of indices.

    :param obj: object of unknown type.
    :return: python list of indices.
    """
    if isinstance(obj, pd.DataFrame):
        indeces = obj['index'].tolist()
    elif IObservationDescriptions_class.isInstance(obj):
        indeces = obj.getValueProperties("index")
        indeces = input_to_py_list(indeces)
    return indeces
