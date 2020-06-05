#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Module containing various classes that are used by the ensemble kalman filter.
When adding your own classes be sure that they include all the methods given by the
default classes JModelFactory, JModelInstance, PyTime and JStochObserver.
Type conversion for method outputs are taken care of, so there is some flexibility in that regard.
The data type of inputs are not flexible unless the method only gets called within this script,
so be sure to carefully read the documentation of the default classes.

Created on Tue Nov 20 15:27:05 2018

@author: hegeman
"""
import warnings
import os
from py4j.java_gateway import JavaGateway
import py_openda.utils.py4j_utils as utils
from py_openda.interfaces.IModelFactory import IModelFactory
from py_openda.interfaces.IStochModelInstance import IStochModelInstance
from py_openda.interfaces.IStochObserver import IStochObserver
from py_openda.interfaces.ITime import ITime


try:
    gateway = JavaGateway()   # connect to the JVM
except:
    warnings.warn("Cannot connect to JVM did you start oda_py4j. Java building blocks cannot be used")



class JModelFactory(IModelFactory):
    """
    Wrapperclass for using a stochModelFactory from Java.
    """

    def __init__(self, config, scriptdir):
        """
        :param config: dictionary used for configuration.
        :param scriptdir: location of the main .oda file.
        """
        model_input_dir = os.path.join(scriptdir, config.get('workingDirectory'))
        model_config_xml = config.get('configFile')
        self.model_factory = None
        model_object = 'gateway.jvm.'+config.get('@className')
        exec('self.model_factory =%s()' % model_object)
        utils.initialize_openda_configurable(self.model_factory, model_input_dir, model_config_xml)

    def get_instance(self, noise_config, main_or_ens):
        """
        Create an instance of the stochastic Model.

        :param noise_config: dictionary as given by EnkfAlgorithm.xml for the noise configuration.
        :param main_or_ens: determines the ouput level of the model.
        :return: the stochastic Model instance.
        """
        if main_or_ens == 'main':
            output_level = gateway.jvm.org.openda.interfaces.IStochModelFactory.OutputLevel.ModelDefault
        elif main_or_ens == 'ens':
            output_level = gateway.jvm.org.openda.interfaces.IStochModelFactory.OutputLevel.Suppress
        model = self.model_factory.getInstance(output_level)
        return JModelInstance(model, noise_config, main_or_ens)

class JStochObserver(IStochObserver):
    """
    Wrapperclass for using a stochObserver from Java.
    """

    def __init__(self, config=None, scriptdir=None, clone=None):
        """
        :param config: dictionary used for configuration.
        :param scriptdir: location of the main .oda file.
        :param clone: if None (default), the class will initialize from configuration, otherwise
        the class will be a copy of clone.
        """
        if clone is None:
            observer_input_dir = os.path.join(scriptdir, config.get('workingDirectory'))
            observer_config_xml = config.get('configFile')
            self.observer = None
            observer_object = 'gateway.jvm.'+config.get('@className')
            exec('self.observer =%s()' % observer_object)
            utils.initialize_openda_configurable(self.observer, observer_input_dir,
                                                 observer_config_xml)
        else:
            self.observer = clone

    def create_selection(self, model_span):
        """
        Create a new observer, containing a selection of the present observer,
        based on the given time span.

        :param model_span: time span with selection.
        :return: stochastic observer containing the required selection.
        """
        time_precision = gateway.jvm.org.openda.utils.performance.OdaGlobSettings.getTimePrecision()
        selection_span = gateway.jvm.org.openda.utils.Time(model_span.get_start(), model_span.get_end())
        selection_span = selection_span.extendInterval(time_precision)
        return JStochObserver(clone=self.observer.createSelection(selection_span))

    def get_times(self):
        """
        Get all different times in increasing order. There is at least one observation for each time.

        :return: some type of vector containing the times
        """
        return self.observer.getTimes()

    def get_count(self):
        """
        Total number of observations.

        :return: the number of observations.
        """
        return self.observer.getCount()

    def get_observation_descriptions(self):
        """
        Get the observation descriptions.

        :return: observation descriptions which are compatible with the used model instance
        """
        return self.observer.getObservationDescriptions()

    def get_sqrt_covariance(self):
        """
        Get the covariance matrix for the stochastic observations.

        :return: the covariance matrix as numpy array.
        """
        #TODO: This part is not very generic!
        j_sqrt = self.observer.getSqrtCovariance().asVectorArray()
        py_sqrt = utils.j_vector_array_to_np_array(j_sqrt)
        return py_sqrt

    def get_realizations(self):
        """
        Get realization values for all observations, for one ensemble member.

        :return: the realizations.
        """
        return self.observer.getRealizations()


class JModelInstance(IStochModelInstance):
    """
    Wrapperclass for using a stochModelInstance from Java.
    """

    def __init__(self, model, noise_config, main_or_ens):
        """
        :param model: model instance as provided by the factory.
        :param noise_config: dictionary as given by EnkfAlgorithm.xml for the noise configuration.
        :param main_or_ens: determines the ouput level of the model.
        """
        self.model = model
        if noise_config is None:
            if main_or_ens == "main":
                noise_config = {'@stochParameter':False, '@stochForcing':False, '@stochInit':False}
            elif main_or_ens == "ens":
                noise_config = {'@stochParameter':False, '@stochForcing':True, '@stochInit':True}
        
        if noise_config.get('@stochInit'):
            init = self.model.getStateUncertainty()
            self.model.axpyOnState(1.0, init.createRealization())

        if noise_config.get('@stochParameter'):
            pars = self.model.getParameterUncertainty().createRealization()
            self.model.setParameters(pars)

        self.model.setAutomaticNoiseGeneration(noise_config.get('@stochForcing'))


    def get_time_horizon(self):
        """
        Get the computational time horizon of the model (begin and end time).

        :return: the time horizon (containing begin and end time).
        """
        return JTime(self.model.getTimeHorizon())

    def get_current_time(self):
        """
        Get the model instance's current simulation time stamp.

        :return: The model's current simulation time stamp.
        """
        return JTime(self.model.getCurrentTime())

    def announce_observed_values(self, descriptions):
        """
        Tells model that it can expect to be asked for model values corresponding to the observations
        described. The model can make arrangement to save these values. The method compute run over a long
        interval at once, not stopping at each time with observations.
        This is meant to increase the performance especially of calibration algorithms.

        :param descriptions: an ObservationDescriptions object with meta data for the observations.
        :return:
        """
        self.model.announceObservedValues(descriptions)

    def compute(self, time):
        """
        Let the stochastic model instance compute to the requested target time stamp.
        This function can not be used to go back in time.

        :param time: Time to compute to.
        :return:
        """
        time = gateway.jvm.org.openda.utils.Time(time.get_start())
        self.model.compute(time)

    def get_observations(self, descriptions):
        """
        Get model values corresponding to the descriptions.

        :param descriptions: An ObservationDescriptions object with meta data for the observations
        :return: python list with the model values corresponding to the descriptions
        """
        return self.model.getObservationOperator().getObservedValues(descriptions)

    def update_state(self, state_array, main_or_ens):
        """
        Update the state vector of the model.

        :param state_array: numpy array used to update the model state.
        :main_or_ens: "main" for updating the main model, "ens" for ensemble members.
        :return:
        """
        if main_or_ens == "ens":
            delta = utils.input_to_j_vector(state_array)
            self.model.axpyOnState(1.0, delta)
        elif main_or_ens == "main":
            delta_mean = utils.input_to_j_vector(state_array)
            x_main = self.model.getState()
            delta_mean.axpy(-1.0, x_main)
            self.model.axpyOnState(1.0, delta_mean)


    def get_state(self):
        """
        Returns the state of the model.

        :return: State vector.
        """
        return self.model.getState()

class JTime(ITime):
    """
    Wrapperclass for using a Time object from Java.
    """
    def __init__(self, start, end=None):
        """
        Note: if you want to save a Java object as a JTime, only include a start time.

        :param start: start of the time period.
        :param end: end of the time period.
        """
        if end is None:
            self.time = gateway.jvm.org.openda.utils.Time(start)
        else:
            self.time = gateway.jvm.org.openda.utils.Time(start, end)

    def get_start(self):
        """
        Returns the start of the time period.

        :return: start time.
        """
        return self.time.getBeginMJD()

    def get_end(self):
        """
        Returns the start of the time period.

        :return: start time.
        """
        return self.time.getEndMJD()

    def get_is_span(self):
        """
        Check whether self is a time span or a time stamp.

        :return: True if self is a time span.
        """
        return self.time.isSpan()

    def after(self, other_time):
        """
        Check whether self starts after other_time ends.

        :param other_time: time object to be compared
        :return: True if self starts after other_time ends.
        """
        return self.time.after(gateway.jvm.org.openda.utils.Time(other_time.get_start(),
                                                                 other_time.get_end()))

    def get_mjd(self):
        """
        Returns a time stamp in the middle of the time period.

        :return: center of time period.
        """
        return self.time.getMJD()

class PyTime(ITime):
    """
    Class used for keeping track of periods of time.
    """
    def __init__(self, start, step=None, end=None):
        """
        :param start: start of the time period.
        :param end: end of the time period.
        """
        self.start = start
        if end is None:
            self.end = start

        else:
            self.end = end
        if end is None or start == end:
            self.is_span = False
        else:
            self.is_span = True

        if step:
            self.step = step

    def get_start(self):
        """
        Returns the start of the time period.

        :return: start time.
        """
        return self.start

    def get_end(self):
        """
        Returns the end of the time period.

        :return: end time.
        """
        return self.end

    def get_is_span(self):
        """
        Check whether self is a time span or a time stamp.

        :return: True if self is a time span.
        """
        return self.is_span

    def after(self, other_time):
        """
        Check whether self starts after other_time ends.

        :param other_time: time object to be compared
        :return: True if self starts after other_time ends.
        """
        return self.start > other_time.get_end()

    def get_step_mjd(self):
        """
        Get the time step interval in days (as Modified Julian Day).
        :return The time step interval. Throw an exception if is is not available.
        """
        if self.is_span:
            return self.step

    def get_mjd(self):
        """
        Returns a time stamp in the middle of the time period.

        :return: center of time period.
        """
        if not self.is_span:
            return self.start
        else:
            return 0.5*(self.start+self.end)
