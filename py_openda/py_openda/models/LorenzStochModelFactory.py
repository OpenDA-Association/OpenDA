#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Factory for making Lorenz model instances, usable by the ensemble kalman filter algorithm.
Created on Thu Nov 22 11:29:15 2018

@author: hegeman
"""

import os
import xmlschema
from py_openda.models.LorenzStochModelInstance import LorenzStochModelInstance

class LorenzStochModelFactory:
    """
    Factory for making Lorenz model instances
    """
    def __init__(self, config, scriptdir):
        """
        :param config: dictionary used for configuration.
        :param scriptdir: location of the main .oda file.
        """
        xml_path = os.path.join(scriptdir, config.get('workingDirectory'), config.get('configFile'))
        schema = xmlschema.XMLSchema('http://schemas.openda.org/toymodels/LorenzConfig.xsd')
        self.config = schema.decode(xml_path)

        names = self.config.get('parameters').get('@names').split(',')
        param_values = [float(val) for val in self.config.get('parameters').get('$').strip('[]').split(',')]
        param_uncertainty = [float(val) for val in self.config.get('parameterUncertainty')
                             .get('$').strip('[]').split(',')]
        param = dict(zip(names, param_values))
        param_uncertainty = dict(zip(names, param_uncertainty))

        state = [float(val) for val in self.config.get('initialState').strip('[]').split(',')]
        state_uncertainty = [float(val) for val in self.config.get('initialStateUncertainty')
                             .strip('[]').split(',')]

        sys_noise = self.config.get('systemNoise').strip('{[]}').split('],[')
        sys_mean = [float(val) for val in sys_noise[0].split(',')]
        sys_std = [float(val) for val in sys_noise[1].split(',')]

        span = [float(val) for val in self.config.get('simulationTimespan').strip('[]').split(',')]
        self.model_attributes = (param, param_uncertainty, state, state_uncertainty, sys_mean, sys_std, span)

    def get_instance(self, noise_config, main_or_ens):
        """
        Create an instance of the stochastic Model.

        :param noise_config: dictionary as given by EnkfAlgorithm.xml for the noise configuration.
        :param main_or_ens: determines the ouput level of the model.
        :return: the stochastic Model instance.
        """
        return LorenzStochModelInstance(self.model_attributes, noise_config, main_or_ens)
