#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Utilities to parse OpenDA xml configuration files for easy use in python

@author: Nils van Velzen
"""


import os
import xmlschema


_main_schema = xmlschema.XMLSchema('http://schemas.openda.org/openDaApplication.xsd')
_alg_schema = xmlschema.XMLSchema('http://schemas.openda.org/algorithm/enkf.xsd')



def parse_main(input_string):
    """
    Parse a main oda-file

    :return dictionaty with parsed content of the input xml 
    """
    hier = os.getcwd()
    os.chdir(input_string.rsplit('/', 1)[0])
    scriptdir = os.getcwd()
    main_config = _main_schema.decode(input_string.rsplit('/', 1)[1])

    alg_config = _alg_schema.decode('%s/%s' % (main_config.get('algorithm').get('workingDirectory'),
                                              main_config.get('algorithm').get('configString')))

    os.chdir(hier)
    return main_config, alg_config, scriptdir

# ensembleModel@stochParameter=false
# ensembleModel@stochForcing=true
# ensembleModel@stochInit=true
    

