#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Main function for using the dud algorithm to find the parameters corresponding
to the optimal value of a function. If the function you want to use does not reqire
py4j, you can comment out the line 'py_p0 = utils.input_to_py_list(py_p0)' if it causes trouble.

Created on Wed Oct 10 14:40:17 2018

@author: hegeman
"""

#FIXME: netjes tussen Python en Java objecten wisselen.

#from py_openda.costFunctions.PyKwadratic import PyKwadratic as Kwadratic
from py_openda.costFunctions.Kwadratic import Kwadratic
from py_openda.algorithms import Dud

import py_openda.utils.py4j_utils as utils

def main(cost):
    """
    Runs the main dud algorithm for finding the optimal parameters for a certain costfunction.
    :param cost: a model object which has the method get_parameters() which returns the parameters,
    get_obs() for getting the observations with standard deviations, and object_function(p) which returns
    the value for parameters p.
    :return: tuple containing the minimal cost followed by the list of corresponding parameters
    """
    py_p0 = cost.get_parameters()
    py_p0 = utils.input_to_py_list(py_p0)
    (obs_mean, obs_std) = cost.get_obs()
    results = Dud.dud(cost.object_function,  py_p0, obs_mean, obs_std)
    return results
if __name__ == "__main__":
    cost = Kwadratic()
    results = main(cost)
    print("Optimal value = "+str(results[0]))
    print("with the parameters "+str(results[1]))
    print("Done")