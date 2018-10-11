#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct 10 14:40:17 2018

@author: hegeman
"""

from py_openda.costFunctions.SimulationKwadraticCostFunction import *
from py_openda.algorithms import Dud

def main():
    (cost_function, model_params, model_ini, observer) = setup()
    j_p0 = model_params.clone().getValues()
    py_p0 = j_array_to_py_list(j_p0)
    (obs_mean, obs_std) = get_obs()
    results = Dud.dud(object_function,  py_p0, obs_mean, obs_std,)
#    results = dud_old(f, [2,-3], [1,2,2])
#    results = dud_old(f2, [2,3], [1, 0.5, 0.5])
#    results = dud_old(g, [2,-3], [0.909, -0.416, 0.841])
    #print("Optimal value ="+str(results.x))
    print("Optimal value = "+str(results[0]))
    print("with the parameters "+str(results[1]))
    print("Done")
if __name__ == "__main__":
    main()