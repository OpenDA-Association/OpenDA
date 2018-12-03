#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Module for solving a least squares fitting problem using the Dud (doesn't use derivatives) algorithm,
first described by Ralston and Jennrich in 1978.
The main function of the module is dud(...) which uses the other functions in the module to find an answer.
Created on Wed Oct 10 14:35:15 2018

@author: hegeman
"""

import numpy as np
from scipy.linalg import lu_factor, lu_solve

def check_A(A):
    """
    Function to check if matrix A is singular. Exists to prevent errors when inverting the matrix.

    :param A: matrix.
    :return: boolean that states whether or not A is singular.
    """
    bad = False
    if np.linalg.cond(A) > 1e10:
        print("WARNING: gradient is approximately zero. Iteration is stopped.")
        bad = True
    return bad

def initialize_dud(func, p_old, obs, std, start_dist=1.1):
    """
    Function used to find the initial search directions.

    :param func: the function for which we aim to find the optimal parameters.
    :param p_old: first guess of the parameters.
    :param obs: list of observartions we aim to reproduce.
    :param std: list of corresponding standard deviations.
    :keyword argument start_dist: Factor by which p_old is multiplied to find the search directions
    (default 1.1).
    :return: tuple with the parameters, function evaluations and total costs.
    """
    p_number = len(p_old)
    plist = p_old.copy()
    plist += func(plist)
    plist.append(sum(0.5*((y-x)/z)**2 for y, x, z in zip(obs, plist[p_number:], std)))
    params = np.transpose(np.array([plist]))
    for i in range(p_number):
        plist = p_old.copy()
        plist[i] = plist[i]*start_dist
        plist += func(plist)
        plist.append(sum(0.5*((y-x)/z)**2 for y, x, z in zip(obs, plist[p_number:], std)))
        params = np.c_[params, np.transpose(np.array([plist]))]
    params = params[:, np.argsort(-params[-1, :])]
    parameters = params[:p_number, :]
    func_evals = params[p_number:-1, :]
    total_cost = params[-1, :]
    return (parameters, func_evals, total_cost)

def find_next_params(p_number, parameters, func_evals, obs, std, max_step=10):
    """
    Function used to find the next search direction.

    :param func: the function for which we aim to find the optimal parameters.
    :param parameters: array with the parameters.
    :param func_evals: array with function predicions at the observation locations.
    :param obs: list of observartions we aim to reproduce.
    :param std: list of corresponding standard deviations.
    :keyword argument max_step: upper limit for how much the parameters change (default 10).
    :return: tuple which contains a boolean that is true if finding new parameters was impossible
    and a list containing the next search direction.
    """
    p_new = [None]*p_number
    Delta_P = np.array([parameters[:, :-1]
                        - np.transpose(np.array([parameters[:, -1]]))])
    Delta_F = np.transpose(np.c_[np.divide([(func_evals[:, x] -
                                             func_evals[:, -1]) for x in range(p_number)], std)])
    residue = np.array(obs)-func_evals[:, -1]
    A = np.transpose(Delta_F).dot(Delta_F)
    if check_A(A):
        return (True, p_new)
    alpha = lu_solve(lu_factor(A), np.transpose(Delta_F).dot(np.divide(residue, std)))
    if np.max(abs(alpha)) > max_step:
        alpha *= max_step/np.max(abs(alpha))
    p_new = parameters[:, -1] + Delta_P.dot(np.transpose(alpha))
    return (False, p_new)

def line_search(func, parameters, func_evals, total_cost, obs, std, p_new):
    """
    Line search that looks along the next search direction for parameters that lower the total cost.

    :param func: the function for which we aim to find the optimal parameters.
    :param parameters: array with the parameters.
    :param func_evals: array with function predicions at the observation locations.
    :param total_cost: the total cost of the different parameter sets.
    :param obs: list of observartions we aim to reproduce.
    :param std: list of corresponding standard deviations.
    :param p_new: parameters that suggest the search direction.
    :return: tuple with the next parameters, function evaluations and total costs.
    """
    next_parameters = p_new
    next_func_evals = func(next_parameters)
    next_total_cost = sum(0.5*((y-x)/z)**2 for y, x, z in zip(obs, next_func_evals, std))
    d = 1
    while next_total_cost > total_cost[-1]:
        d *= 0.5
        next_parameters = d*p_new+(1-d)*parameters[:, -1]
        next_func_evals = func(next_parameters)
        next_total_cost = sum(0.5*((y-x)/z)**2 for y, x, z in zip(obs, next_func_evals, std))
        if abs(d) < 0.0625:
            if d < 0:
                break
            d *= -1
    return (next_parameters, next_func_evals, next_total_cost)

def dud(func, p_old, obs, std, xtol=1e-3, start_dist=1.1):
    """
    Main function which minimizes a least squares problem without using derivatives.

    :param func: the function for which we aim to find the optimal parameters.
    :param p_old: first guess of the parameters.
    :param obs: list of observartions we aim to reproduce.
    :param std: list of corresponding standard deviations.
    :keyword argument xtol: desired accuracy of the result (default 1e-3).
    :keyword argument start_dist: Factor by which p_old is multiplied to find
    the search directions (default 1.1).
    :return: tuple containing the minimal cost followed by the list of corresponding parameters.
    """
    finish = 0
    max_step = 10
    p_number = len(p_old)
    (parameters, func_evals, total_cost) = initialize_dud(func, p_old, obs, std, start_dist)
    while True:
        (stop, p_new) = find_next_params(p_number, parameters, func_evals, obs, std, max_step)
        if stop:
            break
        (next_parameters, next_func_evals, next_total_cost) = line_search(func, parameters,
                                                                          func_evals, total_cost,
                                                                          obs, std, p_new)
        next_params = np.concatenate((next_parameters.transpose(),
                                      np.array([next_func_evals]).transpose(),
                                      np.array([[next_total_cost]])))
        all_params = np.concatenate((parameters, func_evals, np.expand_dims(total_cost, 0)))
        all_params = np.delete(all_params, 0, 1)
        all_params = np.c_[all_params, next_params]
        all_params = all_params[:, np.argsort(-all_params[-1, :])]
        parameters = all_params[:p_number, :]
        func_evals = all_params[p_number:-1, :]
        total_cost = all_params[-1, :]
        if abs((total_cost[-1] - total_cost[-2]) / total_cost[-2]) < xtol:
            finish += 1
        else:
            finish = 0
        if finish > 2:
            break
    return (total_cost[-1], parameters[:, -1])
