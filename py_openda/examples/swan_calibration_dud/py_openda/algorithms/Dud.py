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
    Function to check if matrix A is singular
    
    :param A: matrix
    :return: boolean that states whether or not A is singular 
    """
    bad = False
    if np.linalg.cond(A) > 1e10:
        print("WARNING: gradient is approximately zero. Iteration is stopped.")
        bad = True
    return bad

def initialize_dud(func, p_old, obs, std, start_dist = 1.1):
    """
    Function used to find the initial search directions
    
    :param func: the function for which we aim to find the optimal parameters
    :param p_old: first guess of the parameters
    :param obs: list of observartions we aim to reproduce
    :param std: list of corresponding standard deviations
    :keyword argument start_dist: Factor by which p_old is multiplied to find the search directions (default 1.1)
    :return: array with the parameters, function evaluations and total cost for rows, and different parameter sets for columns
    """
    p_number = len(p_old)
    plist = p_old.copy()
    plist += func(plist)
    plist.append(sum(0.5*((y-x)/z)**2 for y,x,z in zip(obs, plist[p_number:], std)))
    params = np.transpose(np.array([plist]))
    for i in range(p_number):
        plist = p_old.copy()
        plist[i] = plist[i]*start_dist
        plist += func(plist)
        plist.append(sum(0.5*((y-x)/z)**2 for y,x,z in zip(obs, plist[p_number:], std)))
        params = np.c_[params, np.transpose(np.array([plist]))]
    params = params[:, np.argsort(-params[-1, :])]
    return params

def find_next_params(p_number, params, obs, std, max_step = 10):
    """
    Function used to find the next search direction
    
    :param func: the function for which we aim to find the optimal parameters
    :param params: array with the parameters, function evaluations and total cost for rows, and different parameter sets for columns
    :param obs: list of observartions we aim to reproduce
    :param std: list of corresponding standard deviations
    :keyword argument max_step: upper limit for how much the parameters change (default 10)
    :return: tuple which contains a boolean that is true if finding new parameters was impossible and a list containing the next search direction
    """
    p_new = [None]*p_number
    Delta_P = np.array([params[:p_number, :-1] - np.transpose(np.array([params[:p_number, -1]]))])
    Delta_F = np.transpose(np.c_[np.divide([(params[p_number:-1, x] - params[p_number:-1, -1]) for x in range(p_number)], std)])
    residue = np.array(obs)-params[p_number:-1, -1]
    A = np.transpose(Delta_F).dot(Delta_F)
    if check_A(A):
        return (True, p_new)
    alpha = lu_solve(lu_factor(A), np.transpose(Delta_F).dot(np.divide(residue, std))) 
    if np.max(abs(alpha)) > max_step:
        alpha *= max_step/np.max(abs(alpha))
    p_new = params[:p_number, -1] + Delta_P.dot(np.transpose(alpha))
    return (False, p_new)

def line_search(func, params, obs, std, p_new):
    """
    Line search that looks along the next search direction for parameters that lower the total cost
    
    :param func: the function for which we aim to find the optimal parameters
    :param params: array with the parameters, function evaluations and total cost for rows, and different parameter sets for columns
    :param obs: list of observartions we aim to reproduce
    :param std: list of corresponding standard deviations
    :param p_new: parameters that suggest the search direction
    :return: next column to be added to params
    """
    next_params = params[:, -1].copy()
    p_number = np.shape(p_new)[1]
    next_params[:p_number] = p_new
    next_params[p_number:-1] = func(next_params[:p_number])
    next_params[-1] = sum(0.5*((y-x)/z)**2 for y,x,z in zip(obs, next_params[p_number:-1], std))
    d = 1
    while next_params[-1] > params[-1, -1]:
        d *= 0.5
        next_params[:p_number] = d*p_new+(1-d)*params[:p_number, -1]
        next_params[p_number:-1] = func(next_params[:p_number])
        next_params[-1] = sum(0.5*((y-x)/z)**2 for y,x,z in zip(obs, next_params[p_number:-1], std))
        if abs(d) < 0.0625:
            if d < 0:
                break
            d *= -1
    return next_params

def dud(func, p_old, obs, std, xtol = 1e-3, start_dist = 1.1):
    """
    Main function which minimizes a least squares problem without using derivatives.
    
    :param func: the function for which we aim to find the optimal parameters
    :param p_old: first guess of the parameters
    :param obs: list of observartions we aim to reproduce
    :param std: list of corresponding standard deviations
    :keyword argument xtol: desired accuracy of the result (default 1e-3)
    :keyword argument start_dist: Factor by which p_old is multiplied to find the search directions (default 1.1)
    :return: tuple containing the minimal cost followed by the list of corresponding parameters
    """
    Fin = 0
    max_step = 10
    p_number = len(p_old)
    params = initialize_dud(func, p_old, obs, std, start_dist)
    while True:
        (stop, p_new) = find_next_params(p_number, params, obs, std, max_step)
        if stop:
            break
        next_params = line_search(func, params, obs, std, p_new)
        params = np.delete(params, 0, 1)
        params = np.c_[params, np.transpose(next_params)]
        params = params[:, np.argsort(-params[-1, :])]
        if abs((params[-1, -1] - params[-1, -2]) / params[-1, -2]) < xtol:
            Fin += 1
        else:
            Fin = 0
        if Fin > 2:
            break
    return (params[-1, -1], params[:p_number, -1].tolist())