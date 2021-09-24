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
import math
def check_A(A):
    """
    Function to check if matrix A is singular. Exists to prevent errors when inverting the matrix.

    :param A: matrix.
    :return: boolean that states whether or not A is singular.
    """
    bad = False
    cnum = np.linalg.cond(A)
    print("Condition number is:"+str(cnum))
    if np.linalg.cond(A) > 1e20:
        print("WARNING: gradient is approximately zero. Iteration is stopped.")
        print("A="+str(A))
        bad = True
    return bad


def initialize_dud(func, p_old, obs, std, p_pert=None, l_bound=None, u_bound=None, start_dist=1.1, start_eps=0.1):
    """
    Function used to find the initial search directions.

    :param func: the function for which we aim to find the optimal parameters.
    :param p_old: first guess of the parameters.
    :param obs: list of observations we aim to reproduce.
    :param std: list of corresponding standard deviations.
    :param p_pert: the initial pertubation of the parameters (user provided)
    :param start_dist: Factor by which p_old is multiplied to find the search directions
    (default 1.1).
    :return: tuple with the parameters, function evaluations and total costs.
    """
    p_number = len(p_old)
    plist = p_old.copy()
    plist += func(plist)
    plist.append(sum(0.5*((y-x)/z)**2 for y, x, z in zip(obs, plist[p_number:], std)))
    params = np.transpose(np.array([plist]))
    # Setup the initial pertubations
    # Make sure we do not perturb outside the variable bounds
    for i in range(p_number):
        plist = p_old.copy()
        if p_pert:
            p_step = p_pert[i]
        else:
            p_step = plist[i] * (start_dist-1.0) + start_eps
        #Check boundaries and try to fix issues by swapping direction
        if not (l_bound[i] < plist[i]+p_step < u_bound[i]):
            if not (l_bound[i] < plist[i]-p_step < u_bound[i]):
                raise ValueError("initial pertubation is too large it extends boundaries in both directions")
            else:
                plist[i] = plist[i] - p_step
        else:
            plist[i] = plist[i] + p_step

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
    :param func_evals: array with function predictions at the observation locations.
    :param obs: list of observations we aim to reproduce.
    :param std: list of corresponding standard deviations.
    :keyword argument max_step: upper limit for how much the parameters change (default 10).
    :return: tuple which contains a boolean that is true if finding new parameters was impossible
    and a list containing the next search direction.
    """
    p_new = [None]*p_number
    Delta_P = np.array(parameters[:, :-1]
                        - np.transpose(np.array([parameters[:, -1]])))
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


def check_step_conv(p_new, parameters, std, p_tol=1.0e-4):
    p_step = p_new - parameters[:, -1]
    p_rel = [abs(dp) / max(abs(sigma), 1.0e-8) for dp, sigma in zip(p_step, std)]
    convergence = False
    print("relative stepsize:"+str(p_rel))
    if max(p_rel)<p_tol:
        print("converged max relative stepsize is <"+str(p_tol))
        convergence = True
    return convergence



def max_step_p_new(parameters, p_new, l_bound, u_bound, alpha_min=1e-2):

    # Check for valid starting point
    if not all([l <= p <= u for p, l, u in zip(parameters[:, -1], l_bound, u_bound)]):
        print("print all parameters and bound, error will follow")
        print("lower, param, upper")
        for p, l, u in zip(parameters[:, -1], l_bound, u_bound):
            print (str(l)+" "+str(p) + " "+str(u))
        raise ValueError("current parameters are outside upper and lower bounds")

    # Compute max stepsize (within alpha_min)
    alpha = 1.0
    # Compute the maximum relative stepsize
    alpha_max=[]
    bound_reset_needed=False
    for p, p_n, l, u, in zip(parameters[:, -1], p_new, l_bound, u_bound):
        if p_n < l:
            alpha_max_i = (p - l) / (p - p_n)
        elif p_n > u:
            alpha_max_i = (u - p) / (p_n - p)
        else:
            alpha_max_i = 1.0
        if alpha_max_i < alpha_min:
            alpha_max_i = 1.0
            bound_reset_needed = True
        alpha = min(alpha, alpha_max_i)

    if (bound_reset_needed):
        print("Warning: bound reset needed, adjusting search direction")
    p_corrected =[]
    if alpha<1.0: alpha=alpha*0.99

    i = 0
    for p, p_n, l, u, in zip(parameters[:, -1], p_new, l_bound, u_bound):
        p_corrected_i = p + alpha * (p_n - p)
        if p_corrected_i < l:
            print("Parameter: "+str(i)+" hits lower boundary "+str(l))
            p_corrected_i = 0.5*(p+l)   #Half distance between boundary and current value
        elif p_corrected_i > u:
            print("Parameter: "+str(i)+" hits upper boundary "+str(u))
            p_corrected_i = 0.5*(u+p)    #Half distance between boundary and current value
        p_corrected.append(p_corrected_i)
        i+=1
    return np.array(p_corrected)



def line_search(func, parameters, func_evals, total_cost, obs, std, p_new):
    """
    Line search that looks along the next search direction for parameters that lower the total cost.

    :param func: the function for which we aim to find the optimal parameters.
    :param parameters: array with the parameters.
    :param func_evals: array with function predictions at the observation locations.
    :param total_cost: the total cost of the different parameter sets.
    :param obs: list of observations we aim to reproduce.
    :param std: list of corresponding standard deviations.
    :param p_new: parameters that suggest the search direction.
    :return: tuple with the next parameters, function evaluations and total costs.
    """
    print("Linesearch: Cost to reduce:"+str(math.sqrt(2.0* total_cost[-1])))
    p_step = p_new-parameters[:, -1]
    p_rel = [abs(dp)/max(abs(p),1.0e-8) for dp, p in zip(p_step, parameters[:, -1])]
    print("search-direction"+str(p_new-parameters[:, -1]))
    #print("relative stepsize ="+str(p_rel))

    next_parameters = p_new
    next_func_evals = func(next_parameters)
    next_total_cost = sum(0.5*((y-x)/z)**2 for y, x, z in zip(obs, next_func_evals, std))
    d = 1
    print("alpha="+str(d)+" cost="+str(math.sqrt(2.0*next_total_cost)))

    while next_total_cost > total_cost[-1]:
        d *= 0.5
        next_parameters = d*p_new+(1-d)*parameters[:, -1]
        next_func_evals = func(next_parameters)
        next_total_cost = sum(0.5*((y-x)/z)**2 for y, x, z in zip(obs, next_func_evals, std))
        print("alpha=" + str(d) + " cost=" + str(math.sqrt(2.0* next_total_cost)))
        # Step becomes too small stop searcing
        if abs(d) < 0.0625:
            break
    succes =  next_total_cost < total_cost[-1]

    return (next_parameters, next_func_evals, next_total_cost, succes)


def dud(func, p_old, p_pert, obs, std, xtol=1e-3, start_dist=1.1, l_bound=None, u_bound=None, max_iter=10, max_restart=1):
    """
    Main function which minimizes a least squares problem without using derivatives.

    :param func: the function for which we aim to find the optimal parameters.
    :param p_old: first guess of the parameters.
    :param obs: list of observations we aim to reproduce.
    :param std: list of corresponding standard deviations.
    :param xtol: desired accuracy of the result (default 1e-3).
    :param start_dist: Factor by which p_old is multiplied to find
    :param l_bound: lower bound on variables  (Note: DUD uses bounds in linesearch to avoid object function evaluation
                                                    with illegal values there is no robust handing of constraints)
    :param u_bound: upper bound on variables
    :param max_iter: max number of DUD (outer) iterations
    :max_restart: max number of times DUD reinitialises when no improvement can be found (re-perturbing all parameters)
    the search directions (default 1.1).
    :return: tuple containing the minimal cost followed by the list of corresponding parameters.
    """
    if l_bound is None:
        l_bound = [-float('inf')]*len(p_old)
    if u_bound is None:
        u_bound = [float('inf')]*len(p_old)

    finish = 0
    max_step = 10
    p_number = len(p_old)
    (parameters, func_evals, total_cost) = initialize_dud(func, p_old, obs, std, p_pert=p_pert,
                                                          l_bound=l_bound, u_bound=u_bound, start_dist=start_dist,
                                                          start_eps=0.1)
    hist = {}
    hist["parameters"] = list(parameters.copy())
    hist["func_evals"] = list(func_evals.copy())
    hist["total_cost"] = list(total_cost.copy())
    iter =0

    restart_cycles = 0
    reinitialize = False

    while True:



        (stop, p_new) = find_next_params(p_number, parameters, func_evals, obs, std, max_step)
        if stop:
            break

        # Check convergence based on stepsize
        if check_step_conv(p_new, parameters, std, p_tol=1.0e-4):
            break

        p_new = max_step_p_new(parameters, p_new, l_bound, u_bound)
        (next_parameters, next_func_evals, next_total_cost, success) = line_search(func, parameters,
                                                                          func_evals, total_cost,
                                                                          obs, std, p_new)

        if not success and restart_cycles<max_restart:
            reinitialize=True
            restart_cycles+=1
        if reinitialize:
            print("DUD: Reinitialize linearization")
            #Linear approximation can become quite poor.
            p_pert_restart = [p*0.1 for p in p_pert]
            (parameters, func_evals, total_cost) = initialize_dud(func, list(next_parameters), obs, std, p_pert=p_pert_restart,
                                                                  l_bound=l_bound, u_bound=u_bound,
                                                                  start_dist=start_dist,
                                                                  start_eps=0.1)
            reinitialize = False
        else:
            next_params = np.concatenate((next_parameters,
                                          np.array(next_func_evals),
                                          np.array([next_total_cost])))

            hist["parameters"].append(next_parameters.copy())
            hist["func_evals"].append(next_func_evals.copy())
            hist["total_cost"].append(next_total_cost.copy())


            all_params = np.concatenate((parameters, func_evals, np.expand_dims(total_cost, 0)))
            all_params = np.delete(all_params, 0, 1)
            all_params = np.c_[all_params, next_params]
            all_params = all_params[:, np.argsort(-all_params[-1, :])]
            parameters = all_params[:p_number, :]
            func_evals = all_params[p_number:-1, :]
            total_cost = all_params[-1, :]


            if abs(total_cost[-1] - total_cost[-2]) < xtol * abs(total_cost[-2]):
                finish += 1
            else:
                finish = 0
            if finish > 2:
                break
            iter+=1
            if iter>=max_iter:
                print("terminated in max number of iterations")
                break

    #tidy up hist
    hist["parameters"] = np.array([list(p) for p in hist["parameters"]]).T.tolist()
    hist["func_evals"] = np.array([list(p) for p in hist["func_evals"]]).T.tolist()

    return (total_cost[-1], parameters[:, -1], hist)
