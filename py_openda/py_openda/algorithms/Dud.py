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

ITER_PRINT = 0

class dud_printer:
    def __init__(self):
        self.iter = 0

    def print(self, str):
        """
        Print information with iteration info in front
        :param str: String to print
        :return: none
        """
        print("#%3d "%self.iter +":"+ str)

    def set_iter(self,iter):
        """
        Set iteration number
        :param iter: iteration number to set
        :return: none
        """
        self.iter = iter


class masked_parameters:
    """
    Special kind op parameter that handles parameters that are kept out of the optimization
    """
    def __init__(self, p, active):
        """
        Setup masked parameters.
        :param p: all parameters including those that are frozen
        :param active: boolean array that indicates for each parameter whether is is active of not (=frozen)
        """
        if len(p) != len(active):
            raise ValueError("arguments p and active must have the same length: len(p)="+str(len(p)+
                             " len(active)="+str(len(active))))
        self.p_all = p.copy()
        self.active_all = active.copy()
        self.idx_active = []
        for i,a in zip(range(len(p)), active):
            if a:
                self.idx_active.append(i)

    def get_all(self):
        """
        Retrun (copy of) all model parametrs including those that are frozen (e.g. for evaluation of object function)
        :return: parameters (all)
        """
        return self.p_all.copy()

    def get_act(self):
        """
        Return (copy of) all active model parameters excluding those that are frozen
        :return: parameters (active)
        """
        p = [self.p_all[i] for i in self.idx_active]
        return p

    def set_act(self,p):
        """"
        set a new value for the active, non-frozen parameters
        """
        if len(p) != len(self.idx_active):
            raise ValueError("length of parameter array p (="+str(len(p))+" is not correct. Expecting length "+
                             str(len(self.idx_active)))
        for val,i in zip(p,self.idx_active):
            self.p_all[i] = val

    def set_all(self,p):
        """"
        ":param p
        set all parameters frozen and non-forzen
        """
        if len(p) != len(self.p_all):
            raise ValueError("length of parameter array p (=" + str(len(p)) + " is not correct. Expecting length " +
                             str(len(self.p_all)))
        self.p_all = p.copy()

    def copy(self):
        """
        Create a copy
        :return: copy of instance
        """
        return masked_parameters(self.p_all, self.active_all)

    def set_mask(self,active):
        if len(active) != len(self.active_all):
            raise ValueError("length of parameter array active (="+str(len(active))+" is not correct. Expecting length "+
                             str(len(self.active)))
        self.idx_active=[]
        for i,a in zip(range(len(p)), active):
            if a:
                self.idx_active.append(i)
        self.active = active.copy()

    def set_submask(self, active):
        """
        Apply a mask to the active variables
        :param active: for each currently active variable indicate to keep it active (True) or not (False)
        :return:
        """
        if len(active) != len(self.idx_active):
            raise ValueError("length of parameter array maks (="+str(len(mask))+" is not correct. Expecting length "+
                             str(len(self.idx_active)))
        idx_keep = []
        self.active_all = [False] * len(self.p_all)
        for idx, act in zip(self.idx_active, active):
            if act:
                idx_keep.append(idx)
                self.active_all[idx]=True
        self.idx_active = idx_keep

    def get_active_mask(self):
        """
        Retrun a mask arrat of the lenght of all parameters indicating whether they are active or not
        :return: mask array of length total number of parameters
        """
        active = [False]*len(self.p_all)
        for idx in self.idx_active:
            active[idx]=True
        return active

    def len_act(self):
        """
        Return number of active parameters

        :return:number of active parameters
        """
        return len(self.idx_active)

def marked_array(vec, marker):
    if len(vec) != len(vec):
        raise ValueError("length vector (=" + str(len(vec)) + ") is different from length marker array (" +
                         str(len(vec)))+")"
    str ="["
    for i in range(len(marker)):
        str+="%e"%vec[i]
        if marker[i]:
            str+="*"
        if i<len(marker)-1:
            str+=" ,"
    str+="]"
    return str

def check_A(printer, A, treshhold=1e20):
    """
    Function to check if matrix A is singular. Exists to prevent errors when inverting the matrix.

    :param A: matrix.
    :param treshhold: treshold for an acceotable condition number
    :return: boolean that states whether or not A is singular.
    """
    bad = False
    cnum = np.linalg.cond(A)
    printer.print("* Condition number is: "+"%5.2e"%cnum)
    if np.linalg.cond(A) > treshhold:
        printer.print("!!> WARNING: gradient is approximately zero. Iteration is stopped.")
        printer.print("A="+str(A))
        bad = True
    return bad

def select_matrix_active(A, idx_row=None, idx_col=None):
    """
    Construct a new matrix from selected rows and columns of given matrix

    :param A: Input matrix
    :param idx_row: indices of the selected rows of A that should be copied to the new matrix
    :param idx_col: indices of the selected columns of A that should be copied to the new matrix

    Note: the order of indices iun idx_row and idx_col is mantianed hence duplication and permutation
    of elements can be carried out as well.
    """
    (n_row,n_col) = A.shape
    if idx_row == None: idx_row = range(n_row)
    if idx_col == None: idx_col = range(n_col)

    B=np.zeros((len(idx_row), len(idx_col)))
    for i in range(len(idx_row)):
        for j in range(len(idx_col)):
            B[i,j]=A[idx_row[i],idx_col[j]]
    return B

def initialize_dud(func, p_old:masked_parameters, obs, std, p_pert_m:masked_parameters,
                   l_bound:masked_parameters, u_bound:masked_parameters, start_dist=1.1, start_eps=0.1):
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
    p_number = p_old.len_act()
    plist = p_old.get_act()           # Store the active parameters in the list
    plist += func(p_old.get_all())    # Extent the list with the model predictions for all observations
    plist.append(sum(0.5*((y-x)/z)**2 for y, x, z in zip(obs, plist[p_number:], std))) # Add object function
    params = np.transpose(np.array([plist]))
    l_bound_act_vals = l_bound.get_act()
    u_bound_act_vals = u_bound.get_act()
    p_pert = p_pert_m.get_act()

    # Setup the initial pertubations
    # Make sure we do not perturb outside the variable bounds
    for i in range(p_number):
        plist = p_old.get_act()
        p_step = p_pert[i]

        #Check boundaries and try to fix issues by swapping direction
        if not (l_bound_act_vals[i] < plist[i]+p_step < u_bound_act_vals[i]):
            if not (l_bound_act_vals[i] < plist[i]-p_step < u_bound_act_vals[i]):
                raise ValueError("initial pertubation is too large it extends boundaries in both directions")
            else:
                plist[i] = plist[i] - p_step
        else:
            plist[i] = plist[i] + p_step
        #Evaluate model
        p_eval = p_old.copy()
        p_eval.set_act(plist)
        plist += func(p_eval.get_all())

        plist.append(sum(0.5*((y-x)/z)**2 for y, x, z in zip(obs, plist[p_number:], std)))
        params = np.c_[params, np.transpose(np.array([plist]))]
    params = params[:, np.argsort(-params[-1, :])]
    parameters = params[:p_number, :]
    func_evals = params[p_number:-1, :]
    total_cost = params[-1, :]
    return (parameters, func_evals, total_cost)





def find_next_params(printer,p, parameters, func_evals, obs, std, max_step=10):
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

    p_new = p.copy()
    p_number = p_new.len_act()
    Delta_P = np.array(parameters[:, :-1]
                        - np.transpose(np.array([parameters[:, -1]])))
    Delta_F = np.transpose(np.c_[np.divide([(func_evals[:, x] -
                                             func_evals[:, -1]) for x in range(p_number)], std)])
    residue = np.array(obs)-func_evals[:, -1]
    A = np.transpose(Delta_F).dot(Delta_F)

    if check_A(printer,A):
        return (True, p_new)
    alpha = lu_solve(lu_factor(A), np.transpose(Delta_F).dot(np.divide(residue, std)))
    if np.max(abs(alpha)) > max_step:
        alpha *= max_step/np.max(abs(alpha))
    p_new_active = parameters[:, -1] + Delta_P.dot(np.transpose(alpha))

    p_new.set_act(p_new_active)
    return (False, p_new)



def check_step_conv(printer,p_new, parameters, std, p_tol=1.0e-3):
    p_step = p_new.get_act() - parameters[:, -1]
    p_rel = [abs(dp) / max(abs(sigma), 1.0e-8) for dp, sigma in zip(p_step, std)]

    marker = [xr < p_tol for xr in p_rel]
    marked_list = marked_array(p_rel,marker)
    printer.print("* Relative stepsize: " + marked_list)

    convergence = False
    if max(p_rel)<p_tol:
        printer.print("==> Converged max relative stepsize is <"+str(p_tol))
        convergence = True
    return convergence



def max_step_p_new(printer,p_curr, p_new, l_bound, u_bound, alpha_min=1e-2):
    p_curr_act = p_curr.get_act()
    l_bound_act = l_bound.get_act()
    u_bound_act = u_bound.get_act()
    p_new_act = p_new.get_act()

    # Check for valid starting point if not something went wrong in the algorithm.
    if not all([l <= p <= u for p, l, u in zip(p_curr_act, l_bound_act, u_bound_act)]):
        printer.print("print all parameters and bound, error will follow")
        printer.print("lower, param, upper")
        for p, l, u in zip(parameters_act[:, -1], l_bound_act, u_bound_act):
            printer.print(str(l)+" "+str(p) + " "+str(u))
        raise ValueError("current parameters are outside upper and lower bounds")

    # Compute max stepsize (within alpha_min)
    alpha = 1.0
    # Compute the maximum relative stepsize
    alpha_max=[]

    # parameters used in case of potential reinitialization
    p_reinit_act=[]

    bound_reset_needed=[False]*len(l_bound_act)
    hit = [None]*len(l_bound_act)
    for p, p_n, l, u, idx, in zip(p_curr_act, p_new_act, l_bound_act, u_bound_act, range(len(l_bound_act))):
        pr=None #Not needed but just in case of programming error we will not use a previously set value
        if p_n < l:
            alpha_max_i = (p - l) / (p - p_n)
            pr = l
        elif p_n > u:
            alpha_max_i = (u - p) / (p_n - p)
            pr = u
        else:
            alpha_max_i = 1.0
        if alpha_max_i < alpha_min:
            alpha_max_i = 1.0
            bound_reset_needed[idx] = True
        else:
           pr = p # Hitting bound will not trigger fixing this variable
        p_reinit_act.append(pr)
        alpha = min(alpha, alpha_max_i)

    if (any(bound_reset_needed)):
        printer.print("Warning: bound reset needed, adjusting search direction")
        sub_active = [(not reset) for reset in bound_reset_needed]
        p_reinit = p_curr.copy()
        p_reinit.set_act(p_reinit_act)
        p_reinit.set_submask(sub_active)
    else:
        p_reinit = None

    p_corrected_active =[]
    if alpha<1.0: alpha=alpha*0.99


    #We do not need this anymore....
    i = 0
    for p, p_n, l, u, in zip(p_curr_act, p_new_act, l_bound_act, u_bound_act):
        p_corrected_i = p + alpha * (p_n - p)
        if p_corrected_i < l:
            printer.print("Parameter: "+str(i)+" hits lower boundary "+str(l))
            p_corrected_i = 0.5*(p+l)   #Half distance between boundary and current value
        elif p_corrected_i > u:
            printer.print("Parameter: "+str(i)+" hits upper boundary "+str(u))
            p_corrected_i = 0.5*(u+p)    #Half distance between boundary and current value
        p_corrected_active.append(p_corrected_i)
        i+=1
    p_corrected = p_new.copy()
    p_corrected.set_act(p_corrected_active)
    return p_corrected, p_reinit



def line_search(printer,func, parameters, func_evals, total_cost, obs, std, p_new, alpha_min):
    """
    Line search that looks along the next search direction for parameters that lower the total cost.

    :param func: the function for which we aim to find the optimal parameters.
    :param parameters: array with the parameters.
    :param func_evals: array with function predictions at the observation locations.
    :param total_cost: the total cost of the different parameter sets.
    :param obs: list of observations we aim to reproduce.
    :param std: list of corresponding standard deviations.
    :param p_new: parameters that suggest the search direction.
    :param alpha_min: minimal damping factor
    :return: tuple with the next parameters, function evaluations and total costs, flag indicating improvement.
    """
    printer.print("* Linesearch: Cost to reduce:"+str(math.sqrt(2.0* total_cost[-1])))

    printer.print("   -alpha=0.00000" + " p="+str(list(parameters[:, -1])) + " obj="+ str(math.sqrt(2.0 * total_cost[-1])))

    p_step = p_new.get_act()-parameters[:, -1]
    p_rel = [abs(dp)/max(abs(p),1.0e-8) for dp, p in zip(p_step, parameters[:, -1])]
    #print("search-direction"+str(p_new.get_act()-parameters[:, -1]))
    #print("relative stepsize ="+str(p_rel))

    next_parameters = p_new.get_act()
    #print("evaluate with"+str(next_parameters))
    next_func_evals = func(p_new.get_all())  #Note next_parameters == p_new at this point
    next_total_cost = sum(0.5*((y-x)/z)**2 for y, x, z in zip(obs, next_func_evals, std))
    d = 1
    printer.print("   -alpha="+"%6.5f"%d+" p=" + str(p_new.get_all()) + " obj=" + str(math.sqrt(2.0*next_total_cost)))

    while next_total_cost > total_cost[-1]:
        d *= 0.5
        if abs(d) < alpha_min:
            printer.print("   !!> Linesearch terminiation alpha<"+str(alpha_min))
            break
        next_parameters_act = d*np.array(p_new.get_act())+(1-d)*parameters[:, -1]
        next_parameters_eval = p_new.copy()
        next_parameters_eval.set_act(next_parameters_act)
        next_parameters=next_parameters_eval.get_act()
        next_func_evals = func(next_parameters_eval.get_all())
        next_total_cost = sum(0.5*((y-x)/z)**2 for y, x, z in zip(obs, next_func_evals, std))
        printer.print("   -alpha=" + "%6.5f"%d + " p=" + str(next_parameters_eval.get_all()) + " obj=" + str(math.sqrt(2.0 * next_total_cost)))

    succes =  next_total_cost < total_cost[-1]

    return (next_parameters, next_func_evals, next_total_cost, succes)


def dud(func, p_start, p_std, p_pert, obs, std, xtol=1e-3, p_tol=1e-4, start_dist=1.1, l_bound=None, u_bound=None, max_iter=10,
        max_restart=1, alpha_min=0.1):
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
    :param max_iter: max number of DUD iterations (sum of all inner iterations)
    :param alpha_min: minimal damping factor. DUD is restarted when damping factor drops below this value
    :max_restart: max number of times DUD reinitialises when no improvement can be found (re-perturbing all parameters)
    the search directions (default 1.1).
    :return: tuple containing the minimal cost followed by the list of corresponding parameters.
    """

    #Setup boundaries
    if l_bound is None:
        l_bound = [-float('inf')]*len(p_start)
    if u_bound is None:
        u_bound = [float('inf')]*len(p_start)
    #Setup (initial) pertubation
    if p_pert is None:
        start_eps = 0.1
        p_pert = [p_start[i]* (start_dist - 1.0) + start_eps for i in range(len(p_start))]

    finish = 0
    max_step = 10

    #For the handling of bounds we can activate and deactivate parameters.
    #Initially all parameters are active
    active = [True] * len(p_start)
    p_curr = masked_parameters(p_start, active)
    l_bound_masked = masked_parameters(l_bound, active)
    u_bound_masked = masked_parameters(u_bound, active)
    p_pert_masked = masked_parameters(p_pert, active)

    #The pertubation size can change therefore

    do_new_outer_loop = True
    #Outerloop, start each time with a new initialization of the system

    iter = 0
    restart_cycles = 0
    set_last_value = True
    printer = dud_printer()

    while do_new_outer_loop:

        # Initialize by perturbing all parameters
        # -Check whether there all still parameters to optimize (all might be fixed to boundaries)
        if (len(p_curr.idx_active) == 0):
            printer.print("!!> No parameers (left) to optimize")
            do_new_outer_loop = False
            set_last_value = False
            break

        # Notes: p_new is our current approximation of the solution
        (parameters, func_evals, total_cost) = initialize_dud(func, p_curr, obs, std, p_pert_m=p_pert_masked,
                                                              l_bound=l_bound_masked, u_bound=u_bound_masked, start_dist=start_dist,
                                                              start_eps=0.1)

        # TODO: For new outerloop we should not throw the previous steps away!
        hist = {}
        hist["parameters"] = list(parameters.copy())
        hist["func_evals"] = list(func_evals.copy())
        hist["total_cost"] = list(total_cost.copy())


        iterate_innerloop = True

        #Inner loop
        while iterate_innerloop:

            # Find new optimum of linear apporixmation of optimizatiopn problem
            (stop, p_propose) = find_next_params(printer, p_curr, parameters, func_evals, obs, std, max_step)
            if stop:
                do_new_outer_loop = False
                break

            # Check convergence based on stepsize
            if check_step_conv(printer, p_propose, parameters, p_std, p_tol=p_tol):
                do_new_outer_loop = False
                iterate_innerloop = False #Last linesearch and then done


            # Adust approximate solution by limiting stepsize and checking the selected bounds
            p_propose, p_reinit = max_step_p_new(printer, p_curr, p_propose, l_bound_masked, u_bound_masked)

            # We perform a linesearch and find a new best value but only when there are not issues with bounds
            if (p_reinit is None):
                # Perform a linesearch in order to reduce object function
                (next_parameters, next_func_evals, next_total_cost, success) = line_search(printer, func, parameters,
                                                                                  func_evals, total_cost,
                                                                                  obs, std, p_propose, alpha_min=alpha_min)

                # Heuristic part to deal with parameters hitting boundaries and poor convergence
                if not success and restart_cycles<max_restart:
                    # Linesearch is no succes but we are allowed to restart
                    printer.print("!!> DUD will be reinitialized due to stagnation in convergence")
                    # Decrease pertubation (compared to input pertubation)
                    p_pert_restart = [p * 0.1 for p in p_pert]
                    p_pert_masked.set_all(p_pert_restart)
                    p_curr.set_act(parameters[:, -1])   #TODO SHOULD BE same as p_cur
                    do_new_outer_loop = True
                    restart_cycles+=1
                    break
                elif not success:
                    # Linesearch is no succes but we are not going to restart anymore
                    # That means that we are done.
                    do_new_outer_loop=False
                    break

                next_params = np.concatenate((next_parameters,
                                              np.array(next_func_evals),
                                              np.array([next_total_cost])))

            else:
                # When a parameter hits the bound we fix its value and restart DUD
                printer.print("!!> DUD will be reinitialized due to parameter hitting bound")
                # set problematic valiable to bound


                # adjust masked arrays when needed
                active_p_reinit = p_reinit.get_active_mask()
                l_bound_masked = masked_parameters(l_bound, active_p_reinit)
                u_bound_masked = masked_parameters(u_bound, active_p_reinit)

                # Decrease pertubation
                p_pert_restart = [p*0.1 for p in p_pert]
                p_pert_restart_masked = masked_parameters(p_pert_restart, active_p_reinit)

                #next_parameters_act = p_new.copy() # glue active/non-active together
                #p_new = next_parameters_act
                p_pert_masked = p_pert_restart_masked

                # set reinitialization value
                p_curr = p_reinit

                do_new_outer_loop = True
                break

            #TODO WHAT TO DO WHEN WE RESTART......
            hist["parameters"].append(next_parameters.copy())
            hist["func_evals"].append(next_func_evals.copy())
            hist["total_cost"].append(next_total_cost.copy())

            p_number = p_propose.len_act()
            all_params = np.concatenate((parameters, func_evals, np.expand_dims(total_cost, 0)))
            all_params = np.delete(all_params, 0, 1)
            all_params = np.c_[all_params, next_params]
            all_params = all_params[:, np.argsort(-all_params[-1, :])]
            parameters = all_params[:p_number, :]
            func_evals = all_params[p_number:-1, :]
            total_cost = all_params[-1, :]

            # Set new best approximation
            p_curr.set_act(parameters[:, -1])

            if abs(total_cost[-1] - total_cost[-2]) < xtol * abs(total_cost[-2]):
                finish += 1
            else:
                finish = 0
            if finish > 2:
                printer.print("==> Stop iterations, no significant reduction of object function (2x)")
                do_new_outer_loop = False
                break
            iter+=1
            printer.set_iter(iter)
            if iter>=max_iter:
                printer.print("==> Stop iteraration max number of iterations reached")
                do_new_outer_loop=False
                do_new_outer_loop = False
                break

        #tidy up hist
        hist["parameters"] = np.array([list(p) for p in hist["parameters"]]).T.tolist()
        hist["func_evals"] = np.array([list(p) for p in hist["func_evals"]]).T.tolist()

    # Get the parameters that correspond to lowest object function
    if set_last_value:
        p_curr.set_act(parameters[:, -1])


    return (total_cost[-1], p_curr.get_all(), hist)
