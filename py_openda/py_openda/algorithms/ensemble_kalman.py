#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Main module containing the functions used for ensemble kalman filters.


Created on Tue Nov 20 15:43:21 2018

@author: hegeman
"""
import numpy as np

def kalman_algorithm(enkf):
    """
    Main algorithm for ensemble kalman filtering. Runs the prediction step and the update step.

    :param enkf: object which houses the main model, observer, ensembles and the time.
    :return: Python list corresponding to the Java result pred_f_central.
    """
    enkf.next_predictions()
    (predictions, mean_predicitons) = enkf.get_ensemble_vectors_state()
    (observations, mean_observations) = enkf.get_ensemble_vectors_forecast()
    pred_f_central = enkf.get_results()

    K = kalman_matrix(enkf, predictions, observations)
    kalman_update(enkf, observations, predictions, mean_observations, mean_predicitons, K)

    return pred_f_central

def kalman_matrix(enkf, predictions, observations):
    """
    Function for generating the kalman gain from the statevector and observations.

    :param enkf: object which houses the main model, observer, ensembles and the time.
    :param predictions: numpy array containing the state vectors for each ensemble member.
    :param observations: numpy array containing the values of each ensemble member at the observed location.

    :return K: kalman gain
    """
    sqrt_r = enkf.get_covariance_matrix()
    n_observations = observations.shape[0]
    n_ensemble = predictions.shape[1]
    state_lenght = predictions.shape[0]
    sqrt_q_min1 = (n_ensemble -1.0)**0.5

    pred_mat = observations.copy()
    pred_mat /= sqrt_q_min1
    D = np.zeros((n_observations, n_observations))
    D = pred_mat.dot(pred_mat.transpose())
    D += sqrt_r.dot(sqrt_r.transpose())

    predictions /= sqrt_q_min1
    D_inverse = np.linalg.inv(D)
    E = np.zeros((n_ensemble, n_observations))
    E = pred_mat.transpose().dot(D_inverse)
    K = np.zeros((state_lenght, n_observations))
    for i in range(n_observations):
        for j in range(n_ensemble):
            K[:, i] += E[j, i]*predictions[:, j]
    return K

def kalman_update(enkf, observations, predictions, mean_observations, mean_predicitons, K):
    """
    Function for updating the states, model and ensemble member using the kalman gain.

    :param enkf: object which houses the main model, observer, ensembles and the time.
    :param observations: numpy array containing the values of each ensemble member at the observed location.
    :param predictions: numpy array containing the state vectors for each ensemble member.
    :param mean_observations: numpy array containing the ensemble mean at the observed location.
    :param mean_observations: numpy array containing the ensemble mean of the state vector.
    :param K: kalman gain

    :return:
    """
    state_lenght = K.shape[0]
    n_ensemble = observations.shape[1]
    for i in range(n_ensemble):
        innovation = enkf.get_realizations() - observations[:, i] - mean_observations.transpose()
        delta = np.zeros((state_lenght, 1))
        delta[:] = K.dot(innovation.transpose())
        #TODO: State update happens both here and within the model object.
        #Looks terrible but saves 1 conversion.
        enkf.update_state(i, delta)
        for j in range(predictions.shape[0]):
            predictions[j, i] = predictions[j, i] + delta[j]
    prediction_mean = np.array([np.mean(predictions, axis=1)]).transpose() + mean_predicitons
    enkf.update_model(prediction_mean)

def no_filter(enkf):
    """
    Algorithm for running the model without filtering.

    :param enkf: object which houses the main model, observer, ensembles and the time.
    :return pred_f_central: result corresponding to the Java result x_f_central.
    """
    enkf.next_predictions()
    pred_f_central = enkf.get_results()
    return pred_f_central
