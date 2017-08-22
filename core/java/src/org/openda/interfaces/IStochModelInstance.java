/* OpenDA v2.4.1 
* Copyright (c) 2017 OpenDA Association 
* All rights reserved.
* 
* This file is part of OpenDA. 
* 
* OpenDA is free software: you can redistribute it and/or modify 
* it under the terms of the GNU Lesser General Public License as 
* published by the Free Software Foundation, either version 3 of 
* the License, or (at your option) any later version. 
* 
* OpenDA is distributed in the hope that it will be useful, 
* but WITHOUT ANY WARRANTY; without even the implied warranty of 
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
* GNU Lesser General Public License for more details. 
* 
* You should have received a copy of the GNU Lesser General Public License
* along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
*/

package org.openda.interfaces;


/**
 * Interface to specify the functions for a stochastic model.
 */
public interface IStochModelInstance extends IModelInstance {

    /*********************************
     *** State And Parameter Exchange
     *********************************/

    /**
     * Get the full set of state variables from a model.
     *
     * @return A Vector containing the values of all state variables in the model.
     */
    IVector getState();

    /**
     * Peform a <c>state variable += alpha * vector</c> operation on each state variable in the model.
     * @param alpha The <c>alpha</c> in <c>state variable += alpha * vector</c>.
     * @param vector A Vector containing the values for the axpy-operation on all state variables in the model.
     */
    void axpyOnState(double alpha, IVector vector);


    /* Parameters*/

    /**
     * Get the full set of parameters from a model.
     *
     * @return A Vector containing the values of all parameters in the model.
     */
    IVector getParameters();

    /**
     * Set the full set of parameters for a model.
     *
     * @param parameters A Vector containing values for all parameters in the model.
     */
    void setParameters(IVector parameters);

    /**
     * Peform a <c>parameter += alpha * vector</c> operation on each parameter in the model.
     *
     * @param alpha The <c>alpha</c> in <c>parameter += alpha * vector</c>.
     * @param vector         A Vector containing the values for the axpy-operation on all parameters in the model.
     */
    void axpyOnParameters(double alpha, IVector vector);

    /**
	 * Get the uncertainty of the stochastic model's state at initial time
	 * or uncertainty of background at the current time.
	 * This method is not used for computing the analysis in Kalman filter type algorithms!
	 *
	 * @return A Stochastic Vector containing uncertainty for the state. Returning null means that
	 * there is no state uncertainty specified.
	 */
	IStochVector getStateUncertainty();

    /**
     * Get the uncenrtainty for the stochastic model's parameters.
     *
     * @return A Stochastic Vector containing the uncertainty for the parameters.
     */
    IStochVector getParameterUncertainty();

    /**
     * Get the stochastic model's white noise.
     *
     * @param time The time stamp or time span to retreive the noise for.
     * @return A Stochastic Vector containing the white noise.
     */
    IStochVector[] getWhiteNoiseUncertainty(ITime time);


    /* Stochastic forcing, model uncertainty or weak constraints */

    /**
     * Boolean flag indicating WhiteNoiseUncertainty is the same for all times
     *
     * @return <code>True</code> if WhiteNoiseUncertainty is the same for all times.
     */
    boolean isWhiteNoiseStationary();

    /**
     * Get a set of white noise from a model for the requested timespan.
     *
     * @param timeSpan use this timespan as a selection
     * @return A Vector containing the values of all white noise in the model.
     */
    ITime[] getWhiteNoiseTimes(ITime timeSpan);

    /**
     * Get the full set of white noise from a model.
     *
     * @param timeSpan use this timespan as a selection
     * @return A Vector containing the values of all white noise in the model.
     */
    IVector[] getWhiteNoise(ITime timeSpan);

    /**
     * Set the full set of white noise of a model.
     *
     * @param whiteNoise An array of Vectors containing white noise for some span.
     */
    void setWhiteNoise(IVector whiteNoise[]);

    /**
     * Peform a <c>white noise element += alpha * vector</c> operation on each white noise element in the model.
     *
     * @param vector         A Vector containing the values for the axpy-operation on all white noise in the model.
     * @param alpha The <c>alpha</c> in <c>white noise element += alpha * vector</c>.
     */
    void axpyOnWhiteNoise(double alpha, IVector vector[]);

    /**
     * Set or unset automatic generation of noise within the stochastic model
     *
     * @param value <code>true</code> to set and <code>false</code> to unset.
     */
    void setAutomaticNoiseGeneration(boolean value);

    /**************************************
     *** Matching to observations
     *    TODO How to handle missing values.
     *    TODO How to handle subselection within a model
     **************************************/

    /**
     * Get model values corresponding to a number of observations
     *
     * @param observationDescriptions An ObservationDescriptions object with meta data for the observations
     * @return vector with the model values corresponding to each observation given in the descriptions
     */

    IVector getObservedValues(IObservationDescriptions observationDescriptions);

    /**
     * Tell model that it can expect to be asked for model values corresponding to the observations
     * described. The model can make arrangement to save these values. The method compute run over a long
     * interval at once, not stopping at each time with observations. This is meant to increase the performance
     * especially of calibration algorithms.
     *
     * @param observationDescriptions An ObservationDescriptions object with meta data for the observations
     */
    void announceObservedValues(IObservationDescriptions observationDescriptions);


    /**************************************
     *** Scaling
     **************************************/

    /**
     * Get model suggested scaling for the state to improve the preconditioning of the
     * data-assimilation. This can eg. be based on some norm or size of cells.
     *
     * @return A Vector containing values for all state variables in the model.
     */
    IVector getStateScaling();

    /**
     * Get model suggested scaling for applying a Schur-product on the state
     *
     * @return An array of Vectors (one per obs) containing scaling values for each obs.
     * @param observationDescriptions The observation descriptions.
     */
    IVector[] getStateScaling(IObservationDescriptions observationDescriptions);

}

