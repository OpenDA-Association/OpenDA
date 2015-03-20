/* MOD_V2.0
* Copyright (c) 2012 OpenDA Association
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

using System;

namespace OpenDA.DotNet.Interfaces
{
    public interface IStochModelInstance : IModelInstance
    {
        /*********************************
         *** State And Parameter Exchange
         *********************************/

        /// <summary>
        /// Get the full set of state variables from a model.
        /// </summary>
        /// <returns>A Vector containing the values of all state variables in the model.</returns>
        IVector State { get; }

        /// <summary>
        /// Peform a <c>state variable += alpha * vector</c> operation on each state variable in the model.
        /// </summary>
        /// <param name="alpha">The <c>alpha</c> in <c>state variable += alpha * vector</c>.</param>
        /// <param name="vector">A Vector containing the values for the axpy-operation on all state variables in the model.</param>
        void AxpyOnState(double alpha, IVector vector);

        /* Parameters*/

        /// <summary>
        /// Get the full set of parameters from a model.
        /// Set the full set of parameters for a model.
        /// </summary>
        /// <returns>A Vector containing the values of all parameters in the model.</returns>
        IVector Parameters
        {
            get;
            set;
        }

        /// <summary>
        /// Peform a <c>parameter += alpha * vector</c> operation on each parameter in the model.
        /// </summary>
        /// <param name="alpha">The <c>alpha</c> in <c>parameter += alpha * vector</c>.</param>
        /// <param name="vector">A Vector containing the values for the axpy-operation on all parameters in the model.</param>
        void AxpyOnParameters(double alpha, IVector vector);

        /// <summary>
        /// Get the uncertainty of the tochastic model's state at initial time
        /// or uncertainty of background at the current time.
        /// This method is not used for computing the analysis in Kalman filter type algorithms!
        /// </summary>
        /// <returns>A Stochastic Vector containing uncertainty for the state.</returns>
        IStochVector StateUncertainty { get; }

        /// <summary>
        /// Get the uncenrtainty for the stochastic model's parameters.
        /// </summary>
        /// <returns>A Stochastic Vector containing the uncertainty for the parameters.</returns>
        IStochVector ParameterUncertainty { get; }

        /// <summary>
        /// Get the stochastic model's white noise.
        /// </summary>
        /// <param name="time">The time stamp or time span to retreive the noise for.</param>
        /// <returns>A Stochastic Vector containing the white noise.</returns>
        IStochVector[] GetWhiteNoiseUncertainty(ITime time);

        /* Stochastic forcing, model uncertainty or weak constraints */

        /// <summary>
        /// Boolean flag indicating WhiteNoiseUncertainty is same for all times
        /// </summary>
        /// <returns>True if WhiteNoiseUncertainty is same for all times.</returns>
        bool IsWhiteNoiseStationary { get; }

        /// <summary>
        /// Get a set of white noise from a model for the requested timespan.
        /// </summary>
        /// <param name="timeSpan">use this timespan as a selection</param>
        /// <returns>A Vector containing the values of all white noise in the model.</returns>
        ITime[] GetWhiteNoiseTimes(ITime timeSpan);

        /// <summary>
        /// Get the full set of white noise from a model.
        /// </summary>
        /// <param name="timeSpan">use this timespan as a selection</param>
        /// <returns>A Vector containing the values of all white noise in the model.</returns>
        IVector[] GetWhiteNoise(ITime timeSpan);

        /// <summary>
        /// Set the full set of white noise of a model.
        /// </summary>
        /// <param name="whiteNoise">An array of Vectors containing white noise for some span.</param>
        void SetWhiteNoise(IVector[] whiteNoise);

        /// <summary>
        /// Peform a <c>white noise element += alpha * vector</c> operation on each white noise element in the model.    
        /// </summary>
        /// <param name="alpha">A Vector containing the values for the axpy-operation on all white noise in the model.</param>
        /// <param name="vector">The <c>alpha</c> in <c>white noise element += alpha * vector</c>.</param>
        void AxpyOnWhiteNoise(double alpha, IVector[] vector);

        /// <summary>
        /// Set or unset automatic generation of noise within the stochastic model
        /// </summary>
        /// <param name="value">rue to set and false to unset.</param>
        void SetAutomaticNoiseGeneration(bool value);

        /**************************************
         *** Matching to observations
         *    TODO How to handle missing values.
         *    TODO How to handle subselection within a model
         **************************************/

        /// <summary>
        /// Get model values corresponding to a number of observations
        /// </summary>
        /// <param name="observationDescriptions">An ObservationDescriptions object with meta data for the observations</param>
        /// <returns>vector with the model values corresponding to each observation given in the descriptions</returns>
        IVector GetObservedValues(IObservationDescriptions observationDescriptions);

        /// <summary>
        /// Tell model that it can expect to be asked for model values corresponding to the observations
        /// described. The model can make arrangement to save these values. The method compute run over a long
        /// interval at once, not stopping at each time with observations. This is meant to increase the performance
        /// especially of calibration algorithms.
        /// </summary>
        /// <param name="observationDescriptions">An ObservationDescriptions object with meta data for the observations</param>
        void AnnounceObservedValues(IObservationDescriptions observationDescriptions);

        /**************************************
        *** Scaling
        **************************************/

        /// <summary>
        /// Get model suggested scaling for the state to improve the preconditioning of the
        /// data-assimilation. This can eg. be based on some norm or size of cells.
        /// </summary>
        /// <returns>A Vector containing values for all state variables in the model.</returns>
        IVector StateScaling { get; }

        /// <summary>
        ///Get model suggested scaling for applying a Schur-product on the state 
        /// </summary>
        /// <param name="observationDescriptions">The observation descriptions.</param>
        /// <returns>An array of Vectors (one per obs) containing scaling values for each obs.</returns>
        IVector[] GetStateScaling(IObservationDescriptions observationDescriptions);
    }
}