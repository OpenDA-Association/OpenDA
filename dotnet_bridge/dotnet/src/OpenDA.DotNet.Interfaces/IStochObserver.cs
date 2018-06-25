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
using System.Collections.Generic;


namespace OpenDA.DotNet.Interfaces
{

    /**
    * Stochastic Observer
    */
    public interface IStochObserver
    {
        /**
        *
        * Initialize the Stochastic Observer
        * @param workingDir    directory containing configuration
        * @param arguments     arguments needed to configure the instance
        */
        void Initialize(String workingDir, String[] arguments);

        /**
        *
        * Create an new Stochastic Observer, containing a selection of the present stochastic observer.<br>
        * The selection criteria is a timeSpan. The start time of the interval is not included, the end time is
        * included, i.e. t_start<t<=t_end
        * @param selectionTimes    timeSpan with selection.
        * @return                  Stochastic Observer containing the required selection.
        */
        IStochObserver createSelection(ITime selectionTimes);

        /**
         * Create an new Stochastic Observer, containing a selection of the present stochastic observer.<br>
         * The selection criteria is the type of observations: assimilation or validation
         *
         * @param observationType The requested type of observations.
         * @return Stochastic Observer containing the required selection.
         */
        IStochObserver createSelection(Type observationType);

        /**
         * Number of observations in the Stochastic Observer.
         * @return The number of observations.
         */
        int getCount();

        /**
         * Get the values for all observations.
         * @return The observed values.
         */
        double[] getValues();

        /**
         * Get realization values for all observations, for one ensemble member.
         * @return The realizations.
         */
        double[] getRealizations();

        /**
         * Get expectation values for all stochastic observations.
         * @return The expectations.
         */
        double[] getExpectations();

        /**
         * Evaluate the PDF for stochastic observations, given the values for those observation.
         * @param values values for observation's PDF-evaluation.
         * @return The PDF evaluations.
         */
        double evaluatePDF(double[] values);

        /**
         * Evaluate the PDF for stochastic observations, given the values for those observation.
         * @param values values for observation's PDF-evaluation.
         * @return The PDF evaluations.
         */
        double[] evaluateMarginalPDFs(double[] values);

        /**
         * Get the covariance matrix (as a vector) for the stochastic observations.
         * @return The covariance matrix.
         */
        ISqrtCovariance getSqrtCovariance();

        /**
         * Get the standard deviation for each each stochastic observation.
         * @return The standard deviations.
         */
        double[] getStandardDeviations();

        ITime[] Times { get; }

        /**
         * free the Stochastic Observer.
         */
        void free();

        /**
         * Get the observation descriptions.
         * @return The Observation Descriptions 
         */
        IObservationDescriptions getObservationDescriptions();
    }
}
