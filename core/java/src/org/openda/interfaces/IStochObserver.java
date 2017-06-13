/* OpenDA v2.4 
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


import java.io.Serializable;

/**
 * Stochastic observer interface.
 */
public interface IStochObserver extends IConfigurable, IInstance, Serializable {

    /**
     * List of available observer types.
     */
	public enum Type {
		Assimilation, Validation
	}

	/**
     * Create an new Stochastic Observer, containing a selection of the present stochastic observer.<br>
     * The selection criterium is in the form of an SQLite query.
     * @param selection    Selection querry
     * @return             Stochastic Observer containing the required selection.
     */
    public IStochObserver createSelection(String selection);

    /**
    *
    * Create an new Stochastic Observer, containing a selection of the present stochastic observer.<br>
    * The selection criteria is a timeSpan. The start time of the interval is not included, the end time is
    * included, i.e. t_start<t<=t_end
    * @param selectionTimes    timeSpan with selection.
    * @return                  Stochastic Observer containing the required selection.
    */
   public IStochObserver createSelection(ITime selectionTimes);

	/**
	 * Create an new Stochastic Observer, containing a selection of the present stochastic observer.<br>
	 * The selection criteria is the type of observations: assimilation or validation
	 *
	 * @param observationType The requested type of observations.
	 * @return Stochastic Observer containing the required selection.
	 */
	public IStochObserver createSelection(Type observationType);

    /**
     * Create an new Selector, containing a selection of the present stochastic observer.<br>
     * The same selection can used to make selection for the corresponding vectors of observation and prediction.
     * The selection criteria is the type of observations: assimilation or validation
     *
     * @param observationType The requested type of observations.
     * @return Selector containing the required indices selection.
     */
	public ISelector createSelector(Type observationType);
	//        IVector selectedValues = ISelector.apply(IVector allValues)
	//   -- this is similar to IRelationTable

	/**
     * Total number of observation values in the Stochastic Observer.
     * @return The number of observations.
     */
    public int getCount();

    /**
     * Get the values for all observations.
     * @return The observed values.
     */
    public IVector getValues();

    /**
     * Get realization values for all observations, for one ensemble member.
     * @return The realizations.
     */
    public IVector getRealizations();

    /**
     * Get expectation values for all stochastic observations.
     * @return The expectations.
     */
    public IVector getExpectations();

    /**
     * Evaluate the PDF for stochastic observations, given the values for those observation.
     * @param values values for observation's PDF-evaluation.
     * @return The PDF evaluations.
     */
    public double evaluatePDF(IVector values);

    /**
     * Evaluate the PDF for stochastic observations, given the values for those observation.
     * @param values values for observation's PDF-evaluation.
     * @return The PDF evaluations.
     */
    public IVector evaluateMarginalPDFs(IVector values);

    /**
     * Get the covariance matrix (as a vector) for the stochastic observations.
     * @return The covariance matrix.
     */
    public ISqrtCovariance getSqrtCovariance();

    /**
     * Get the standard deviation for each stochastic observation.
     * @return The standard deviations.
     */
    public IVector getStandardDeviations();

    /**
     * Get all different times in increasing order. There is at least one observation for each time.
     * It is likely that observer.createSelection(time[i]) will be used to walk through the
     * observations. The implementation of the stochobserver should garantee that al observations are
     * returned in exactly one batch this way.
     * @return array with all uniquely different times.
     */
    public ITime[] getTimes();

    /**
     * free the Stochastic Observer.
     */
    public void free();

    /**
     * Get the observation descriptions.
     * @return The Observation Descriptions 
     */
    IObservationDescriptions getObservationDescriptions();

    /**
     * Set the instance's parent
     * @param parent Parent for the stoch observer (usually the parent is the Algorithm)
     */
    public void setParent(IInstance parent);
}
