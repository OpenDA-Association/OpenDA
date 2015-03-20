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
package org.openda.algorithms;

import org.openda.interfaces.*;

public interface LeastSquaresCostFunction extends ICostFunction{

    /**
     * Get mean and covariance for the parameters
     * @return StochVector with description of uncertainty for the parameters
     */
    public abstract IStochVector getParameterUncertainty();
    
    /**
     * Get predictions corresponding to the last call to evaluate so far
     * @return Vector with predictions
     */
    public abstract IVector getLastPredictions();

    /**
     * Get uncertainty for the observations
     * @return StochVector with observation uncertainties
     */
    public abstract IStochVector getObservationUncertainty();
    
    /**
     * Use background term for cost function, i.e. add Jb = (p-p0)'/B(p-p0)
     * @param boolean to turn backgroundterm on(true) of off(false)
     */
    public abstract void setBackgroundTerm(boolean onIsTrue);
    
    /**
     * Test if background term is in use for this cost function
     * @return
     */
    public abstract boolean doAddBackgroundTerm();
    
    /**
     * Get predictions corresponding to the lowest cost value evaluated so far
     * @return Vector with predictions
     */
    public abstract IVector getOptimalPredictions();
    
    /**
     * Get predictions for each function evaluation until now
     * @return Array of Vectors one for each evaluation predictions
     */
    public abstract IVector[] getAllPredictions();
    
    /**
     * Make a copy of the costfunction.
     * @return cloned function
     */
    public LeastSquaresCostFunction clone();


    /* 
     * copied from CostFunction
     * 
     * */
    
	/*
	 * Evaluate the costfunction for some parameters
	 * @param p : Vector with parameters
	 * @return cost as double
	 */
	//public abstract double evaluate(Vector p);
	
	/*
	 * Get all the costValues evaluated so far
	 * @return costvalue for each evaluation
	 */
	//public abstract Vector getCosts();
	
	/*
	 * Get the parameters used for each evaluation
	 * @return array of parameters, each as a Vector 
	 */
	//public abstract Vector[] getParameters();
	
	/*
	 * Get optimal costs
	 * @return optimal cost as double
	 */
	//public abstract double getOptimalCost();
	
	/*
	 * Get parameters leading to optimal cost
	 * @return optimal parameters as Vector
	 */
	//public abstract Vector getOptimalParameters();

	/*
	 *  Output info about this costfunction
	 * @return
	 */
	//public abstract String toString();

}
