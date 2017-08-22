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
package org.openda.algorithms;
import org.openda.interfaces.IVector;

public interface ICostFunction {


//	/**
//	 * Evaluate the costfunction for some parameters
//	 * @param p : Vector with parameters
//	 * @return cost as double
//	 */
//	public double evaluate(IVector p);

	/**
	 * Evaluate the costfunction for some parameters
	 * @param p : Vector with parameters
	 * @param context : String of information on from where evaluate is called
	 * @return cost as double
	 */
	public double evaluate(IVector p, String context);

	/**
	 * Get capabilities for parallel computing.
	 * @return
	 */
	public boolean getTryParallel();
	public void setTryParallel(boolean tryParallel);

	
	/**
	 * Try to perform some computations for the next evaluation. This function need not be 
	 * called by all algorithms. The next evaluate MUST contain the same vector. This method
	 * is mainly intended to allow for parallel computing.
	 * One can also provide an emplty method if no parallel computing is required.
	 * @param p
	 */
	public void prepare(IVector p);
	
	/**
	 * Get all the costValues evaluated so far
	 * @return costvalue for each evaluation
	 */
	public IVector getCosts();
	
	/**
	 * Get the parameters used for each evaluation
	 * @return array of parameters, each as a Vector 
	 */
	public IVector[] getParameters();
	
	/**
	 * Get optimal costs
	 * @return optimal cost as double
	 */
	public double getOptimalCost();
	
	/**
	 * Get parameters leading to optimal cost
	 * @return optimal parameters as Vector
	 */
	public IVector getOptimalParameters();
	
	/**
	 * Get factor used for premultiplication. This is often used to add
	 * a factor of 0.5 to the cost e.g. J = 0.5 * (observed - predicted)*(observed - predicted)/covariance
	 * @return multiplication factor
	 */
	public double getMultiplicationFactor();

	/**
	 *  Output info about this costfunction
	 * @return
	 */
    public void writeResults();
    
    /**
     * Make a copy of the costfunction.
     * @return cloned function
     */
    public ICostFunction clone();
}
