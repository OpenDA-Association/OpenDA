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
package org.openda.utils;


/**
 * This is an implementation of a special counter that is used in distributed computations
 * The increment of this counter is determined by static increment
 * This counter can be used to replace static counter variables in round robin parallel distribution
 *
 * Assumptions:
 * - The ordering of double[] java-arrays is with the first dimension running slowest, eg the array [[1,2],[3,4],[5,6]]
 *   is returned/set as [1,2,3,4,5,6] , this is consistent with conventions in c or java.
 * - Indices start at 0, consistent with c/java conventions.
 * - The index for dimension starts counting at 0, thus 0 denotes the first dimension.
 *
 * @author Nils van Velzen
 *
 */

public class DistributedCounter {
	public static int increment = 1;
	public static int offset    = 0;
	private int val;

	/**
     * Create new counter the counter is initialized with the (static) value "offset"
     */
	public DistributedCounter(){
		val=offset;
	}

	/**
     * Create new counter the counter is initialized with given value plus the (static) value offset
	 * Typically this constructor is used for counter (globally) starting at 1 and not 0.
     *
     * @param initialValue Start value of (global) counter
     */
	public DistributedCounter(int initialValue){
		val=initialValue+offset;
	}

	/**
     * Return the current value of the counter
     *
     * @return value of the counter
     */
	public int val(){
		return val;
	}

	/**
     * Increment the counter with (static) value "increment"
     *
     * @return value of the counter
     */
	public void inc(){
		val+=increment;
		System.out.print("New value Distributed counter "+val+"\n");

	}

}
