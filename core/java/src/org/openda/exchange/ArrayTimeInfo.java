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
package org.openda.exchange;
import org.openda.interfaces.IArray;
import org.openda.utils.Array;

/**
 * Time info for Array based storage in ExchangeItems. 
 * 
 * @author verlaanm
 *
 */
public class ArrayTimeInfo extends TimeInfo {

	private int timeIndex=0;
	
	/**
	 * Constructor for time info for IArray based values.
	 * The index of time in IArray with Values. Often time is the first index (=0)
	 * This can also be considered as a pointer to the dimension corresponding to time
	 * in the multidimensional values array.
	 * @param times MJD's
	 * @param timeIndex pointer to dimension for values
	 */
	public ArrayTimeInfo(double[] times, int timeIndex){
		super(times);
		this.timeIndex=timeIndex;
	}
	
	/**
	 * One dimensional Array with times. Each value corresponds to a slice in the values array.
	 * @return 1D times array
	 */
	public IArray getTimeArray(){
		double[] times= this.getTimes();
		return new Array(times,new int[]{times.length},false);
	}
	
	/**
	 * Index of time in IArray with Values. Often time is the first index (=0)
	 * This can also be considered as a pointer to the dimension corresponding to time
	 * in the multidimensional values array.
	 * @return index
	 */
	public int getTimeIndex(){
		return this.timeIndex;
	}
	
	/**
	 * Set value for time index, see getTimeIndex()
	 * @param timeIndex
	 */
	public void setTimeIndex(int timeIndex){
		this.timeIndex=timeIndex;
	}
}
