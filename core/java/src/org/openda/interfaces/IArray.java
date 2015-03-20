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

package org.openda.interfaces;

import java.io.Serializable;

/**
 * Abstract version of an array of doubles with multiple dimensions, as for instance available in Fortran or
 * Matlab. It is written as an interface to allow for other implementations with highly optimized code or using parallel
 * computing.
 *<p>
 * Assumptions:
 * <ul>
 * <li> The ordering of double[] java-arrays is with the first dimension running slowest, eg the array [[1,2],[3,4],[5,6]]
 *   is returned/set as [1,2,3,4,5,6] , this is consistent with conventions in c or java.
 * <li>Indices start at 0, consistens with c/java conventions.
 * <li>The index for dimension starts counting at 0, thus 0 denotes the first dimension.
 * <li>Negative indices are allowed to start counting from the end, eg -1 denotes the last element.
 * </ul>
 *
 * @author verlaanm
 *
 */
public interface IArray extends Serializable{

	/**
	 * Get number of dimensions, i.e. 1 for a vector and 2 for a matrix
	 * @return rank of the array
	 */
	public int getNumberOfDimensions();

	/**
	 * Get the size of the array for each dimension. The length of the return value equals
	 * the number of dimensions.
	 * @return size of array, eg [3,4,10]
	 */
	public int[] getDimensions();

	/**
	 * Total number of elements in an array. This is equal to the product of the values
	 * returned by getDimension and is thus just for c
	 * @return total number of elements
	 */
	public int length();

	/**
	 * Get all values of this array as an array of doubles.
	 * @return array of doubles
	 */
	public double[] getValuesAsDoubles();

    /**
	 * Get all values of an array. The values are guaranteed be a copy if
	 * copyValues==true or may be a reference if copyValues==false.
	 * Note that the values are never guaranteed to be a reference and can not be used
	 * to change the array. 
	 * @param copyValues values to be copied
	 * @return array of doubles
	 */
	public double[] getValuesAsDoubles(boolean copyValues);

	
	/**
	 * Get a part of the values from the array. 
	 * Let a=[[1,2,3][4,5,6]] then getValuesAsDoubles(1,2) returns [2,3]
	 * Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
	 * @param firstIndex specifies the start of the selection
	 * @param lastIndex specifies the end of the selection
	 * @return array of doubles
	 */
	public double[] getValuesAsDoubles(int firstIndex, int lastIndex);

	/**
	 * Get a value from an array for specific indices. 
	 * Eg let a=[[1,2,3][4,5,6]] then getValueAsDouble(5)
	 * returns 6
	 * Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
     * @param index index specifier
	 * @return double value at the specific indices
	 */
	public double getValueAsDouble(int index);

	/**
	 * Get a value from an array for specific indices. 
	 * Eg let a=[[1,2,3][4,5,6]] then getValueAsDouble([1,2])
	 * returns 6
	 * Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
     * @param indices index specifier
	 * @return double value at the specific indices
	 */
	public double getValueAsDouble(int[] indices);

	
    /**
     * Set whole vector equal to a constant value.
     * <p/>
     * Note:  This method can only be used if all elements of the vector
     * have the same data type.
     *
     * @param value value to set
     */
    void setConstant(double value);

    /**
	 * Set the values of this array.
	 * @param values the values as an array of doubles
	 */
	public void setValuesAsDoubles(double[] values);

	/**
	 * Set part of the values of this array.
	 * Let a=[[1,2,3][4,5,6]] then setValuesAsDoubles(1,2,[20,30]) will result in a=[[1,20,30][4,5,6]]
	 * Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
	 * @param firstIndex specifies the start of the selection
	 * @param lastIndex specifies the end of the selection
	 * @param values that will replace the selected range of numbers
	 */
	public void setValuesAsDoubles(int firstIndex, int lastIndex, double[] values);

	/**
	 * Get a value from an array for specific indices. 
	 * Eg let a=[[1,2,3][4,5,6]] then setValueAsDouble(5,60.)
	 * results in a=[[1,2,3][4,5,60.]]
	 * Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
     * @param index index specifier
	 * @param value at the specific indices
	 */
	public void setValueAsDouble(int index, double value);

	/**
	 * Set a value from an array for specific indices. 
	 * Eg let a=[[1,2,3][4,5,6]] then setValueAsDouble([1,2],60.)
	 * results in a=[[1,2,3][4,5,60.]]
	 * Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
     * @param indices index specifier
	 * @param value at the specific indices
	 */
	public void setValueAsDouble(int[] indices, double value);
	
	/**
	 * Perform a values += alpha * axpyValues</c> operation on each value in this array.
	 * @param alpha the <c>alpha</c> in <c>state variable += alpha * vector</c>.
	 * @param axpyValues the values for the axpy-operation on all values in this array.
	 */
	public void axpyOnValues(double alpha, double[] axpyValues);

	/**
	 * Multiply each value in this array with the corresponding multiplication factor.
	 * @param multiplicationFactors the multiplication factors for all array values.
	 */
	public void multiplyValues(double[] multiplicationFactors);

	/**
	 * Change the dimensions of an array. The new array should have the same length
	 * @param dimensions the new dimensions of the array
	 */
	public void reshape(int[] dimensions);
	
	/**
	 * Is it allowed to write beyond the current end of the array. Some implementations, especially ones writing to disk
	 * can increase the size of the first dimension of the array. 
	 * 
	 * Multiple growing dimensions requires moving the existing data if it implemented with a one-dimensional storage, 
	 * eg as double[]. This is NOT SUPPORTED.  
	 *
	 * @return <code>True</code> if it is allowed to write beyond the end of the array.
	 */
	public boolean allowsGrowingFirstDimension();

	/**
	 * Get part of the array by selection of a subset in one dimension.
	 * Eg. a=[[1,2,3],[4,5,6]] a.getSlice(0,0) returns [1,2,3] 
	 * Note that the number of dimensions IS reduced by one.
	 * @param dimension specifies the dimension to select from
	 * @param index specifies the starting index for the selection
	 * @return a slice of the array
	 */
	public IArray getSlice(int dimension, int index);

	/**
	 * Get part of the array by selection of a subset in one dimension.
	 * Eg. a=[[1,2,3],[4,5,6],[7,8,9]] a.getSlice(0,0,1) returns [[1,2],[3,4]] 
	 * Note that the number of dimensions is NOT reduced by one.
	 * @param dimension specifies the dimension to select from
	 * @param minIndex specifies the start index for the selection
	 * @param maxIndex specifies the end index for the selection
	 * @return a slice of the array
	 */
	public IArray getSlice(int dimension, int minIndex, int maxIndex);

	/**
	 * Get part of the array by selection of a subset in one dimension.
	 * Eg. a=[[1,2,3],[4,5,6],[7,8,9]] a.getSlice(0,0,1) returns [1,2,3,4] 
	 * @param dimension specifies the dimension to select from
	 * @param minIndex specifies the start index for the selection
	 * @param maxIndex specifies the end index for the selection
	 * @return a slice of the array
	 */
	public double[] getSliceAsDoubles(int dimension, int minIndex, int maxIndex);

	/**
	 * Set the values of a part of an array. 
	 * Eg. a=[[1,2,3],[4,5,6],[7,8,9]] and a.setSlice([11,12,13],1,1) 
	 * sets the second column a=[[1,11,3],[4,12,6],[7,13,9]]
	 * Note that the dimension of the slice is one smaller than for the array.
	 * @param slice array containing the values to put in IArray
	 * @param dimension specifies in which dimension the values must be set
	 * @param index specifies the start index to set the values
	 */
	public void setSlice(IArray slice, int dimension, int index);

	/**
	 * Set the values of a part of an array.
	 * Eg. a=[[1,2,3],[4,5,6],[7,8,9]] and a.setSlice([11,12,13],1,1)
	 * sets the second column a=[[1,11,3],[4,12,6],[7,13,9]]
	 * Note that the dimension of the slice is one smaller than for the array.
	 * @param slice array containing the values to put in IArray
	 * @param dimension specifies in which dimension the values must be set
	 * @param index specifies the start index to set the values
	 */
	public void setSlice(double[] slice, int dimension, int index);

	/**
	 * Set the values of a part of an array. 
	 * Eg. a=[[1,2,3],[4,5,6],[7,8,9]] and a.setSlice([[11,12,13],[14,15,16]],1,1,2) 
	 * sets the second and third columns a=[[1,11,14],[4,12,15],[7,13,16]]
	 * Note that the dimension of the slice is the same as for the array.
	 * @param slice array containing the values to put in IArray
	 * @param dimension specifies in which dimension the values must be set
	 * @param minIndex specifies the start index to set the values
	 * @param maxIndex specifies the end index to set the values
	 */
	public void setSlice(IArray slice, int dimension, int minIndex, int maxIndex);

	/**
	 * Set the values of a part of an array. 
	 * Eg. a=[[1,2,3],[4,5,6],[7,8,9]] and a.setSlice([11,12,13,14,15,16],1,1,2) 
	 * sets the second and third columns a=[[1,11,14],[4,12,15],[7,13,16]]
	 * Note that the dimension of the slice is the same as for the array.
	 * @param slice array containing the values to put in IArray
	 * @param dimension should be the same as for the array
	 * @param minIndex specifies the start index to set the values
	 * @param maxIndex specifies the end index to set the values
	 */
	public void setSlice(double[] slice, int dimension, int minIndex, int maxIndex);

	/**
	 * Convert indices in multiple dimensions to position in the one-dimensional array as
	 * returned eg by getValuesAsDoubles.
     * <br></br>
	 * Eg. a=[[1,2,3],[4,5,6],[7,8,9]] then valueIndex([1,0]) returns 3 which points to the value 4 here.  
	 * @param indices indices in the multidimensional array
	 * @return corresponding position in the one-dimensional array
	 */
	public int valueIndex(int[] indices);
}