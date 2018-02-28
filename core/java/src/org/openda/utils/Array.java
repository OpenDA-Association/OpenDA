/* OpenDA v2.4.3 
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

import java.util.ArrayList;
import java.util.Arrays;

import org.openda.interfaces.IArray;

/**
 * This is an implementation of an array of doubles with multiple dimensions, a is eg available in fortran or
 * matlab. It is written as an interface to allow for other implementations with highly optimized code or using parallel
 * computing.
 * 
 * Assumptions:
 * - The ordering of double[] java-arrays is with the first dimension running slowest, eg the array [[1,2],[3,4],[5,6]]
 *   is returned/set as [1,2,3,4,5,6] , this is consistent with conventions in c or java.
 * - Indices start at 0, consistent with c/java conventions.
 * - The index for dimension starts counting at 0, thus 0 denotes the first dimension.
 * 
 * @author verlaanm
 *
 */
public class Array implements IArray {

	//settings
	final static int maxDimensions = 20;

	//storage array
	private double[] values=null;
	private int[] dimensions=null;

	/**
	 * Constructor based on java arrays as input.
	 * @param values
	 * @param dimensions
	 * @param copyValues
	 */
	public Array(double[] values, int[] dimensions, boolean copyValues){
		if (values == null) {
			throw new IllegalArgumentException("values is null.");
		}
		if (dimensions == null) {
			throw new IllegalArgumentException("dimensions is null.");
		}

		//check size
		int rank=dimensions.length;
		int numberOfValues=dimensions[0];
		for(int i=1;i<rank;i++){
			numberOfValues*=dimensions[i];
		}
		if(numberOfValues!=values.length){
			throw new RuntimeException("DoublesArray constructor: The number of values does not match the dimensions");
		}
		//copy data
		if(!copyValues){
			this.values=values; 
		}else{
			this.values = new double[values.length];
			System.arraycopy(values, 0, this.values, 0, values.length);
		}
		this.dimensions=new int[dimensions.length];
		System.arraycopy(dimensions, 0, this.dimensions, 0, dimensions.length);
	}

	public Array(double[] values){
		this(values,new int[]{values.length},true);
	}
	
	public Array(int[] dimensions){
		int rank=dimensions.length;
		int numberOfValues=dimensions[0];
		for(int i=1;i<rank;i++){
			numberOfValues*=dimensions[i];
		}
		this.values = new double[numberOfValues];
		this.dimensions=new int[dimensions.length];
		System.arraycopy(dimensions, 0, this.dimensions, 0, dimensions.length);
	}

	public Array(int dimension){
		this.values = new double[dimension];
		this.dimensions=new int[]{dimension};
		System.arraycopy(dimensions, 0, this.dimensions, 0, dimensions.length);		
	}
	/**
	 * Constructor based on IArray as input.
	 * @param sourceArray
	 */
	public Array(IArray sourceArray){
		this.values=sourceArray.getValuesAsDoubles(true); 
		this.dimensions=sourceArray.getDimensions();
	}

	/**
	 * Constructor based on formatted String as input. Format is '{{1,2},{3,4}}' 
	 * @param source
	 */
	public Array(String source){
		int stringIndex=0;
		ArrayList<Double> valuesList= new ArrayList<Double>();
		int[] counter = new int[maxDimensions];
		int[] dims    = new int[maxDimensions];
		int curDim=-1; //current dimension 
		int rank=0;
		while(stringIndex<source.length()){
			if(source.charAt(stringIndex)=='{'){
				curDim++;
				if(curDim==rank) rank=curDim+1;
				stringIndex++;
			}else if(source.charAt(stringIndex)=='}'){
				if(counter[curDim]>dims[curDim]) dims[curDim]=counter[curDim];
				counter[curDim]=0;
				curDim--;
				if(curDim<-1){
					throw new RuntimeException("Too many closing brackets in array at position="+stringIndex
							+" in string="+source);
				}
				stringIndex++;
			}else if(source.charAt(stringIndex)==','){
				counter[curDim]++;
				stringIndex++;
			}else{ //try to find a number
				int indexEnd=source.indexOf('}', stringIndex);
				// if a comma comes first
				int indexComma=source.indexOf(',', stringIndex);
				if((indexComma>=0)&(indexComma<indexEnd)){
					indexEnd = indexComma;
				}
				String numberString=source.substring(stringIndex, indexEnd);
				double value;
				try {
					value = Double.parseDouble(numberString);
				} catch (NumberFormatException e) {
					throw new RuntimeException("Problems parsing array at position="+stringIndex
							+"while processing number "+numberString);
				}
				valuesList.add(value);
				stringIndex=indexEnd;
			}
		}
		// store values
		int n=valuesList.size();
		this.values = new double[n];
		for(int i=0;i<n;i++){
			this.values[i]=valuesList.get(i);
		}
		//store dimensions
		this.dimensions = new int[rank];
		for(int i=0;i<rank;i++){
			this.dimensions[i]=dims[i]+1;
		}
	}

	/**
	 * Get number of dimensions, eg 1 for a vector and 2 for a matrix
	 * @return rank of the array
	 */
	public int getNumberOfDimensions(){
		return this.dimensions.length;
	}

	/**
	 * Get the size of the array for each dimension. The length of the return value equals
	 * the number of dimensions.
	 * @return size of array, eg [3,4,10]
	 */
	public int[] getDimensions(){
		int n=this.dimensions.length;
		int[] result = new int[n];
		System.arraycopy(this.dimensions, 0, result, 0, n);
		return result;
	}

	/**
	 * Total number of elements in an array. This is equal to the product of the values
	 * returned by getDimension and is thus just for c
	 * @return
	 */
	public int length(){
		int rank=dimensions.length;
		int numberOfValues=dimensions[0];
		for(int i=1;i<rank;i++){
			numberOfValues*=dimensions[i];
		}
		return numberOfValues;
	}

	/**
	 * Get all values of this array as an array of doubles.
	 * @return The values as an array of doubles
	 */
	public double[] getValuesAsDoubles() {
		return getValuesAsDoubles(true);
	}

	/**
	 * Get all values of an array. The values are guaranteed be a copy if
	 * copyValues==true or may be a reference if copyValues==false.
	 * Note that the values are never guaranteed to be a reference and can not be used
	 * to change the array. 
	 * @param copyValues
	 * @return
	 */
	public double[] getValuesAsDoubles(boolean copyValues){
		double[] result=null;
		if(copyValues){
			int n=this.values.length;
			result=new double[n];
			System.arraycopy(this.values, 0, result, 0, n);
		}else{
			result=this.values;
		}
		return result;
	}


	/**
	 * Get a part of the values from the array. 
	 * Eg let a=[[1,2,3][4,5,6]] then getValuesAsDoubles(1,2)
	 * returns [2,3]
	 * Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
	 * @param firstIndex
	 * @param lastIndex
	 * @return
	 */
	public double[] getValuesAsDoubles(int firstIndex, int lastIndex){
		int n=this.values.length;
		if(firstIndex<0){firstIndex+=n;}
		if(lastIndex<0){lastIndex+=n;}
		int resultLength=lastIndex-firstIndex+1;
		double[] result=new double[resultLength];
		System.arraycopy(this.values, firstIndex, result, 0, resultLength);
		return result;
	}

	/**
	 * Get a value from an array for specific indices. 
	 * Eg let a=[[1,2,3][4,5,6]] then getValueAsDouble(5)
	 * returns 6
	 * Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
     * @param index index specifier
	 * @return double value at the specific indices
	 */
	public double getValueAsDouble(int index) {
		int n=this.values.length;
		if(index<0){index+=n;}
		return this.values[index];
	}


	/**
	 * Get a value from an array for specific indices. 
	 * Eg let a=[[1,2,3][4,5,6]] then getValueAsDouble([1,2])
	 * returns 6
	 * Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
	 * @return
	 */
	public double getValueAsDouble(int[] indices){
		int index = valueIndex(indices);
		return this.values[index];
	}

	/**
	 * Set the values of this array.
	 * @param values the values as an array of doubles
	 */
	public void setValuesAsDoubles(double[] values) {
		this.values = values;
	}

	/**
	 * Set a value from an array for specific indices. 
	 * Eg let a=[[1,2,3][4,5,6]] then setValueAsDouble([1,2],60.)
	 * results in a=[[1,2,3][4,5,60.]]
	 * Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
     * @param indices index specifier
	 * @param value at the specific indices
	 */
	public void setValueAsDouble(int[] indices, double value){
		int index = valueIndex(indices);
		this.values[index]=value;
	}

	/**
	 * Set part of the values of this array.
	 * Let a=[[1,2,3][4,5,6]] then setValuesAsDoubles(1,2,[20,30]) will result in a=[[1,20,30][4,5,6]]
	 * Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
	 * @param firstIndex specifies the start of the selection
	 * @param lastIndex specifies the end of the selection
	 * @param values that will replace the selected range of numbers
	 */
	public void setValuesAsDoubles(int firstIndex, int lastIndex, double[] values) {
		int n=this.values.length;
		if(firstIndex<0){firstIndex+=n;}
		if(lastIndex<0){lastIndex+=n;}
		int resultLength=lastIndex-firstIndex+1;
		System.arraycopy(values, 0, this.values, firstIndex, resultLength);
	}

	/**
	 * Get a value from an array for specific indices. 
	 * Eg let a=[[1,2,3][4,5,6]] then setValueAsDouble(5,60.)
	 * results in a=[[1,2,3][4,5,60.]]
	 * Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
     * @param index index specifier
	 * @param value at the specific indices
	 */
	public void setValueAsDouble(int index, double value) {
		int n=this.values.length;
		if(index<0){index+=n;}
		this.values[index]=value;
	}

    /**
     * Set whole vector equal to a constant value.
     * <p/>
     * Note:  This method can only be used if all elements of the vector
     * have the same data type.
     *
     * @param value value to set
     */
	public void setConstant(double value) {
		int n=this.values.length;
		for(int i=0;i<n;i++){
			this.values[i]=value;
		}
	}

	/**
	 * Perform a values += alpha * axpyValues</c> operation on each value in this array.
	 * @param alpha the <c>alpha</c> in <c>state variable += alpha * vector</c>.
	 * @param axpyValues the values for the axpy-operation on all values in this array.
	 */
    public void axpyOnValues(double alpha, double[] axpyValues) {
        double[] values = getValuesAsDoubles();
        for (int i = 0; i < values.length; i++) {
            values[i] += alpha * axpyValues[i];
        }
        setValuesAsDoubles(values);
    }

	/**
	 * Multiply each value in this array with the corresponding multiplication factor.
	 * @param multiplicationFactors the multiplication factors for all array values.
	 */
    public void multiplyValues(double[] multiplicationFactors) {
        double[] values = getValuesAsDoubles();
        for (int i = 0; i < values.length; i++) {
            values[i] *= multiplicationFactors[i];
        }
        setValuesAsDoubles(values);
    }

    /**
	 * Change the dimensions of an array. The new array should have the same length
	 * @param dimensions
	 */
	public void reshape(int[] dimensions){
		int rank=dimensions.length;
		int numberOfValues=dimensions[0];
		for(int i=1;i<rank;i++){
			numberOfValues*=dimensions[i];
		}
		if(numberOfValues!=this.length()){
			throw new RuntimeException("Can not reshape to a different length. "
					+"Length "+this.length()+" to "+numberOfValues);
		}

	}

	/**
	 * Is it allowed to write beyond the current end of the array. Some implementations, especially ones writing to disk
	 * can increase the size of the first dimension of the array. 
	 * 
	 * Multiple growing dimensions requires moving the existing data if it implemented with a one-dimensional storage, 
	 * eg as double[]. This is NOT SUPPORTED.  
	 *
	 * @return 
	 */
	public boolean allowsGrowingFirstDimension(){
		return false;
	}

	/**
	 * Get part of the array by selection of a subset in one dimension.
	 * Eg. a=[[1,2,3],[4,5,6]] a.getSlice(0,0) returns [1,2,3] 
	 * Note that the number of dimensions IS reduced by one.
	 * @param dimension
	 * @param index
	 * @return
	 */
	public IArray getSlice(int dimension, int index){
		// indexing eg A(:,: ,index,:,:  )
		//             A(left,at   ,right)
		int[] dimsLeft = new int[dimension];
		System.arraycopy(this.dimensions, 0, dimsLeft, 0, dimension);
		int rankRight=this.dimensions.length-dimension-1;
		int[] dimsRight= new int[rankRight];
		System.arraycopy(this.dimensions, dimension+1, dimsRight, 0, rankRight);
		int[] dims=intConcat(dimsLeft, dimsRight);
		Array result = new Array(dims);
		
		// prepare for copy
		int blockSize = 1;
		for(int i=0;i<dimsRight.length;i++) blockSize*=dimsRight[i];
		int offset = index*blockSize;
		int stride = dimensions[dimension]*blockSize;
		int count  = 1;
		for(int i=0;i<dimsLeft.length;i++) count*=dimsLeft[i];
		// copy values
		int srcPos = offset; 
		int destPos = 0;
		for(int i=0;i<count;i++){
			System.arraycopy(values, srcPos, result.values, destPos, blockSize);
			srcPos+=stride;
			destPos+=blockSize;
		}
		return result;
	}
	
	private int[] intConcat(int[] part1,int[] part2){
		int[] result = new int[part1.length+part2.length];
		System.arraycopy(part1, 0, result, 0, part1.length);
		System.arraycopy(part2, 0, result, part1.length, part2.length);
		return result;
	}
	

	/**
	 * Get part of the array by selection of a subset in one dimension.
	 * Eg. a=[[1,2,3],[4,5,6],[7,8,9]] a.getSlice(0,0,1) returns [[1,2],[3,4]] 
	 * Note that the number of dimensions is NOT reduced by one.
	 * @param dimension
	 * @param minIndex
	 * @param maxIndex
	 * @return
	 */
	public IArray getSlice(int dimension, int minIndex, int maxIndex){
		// indexing eg A(:,: ,index,:,:  )
		//             A(left,at   ,right)
		int[] dimsLeft = new int[dimension];
		System.arraycopy(this.dimensions, 0, dimsLeft, 0, dimension);
		int rankRight=this.dimensions.length-dimension-1;
		int[] dimsRight= new int[rankRight];
		System.arraycopy(this.dimensions, dimension+1, dimsRight, 0, rankRight);
		int[] dims=new int[dimensions.length];
		System.arraycopy(this.dimensions, 0, dims, 0, dimensions.length);
		dims[dimension]=maxIndex-minIndex+1;
		Array result = new Array(dims);
		
		// prepare for copy
		int blockSize = 1;
		for(int i=0;i<dimsRight.length;i++) blockSize*=dimsRight[i];
		int minOffset = minIndex*blockSize;
		int stride = dimensions[dimension]*blockSize;
		int count  = 1;
		for(int i=0;i<dimsLeft.length;i++) count*=dimsLeft[i];
		// copy values
		int srcPos = minOffset; 
		int destPos = 0;
		for(int i=0;i<count;i++){
			for(int j=0;j<dims[dimension];j++){
				System.arraycopy(values, srcPos, result.values, destPos, blockSize);
				srcPos+=blockSize;
				destPos+=blockSize;
			}
			srcPos+=stride;
			destPos+=blockSize;
		}
		return result;
	}

	/**
	 * Get part of the array by selection of a subset in one dimension.
	 * Eg. a=[[1,2,3],[4,5,6],[7,8,9]] a.getSlice(0,0,1) returns [1,2,3,4] 
	 * @param dimension
	 * @param minIndex
	 * @param maxIndex
	 * @return
	 */
	public double[] getSliceAsDoubles(int dimension, int minIndex, int maxIndex){
		int numberOfValues=this.length()/this.dimensions[dimension];
		IArray slice = getSlice(dimension, minIndex, maxIndex);
		double[] result=slice.getValuesAsDoubles(false);
		return result;
	}

	/**
	 * Set the values of a part of an array. 
	 * Eg. a=[[1,2,3],[4,5,6],[7,8,9]] and a.setSlice([11,12,13],1,1) 
	 * sets the second column a=[[1,11,3],[4,12,6],[7,13,9]]
	 * Note that the dimension of the slice is one smaller than for the array.
	 * @param slice
	 * @param dimension
	 * @param index
	 */
	public void setSlice(IArray slice, int dimension, int index){
		// indexing eg A(:,: ,index,:,:  )
		//             A(left,at   ,right)
		int[] dimsLeft = new int[dimension];
		System.arraycopy(this.dimensions, 0, dimsLeft, 0, dimension);
		int rankRight=this.dimensions.length-dimension-1;
		int[] dimsRight= new int[rankRight];
		System.arraycopy(this.dimensions, dimension+1, dimsRight, 0, rankRight);
		
		// prepare for copy
		int blockSize = 1;
		for(int i=0;i<dimsRight.length;i++) blockSize*=dimsRight[i];
		int offset = index*blockSize;
		int stride = dimensions[dimension]*blockSize;
		int count  = 1;
		for(int i=0;i<dimsLeft.length;i++) count*=dimsLeft[i];
		// copy values
		int srcPos = 0; 
		int destPos = offset;
		double[] sliceValues = slice.getValuesAsDoubles(false);
		for(int i=0;i<count;i++){
			System.arraycopy(sliceValues, srcPos, values, destPos, blockSize);
			destPos+=stride;
			srcPos+=blockSize;
		}
	}

	public void setSlice(double[] slice, int dimension, int index) {
        Array sliceArray = new Array(slice, new int[]{slice.length}, false);
		setSlice(sliceArray, dimension, index);
	}

	/**
	 * Set the values of a part of an array. 
	 * Eg. a=[[1,2,3],[4,5,6],[7,8,9]] and a.setSlice([[11,12,13],[14,15,16]],1,1,2) 
	 * sets the second and third columns a=[[1,11,14],[4,12,15],[7,13,16]]
	 * Note that the dimension of the slice is the same as for the array.
	 * @param slice
	 * @param dimension
	 * @param minIndex
	 * @param maxIndex
	 */
	public void setSlice(IArray slice, int dimension, int minIndex, int maxIndex){
		double[] sliceValues = slice.getValuesAsDoubles(false);
		setSlice(sliceValues,dimension,minIndex,maxIndex);
	}

	/**
	 * Set the values of a part of an array. 
	 * Eg. a=[[1,2,3],[4,5,6],[7,8,9]] and a.setSlice([11,12,13,14,15,16],1,1,2) 
	 * sets the second and third columns a=[[1,11,14],[4,12,15],[7,13,16]]
	 * Note that the dimension of the slice is the same as for the array.
	 * @param slice
	 * @param dimension
	 * @param minIndex
	 * @param maxIndex
	 */
	public void setSlice(double[] slice, int dimension, int minIndex, int maxIndex){
		// indexing eg A(:,: ,index,:,:  )
		//             A(left,at   ,right)
		int[] dimsLeft = new int[dimension];
		System.arraycopy(this.dimensions, 0, dimsLeft, 0, dimension);
		int rankRight=this.dimensions.length-dimension-1;
		int[] dimsRight= new int[rankRight];
		System.arraycopy(this.dimensions, dimension+1, dimsRight, 0, rankRight);
		int[] dims=new int[dimensions.length];
		System.arraycopy(this.dimensions, 0, dims, 0, dimensions.length);
		dims[dimension]=maxIndex-minIndex+1;
		
		// prepare for copy
		int blockSize = 1;
		for(int i=0;i<dimsRight.length;i++) blockSize*=dimsRight[i];
		int minOffset = minIndex*blockSize;
		int stride = dimensions[dimension]*blockSize;
		int count  = 1;
		for(int i=0;i<dimsLeft.length;i++) count*=dimsLeft[i];
		// copy values
		int destPos = minOffset; 
		int srcPos = 0;
		for(int i=0;i<count;i++){
			for(int j=0;j<dims[dimension];j++){
				System.arraycopy(slice, srcPos, this.values, destPos, blockSize);
				destPos+=blockSize;
				srcPos+=blockSize;
			}
			destPos+=stride;
			srcPos+=blockSize;
		}
	}	

	/**
	 * Convert indices in multiple dimensions to position in the one-dimensional array as
	 * returned eg by getValuesAsDoubles
	 * Eg. a=[[1,2,3],[4,5,6],[7,8,9]] then valueIndex([1,0]) returns 3 which points to the value 4 here.  
	 * @param indices
	 * @return position
	 */
	public int valueIndex(int[] indices){
		if(this.dimensions.length!=indices.length){
			throw new RuntimeException("valueIndex: number of indices does not match dimensions of array");
		}
		int blockSize=1;
		int result=0;
		for(int i=indices.length-1;i>=0;i--){
			int indexI=indices[i];
			if(indexI<0){indexI+=this.dimensions[i];}
			result+=indexI*blockSize;
			blockSize*=this.dimensions[i];
		}
		return result;
	}

	/**
	 * Convenience function for valueIndex for small rank arrays
	 * @param i
	 * @param j
	 * @return valueIndex([i,j])
	 */
	public int index2(int i,int j){
		return valueIndex(new int[]{i,j});
	}

	/**
	 * Convenience function for valueIndex for small rank arrays
	 * @param i
	 * @param j
	 * @param k
	 * @return valueIndex([i,j,k])
	 */
	public int index3(int i,int j, int k){
		return valueIndex(new int[]{i,j,k});
	}

	/**
	 * Write contents as String, eg, {{1,2,3},{4,5,6}}
	 */
	public String toString(){
		StringBuffer output = new StringBuffer();
		writeString(0,0,output);
		return output.toString();
	}

	/**
	 * Recursive write routine
	 * @param level 
	 * @param output
	 */
	private int writeString(int level, int startIndex, StringBuffer output){
		int index = startIndex;
		int k= this.dimensions.length;
		output.append("{");
		for(int i=0;i<dimensions[level];i++){
			if(i>0){
				output.append(",");
			}
			if(level==k-1){
				output.append(values[index]);
				index++;
			}else{
				index = writeString(level+1,index,output);
			}
		}
		output.append("}");
		return index;
	}

	/**
	 * Scale all values with some factor in place.
	 * @param factor
	 */
	public void scale(double factor){
		int n=this.values.length;
		for(int i=0;i<n;i++){
			this.values[i]*=factor;
		}
	}
	
	/*
	 * Utilities
	 */
	
	/**
	 * Generate array for a 1D linear range eg {0.0, 0.1, 0.2, 0.3, 0.4} 
	 * @param start first value
	 * @param length number of values
	 * @param step increment between values
	 * @return 
	 */
	public static Array Range1(double start, int length, double step){
		double values[] = new double[length];
		for(int i=0;i<length;i++){
			values[i] = start+i*step;
		}
		return new Array(values);
	}

	
	public boolean equals(Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass() || o.hashCode() != hashCode()) {
			return false;
		}

		Array that = (Array) o;
		if (this.dimensions == null) {
			if (that.dimensions != null) {
				return false;
			}
		} else {//if this.dimensions != null.
			if (!Arrays.equals(this.dimensions, that.dimensions)) {
				return false;
			}
		}
		if (this.values == null) {
			if (that.values != null) {
				return false;
			}
		} else {//if this.values != null.
			if (!Arrays.equals(this.values, that.values)) {
				return false;
			}
		}

		return true;
	}

	
	public int hashCode() {
		int h = Arrays.hashCode(this.dimensions);
		h = 31 * h + Arrays.hashCode(this.values);
		return h;
	}
}
