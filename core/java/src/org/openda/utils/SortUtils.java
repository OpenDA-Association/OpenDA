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
package org.openda.utils;
import java.util.ArrayList;
import java.util.Arrays;

import org.openda.interfaces.IVector;

/**
 * Some utilities for sorting and merging arrays
 * @author verlaanm
 *
 */
public class SortUtils {

	//merge type
	public enum MergeType {left, union, intersection};

	/**
	 * Sort an array and return the result as an array of indices
	 * @param values : to be sorted
	 * @return indices : first value points to smallest value and last one to largest
	 */
	public static int[] sortedIndex(double[] values){

		class indexValuePair
		{
			int index;
			double value;
			public indexValuePair(int index, double value)
			{
				this.index = index;
				this.value = value;
			}
		}
		class ValueComparator implements java.util.Comparator<indexValuePair>
		{
			public int compare(indexValuePair o1, indexValuePair o2)
			{
				return new Double(((indexValuePair)o1).value).compareTo(new Double(((indexValuePair)o2).value));
			}
		}

		int[] result = new int[values.length];
		java.util.ArrayList<indexValuePair> sortedValues = new java.util.ArrayList<indexValuePair>();
		for(int i=0;i<values.length;i++){
			sortedValues.add(new indexValuePair(i,values[i]));
		}
		ValueComparator vc = new ValueComparator();
		java.util.Collections.sort(sortedValues, vc);
		int j=0;
		for(indexValuePair ivp : sortedValues){
			result[j]=ivp.index;
			j++;
		}
		return result;
	}

	/**
	 * Use indices from sorting to sort an additions array of Vectors
	 * Vectors are not copied, only the references are switched
	 * @param vs Vectors to be sorted
	 * @param index indices from sorting
	 * @return sorted vectors
	 */
	public static IVector[] applyIndexToVectors(IVector[] vs,int[] index){
		IVector[] result = new IVector[index.length];
		for(int i=0;i<index.length;i++){
			int j=index[i];
			if(j>=0){
				result[i] = vs[j];				
			}else{
				result[i] = null;
			}
		}
		return result;
	}

	/**
	 * Use indices from sorting to sort an additions array of doubles
	 * Sorts on a copy.
	 * @param cs doubles to be sorted
	 * @param index indices from sorting
	 * @param missingValueDefinition value to use if index<0
	 * @return sorted doubles
	 */
	public static double[] applyIndexToDoubles(double[] cs,int[] index, double missingValueDefinition){
		double[] result = new double[index.length];
		for(int i=0;i<index.length;i++){
			int j= index[i];
			if(j>=0){
				result[i] = cs[j];
			}else{
				result[i] = missingValueDefinition;
			}
		}
		return result;
	}

	/**
	 * Use indices from sorting to sort an additions array of int's
	 * Sorts on a copy.
	 * @param cs ints to be sorted
	 * @param index indices from sorting
	 * @param dummy value to use if index<0
	 * @return sorted ints
	 */
	public static int[] applyIndexToInts(int[] cs,int[] index, int dummy){
		int[] result = new int[index.length];
		for(int i=0;i<cs.length;i++){
			int j= index[i];
			if(j>=0){
				result[i] = cs[j];
			}else{
				result[i] = dummy;
			}
		}
		return result;
	}

	/**
	 * Merges two double arrays. There are three types of merging: left, union, intersection
	 * @param values1 first array of values
	 * @param values2 second array of values
	 * @param mergeType
	 * @param tolerance what difference is allowed for equal values
	 * @return two index arrays, for left and right values. -1 denote non-matching value
	 */
	public static int[][] mergeDoubleIndex(double[] values1, double[] values2, MergeType mergeType, double tolerance) {
		int result[][] = new int[2][];
		ArrayList<Integer> leftIndex = new ArrayList<Integer>();
		ArrayList<Integer> rightIndex = new ArrayList<Integer>();
		if(mergeType==MergeType.left){
			for(int i=0;i<values1.length;i++){
				leftIndex.add(i);
				int j = Arrays.binarySearch(values2,values1[i]);
				if(j>=0){
					// keep j
				}else{
					j=-j-1; //insertion point 0..length
					if((j<values2.length)&&((values2[j]-values1[i])<tolerance)){ //look up
						// keep j
					}else if ((j>0)&&((values1[i]-values2[j-1])<tolerance)) { //look down
						j--;
					}else{
						j=-1; //non matching value
					}
				}
				//System.out.println("merge left i="+i+" j="+j);
				rightIndex.add(j);
			}
		}else if (mergeType==MergeType.union) {
			int i=0;
			int j=0;
			while((i<values1.length)&&(j<values2.length)){
				if(values1[i]<values2[j]){
					leftIndex.add(i);
					if((values2[j]-values1[i])<tolerance){
						rightIndex.add(j);
						i++;j++;
					}else{
						rightIndex.add(-1);
						i++;
					}
				}else{
					rightIndex.add(j);
					if((values1[i]-values2[j])<tolerance){
						leftIndex.add(i);
						i++;j++;
					}else{
						leftIndex.add(-1);
						j++;
					}
				}	
			}
			// remaining unmatched values
			while((i<values1.length)){
				leftIndex.add(i);
				rightIndex.add(-1);
				i++;
			}
			while((j<values2.length)){
				leftIndex.add(-1);
				rightIndex.add(j);
				j++;
			}
		}else if (mergeType==MergeType.intersection) {
			for(int i=0;i<values1.length;i++){
				int j = Arrays.binarySearch(values2,values1[i]);
				if(j>=0){
					leftIndex.add(i);
					rightIndex.add(j);
				}else{
					j=-j-1; //insertion point 0..length
					if((j<values2.length)&&((values2[j]-values1[i])<tolerance)){ //look up
						leftIndex.add(i);
						rightIndex.add(j);
					}else if ((j>0)&&((values1[i]-values2[j-1])<tolerance)) { //look down
						j--;
						leftIndex.add(i);
						rightIndex.add(j);
					}else{
						j=-1; //non matching value
					}
				}
				//System.out.println("merge left i="+i+" j="+j);
			}
		}else{
			throw new RuntimeException("This should not occur. Invalid merge type.");
		}
		result[0] = new int[leftIndex.size()];
		int i=0;
		for(Integer ind : leftIndex){
			result[0][i] = ind;
			i++;
		}
		result[1] = new int[rightIndex.size()];
		int j=0;
		for(Integer ind : rightIndex){
			result[1][j] = ind;
			j++;
		}
		return result;
	}


	/**
	 * Find the index of number in a sorted list of numbers.
	 * @param values list of values to search through. The array is sorted in increasing order.
	 * @param toFind (inf and -inf are NOT allowed)
	 * @param tolerance in what range to accept as the same
	 * @return Index of matching value or -1 if not found
	 */
	public static int findMatchingIndex(double[] values, double toFind, double tolerance){
		if (values == null) throw new IllegalArgumentException("values == null");
		if (Double.isInfinite(toFind)) throw new IllegalArgumentException("Double.isInfinite(toFind)");
		if (Double.isNaN(toFind)) throw new IllegalArgumentException("Double.isNaN(toFind)");

		if (values.length == 0) {//if no values, then nothing to find.
			return -1;
		}

		int result=-1;
		int n=values.length;
		int searchIndex=binarySearchIndex(values, toFind);
		if(searchIndex>=0){
			if(searchIndex<(n-1)){
				if(Math.abs(values[searchIndex]-toFind)<tolerance){ 
					result=searchIndex; 
				}else if(Math.abs(values[searchIndex+1]-toFind)<tolerance){ 
					result=searchIndex+1; 
				}else{
					result=-1;
				}
			}else{ // =n-1
				if(Math.abs(values[searchIndex]-toFind)<tolerance){
					result=n-1; // toFind==values[n-1]
				}else{
					result=-1; // toFind>values[n-1]
				}
			}
		}else if(Math.abs(values[searchIndex+1]-toFind)<tolerance){ //i=0
			result=0;
		}else{
			result=-1; // toFind<values[0]
		}
		return result;
	}

	/**
	 * Find the index i such that values[i]<=toFind<values[i+1] .
	 * i=n for values[n]<=toFind with n=length(values)
	 * i=-1 for toFind<values[0]
	 * @param values sorted in increasing order values[i]<=values[i+1] i=0..(n-2)
	 * @param toFind
	 * @return
	 */
	public static int binarySearchIndex(double[] values, double toFind){
		int n=values.length;
		int left=-1; // virtual values[-1]=-inf
		int right=n; // virtual values[n]=inf
		// values[left]<=toFind<values[right]
		while(right>left+1){
			int middle=(right+left)/2;
			//  vb 1 toFind=2.5
			//  -inf 0 1 2 3 inf (n=4)
			//  left=-1      right=4
			//  1)  middle=1 => left=1 right=4
			//  2)  middle=2 => left=2 right=4
			//  3)  middle=3 => righ=3 done
			//  vb 2 toFind=0.5
			//  -inf 0 1 2 inf (n=3)
			//  left=-1      right=3
			//  1)  middle=1 => right=1 left=-1
			//  2)  middle=0 => left=0 right=1 done
			if(toFind<values[middle]){
				right=middle;
			}else{
				left=middle;
			}
		}
		return left;
	}

}
