/*
$URL: https://repos.deltares.nl/repos/openda/openda_1/public/trunk/core/native/src/cta/cta_time.c $
$Revision: 2751 $, $Date: 2011-09-09 08:58:46 +0200 (Fri, 09 Sep 2011) $

COSTA: Problem solving environment for data assimilation
Copyright (C) 2012  Nils van Velzen

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <stdio.h>
#include <math.h>
#include "cta_array.h"

#include "cta_mem.h"
#include "f_cta_utils.h"
#include "cta_errors.h"
#include "cta_message.h"

#define CTA_ARRAY_FREE_F77                                  F77_CALL(CTA_ARRAY_FREE,CTA_ARRAY_FREE)                         
#define CTA_ARRAY_CREATEASDOUBLES_F77                       F77_CALL(CTA_ARRAY_CREATEASDOUBLES,CTA_ARRAY_CREATEASDOUBLES)              
#define CTA_ARRAY_GETNDIMENSIONS_F77                        F77_CALL(CTA_ARRAY_GETNDIMENSIONS,CTA_ARRAY_GETNDIMENSIONS)               
#define CTA_ARRAY_GETDIMENSIONS_F77                         F77_CALL(CTA_ARRAY_GETDIMENSIONS,CTA_ARRAY_GETDIMENSIONS)                
#define CTA_ARRAY_LENGTH_F77                                F77_CALL(CTA_ARRAY_LENGTH,CTA_ARRAY_LENGTH)                       
#define CTA_ARRAY_GETVALUESASDOUBLES_F77                    F77_CALL(CTA_ARRAY_GETVALUESASDOUBLES,CTA_ARRAY_GETVALUESASDOUBLES)           
#define CTA_ARRAY_GETVALUESASDOUBLES_INDEXRANGE_F77         F77_CALL(CTA_ARRAY_GETVALUESASDOUBLES_INDEXRANGE,CTA_ARRAY_GETVALUESASDOUBLES_INDEXRANGE)
#define CTA_ARRAY_GETVALUEASDOUBLES_INDEX_F77               F77_CALL(CTA_ARRAY_GETVALUEASDOUBLES_INDEX,CTA_ARRAY_GETVALUEASDOUBLES_INDEX)      
#define CTA_ARRAY_GETVALUEASDOUBLE_INDICES_F77              F77_CALL(CTA_ARRAY_GETVALUEASDOUBLE_INDICES,CTA_ARRAY_GETVALUEASDOUBLE_INDICES)     
#define CTA_ARRAY_SETVALUESASDOUBLES_F77                    F77_CALL(CTA_ARRAY_SETVALUESASDOUBLES,CTA_ARRAY_SETVALUESASDOUBLES)           
#define CTA_ARRAY_SETVALUEASDOUBLE_INDICES_F77              F77_CALL(CTA_ARRAY_SETVALUEASDOUBLE_INDICES,CTA_ARRAY_SETVALUEASDOUBLE_INDICES)     
#define CTA_ARRAY_SETVALUESASDOUBLES_INDEXRANGE_F77         F77_CALL(CTA_ARRAY_SETVALUESASDOUBLES_INDEXRANGE,CTA_ARRAY_SETVALUESASDOUBLES_INDEXRANGE)
#define CTA_ARRAY_SETVALUEASDOUBLE_INDEX_F77                F77_CALL(CTA_ARRAY_SETVALUEASDOUBLE_INDEX,CTA_ARRAY_SETVALUEASDOUBLE_INDEX)       
#define CTA_ARRAY_SETCONSTANT_F77                           F77_CALL(CTA_ARRAY_SETCONSTANT,CTA_ARRAY_SETCONSTANT)                  
#define CTA_ARRAY_AXPYONVALUES_F77                          F77_CALL(CTA_ARRAY_AXPYONVALUES,CTA_ARRAY_AXPYONVALUES)                 
#define CTA_ARRAY_MULTIPLYVALUES_F77                        F77_CALL(CTA_ARRAY_MULTIPLYVALUES,CTA_ARRAY_MULTIPLYVALUES)               
#define CTA_ARRAY_RESHAPE_F77                               F77_CALL(CTA_ARRAY_RESHAPE,CTA_ARRAY_RESHAPE)                      
#define CTA_ARRAY_GETSLICE_F77                              F77_CALL(CTA_ARRAY_GETSLICE,CTA_ARRAY_GETSLICE)                     
#define CTA_ARRAY_GETSLICE_RANGE_F77                        F77_CALL(CTA_ARRAY_GETSLICE_RANGE,CTA_ARRAY_GETSLICE_RANGE)               
#define CTA_ARRAY_GETSLICEASDOUBLES_RANGE_F77               F77_CALL(CTA_ARRAY_GETSLICEASDOUBLES_RANGE,CTA_ARRAY_GETSLICEASDOUBLES_RANGE)      
#define CTA_ARRAY_SETSLICEASDOUBLES_F77                     F77_CALL(CTA_ARRAY_SETSLICEASDOUBLES,CTA_ARRAY_SETSLICEASDOUBLES)            
#define CTA_ARRAY_SETSLICEASARRAY_F77                       F77_CALL(CTA_ARRAY_SETSLICEASARRAY,CTA_ARRAY_SETSLICEASARRAY)              
#define CTA_ARRAY_SETSLICEASARRAY_RANGE_F77                 F77_CALL(CTA_ARRAY_SETSLICEASARRAY_RANGE,CTA_ARRAY_SETSLICEASARRAY_RANGE)        
#define CTA_ARRAY_SETSLICEASDOUBLES_RANGE_F77               F77_CALL(CTA_ARRAY_SETSLICEASDOUBLES_RANGE,CTA_ARRAY_SETSLICEASDOUBLES_RANGE)      
#define CTA_ARRAY_VALUEINDEX_F77                            F77_CALL(CTA_ARRAY_VALUEINDEX,CTA_ARRAY_VALUEINDEX)                   


#define CLASSNAME "CTA_Array"

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
 * @author nils van velzen
 *
 */

/* Struct holding all data associated to an native array */
typedef struct {
   double  *values;
   int     n;
   int     nValues;
   int     *dimensions;
   int     nDimensions;
} CTAI_Array;

#define MaxDimensions (20)


/* Some utility routines */

#undef METHOD
#define METHOD "CTAI_Array_getData"
int CTAI_Array_getData(CTA_Array h, CTAI_Array **cthis){

   int retval=CTA_Handle_Check((CTA_Handle) h,CTA_ARRAY);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Handle is not a cta_array handle");
      return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) h,(void **) cthis);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Cannot retrieve handle data");
      return retval;
   }
   return CTA_OK;
}


#undef METHOD
#define METHOD "CTAI_Array_getData"
int CTAI_Array_valueIndex(CTAI_Array *cthis, int nIndices, int *indices){

   int blockSize=1;
   int result=0;
   int i;

   if (cthis->nDimensions!=nIndices){
      char msg[120];
      sprintf(msg, "number of indices (%d) does not match dimensions of array (%d)",nIndices, cthis->nDimensions);
      CTA_WRITE_ERROR(msg);
      return 0;
   }

   for(i=cthis->nDimensions-1;i>=0;i--){
      int indexI=indices[i];
      if(indexI<0){indexI+=cthis->dimensions[i];}
      result+=indexI*blockSize;
      blockSize*=cthis->dimensions[i];
   }
   return result;
}




int CTAI_Array_Create(CTA_Array *h){
   CTAI_Array *cthis;
   int retval;

   cthis=CTA_Malloc(sizeof(CTAI_Array));
   cthis->values=NULL;
   cthis->n=0;
   cthis->nValues=0;
   cthis->nDimensions=0;
   retval=CTA_Handle_Create("array",CTA_ARRAY,cthis,h);
   if (retval) {
      CTA_WRITE_ERROR("Cannot create time handle");
      CTA_Free(cthis);
      return retval;
   }
   return CTA_OK;
}

int CTA_Array_Free(CTA_Array *h){

   CTAI_Array *cthis;
   int ierr=CTAI_Array_getData(*h, &cthis);
   if (ierr==CTA_OK){
      if (cthis->values){
         CTA_Free(cthis->values);
      }
      CTA_Free(cthis);
      ierr=CTA_Handle_Free(h);
   }
   return ierr;
}


int CTAI_Array_CreateDim(int nDimensions, int *dimensions, CTA_Array *h){
   CTAI_Array *cthis;

   int ierr=CTAI_Array_Create(h);

   ierr=CTAI_Array_getData(*h, &cthis);
   if (ierr==CTA_OK){
      int i;
      int n=dimensions[0];
	  cthis->nDimensions=nDimensions;
      cthis->dimensions=CTA_Malloc(nDimensions*sizeof(int));
      for (i=0;i<nDimensions;i++){
         cthis->dimensions[i]=dimensions[i];
      }
      for (i=1;i<nDimensions;i++){
         n*=dimensions[i];
      }
      cthis->n=n;
      cthis->values=CTA_Malloc(n*sizeof(double));
   }
   else {
      CTA_WRITE_ERROR("Handle is not correct");
      return ierr;
   }
   return CTA_OK;
}

int CTA_Array_CreateAsDoubles(double *values, int nDimensions, int *dimensions, CTA_Array *h){
   CTAI_Array *cthis;
   int ierr;
   CTAI_Array_CreateDim(nDimensions, dimensions, h);
   ierr=CTAI_Array_getData(*h, &cthis);
   if (ierr==CTA_OK){
      int i;
      for (i=0;i<cthis->n;i++){
         cthis->values[i]=values[i];
      }
   }
   else {
      CTA_WRITE_ERROR("Handle is not correct");
   }
   return ierr;
}



	//storage array

	/**
	 * Constructor based on java arrays as input.
	 * @param values
	 * @param dimensions
	 * @param copyValues
	 */
////	public Array(double[] values, int[] dimensions, boolean copyValues){
////		//check size
////		int rank=dimensions.length;
//	int numberOfValues=dimensions[0];
//		for(int i=1;i<rank;i++){
//			numberOfValues*=dimensions[i];
//		}
//		if(numberOfValues!=values.length){
//			throw new RuntimeException("DoublesArray constructor: The number of values does not match the dimensions");
//		}
//		//copy data
//		if(!copyValues){
//			cthis.values=values; 
//		}else{
//			cthis.values = new double[values.length];
//			System.arraycopy(values, 0, cthis.values, 0, values.length);
//		}
//		cthis.dimensions=new int[dimensions.length];
//		System.arraycopy(dimensions, 0, cthis.dimensions, 0, dimensions.length);
//	}
//
//	public Array(double[] values){
//		cthis(values,new int[]{values.length},true);
//	}
//	
//	public Array(int[] dimensions){
//		int rank=dimensions.length;
//		int numberOfValues=dimensions[0];
//		for(int i=1;i<rank;i++){
//			numberOfValues*=dimensions[i];
//		}
//		cthis.values = new double[numberOfValues];
//		cthis.dimensions=new int[dimensions.length];
//		System.arraycopy(dimensions, 0, cthis.dimensions, 0, dimensions.length);
//	}
//
//	public Array(int dimension){
//		cthis.values = new double[dimension];
//		cthis.dimensions=new int[]{dimension};
//		System.arraycopy(dimensions, 0, cthis.dimensions, 0, dimensions.length);		
//	}
//	/**
//	 * Constructor based on IArray as input.
//	 * @param sourceArray
//	 */
//	public Array(IArray sourceArray){
//		cthis.values=sourceArray.getValuesAsDoubles(true); 
//		cthis.dimensions=sourceArray.getDimensions();
//	}
//
//	/**
//	 * Constructor based on formatted String as input. Format is '{{1,2},{3,4}}' 
//	 * @param source
//	 */
//	public Array(String source){
//		int stringIndex=0;
//		ArrayList<Double> valuesList= new ArrayList<Double>();
//		int[] counter = new int[maxDimensions];
//		int[] dims    = new int[maxDimensions];
//		int curDim=-1; //current dimension 
//		int rank=0;
//		while(stringIndex<source.length()){
//			if(source.charAt(stringIndex)=='{'){
//				curDim++;
//				if(curDim==rank) rank=curDim+1;
//				stringIndex++;
//			}else if(source.charAt(stringIndex)=='}'){
//				if(counter[curDim]>dims[curDim]) dims[curDim]=counter[curDim];
//				counter[curDim]=0;
//				curDim--;
//				if(curDim<-1){
//					throw new RuntimeException("Too many closing brackets in array at position="+stringIndex
//							+" in string="+source);
//				}
//				stringIndex++;
//			}else if(source.charAt(stringIndex)==','){
//				counter[curDim]++;
//				stringIndex++;
//			}else{ //try to find a number
//				int indexEnd=source.indexOf('}', stringIndex);
//				// if a comma comes first
//				int indexComma=source.indexOf(',', stringIndex);
//				if((indexComma>=0)&(indexComma<indexEnd)){
//					indexEnd = indexComma;
//				}
//				String numberString=source.substring(stringIndex, indexEnd);
//				double value;
//				try {
//					value = Double.parseDouble(numberString);
//				} catch (NumberFormatException e) {
//					throw new RuntimeException("Problems parsing array at position="+stringIndex
//							+"while processing number "+numberString);
//				}
//				valuesList.add(value);
//				stringIndex=indexEnd;
//			}
//		}
//		// store values
//		int n=valuesList.size();
//		cthis.values = new double[n];
//		for(int i=0;i<n;i++){
//			cthis.values[i]=valuesList.get(i);
//		}
//		//store dimensions
//		cthis.dimensions = new int[rank];
//		for(int i=0;i<rank;i++){
//			cthis.dimensions[i]=dims[i]+1;
//		}
//	}

	/**
	 * Get number of dimensions, eg 1 for a vector and 2 for a matrix
	 * @return rank of the array
	 */
#undef METHOD
#define METHOD "CTA_Array_getNumberOfDimensions"
int CTA_Array_getNumberOfDimensions(CTA_Array h){
   CTAI_Array *cthis=NULL;
   int ierr=CTAI_Array_getData(h, &cthis);
   if (ierr==CTA_OK){
	return cthis->nDimensions;
   }
   else {
      CTA_WRITE_ERROR("Handle is not correct");
      return 0;
   }
}  


/**
 * Get the number of array dimensions 
 * @return size of array, eg 2 for the array [[3,4,10],[3,4,10]]
 */
#undef METHOD
#define METHOD "CTA_Array_getnDimensions"
int CTA_Array_getnDimensions(CTA_Array h, int *nDimensions){
   CTAI_Array *cthis=NULL;
   int ierr=CTAI_Array_getData(h, &cthis);
   if (ierr==CTA_OK){
      *nDimensions=cthis->nDimensions;
   }
   else {
      CTA_WRITE_ERROR("Handle is not correct");
   }
   return ierr;
}


/**
 * Get the size of the array for each dimension. The length of the return value equals
 * the number of dimensions.
 * @return size of array, eg [3,4,10]
 */
#undef METHOD
#define METHOD "CTA_Array_getDimensions"
int CTA_Array_getDimensions(CTA_Array h, int *dimensions){
   CTAI_Array *cthis=NULL;
   int ierr=CTAI_Array_getData(h, &cthis);
   if (ierr==CTA_OK){
      int i;
      for (i=0; i<cthis->nDimensions; i++){
         dimensions[i]=cthis->dimensions[i];
      }
   }
   else {
      CTA_WRITE_ERROR("Handle is not correct");
   }
   return ierr;
}

/**
 * Total number of elements in an array. this is equal to the product of the values
 * returned by getDimension and is thus just for c
 * @return
 */
#undef METHOD
#define METHOD "CTA_Array_length"
int CTA_Array_length(CTA_Array h){
   CTAI_Array *cthis=NULL;
   int ierr=CTAI_Array_getData(h, &cthis);
   if (ierr==CTA_OK){
      return cthis->n;
   }
   else {
      CTA_WRITE_ERROR("Handle is not correct");
   }
   return ierr;
}

/**
 * Get all values of an array. The values are guaranteed be a copy if
 * copyValues==true or may be a reference if copyValues==false.
 * Note that the values are never guaranteed to be a reference and can not be used
 * to change the array. 
 * @param copyValues
 * @return
 */
#undef METHOD
#define METHOD "CTA_Array_getValuesAsDoubles"
int CTA_Array_getValuesAsDoubles(CTA_Array h, double *values){
   CTAI_Array *cthis=NULL;
   int ierr=CTAI_Array_getData(h, &cthis);
   if (ierr==CTA_OK){
      int i;
      for (i=0; i<cthis->n; i++){
         values[i]=cthis->values[i];
      }
   }
   else {
      CTA_WRITE_ERROR("Handle is not correct");
   }
   return ierr;
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
#undef METHOD
#define METHOD "CTA_Array_getValuesAsDoubles_indexrange"
int CTA_Array_getValuesAsDoubles_indexrange(CTA_Array h, int firstIndex, int lastIndex, double *values){
   CTAI_Array *cthis=NULL;
   int ierr=CTAI_Array_getData(h, &cthis);
   if (ierr==CTA_OK){
      int resultLength;
	  int i;
      int n=cthis->n;
      if(firstIndex<0){firstIndex+=n;}
      if(lastIndex<0){lastIndex+=n;}
      resultLength=lastIndex-firstIndex+1;      
      for (i=0;i<resultLength;i++){
         values[i]=cthis->values[firstIndex+i];
      }
   }
   else {
      CTA_WRITE_ERROR("Handle is not correct");
   }
   return ierr;
}

/**
 * Get a value from an array for specific indices. 
 * Eg let a=[[1,2,3][4,5,6]] then getValueAsDouble(5)
 * returns 6
 * Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
 * @param indices index specifier
 * @return double value at the specific indices
 */

#undef METHOD
#define METHOD "CTA_Array_getValueAsDoubles_index"
int CTA_Array_getValueAsDoubles_index(CTA_Array h, int index, double *value) {
   CTAI_Array *cthis=NULL;
   int ierr=CTAI_Array_getData(h, &cthis);
   if (ierr==CTA_OK){
      int n=cthis->n;
      if(index<0){index+=n;}
      *value=cthis->values[index];
   }
   else {
      CTA_WRITE_ERROR("Handle is not correct");
   }
   return ierr;
}


/**
 * Get a value from an array for specific indices. 
 * Eg let a=[[1,2,3][4,5,6]] then getValueAsDouble([1,2])
 * returns 6
 * Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
 * @return
 */
#undef METHOD
#define METHOD "CTA_Array_getValueAsDoubles_indices"
int CTA_Array_getValueAsDouble_indices(CTA_Array h, int nIndices, int* indices, double *value){
   CTAI_Array *cthis=NULL;
   int ierr=CTAI_Array_getData(h, &cthis);
   if (ierr==CTA_OK){
      int index = CTAI_Array_valueIndex(cthis, nIndices, indices);
      *value=cthis->values[index];
   }
   else {
      CTA_WRITE_ERROR("Handle is not correct");
   }
   return ierr;
}

/**
 * Set the values of this array.
 * @param values the values as an array of doubles
 */
#undef METHOD
#define METHOD "CTA_Array_setValuesAsDoubles"
int CTA_Array_setValuesAsDoubles(CTA_Array h, double *values, int length) {
   CTAI_Array *cthis=NULL;
   int ierr=CTAI_Array_getData(h, &cthis);
   if (ierr==CTA_OK){
      if (cthis->n==length){
         int i;
         for (i=0;i<length;i++){
            cthis->values[i]=values[i];
         }
      }
      else {
         char msg[120];
         sprintf(msg,"The length of the array is %d but the user tries to set %d values\n",cthis->n,length);
         CTA_WRITE_ERROR(msg);
      }
   }
   else {
      CTA_WRITE_ERROR("Handle is not correct");
   }
   return ierr;
}

/**
 * Set a value from an array for specific indices. 
 * Eg let a=[[1,2,3][4,5,6]] then setValueAsDouble([1,2],60.)
 * results in a=[[1,2,3][4,5,60.]]
 * Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
 * @param indices index specifier
 * @param value at the specific indices
 */
#undef METHOD
#define METHOD "CTA_Array_setValueAsDouble_indices"
int CTA_Array_setValueAsDouble_indices(CTA_Array h, int nIndices, int *indices, double value){
   CTAI_Array *cthis=NULL;
   int ierr=CTAI_Array_getData(h, &cthis);
   if (ierr==CTA_OK){
      int index = CTAI_Array_valueIndex(cthis, nIndices, indices);
      cthis->values[index]=value;
   }
   else {
      CTA_WRITE_ERROR("Handle is not correct");
   }
   return ierr;
}

/**
 * Set part of the values of this array.
 * Let a=[[1,2,3][4,5,6]] then setValuesAsDoubles(1,2,[20,30]) will result in a=[[1,20,30][4,5,6]]
 * Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
 * @param firstIndex specifies the start of the selection
 * @param lastIndex specifies the end of the selection
 * @param values that will replace the selected range of numbers
 */
#undef METHOD
#define METHOD "CTA_Array_setValuesAsDoubles_indexrange"
int CTA_Array_setValuesAsDoubles_indexrange(CTA_Array h, int firstIndex, int lastIndex, double *values){
   CTAI_Array *cthis=NULL;
   int ierr=CTAI_Array_getData(h, &cthis);
   if (ierr==CTA_OK){
      int resultLength;
	  int i;
	  int n=cthis->n;
      if(firstIndex<0){firstIndex+=n;}
      if(lastIndex<0){lastIndex+=n;}
      resultLength=lastIndex-firstIndex+1;      
      for (i=0;i<resultLength;i++){
         cthis->values[firstIndex+i]=values[i];
      }
   }
   else {
      CTA_WRITE_ERROR("Handle is not correct");
   }
   return ierr;
}

	/**
	 * Get a value from an array for specific indices. 
	 * Eg let a=[[1,2,3][4,5,6]] then setValueAsDouble(5,60.)
	 * results in a=[[1,2,3][4,5,60.]]
	 * Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
     * @param indices index specifier
	 * @param value at the specific indices
	 */
#undef METHOD
#define METHOD "CTA_Array_setValueAsDouble_index"
int CTA_Array_setValueAsDouble_index(CTA_Array h, int index, double value) {
   CTAI_Array *cthis=NULL;
   int ierr=CTAI_Array_getData(h, &cthis);
   if (ierr==CTA_OK){
      int n=cthis->n;
      if(index<0){index+=n;}
      cthis->values[index]=value;
   }
   else {
      CTA_WRITE_ERROR("Handle is not correct");
   }
   return ierr;
}

/**
 * Set whole vector equal to a constant value.
 * <p/>
 * Note:  This method can only be used if all elements of the vector
 * have the same data type.
 *
 * @param value value to set
 */
#undef METHOD
#define METHOD "CTA_Array_setConstant"
int CTA_Array_setConstant(CTA_Array h, double value) {
   CTAI_Array *cthis=NULL;
   int ierr=CTAI_Array_getData(h, &cthis);
   if (ierr==CTA_OK){
      int i;
      for (i=0;i<cthis->n;i++){
         cthis->values[i]=value;
      }
   }
   else {
      CTA_WRITE_ERROR("Handle is not correct");
   }
   return ierr;
}

/**
 * Perform a values += alpha * axpyValues</c> operation on each value in this array.
 * @param alpha the <c>alpha</c> in <c>state variable += alpha * vector</c>.
 * @param axpyValues the values for the axpy-operation on all values in this array.
 */
#undef METHOD
#define METHOD "CTA_Array_axpyOnValues"
int CTA_Array_axpyOnValues(CTA_Array h, double alpha, double *axpyValues) {
   CTAI_Array *cthis=NULL;
   int ierr=CTAI_Array_getData(h, &cthis);
   if (ierr==CTA_OK){
      int i;
      for (i=0;i<cthis->n;i++){
         cthis->values[i] += alpha*axpyValues[i];
      }
   }
   else {
      CTA_WRITE_ERROR("Handle is not correct");
   }
   return ierr;
}

/**
 * Multiply each value in this array with the corresponding multiplication factor.
 * @param multiplicationFactors the multiplication factors for all array values.
 */
#undef METHOD
#define METHOD "CTA_Array_multiplyValues"
int CTA_Array_multiplyValues(CTA_Array h, double* multiplicationFactors) {
   CTAI_Array *cthis=NULL;
   int ierr=CTAI_Array_getData(h, &cthis);
   if (ierr==CTA_OK){
      int i;
      for (i=0;i<cthis->n;i++){
         cthis->values[i] *= multiplicationFactors[i];
      }
   }
   else {
      CTA_WRITE_ERROR("Handle is not correct");
   }
   return ierr;
}

/**
 * Change the dimensions of an array. The new array should have the same length
 * @param dimensions
 */
#undef METHOD
#define METHOD "CTA_Array_reshape"
int CTA_Array_reshape(CTA_Array h, int nDimensions, int *dimensions){
   CTAI_Array *cthis=NULL;
   int ierr=CTAI_Array_getData(h, &cthis);
   if (ierr==CTA_OK){
      int n=dimensions[0];
      int i;
      for(i=1;i<nDimensions;i++){
         n*=dimensions[i];
      }
      if(n!=cthis->n){
         char msg[120];
         sprintf(msg,"Can not reshape to a different length. Length %d to %d.",cthis->n, n);
         CTA_WRITE_ERROR(msg);
         return CTA_DIMENSION_ERROR;
      }
      else {
         int iDim;
		 cthis->nDimensions=nDimensions;
         for (iDim=0; iDim<nDimensions; iDim++){
            cthis->dimensions[i]=dimensions[i];
         }
      }
   }
   else {
      CTA_WRITE_ERROR("Handle is not correct");
   }
   return ierr;
}


///**
// * Is it allowed to write beyond the current end of the array. Some implementations, especially ones writing to disk
// * can increase the size of the first dimension of the array. 
// * 
// * Multiple growing dimensions requires moving the existing data if it implemented with a one-dimensional storage, 
// * eg as double[]. This is NOT SUPPORTED.  
// *
// * @return 
// */
//// boolean allowsGrowingFirstDimension(){
////		return false;
////}
//





/**
 * Get part of the array by selection of a subset in one dimension.
 * Eg. a=[[1,2,3],[4,5,6]] a.getSlice(0,0) returns [1,2,3] 
 * Note that the number of dimensions IS reduced by one.
 * @param dimension
 * @param index
 * @return
 */
#undef METHOD
#define METHOD "CTA_Array_getSlice"
int CTA_Array_getSlice(CTA_Array h, int dimension, int index, CTA_Array *h_out){

   CTAI_Array *cthis=NULL;
   CTAI_Array *cthis_out=NULL;
   int ierr;

   *h_out=CTA_NULL;
   ierr=CTAI_Array_getData(h, &cthis);

   if (ierr==CTA_OK){
      // indexing eg A(:,: ,index,:,:  )
      //             A(left,at   ,right)
      int nDimsLeft=dimension;
      int dimsLeft[MaxDimensions];
      int dims[MaxDimensions];
	  int rankRight;
      int dimsRight[MaxDimensions];
	  int nDims;
	  int i;
      for (i=0; i<dimension; i++){
         dimsLeft[i]=cthis->dimensions[i];
         dims[i]=dimsLeft[i];
      }
      rankRight=cthis->nDimensions-dimension-1;      
      for (i=0;i<rankRight;i++){
         dimsRight[i]=cthis->dimensions[dimension+1+i];
         dims[i+dimension]=dimsRight[i];
      }

      nDims=nDimsLeft+rankRight;
      ierr=CTAI_Array_CreateDim(nDims,  dims, h_out);
      if (ierr==CTA_OK){
         ierr=CTAI_Array_getData(*h_out, &cthis_out);
      }
      if (ierr==CTA_OK){

         // prepare for copy
         int blockSize = 1;
		 int offset;
         int i;
		 int srcPos; 
         int destPos;
		 int stride;
		 int count;

         for(i=0;i<rankRight;i++) blockSize*=dimsRight[i];
         offset = index*blockSize;
         stride = cthis->dimensions[dimension]*blockSize;
         count  = 1;
         for(i=0;i<nDimsLeft;i++) count*=dimsLeft[i];
         // copy values
         srcPos = offset; 
         destPos = 0;
         for(i=0;i<count;i++){
            int j;
            for (j=0;j<blockSize;j++){
               cthis_out->values[destPos+j]=cthis->values[srcPos+j];
            }
            srcPos+=stride;
            destPos+=blockSize;
         }
      }
      else {
         CTA_WRITE_ERROR("Cannot allocate return array");
      }
   }
   else {
      CTA_WRITE_ERROR("Handle is not correct");
   }
   return ierr;
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
#undef METHOD
#define METHOD "CTA_Array_getSlice_range"
int CTA_Array_getSlice_range(CTA_Array h, int dimension, int minIndex, int maxIndex, CTA_Array *h_out){
	// indexing eg A(:,: ,index,:,:  )
	//             A(left,at   ,right)
   CTAI_Array *cthis=NULL;
   CTAI_Array *cthis_out=NULL;
   int ierr=CTAI_Array_getData(h, &cthis);
   if (ierr==CTA_OK){
	int rankRight;
      int dimsLeft[MaxDimensions];
      int dimsRight[MaxDimensions];
      int dims[MaxDimensions];
	  int nDims;
	  int i;
      int nDimsLeft=dimension;
      for (i=0;i<dimension;i++){
         dimsLeft[i]=cthis->dimensions[i];
      }
      rankRight=cthis->nDimensions-dimension-1;      
      for (i=0;i<rankRight;i++){
         dimsRight[i]=cthis->dimensions[dimension+1+i];
      }
      for (i=0; i<cthis->nDimensions;i++){
         dims[i]=cthis->dimensions[i];
      }
      dims[dimension]=maxIndex-minIndex+1;
      nDims=cthis->nDimensions;

      ierr=CTAI_Array_CreateDim(nDims,  dims, h_out);
      if (ierr==CTA_OK){
         ierr=CTAI_Array_getData(*h_out, &cthis_out);
      }
      if (ierr==CTA_OK){
         // prepare for copy
		 int minOffset;
		 int stride;
		 int count;
		 int srcPos; 
         int destPos;
         int j,k;

         int blockSize = 1;
         for(i=0;i<rankRight;i++) blockSize*=dimsRight[i];
         minOffset = minIndex*blockSize;
         stride = cthis->dimensions[dimension]*blockSize;
         count  = 1;
         for(i=0;i<nDimsLeft;i++) count*=dimsLeft[i];
         // copy values
         srcPos = minOffset; 
         destPos = 0;
         for(i=0;i<count;i++){
	    for(j=0;j<dims[dimension];j++){
               for (k=0; k<blockSize; k++){
                  cthis_out->values[destPos+k]=cthis->values[srcPos+k];
               }           	
               srcPos+=blockSize;
               destPos+=blockSize;
            }
            srcPos+=stride;
            destPos+=blockSize;
         }
      }
      else {
         CTA_WRITE_ERROR("Cannot allocate return array");
      }
   }
   else {
      CTA_WRITE_ERROR("Handle is not correct");
   }
   return ierr;
}

/**
 * Get part of the array by selection of a subset in one dimension.
 * Eg. a=[[1,2,3],[4,5,6],[7,8,9]] a.getSlice(0,0,1) returns [1,2,3,4] 
 * @param dimension
 * @param minIndex
 * @param maxIndex
 * @return
 */
#undef METHOD
#define METHOD "CTA_Array_getSliceAsDoubles_range"
int CTA_Array_getSliceAsDoubles_range(CTA_Array h, int dimension, int minIndex, int maxIndex, double *values){
	// indexing eg A(:,: ,index,:,:  )
	//             A(left,at   ,right)
   CTAI_Array *cthis=NULL;
   int ierr=CTAI_Array_getData(h, &cthis);
   if (ierr==CTA_OK){
      int dimsLeft[MaxDimensions];
	  int rankRight;
      int dimsRight[MaxDimensions];
      int i;
	  int blockSize;
	  int count;
	  int minOffset;
      int stride;
	  int srcPos; 
      int destPos;
      int j,k;
      int nDimsLeft=dimension;
	  int dims[MaxDimensions];
      for (i=0;i<dimension;i++){
         dimsLeft[i]=cthis->dimensions[i];
      }
      rankRight=cthis->nDimensions-dimension-1;
      for (i=0;i<rankRight;i++){
         dimsRight[i]=cthis->dimensions[dimension+1+i];
      }
      for (i=0; i<cthis->nDimensions;i++){
         dims[i]=cthis->dimensions[i];
      }
      dims[dimension]=maxIndex-minIndex+1;

      // prepare for copy
      blockSize = 1;
      for(i=0;i<rankRight;i++) blockSize*=dimsRight[i];
      minOffset = minIndex*blockSize;
      stride = cthis->dimensions[dimension]*blockSize;
      count  = 1;
      for(i=0;i<nDimsLeft;i++) count*=dimsLeft[i];
      // copy values
      srcPos = minOffset; 
      destPos = 0;
      for(i=0;i<count;i++){
         for(j=0;j<dims[dimension];j++){
            for (k=0; k<blockSize; k++){
               values[destPos+k]=cthis->values[srcPos+k];
            }           	
            srcPos+=blockSize;
            destPos+=blockSize;
         }
         srcPos+=stride;
         destPos+=blockSize;
      }
   }
   else {
      CTA_WRITE_ERROR("Handle is not correct");
   }
   return ierr;
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
#undef METHOD
#define METHOD "CTA_Array_setSliceAsDoubles"
int CTA_Array_setSliceAsDoubles(CTA_Array h, double *slice, int dimension, int index){
   CTAI_Array *cthis=NULL;
   int ierr=CTAI_Array_getData(h, &cthis);
   if (ierr==CTA_OK){
      // indexing eg A(:,: ,index,:,:  )
      //             A(left,at   ,right)
      int dimsLeft[MaxDimensions];
	  int rankRight;
      int dimsRight[MaxDimensions];
      int nDimsLeft=dimension;
      int i;
	  int blockSize;
      int offset;
      int stride;
      int count;
	  int srcPos; 
      int destPos;

      for (i=0;i<dimension;i++){dimsLeft[i]=cthis->dimensions[i];}

      rankRight=cthis->nDimensions-dimension-1;
      
      for (i=0;i<rankRight;i++){
         dimsRight[i]=cthis->dimensions[dimension+1+i];
      }

      // prepare for copy
      blockSize = 1;
      for(i=0;i<rankRight;i++) blockSize*=dimsRight[i];
      offset = index*blockSize;
      stride = cthis->dimensions[dimension]*blockSize;
      count  = 1;
      for(i=0;i<nDimsLeft;i++) count*=dimsLeft[i];
      // copy values
      srcPos = 0; 
      destPos = offset;
      for(i=0;i<count;i++){
         int j;
         for (j=0; j<blockSize; j++){
            cthis->values[destPos+j]=slice[srcPos+j];
         }
         destPos+=stride;
         srcPos+=blockSize;
      }
   }
   else {
      CTA_WRITE_ERROR("Handle is not correct");
   }
   return ierr;
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
#undef METHOD
#define METHOD "CTA_Array_setSliceAsArray"
int CTA_Array_setSliceAsArray(CTA_Array h, CTA_Array slice_h, int dimension, int index){
   CTAI_Array *slice;
   int ierr=CTAI_Array_getData(slice_h, &slice);
   if (ierr==CTA_OK){
      ierr=CTA_Array_setSliceAsDoubles(h, slice->values, dimension, index);
      if (ierr!=CTA_OK){
         CTA_WRITE_ERROR("Error setting the values of the slice");
      }
   }
   else {
      CTA_WRITE_ERROR("Handle slice_h is not correct");
   }
   return CTA_OK;
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
#undef METHOD
#define METHOD "CTA_Array_setSliceAsArray_range"
int CTA_Array_setSliceAsArray_range(CTA_Array h, CTA_Array slice_h, int dimension, int minIndex, int maxIndex){
   CTAI_Array *slice;
   int ierr=CTAI_Array_getData(slice_h, &slice);
   if (ierr==CTA_OK){
      ierr=CTA_Array_setSliceAsDoubles_range(h, slice->values, dimension, minIndex, maxIndex);
      if (ierr!=CTA_OK){
         CTA_WRITE_ERROR("Error setting the values of the slice");
      }
   }
   else {
      CTA_WRITE_ERROR("Handle slice_h is not correct");
   }
   return ierr;

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
#undef METHOD
#define METHOD "CTA_Array_setSliceAsDoubles_range"
int CTA_Array_setSliceAsDoubles_range(CTA_Array h, double *slice, int dimension, int minIndex, int maxIndex){
   CTAI_Array *cthis=NULL;
   int rankRight;
   int dimsRight[MaxDimensions];


   int ierr=CTAI_Array_getData(h, &cthis);
   if (ierr==CTA_OK){

      // indexing eg A(:,: ,index,:,:  )
      //             A(left,at   ,right)
      int dimsLeft[MaxDimensions];
	  int dims[MaxDimensions];
      int nDimsLeft=dimension;
      int i;
	  int blockSize;
	  int minOffset;
      int stride;
      int count;
	  int destPos; 
      int srcPos;

      for (i=0; i<dimension; i++){dimsLeft[i]=cthis->dimensions[i];}
      rankRight=cthis->nDimensions-1;
      for (i=0; i<rankRight; i++){dimsRight[i]=cthis->dimensions[dimension+1+i];}
      for (i=0; i<cthis->nDimensions; i++){dims[i]=cthis->dimensions[i];}
      dims[dimension]=maxIndex-minIndex+1;

      // prepare for copy
      blockSize = 1;
      for(i=0;i<rankRight;i++) blockSize*=dimsRight[i];
      minOffset = minIndex*blockSize;
      stride = cthis->dimensions[dimension]*blockSize;
      count  = 1;
      for(i=0;i<nDimsLeft;i++) count*=dimsLeft[i];
      // copy values
      destPos = minOffset; 
      srcPos = 0;
      for(i=0;i<count;i++){
         int j;
         for(j=0;j<dims[dimension];j++){
            int k;
            for (k=0; k<blockSize; k++){
               cthis->values[destPos+k]=slice[srcPos+k];
            }
            destPos+=blockSize;
            srcPos+=blockSize;
         }
         destPos+=stride;
         srcPos+=blockSize;
      }
   }
   else {
      CTA_WRITE_ERROR("Handle is not correct");
   }
   return ierr;
}	


/**
 * Convert indices in multiple dimensions to position in the one-dimensional array as
 * returned eg by getValuesAsDoubles
 * Eg. a=[[1,2,3],[4,5,6],[7,8,9]] then valueIndex([1,0]) returns 3 which points to the value 4 here.  
 * @param indices
 * @return position
 */
#undef METHOD
#define METHOD "CTA_Array_reshape"
int CTA_Array_valueIndex(CTA_Array h, int nIndices, int *indices, int *index){
   CTAI_Array *cthis=NULL;
   int ierr=CTAI_Array_getData(h, &cthis);
   if (ierr==CTA_OK){
      *index = CTAI_Array_valueIndex(cthis, nIndices, indices);
   }
   else {
      *index=0;
      CTA_WRITE_ERROR("Handle is not correct");
   }
   return ierr;
}



CTAEXPORT void CTA_ARRAY_FREE_F77(CTA_Array *h, int *ierr){
   *ierr = CTA_Array_Free(h);
}
CTAEXPORT void CTA_ARRAY_CREATEASDOUBLES_F77(double *values, int *nDimensions, int *dimensions, CTA_Array *h, int *ierr){
   *ierr = CTA_Array_CreateAsDoubles(values, *nDimensions, dimensions, h);
}

//void CTA_Array_getNumberOfDimensions(CTA_Array h);

CTAEXPORT void  CTA_ARRAY_GETNDIMENSIONS_F77(CTA_Array *h, int *nDimensions, int *ierr){
   *ierr = CTA_Array_getnDimensions(*h, nDimensions);
}

CTAEXPORT void CTA_ARRAY_GETDIMENSIONS_F77(CTA_Array *h, int *dimensions, int *ierr){
   *ierr = CTA_Array_getDimensions(*h, dimensions);
}

CTAEXPORT void CTA_ARRAY_LENGTH_F77(CTA_Array *h, int *length, int *ierr){
   *length = CTA_Array_length(*h);
   *ierr = CTA_OK;
}

CTAEXPORT void CTA_ARRAY_GETVALUESASDOUBLES_F77(CTA_Array *h, double *values, int *ierr){
   *ierr = CTA_Array_getValuesAsDoubles(*h, values);
}

CTAEXPORT void CTA_ARRAY_GETVALUESASDOUBLES_INDEXRANGE_F77(CTA_Array *h, int *firstIndex, int *lastIndex, double *values, int *ierr){
   *ierr = CTA_Array_getValuesAsDoubles_indexrange(*h, *firstIndex, *lastIndex, values);
}

CTAEXPORT void CTA_ARRAY_GETVALUEASDOUBLES_INDEX_F77(CTA_Array *h, int *index, double *value, int *ierr){
   *ierr = CTA_Array_getValueAsDoubles_index(*h, *index, value);
}

CTAEXPORT void CTA_ARRAY_GETVALUEASDOUBLE_INDICES_F77(CTA_Array *h, int *nIndices, int* indices, double *value, int *ierr){
   *ierr = CTA_Array_getValueAsDouble_indices(*h, *nIndices, indices, value);
}

CTAEXPORT void CTA_ARRAY_SETVALUESASDOUBLES_F77(CTA_Array *h, double *values, int *length, int *ierr){
   *ierr = CTA_Array_setValuesAsDoubles(*h, values, *length);
}

CTAEXPORT void CTA_ARRAY_SETVALUEASDOUBLE_INDICES_F77(CTA_Array *h, int *nIndices, int *indices, double *value, int *ierr){
   *ierr = CTA_Array_setValueAsDouble_indices(*h, *nIndices, indices, *value);
}

CTAEXPORT void CTA_ARRAY_SETVALUESASDOUBLES_INDEXRANGE_F77(CTA_Array *h, int *firstIndex, int *lastIndex, double *values, int *ierr){
   *ierr = CTA_Array_setValuesAsDoubles_indexrange(*h, *firstIndex, *lastIndex, values);
}

CTAEXPORT void CTA_ARRAY_SETVALUEASDOUBLE_INDEX_F77(CTA_Array *h, int *index, double *value, int *ierr){
   *ierr = CTA_Array_setValueAsDouble_index(*h, *index, *value);
}

CTAEXPORT void CTA_ARRAY_SETCONSTANT_F77(CTA_Array *h, double *value, int *ierr){
   *ierr = CTA_Array_setConstant(*h, *value);
}

CTAEXPORT void CTA_ARRAY_AXPYONVALUES_F77(CTA_Array *h, double *alpha, double *axpyValues, int *ierr){
   *ierr = CTA_Array_axpyOnValues(*h, *alpha, axpyValues);
}

CTAEXPORT void CTA_ARRAY_MULTIPLYVALUES_F77(CTA_Array *h, double* multiplicationFactors, int *ierr){
   *ierr = CTA_Array_multiplyValues(*h, multiplicationFactors);
}

CTAEXPORT void CTA_ARRAY_RESHAPE_F77(CTA_Array *h, int *nDimensions, int *dimensions, int *ierr){
   *ierr = CTA_Array_reshape(*h, *nDimensions, dimensions);
}

CTAEXPORT void CTA_ARRAY_GETSLICE_F77(CTA_Array *h, int *dimension, int *index, CTA_Array *h_out, int *ierr){
   *ierr = CTA_Array_getSlice(*h, *dimension, *index, h_out);
}

CTAEXPORT void CTA_ARRAY_GETSLICE_RANGE_F77(CTA_Array *h, int *dimension, int *minIndex, int *maxIndex, CTA_Array *h_out, int *ierr){
   *ierr = CTA_Array_getSlice_range(*h, *dimension, *minIndex, *maxIndex, h_out);
}

CTAEXPORT void CTA_ARRAY_GETSLICEASDOUBLES_RANGE_F77(CTA_Array *h, int *dimension, int *minIndex, int *maxIndex, double *values, int *ierr){
   *ierr = CTA_Array_getSliceAsDoubles_range(*h, *dimension, *minIndex, *maxIndex, values);
}

CTAEXPORT void CTA_ARRAY_SETSLICEASDOUBLES_F77(CTA_Array *h, double *slice, int *dimension, int *index, int *ierr){
   *ierr = CTA_Array_setSliceAsDoubles(*h, slice, *dimension, *index);
}

CTAEXPORT void CTA_ARRAY_SETSLICEASARRAY_F77(CTA_Array *h, CTA_Array *slice_h, int *dimension, int *index, int *ierr){
   *ierr = CTA_Array_setSliceAsArray(*h, *slice_h, *dimension, *index);
}

CTAEXPORT void CTA_ARRAY_SETSLICEASARRAY_RANGE_F77(CTA_Array *h, CTA_Array *slice_h, int *dimension, int *minIndex, int *maxIndex, int *ierr){
   *ierr = CTA_Array_setSliceAsArray_range(*h, *slice_h, *dimension, *minIndex, *maxIndex);
}

CTAEXPORT void CTA_ARRAY_SETSLICEASDOUBLES_RANGE_F77(CTA_Array *h, double *slice, int *dimension, int *minIndex, int *maxIndex, int *ierr){
   *ierr = CTA_Array_setSliceAsDoubles_range(*h, slice, *dimension, *minIndex, *maxIndex);
}

CTAEXPORT void CTA_ARRAY_VALUEINDEX_F77(CTA_Array *h, int *nIndices, int *indices, int *index, int *ierr){
   *ierr = CTA_Array_valueIndex(*h, *nIndices, indices, index);
}


