/*
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

/**
\file  cta_array.h
\brief Interface description of array component.

   An array in OpenDA is an N-dimensional Matrix of values.
   In a similar wat as in Matlab and Octave a vector is a special case of a Matrix/Array.

*/

#ifndef CTA_ARRAY_H
#define CTA_ARRAY_H
#include "cta_system.h"
#include "cta_handles.h"
#include "cta_datatypes.h"
/* Function Handle */
#ifdef __cplusplus
extern "C" {
#endif

typedef CTA_Handle CTA_Array;

/** \brief Free Array instance
 *
 * \param h IO handle of Array instance CTA_NULL on return
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_Free(CTA_Array *h);

/** \brief Create a new Array instance and fill with given values
 *
 * \param values      I values of new Array
 * \param nDimensions I Number of dimensions of new array
 * \param dimensions  I Number of elements in each dimension
 * \param h           O Array handle of new instance
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_CreateAsDoubles(double *values, int nDimensions, int *dimensions, CTA_Array *h);

/** \brief Get number of dimensions, eg 1 for a vector and 2 for a matrix
 * \param h           I Array handle
 *  \return rank of the array
 */
CTAEXPORT int CTA_Array_getNumberOfDimensions(CTA_Array h);

/** \brief Get the sizes of the individual dimensions
 * \param h           I Array handle
 * \param nDimensions O  size of array, eg 2 for the array [[3,4,10],[3,4,10]]
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_getnDimensions(CTA_Array h, int *nDimensions);

/** \brief Get the size of the array for each dimension. 
 * The length of the return value equals the number of dimensions.
 *
 * \param h          I Array handle
 * \param dimensions O size of array, eg [3,4,10]
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_getDimensions(CTA_Array h, int *dimensions);

/** \brief Total number of elements in an array. 
 * This is equal to the product of the values returned by getDimension and is thus just for c
 *
 * \param h      I Array handle
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_length(CTA_Array h);

/** \brief Get all values of an array.
 * The values are guaranteed be a copy if
 * copyValues==true or may be a reference if copyValues==false.
 * Note that the values are never guaranteed to be a reference and can not be used
 * to change the array. 
 *
 * \param h      I Array handle
 * \param values O Values from the array
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_getValuesAsDoubles(CTA_Array h, double *values);

/** \brief Get a part of the values from the array. 
 * Eg let a=[[1,2,3][4,5,6]] then getValuesAsDoubles(1,2)
 * returns [2,3]
 * Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
 * \param h          I Array handle
 * \param firstIndex I firstIndex of selection
 * \param lastIndex  I lastIndex of selection
 * \param values     O Values from the array
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_getValuesAsDoubles_indexrange(CTA_Array h, int firstIndex, int lastIndex, double *values);

/** \brief Get a value from an array for specific indices. 
 * Eg let a=[[1,2,3][4,5,6]] then getValueAsDouble(5)
 * returns 6
 * Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
 * \param h       I Array handle
 * \param index   I index specifier
 * \param value   O double value at the specific indices
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_getValueAsDoubles_index(CTA_Array h, int index, double *value);

/** \brief Get a value from an array for specific indices. 
 * Eg let a=[[1,2,3][4,5,6]] then getValueAsDouble([1,2])
 * returns 6
 * Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
 * \param h        I Array handle
 * \param nIndices I length of array of indices indices
 * \param indices  I indices of value
 * \param value    O value from array
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_getValueAsDouble_indices(CTA_Array h, int nIndices, int* indices, double *value);

/** \brief Set the values of this array.
 * \param h      I Array handle
 * \param values I the values as an array of doubles
 * \param length I length of array values
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_setValuesAsDoubles(CTA_Array h, double *values, int length);

/** \brief Set a value from an array for specific indices. 
 * Eg let a=[[1,2,3][4,5,6]] then setValueAsDouble([1,2],60.)
 * results in a=[[1,2,3][4,5,60.]]
 * Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
 * \param h         I Array handle
 * \param nIndices  I length of array indices
 * \param indices   I index specifier
 * \param value     I values at the specific indices
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_setValueAsDouble_indices(CTA_Array h, int nIndices, int *indices, double value);

/** \brief Set part of the values of this array.
 * Let a=[[1,2,3][4,5,6]] then setValuesAsDoubles(1,2,[20,30]) will result in a=[[1,20,30][4,5,6]]
 * Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
 * \param h          I Array handle
 * \param firstIndex I specifies the start of the selection
 * \param lastIndex  I specifies the end of the selection
 * \param values     I that will replace the selected range of numbers
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_setValuesAsDoubles_indexrange(CTA_Array h, int firstIndex, int lastIndex, double *values);

/** \brief Get a value from an array for specific indices. 
 * Eg let a=[[1,2,3][4,5,6]] then setValueAsDouble(5,60.)
 * results in a=[[1,2,3][4,5,60.]]
 * Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
 * \param h          I Array handle
 * \param index      I index specifier
 * \param value      I value that will replace the selected number
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_setValueAsDouble_index(CTA_Array h, int index, double value);

/** \brief Set whole vector equal to a constant value.
 * Note:  This method can only be used if all elements of the vector
 * have the same data type.
 * \param h     I Array handle
 * \param value I value to set
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_setConstant(CTA_Array h, double value);

/** \brief Perform a values += alpha * axpyValues</c> operation on each value in this array.
 * \param h          I Array handle
 * \param alpha      I The <c>alpha</c> in <c>state variable += alpha * vector</c>.
 * \param axpyValues I the values for the axpy-operation on all values in this array.
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_axpyOnValues(CTA_Array h, double alpha, double *axpyValues);

/** \brief Multiply each value in this array with the corresponding multiplication factor.
 * \param h                     I Array handle
 * \param multiplicationFactors I the multiplication factors for all array values.
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_multiplyValues(CTA_Array h, double *multiplicationFactors);

/** \brief Change the dimensions of an array. The new array should have the same length
 * \param h           I Array handle
 * \param nDimensions I length of array dimensions
 * \param dimensions  I new dimensions of array
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_reshape(CTA_Array h, int nDimensions, int *dimensions);


/** \brief Get part of the array by selection of a subset in one dimension.
 * Eg. a=[[1,2,3],[4,5,6]] a.getSlice(0,0) returns [1,2,3] 
 * Note that the number of dimensions IS reduced by one.
 * \param h           I Array handle
 * \param dimension   I Dimension to make selection in
 * \param index       I Element in dimension that is selected
 * \param h_out       O new Array with selected selection
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_getSlice(CTA_Array h, int dimension, int index, CTA_Array *h_out);

/** \brief Get part of the array by selection of a subset in one dimension.
 * Eg. a=[[1,2,3],[4,5,6],[7,8,9]] a.getSlice(0,0,1) returns [[1,2],[3,4]] 
 * Note that the number of dimensions is NOT reduced by one.
 * \param h           I Array handle
 * \param dimension   I Dimension to make selection in
 * \param minIndex    I start of range to select
 * \param maxIndex    I end of range to select
 * \param h_out       O new Array with selected selection
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_getSlice_range(CTA_Array h, int dimension, int minIndex, int maxIndex, CTA_Array *h_out);

/** \brief Get part of the array by selection of a subset in one dimension.
 * Eg. a=[[1,2,3],[4,5,6],[7,8,9]] a.getSlice(0,0,1) returns [1,2,3,4] 
 * \param h           I Array handle
 * \param dimension   I Dimension to make selection in
 * \param minIndex    I start of range to select
 * \param maxIndex    I end of range to select
 * \param values      O selected values
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_getSliceAsDoubles_range(CTA_Array h, int dimension, int minIndex, int maxIndex, double *values);


/** \brief Set the values of a part of an array. 
 * Eg. a=[[1,2,3],[4,5,6],[7,8,9]] and a.setSlice([11,12,13],1,1) 
 * sets the second column a=[[1,11,3],[4,12,6],[7,13,9]]
 * Note that the dimension of the slice is one smaller than for the array.
 * \param h         I Array handle
 * \param slice     I Values to set
 * \param dimension I Dimension that is replaced
 * \param index     I Index of selected dimension to replace
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_setSliceAsDoubles(CTA_Array h, double *slice, int dimension, int index);


/** \brief Set the values of a part of an array. 
 * Eg. a=[[1,2,3],[4,5,6],[7,8,9]] and a.setSlice([11,12,13],1,1) 
 * sets the second column a=[[1,11,3],[4,12,6],[7,13,9]]
 * Note that the dimension of the slice is one smaller than for the array.
 * \param h         I Array handle
 * \param slice_h   I Values to set
 * \param dimension I Dimension that is replaced
 * \param index     I Index of selected dimension to replace
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_setSliceAsArray(CTA_Array h, CTA_Array slice_h, int dimension, int index);

/** \brief Set the values of a part of an array. 
 * Eg. a=[[1,2,3],[4,5,6],[7,8,9]] and a.setSlice([[11,12,13],[14,15,16]],1,1,2) 
 * sets the second and third columns a=[[1,11,14],[4,12,15],[7,13,16]]
 * Note that the dimension of the slice is the same as for the array.
 * \param h         I Array handle
 * \param slice_h   I Values to set
 * \param dimension I Dimension that is replaced
 * \param minIndex  I Start of range in Dimension to be replaced
 * \param maxIndex  I End of range in Dimension to be replaced
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_setSliceAsArray_range(CTA_Array h, CTA_Array slice_h, int dimension, int minIndex, int maxIndex);

/** \brief Set the values of a part of an array. 
 * Eg. a=[[1,2,3],[4,5,6],[7,8,9]] and a.setSlice([11,12,13,14,15,16],1,1,2) 
 * sets the second and third columns a=[[1,11,14],[4,12,15],[7,13,16]]
 * Note that the dimension of the slice is the same as for the array.
 * \param h         I Array handle
 * \param slice     I Values to set
 * \param dimension I Dimension that is replaced
 * \param minIndex  I Start of range in Dimension to be replaced
 * \param maxIndex  I End of range in Dimension to be replaced
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_setSliceAsDoubles_range(CTA_Array h, double *slice, int dimension, int minIndex, int maxIndex);

/** \brief Convert indices in multiple dimensions to position in the one-dimensional array as
 * returned eg by getValuesAsDoubles
 * Eg. a=[[1,2,3],[4,5,6],[7,8,9]] then valueIndex([1,0]) returns 3 which points to the value 4 here.  
 * \param h        I Array handle
 * \param nIndices I length of indices
 * \param indices  I indices of element
 * \param index    O position of element
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Array_valueIndex(CTA_Array h, int nIndices, int *indices, int *index);


#ifdef __cplusplus
}
#endif





#endif
