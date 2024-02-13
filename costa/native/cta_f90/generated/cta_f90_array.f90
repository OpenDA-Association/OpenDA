module cta_f90_array

  implicit none

  public

  !  \brief Free Array instance
  ! 
  !  \param h IO handle of Array instance CTA_NULL on return
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_Free
    subroutine CTA_Array_Free( h, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  h
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_Free
  end interface

  !  \brief Create a new Array instance and fill with given values
  ! 
  !  \param values      I values of new Array
  !  \param nDimensions I Number of dimensions of new array
  !  \param dimensions  I Number of elements in each dimension
  !  \param h           O Array handle of new instance
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_CreateAsDoubles
    subroutine CTA_Array_CreateAsDoubles( values, nDimensions, dimensions, h, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      real(8)                       , intent(in   )     ::  values(*)
      integer                       , intent(in   )     ::  nDimensions
      integer                       , intent(in   )     ::  dimensions(*)
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  h
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_CreateAsDoubles
  end interface

  !  \brief Get number of dimensions, eg 1 for a vector and 2 for a matrix
  !  \param h           I Array handle
  !   \param status O rank of the array
  !
  interface CTA_F90_Array_getNumberOfDimensions
    subroutine CTA_Array_getNumberOfDimensions( h, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_getNumberOfDimensions
  end interface

  !  \brief Get the sizes of the individual dimensions
  !  \param h           I Array handle
  !  \param nDimensions O  size of array, eg 2 for the array [[3,4,10],[3,4,10]]
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_getnDimensions
    subroutine CTA_Array_getnDimensions( h, nDimensions, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h
      integer                       , intent(out  )     ::  nDimensions
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_getnDimensions
  end interface

  !  \brief Get the size of the array for each dimension.
  !  The length of the return value equals the number of dimensions.
  ! 
  !  \param h          I Array handle
  !  \param dimensions O size of array, eg [3,4,10]
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_getDimensions
    subroutine CTA_Array_getDimensions( h, dimensions, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h
      integer                       , intent(out  )     ::  dimensions
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_getDimensions
  end interface

  !  \brief Total number of elements in an array.
  !  This is equal to the product of the values returned by getDimension and is thus just for c
  ! 
  !  \param h      I Array handle
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_length
    subroutine CTA_Array_length( h, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_length
  end interface

  !  \brief Get all values of an array.
  !  The values are guaranteed be a copy if
  !  copyValues==true or may be a reference if copyValues==false.
  !  Note that the values are never guaranteed to be a reference and can not be used
  !  to change the array.
  ! 
  !  \param h      I Array handle
  !  \param values O Values from the array
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_getValuesAsDoubles
    subroutine CTA_Array_getValuesAsDoubles( h, values, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h
      real(8)                       , intent(out  )     ::  values
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_getValuesAsDoubles
  end interface

  !  \brief Get a part of the values from the array.
  !  Eg let a=[[1,2,3][4,5,6]] then getValuesAsDoubles(1,2)
  !  returns [2,3]
  !  Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
  !  \param h          I Array handle
  !  \param firstIndex I firstIndex of selection
  !  \param lastIndex  I lastIndex of selection
  !  \param values     O Values from the array
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_getValuesAsDoubles_indexrange
    subroutine CTA_Array_getValuesAsDoubles_indexrange( h, firstIndex, lastIndex, values, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h
      integer                       , intent(in   )     ::  firstIndex
      integer                       , intent(in   )     ::  lastIndex
      real(8)                       , intent(out  )     ::  values
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_getValuesAsDoubles_indexrange
  end interface

  !  \brief Get a value from an array for specific indices.
  !  Eg let a=[[1,2,3][4,5,6]] then getValueAsDouble(5)
  !  returns 6
  !  Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
  !  \param h       I Array handle
  !  \param index   I index specifier
  !  \param value   O double value at the specific indices
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_getValueAsDoubles_index
    subroutine CTA_Array_getValueAsDoubles_index( h, index, value, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h
      integer                       , intent(in   )     ::  index
      real(8)                       , intent(out  )     ::  value
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_getValueAsDoubles_index
  end interface

  !  \brief Get a value from an array for specific indices.
  !  Eg let a=[[1,2,3][4,5,6]] then getValueAsDouble([1,2])
  !  returns 6
  !  Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
  !  \param h        I Array handle
  !  \param nIndices I length of array of indices indices
  !  \param indices  I indices of value
  !  \param value    O value from array
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_getValueAsDouble_indices
    subroutine CTA_Array_getValueAsDouble_indices( h, nIndices, indices, value, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h
      integer                       , intent(in   )     ::  nIndices
      integer                       , intent(in   )     ::  indices
      real(8)                       , intent(out  )     ::  value
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_getValueAsDouble_indices
  end interface

  !  \brief Set the values of this array.
  !  \param h      I Array handle
  !  \param values I the values as an array of doubles
  !  \param length I length of array values
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_setValuesAsDoubles
    subroutine CTA_Array_setValuesAsDoubles( h, values, length, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h
      real(8)                       , intent(in   )     ::  values(*)
      integer                       , intent(in   )     ::  length
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_setValuesAsDoubles
  end interface

  !  \brief Set a value from an array for specific indices.
  !  Eg let a=[[1,2,3][4,5,6]] then setValueAsDouble([1,2],60.)
  !  results in a=[[1,2,3][4,5,60.]]
  !  Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
  !  \param h         I Array handle
  !  \param nIndices  I length of array indices
  !  \param indices   I index specifier
  !  \param value     I values at the specific indices
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_setValueAsDouble_indices
    subroutine CTA_Array_setValueAsDouble_indices( h, nIndices, indices, value, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h
      integer                       , intent(in   )     ::  nIndices
      integer                       , intent(in   )     ::  indices(*)
      real(8)                       , intent(in   )     ::  value
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_setValueAsDouble_indices
  end interface

  !  \brief Set part of the values of this array.
  !  Let a=[[1,2,3][4,5,6]] then setValuesAsDoubles(1,2,[20,30]) will result in a=[[1,20,30][4,5,6]]
  !  Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
  !  \param h          I Array handle
  !  \param firstIndex I specifies the start of the selection
  !  \param lastIndex  I specifies the end of the selection
  !  \param values     I that will replace the selected range of numbers
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_setValuesAsDoubles_indexrange
    subroutine CTA_Array_setValuesAsDoubles_indexrange( h, firstIndex, lastIndex, values, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h
      integer                       , intent(in   )     ::  firstIndex
      integer                       , intent(in   )     ::  lastIndex
      real(8)                       , intent(in   )     ::  values(*)
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_setValuesAsDoubles_indexrange
  end interface

  !  \brief Get a value from an array for specific indices.
  !  Eg let a=[[1,2,3][4,5,6]] then setValueAsDouble(5,60.)
  !  results in a=[[1,2,3][4,5,60.]]
  !  Note that negative values are allowed to denote counting from the end, is -1 denotes the last value.
  !  \param h          I Array handle
  !  \param index      I index specifier
  !  \param value      I value that will replace the selected number
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_setValueAsDouble_index
    subroutine CTA_Array_setValueAsDouble_index( h, index, value, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h
      integer                       , intent(in   )     ::  index
      real(8)                       , intent(in   )     ::  value
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_setValueAsDouble_index
  end interface

  !  \brief Set whole vector equal to a constant value.
  !  Note:  This method can only be used if all elements of the vector
  !  have the same data type.
  !  \param h     I Array handle
  !  \param value I value to set
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_setConstant
    subroutine CTA_Array_setConstant( h, value, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h
      real(8)                       , intent(in   )     ::  value
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_setConstant
  end interface

  !  \brief Perform a values += alpha * axpyValues</c> operation on each value in this array.
  !  \param h          I Array handle
  !  \param alpha      I The <c>alpha</c> in <c>state variable += alpha * vector</c>.
  !  \param axpyValues I the values for the axpy-operation on all values in this array.
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_axpyOnValues
    subroutine CTA_Array_axpyOnValues( h, alpha, axpyValues, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h
      real(8)                       , intent(in   )     ::  alpha
      real(8)                       , intent(in   )     ::  axpyValues(*)
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_axpyOnValues
  end interface

  !  \brief Multiply each value in this array with the corresponding multiplication factor.
  !  \param h                     I Array handle
  !  \param multiplicationFactors I the multiplication factors for all array values.
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_multiplyValues
    subroutine CTA_Array_multiplyValues( h, multiplicationFactors, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h
      real(8)                       , intent(in   )     ::  multiplicationFactors(*)
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_multiplyValues
  end interface

  !  \brief Change the dimensions of an array. The new array should have the same length
  !  \param h           I Array handle
  !  \param nDimensions I length of array dimensions
  !  \param dimensions  I new dimensions of array
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_reshape
    subroutine CTA_Array_reshape( h, nDimensions, dimensions, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h
      integer                       , intent(in   )     ::  nDimensions
      integer                       , intent(in   )     ::  dimensions(*)
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_reshape
  end interface

  !  \brief Get part of the array by selection of a subset in one dimension.
  !  Eg. a=[[1,2,3],[4,5,6]] a.getSlice(0,0) returns [1,2,3]
  !  Note that the number of dimensions IS reduced by one.
  !  \param h           I Array handle
  !  \param dimension   I Dimension to make selection in
  !  \param index       I Element in dimension that is selected
  !  \param h_out       O new Array with selected selection
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_getSlice
    subroutine CTA_Array_getSlice( h, dimension, index, h_out, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h
      integer                       , intent(in   )     ::  dimension
      integer                       , intent(in   )     ::  index
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  h_out
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_getSlice
  end interface

  !  \brief Get part of the array by selection of a subset in one dimension.
  !  Eg. a=[[1,2,3],[4,5,6],[7,8,9]] a.getSlice(0,0,1) returns [[1,2],[3,4]]
  !  Note that the number of dimensions is NOT reduced by one.
  !  \param h           I Array handle
  !  \param dimension   I Dimension to make selection in
  !  \param minIndex    I start of range to select
  !  \param maxIndex    I end of range to select
  !  \param h_out       O new Array with selected selection
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_getSlice_range
    subroutine CTA_Array_getSlice_range( h, dimension, minIndex, maxIndex, h_out, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h
      integer                       , intent(in   )     ::  dimension
      integer                       , intent(in   )     ::  minIndex
      integer                       , intent(in   )     ::  maxIndex
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  h_out
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_getSlice_range
  end interface

  !  \brief Get part of the array by selection of a subset in one dimension.
  !  Eg. a=[[1,2,3],[4,5,6],[7,8,9]] a.getSlice(0,0,1) returns [1,2,3,4]
  !  \param h           I Array handle
  !  \param dimension   I Dimension to make selection in
  !  \param minIndex    I start of range to select
  !  \param maxIndex    I end of range to select
  !  \param values      O selected values
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_getSliceAsDoubles_range
    subroutine CTA_Array_getSliceAsDoubles_range( h, dimension, minIndex, maxIndex, values, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h
      integer                       , intent(in   )     ::  dimension
      integer                       , intent(in   )     ::  minIndex
      integer                       , intent(in   )     ::  maxIndex
      real(8)                       , intent(out  )     ::  values
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_getSliceAsDoubles_range
  end interface

  !  \brief Set the values of a part of an array.
  !  Eg. a=[[1,2,3],[4,5,6],[7,8,9]] and a.setSlice([11,12,13],1,1)
  !  sets the second column a=[[1,11,3],[4,12,6],[7,13,9]]
  !  Note that the dimension of the slice is one smaller than for the array.
  !  \param h         I Array handle
  !  \param slice     I Values to set
  !  \param dimension I Dimension that is replaced
  !  \param index     I Index of selected dimension to replace
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_setSliceAsDoubles
    subroutine CTA_Array_setSliceAsDoubles( h, slice, dimension, index, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h
      real(8)                       , intent(in   )     ::  slice(*)
      integer                       , intent(in   )     ::  dimension
      integer                       , intent(in   )     ::  index
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_setSliceAsDoubles
  end interface

  !  \brief Set the values of a part of an array.
  !  Eg. a=[[1,2,3],[4,5,6],[7,8,9]] and a.setSlice([11,12,13],1,1)
  !  sets the second column a=[[1,11,3],[4,12,6],[7,13,9]]
  !  Note that the dimension of the slice is one smaller than for the array.
  !  \param h         I Array handle
  !  \param slice_h   I Values to set
  !  \param dimension I Dimension that is replaced
  !  \param index     I Index of selected dimension to replace
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_setSliceAsArray
    subroutine CTA_Array_setSliceAsArray( h, slice_h, dimension, index, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  slice_h
      integer                       , intent(in   )     ::  dimension
      integer                       , intent(in   )     ::  index
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_setSliceAsArray
  end interface

  !  \brief Set the values of a part of an array.
  !  Eg. a=[[1,2,3],[4,5,6],[7,8,9]] and a.setSlice([[11,12,13],[14,15,16]],1,1,2)
  !  sets the second and third columns a=[[1,11,14],[4,12,15],[7,13,16]]
  !  Note that the dimension of the slice is the same as for the array.
  !  \param h         I Array handle
  !  \param slice_h   I Values to set
  !  \param dimension I Dimension that is replaced
  !  \param minIndex  I Start of range in Dimension to be replaced
  !  \param maxIndex  I End of range in Dimension to be replaced
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_setSliceAsArray_range
    subroutine CTA_Array_setSliceAsArray_range( h, slice_h, dimension, minIndex, maxIndex, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  slice_h
      integer                       , intent(in   )     ::  dimension
      integer                       , intent(in   )     ::  minIndex
      integer                       , intent(in   )     ::  maxIndex
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_setSliceAsArray_range
  end interface

  !  \brief Set the values of a part of an array.
  !  Eg. a=[[1,2,3],[4,5,6],[7,8,9]] and a.setSlice([11,12,13,14,15,16],1,1,2)
  !  sets the second and third columns a=[[1,11,14],[4,12,15],[7,13,16]]
  !  Note that the dimension of the slice is the same as for the array.
  !  \param h         I Array handle
  !  \param slice     I Values to set
  !  \param dimension I Dimension that is replaced
  !  \param minIndex  I Start of range in Dimension to be replaced
  !  \param maxIndex  I End of range in Dimension to be replaced
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_setSliceAsDoubles_range
    subroutine CTA_Array_setSliceAsDoubles_range( h, slice, dimension, minIndex, maxIndex, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h
      real(8)                       , intent(in   )     ::  slice(*)
      integer                       , intent(in   )     ::  dimension
      integer                       , intent(in   )     ::  minIndex
      integer                       , intent(in   )     ::  maxIndex
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_setSliceAsDoubles_range
  end interface

  !  \brief Convert indices in multiple dimensions to position in the one-dimensional array as
  !  returned eg by getValuesAsDoubles
  !  Eg. a=[[1,2,3],[4,5,6],[7,8,9]] then valueIndex([1,0]) returns 3 which points to the value 4 here.
  !  \param h        I Array handle
  !  \param nIndices I length of indices
  !  \param indices  I indices of element
  !  \param index    O position of element
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Array_valueIndex
    subroutine CTA_Array_valueIndex( h, nIndices, indices, index, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h
      integer                       , intent(in   )     ::  nIndices
      integer                       , intent(in   )     ::  indices(*)
      integer                       , intent(out  )     ::  index
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Array_valueIndex
  end interface


end module cta_f90_array

