module cta_f90_vector

  implicit none

  public

  !  \brief Create a new class (=implementation) of a COSTA vector component.
  ! 
  !  \param name     I  name of the new vector class
  !  \param h_func   I  COSTA function handles for functions that implement class.
  !                     Missing functions must have value CTA_NULL
  !  \param hveccl   O  receives handle of new vector class
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Vector_DefineClass
    subroutine CTA_Vector_DefineClass( name, h_func, hveccl, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      character(len=*)              , intent(in   )     ::  name
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h_func(*)
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hveccl
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Vector_DefineClass
  end interface

  !  \brief Duplicate a vector object.
  ! 
  !  \note Only size, data type and type (class) are duplicated, the content is not
  !        copied.
  ! 
  !  \param hvector1  I  handle of vector to be duplicated
  !  \param hvector2  O  receives handle of new duplicate vector, empty before calling
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Vector_Duplicate
    subroutine CTA_Vector_Duplicate( hvector1, hvector2, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvector1
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hvector2
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Vector_Duplicate
  end interface

  !  \brief Create a new vector.
  ! 
  !  \note
  ! 
  !  \param hveccl   I  vector class of new vector
  !  \param n        I  number of elements
  !  \param datatype I  data type of elements in vector
  !  \param userdata IO user data for creation (depends on class)
  !  \param hvector  O  receives handle of new vector
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Vector_Create
    subroutine CTA_Vector_Create( hveccl, n, datatype, userdata, hvector, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hveccl
      integer                       , intent(in   )     ::  n
      integer                       , intent(in   )     ::  datatype
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  userdata
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hvector
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Vector_Create
  end interface

  !  \brief Get size of vector.
  ! 
  !  \param hvec  I  handle of vector
  !  \param n     O  receives number of elements
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Vector_GetSize
    subroutine CTA_Vector_GetSize( hvec, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      integer                       , intent(out  )     ::  n
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Vector_GetSize
  end interface

  !  \brief Get a copy of a single element in the vector.
  ! 
  !  \param hvec     I  handle of vector
  !  \param i        I  index of element
  !  \param vals     O  receives copy of value in vector, must exist before calling
  !  \param datatype I  data type of *vals, must be the same as data type of elements in vector
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Vector_GetVal
    module procedure CTA_Vector_GetVal_integer
    module procedure CTA_Vector_GetVal_real4
    module procedure CTA_Vector_GetVal_real8
  end interface

  !  \brief Get a copy of all elements in the vector.
  ! 
  !  \param hvec     I  handle of vector
  !  \param vals     O  receives copy of all elements in vector, must exist before calling
  !  \param n        I  length of array vals
  !  \param datatype I  data type of *vals, must be the same as data type of elements in vector
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Vector_GetVals
    module procedure CTA_Vector_GetVals_integer_1d
    module procedure CTA_Vector_GetVals_integer_2d
    module procedure CTA_Vector_GetVals_integer_3d
    module procedure CTA_Vector_GetVals_integer_4d
    module procedure CTA_Vector_GetVals_integer_5d
    module procedure CTA_Vector_GetVals_integer_6d
    module procedure CTA_Vector_GetVals_integer_7d
    module procedure CTA_Vector_GetVals_real4_1d
    module procedure CTA_Vector_GetVals_real4_2d
    module procedure CTA_Vector_GetVals_real4_3d
    module procedure CTA_Vector_GetVals_real4_4d
    module procedure CTA_Vector_GetVals_real4_5d
    module procedure CTA_Vector_GetVals_real4_6d
    module procedure CTA_Vector_GetVals_real4_7d
    module procedure CTA_Vector_GetVals_real8_1d
    module procedure CTA_Vector_GetVals_real8_2d
    module procedure CTA_Vector_GetVals_real8_3d
    module procedure CTA_Vector_GetVals_real8_4d
    module procedure CTA_Vector_GetVals_real8_5d
    module procedure CTA_Vector_GetVals_real8_6d
    module procedure CTA_Vector_GetVals_real8_7d
  end interface

  !  \brief Set a copy of an element in the vector.
  ! 
  !  \note The value is copied in the vector.
  ! 
  !  \param hvec     IO handle of vector
  !  \param i        I  index of element
  !  \param val      I  new value that is copied into element at given index
  !  \param datatype I  data type of *val, must be the same as data type of elements in vector
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Vector_SetVal
    module procedure CTA_Vector_SetVal_integer
    module procedure CTA_Vector_SetVal_real4
    module procedure CTA_Vector_SetVal_real8
  end interface

  !  \brief Set all elementes in the vector.
  ! 
  !  \note The values are copied in the vector.
  ! 
  !  \param hvec     IO handle of vector
  !  \param vals     I  values that need to be copied to vector
  !  \param n        I  number of elements in vals
  !  \param datatype I  data type of *vals, must be the same as data type of elements in vector
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Vector_SetVals
    module procedure CTA_Vector_SetVals_integer_1d
    module procedure CTA_Vector_SetVals_integer_2d
    module procedure CTA_Vector_SetVals_integer_3d
    module procedure CTA_Vector_SetVals_integer_4d
    module procedure CTA_Vector_SetVals_integer_5d
    module procedure CTA_Vector_SetVals_integer_6d
    module procedure CTA_Vector_SetVals_integer_7d
    module procedure CTA_Vector_SetVals_real4_1d
    module procedure CTA_Vector_SetVals_real4_2d
    module procedure CTA_Vector_SetVals_real4_3d
    module procedure CTA_Vector_SetVals_real4_4d
    module procedure CTA_Vector_SetVals_real4_5d
    module procedure CTA_Vector_SetVals_real4_6d
    module procedure CTA_Vector_SetVals_real4_7d
    module procedure CTA_Vector_SetVals_real8_1d
    module procedure CTA_Vector_SetVals_real8_2d
    module procedure CTA_Vector_SetVals_real8_3d
    module procedure CTA_Vector_SetVals_real8_4d
    module procedure CTA_Vector_SetVals_real8_5d
    module procedure CTA_Vector_SetVals_real8_6d
    module procedure CTA_Vector_SetVals_real8_7d
  end interface

  !  \brief Scale a vector.
  ! 
  !  \note scale: x=alpha*x
  ! 
  !  \param hvec     IO handle of vector
  !  \param alpha    I  scalar
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Vector_Scal
    subroutine CTA_Vector_Scal( hvec, alpha, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      real(8)                       , intent(in   )     ::  alpha
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Vector_Scal
  end interface

  !  \brief Copy a vector
  ! 
  !  \note This function copies the vector content (y=x), but does not make a new vector.
  ! 
  !  \param hvec_x  I  handle of sending vector
  !  \param hvec_y  O  handle of receiving vector
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Vector_Copy
    subroutine CTA_Vector_Copy( hvec_x, hvec_y, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec_x
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hvec_y
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Vector_Copy
  end interface

  !  \brief Operation axpy for two vectors x and y.
  ! 
  !  \note axpy: y=alpha*x+y
  ! 
  !  \param hvec_y  IO handle of vector y
  !  \param alpha   I  scalar
  !  \param hvec_x  I  handle of vector x
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Vector_Axpy
    subroutine CTA_Vector_Axpy( hvec_y, alpha, hvec_x, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec_y
      real(8)                       , intent(in   )     ::  alpha
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec_x
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Vector_Axpy
  end interface

  !  \brief Dot product operation of two vectors.
  ! 
  !  \note dot: dotprod=x^t*y
  ! 
  !  \param hvec_x  I  handle of vector x
  !  \param hvec_y  I  handle of vector y
  !  \param dotprod O  receives dot product
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Vector_Dot
    subroutine CTA_Vector_Dot( hvec_x, hvec_y, dotprod, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec_x
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec_y
      real(8)                       , intent(out  )     ::  dotprod
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Vector_Dot
  end interface

  !  \brief Compute 2-norm of vector.
  ! 
  !  \note 2-norm: sqrt(x^t*x)
  ! 
  !  \param hvec_x  I  handle of vector x
  !  \param norm2   O  receives 2-norm
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Vector_Nrm2
    subroutine CTA_Vector_Nrm2( hvec_x, norm2, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec_x
      real(8)                       , intent(out  )     ::  norm2
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Vector_Nrm2
  end interface

  !  \brief Find index of element in vector with largest absolute value
  ! 
  !  \param hvec_x  I  handle of vector
  !  \param iloc    O  receives index of largest absolute value
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Vector_Amax
    subroutine CTA_Vector_Amax( hvec_x, iloc, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec_x
      integer                       , intent(out  )     ::  iloc
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Vector_Amax
  end interface

  !  \brief Find largest length of elements in vector.
  ! 
  !  \note e.g. length of string in a vector of strings
  ! 
  !  \param hvec_x  I  handle of vector
  !  \param maxlen  O  receives largest length of elements in vector
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Vector_GetMaxLen
    subroutine CTA_Vector_GetMaxLen( hvec_x, maxlen, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec_x
      integer                       , intent(out  )     ::  maxlen
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Vector_GetMaxLen
  end interface

  !  \brief Export a vector to file, stdout or pack object.
  ! 
  !  \Note CTA_DEFAULT_VECTOR supports exporting to:\n
  !        file (usrdata a handle of COSTA file)\n
  !        pack object (usrdata a handle of COSTA pack object)\n
  ! 
  !  \param hvec     I  handle of vector
  !  \param usrdata  I  configuration of output
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Vector_Export
    subroutine CTA_Vector_Export( hvec, usrdata, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  usrdata
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Vector_Export
  end interface

  !  \brief Import a vector.
  ! 
  !  \note CTA_DEFAULT_VECTOR supports importing from pack object (usrdata[0] a pack handle).
  !  \note Data type and size of vector can be changed due to this action
  ! 
  !  \param hvec     I  handle of vector
  !  \param usrdata  I  configuration of output
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Vector_Import
    subroutine CTA_Vector_Import( hvec, usrdata, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  usrdata
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Vector_Import
  end interface

  !  \brief Print table, each column built up by a vector.
  ! 
  !  \note CTA_DEFAULT_VECTOR    TODO
  ! 
  !  \param table    I  array of vector handles, these vectors form the table to be filled
  !  \param ncolumns I  number of columns in table
  !                     (number of vector handles in table)
  !  \param vformats I  handle of vector of string (size ncolumns) containing the
  !                     formats for printing each column (C-format)
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Vector_Print_Table
    subroutine CTA_Vector_Print_Table( table, ncolumns, vformats, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  table
      integer                       , intent(in   )     ::  ncolumns
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  vformats
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Vector_Print_Table
  end interface

  !  \brief Free the vector object.
  ! 
  ! 
  !  \Note hvec_x=CTA_NULL is allowed
  ! 
  !  \param hvec_x  IO handle of vector, replaced by CTA_NULL on return
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Vector_Free
    subroutine CTA_Vector_Free( hvec_x, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec_x
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Vector_Free
  end interface

  !  \brief Get type of vector.
  ! 
  !  \param hvec     I  handle of vector
  !  \param datatype O  receives data type of vector
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Vector_GetDatatype
    subroutine CTA_Vector_GetDatatype( hvec, datatype, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      integer                       , intent(out  )     ::  datatype
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Vector_GetDatatype
  end interface

  !  \brief Set whole vector to one single value.
  ! 
  !  \Note hvec=CTA_NULL is allowed, function returns error code
  ! 
  !  \param hvec     IO handle of vector
  !  \param val      I  value that must be set
  !  \param datatype I  data type of *val, must be the same as data type of elements in vector
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Vector_SetConstant
    module procedure CTA_Vector_SetConstant_integer
    module procedure CTA_Vector_SetConstant_real4
    module procedure CTA_Vector_SetConstant_real8
  end interface

  !  \brief Element wise division
  ! 
  !  \note y=x./y
  ! 
  !  \param hvec_y  IO handle of vector y
  !  \param hvec_x  I  handle of vector x
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Vector_ElmDiv
    subroutine CTA_Vector_ElmDiv( hvec_y, hvec_x, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec_y
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec_x
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Vector_ElmDiv
  end interface

  !  \brief Element wise product
  ! 
  !  \note y=x.y
  ! 
  !  \param hvec_y  IO handle of vector y
  !  \param hvec_x  I  handle of vector x
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Vector_ElmProd
    subroutine CTA_Vector_ElmProd( hvec_y, hvec_x, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec_y
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec_x
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Vector_ElmProd
  end interface

  !  \brief Element wise square root
  ! 
  !  \note y=y.^0.5
  ! 
  !  \param  hvec_x  IO handle of vector y
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Vector_ElmSqrt
    subroutine CTA_Vector_ElmSqrt( hvec_x, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec_x
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Vector_ElmSqrt
  end interface


contains

    subroutine CTA_Vector_GetVal_integer( hvec, i, vals, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      integer                       , intent(in   )     ::  i
      integer                       , intent(out  )     ::  vals
      integer                       , intent(out  )     ::  status
      call CTA_Vector_GetVal( hvec, i, vals, CTA_INTEGER, status )
    end subroutine CTA_Vector_GetVal_integer

    subroutine CTA_Vector_GetVal_real4( hvec, i, vals, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      integer                       , intent(in   )     ::  i
      real(4)                       , intent(out  )     ::  vals
      integer                       , intent(out  )     ::  status
      call CTA_Vector_GetVal( hvec, i, vals, CTA_REAL, status )
    end subroutine CTA_Vector_GetVal_real4

    subroutine CTA_Vector_GetVal_real8( hvec, i, vals, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      integer                       , intent(in   )     ::  i
      real(8)                       , intent(out  )     ::  vals
      integer                       , intent(out  )     ::  status
      call CTA_Vector_GetVal( hvec, i, vals, CTA_DOUBLE, status )
    end subroutine CTA_Vector_GetVal_real8

    subroutine CTA_Vector_GetVals_integer_1d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      integer                       , intent(out  )     ::  vals(:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_GetVals( hvec, vals, n, CTA_INTEGER, status )
    end subroutine CTA_Vector_GetVals_integer_1d

    subroutine CTA_Vector_GetVals_integer_2d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      integer                       , intent(out  )     ::  vals(:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_GetVals( hvec, vals, n, CTA_INTEGER, status )
    end subroutine CTA_Vector_GetVals_integer_2d

    subroutine CTA_Vector_GetVals_integer_3d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      integer                       , intent(out  )     ::  vals(:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_GetVals( hvec, vals, n, CTA_INTEGER, status )
    end subroutine CTA_Vector_GetVals_integer_3d

    subroutine CTA_Vector_GetVals_integer_4d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      integer                       , intent(out  )     ::  vals(:,:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_GetVals( hvec, vals, n, CTA_INTEGER, status )
    end subroutine CTA_Vector_GetVals_integer_4d

    subroutine CTA_Vector_GetVals_integer_5d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      integer                       , intent(out  )     ::  vals(:,:,:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_GetVals( hvec, vals, n, CTA_INTEGER, status )
    end subroutine CTA_Vector_GetVals_integer_5d

    subroutine CTA_Vector_GetVals_integer_6d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      integer                       , intent(out  )     ::  vals(:,:,:,:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_GetVals( hvec, vals, n, CTA_INTEGER, status )
    end subroutine CTA_Vector_GetVals_integer_6d

    subroutine CTA_Vector_GetVals_integer_7d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      integer                       , intent(out  )     ::  vals(:,:,:,:,:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_GetVals( hvec, vals, n, CTA_INTEGER, status )
    end subroutine CTA_Vector_GetVals_integer_7d

    subroutine CTA_Vector_GetVals_real4_1d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      real(4)                       , intent(out  )     ::  vals(:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_GetVals( hvec, vals, n, CTA_REAL, status )
    end subroutine CTA_Vector_GetVals_real4_1d

    subroutine CTA_Vector_GetVals_real4_2d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      real(4)                       , intent(out  )     ::  vals(:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_GetVals( hvec, vals, n, CTA_REAL, status )
    end subroutine CTA_Vector_GetVals_real4_2d

    subroutine CTA_Vector_GetVals_real4_3d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      real(4)                       , intent(out  )     ::  vals(:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_GetVals( hvec, vals, n, CTA_REAL, status )
    end subroutine CTA_Vector_GetVals_real4_3d

    subroutine CTA_Vector_GetVals_real4_4d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      real(4)                       , intent(out  )     ::  vals(:,:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_GetVals( hvec, vals, n, CTA_REAL, status )
    end subroutine CTA_Vector_GetVals_real4_4d

    subroutine CTA_Vector_GetVals_real4_5d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      real(4)                       , intent(out  )     ::  vals(:,:,:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_GetVals( hvec, vals, n, CTA_REAL, status )
    end subroutine CTA_Vector_GetVals_real4_5d

    subroutine CTA_Vector_GetVals_real4_6d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      real(4)                       , intent(out  )     ::  vals(:,:,:,:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_GetVals( hvec, vals, n, CTA_REAL, status )
    end subroutine CTA_Vector_GetVals_real4_6d

    subroutine CTA_Vector_GetVals_real4_7d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      real(4)                       , intent(out  )     ::  vals(:,:,:,:,:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_GetVals( hvec, vals, n, CTA_REAL, status )
    end subroutine CTA_Vector_GetVals_real4_7d

    subroutine CTA_Vector_GetVals_real8_1d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      real(8)                       , intent(out  )     ::  vals(:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_GetVals( hvec, vals, n, CTA_DOUBLE, status )
    end subroutine CTA_Vector_GetVals_real8_1d

    subroutine CTA_Vector_GetVals_real8_2d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      real(8)                       , intent(out  )     ::  vals(:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_GetVals( hvec, vals, n, CTA_DOUBLE, status )
    end subroutine CTA_Vector_GetVals_real8_2d

    subroutine CTA_Vector_GetVals_real8_3d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      real(8)                       , intent(out  )     ::  vals(:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_GetVals( hvec, vals, n, CTA_DOUBLE, status )
    end subroutine CTA_Vector_GetVals_real8_3d

    subroutine CTA_Vector_GetVals_real8_4d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      real(8)                       , intent(out  )     ::  vals(:,:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_GetVals( hvec, vals, n, CTA_DOUBLE, status )
    end subroutine CTA_Vector_GetVals_real8_4d

    subroutine CTA_Vector_GetVals_real8_5d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      real(8)                       , intent(out  )     ::  vals(:,:,:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_GetVals( hvec, vals, n, CTA_DOUBLE, status )
    end subroutine CTA_Vector_GetVals_real8_5d

    subroutine CTA_Vector_GetVals_real8_6d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      real(8)                       , intent(out  )     ::  vals(:,:,:,:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_GetVals( hvec, vals, n, CTA_DOUBLE, status )
    end subroutine CTA_Vector_GetVals_real8_6d

    subroutine CTA_Vector_GetVals_real8_7d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      real(8)                       , intent(out  )     ::  vals(:,:,:,:,:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_GetVals( hvec, vals, n, CTA_DOUBLE, status )
    end subroutine CTA_Vector_GetVals_real8_7d

    subroutine CTA_Vector_SetVal_integer( hvec, i, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      integer                       , intent(in   )     ::  i
      integer                       , intent(in   )     ::  val
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetVal( hvec, i, val, CTA_INTEGER, status )
    end subroutine CTA_Vector_SetVal_integer

    subroutine CTA_Vector_SetVal_real4( hvec, i, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      integer                       , intent(in   )     ::  i
      real(4)                       , intent(in   )     ::  val
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetVal( hvec, i, val, CTA_REAL, status )
    end subroutine CTA_Vector_SetVal_real4

    subroutine CTA_Vector_SetVal_real8( hvec, i, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      integer                       , intent(in   )     ::  i
      real(8)                       , intent(in   )     ::  val
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetVal( hvec, i, val, CTA_DOUBLE, status )
    end subroutine CTA_Vector_SetVal_real8

    subroutine CTA_Vector_SetVals_integer_1d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      integer                       , intent(in   )     ::  vals(:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetVals( hvec, vals, n, CTA_INTEGER, status )
    end subroutine CTA_Vector_SetVals_integer_1d

    subroutine CTA_Vector_SetVals_integer_2d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      integer                       , intent(in   )     ::  vals(:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetVals( hvec, vals, n, CTA_INTEGER, status )
    end subroutine CTA_Vector_SetVals_integer_2d

    subroutine CTA_Vector_SetVals_integer_3d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      integer                       , intent(in   )     ::  vals(:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetVals( hvec, vals, n, CTA_INTEGER, status )
    end subroutine CTA_Vector_SetVals_integer_3d

    subroutine CTA_Vector_SetVals_integer_4d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      integer                       , intent(in   )     ::  vals(:,:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetVals( hvec, vals, n, CTA_INTEGER, status )
    end subroutine CTA_Vector_SetVals_integer_4d

    subroutine CTA_Vector_SetVals_integer_5d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      integer                       , intent(in   )     ::  vals(:,:,:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetVals( hvec, vals, n, CTA_INTEGER, status )
    end subroutine CTA_Vector_SetVals_integer_5d

    subroutine CTA_Vector_SetVals_integer_6d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      integer                       , intent(in   )     ::  vals(:,:,:,:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetVals( hvec, vals, n, CTA_INTEGER, status )
    end subroutine CTA_Vector_SetVals_integer_6d

    subroutine CTA_Vector_SetVals_integer_7d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      integer                       , intent(in   )     ::  vals(:,:,:,:,:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetVals( hvec, vals, n, CTA_INTEGER, status )
    end subroutine CTA_Vector_SetVals_integer_7d

    subroutine CTA_Vector_SetVals_real4_1d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      real(4)                       , intent(in   )     ::  vals(:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetVals( hvec, vals, n, CTA_REAL, status )
    end subroutine CTA_Vector_SetVals_real4_1d

    subroutine CTA_Vector_SetVals_real4_2d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      real(4)                       , intent(in   )     ::  vals(:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetVals( hvec, vals, n, CTA_REAL, status )
    end subroutine CTA_Vector_SetVals_real4_2d

    subroutine CTA_Vector_SetVals_real4_3d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      real(4)                       , intent(in   )     ::  vals(:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetVals( hvec, vals, n, CTA_REAL, status )
    end subroutine CTA_Vector_SetVals_real4_3d

    subroutine CTA_Vector_SetVals_real4_4d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      real(4)                       , intent(in   )     ::  vals(:,:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetVals( hvec, vals, n, CTA_REAL, status )
    end subroutine CTA_Vector_SetVals_real4_4d

    subroutine CTA_Vector_SetVals_real4_5d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      real(4)                       , intent(in   )     ::  vals(:,:,:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetVals( hvec, vals, n, CTA_REAL, status )
    end subroutine CTA_Vector_SetVals_real4_5d

    subroutine CTA_Vector_SetVals_real4_6d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      real(4)                       , intent(in   )     ::  vals(:,:,:,:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetVals( hvec, vals, n, CTA_REAL, status )
    end subroutine CTA_Vector_SetVals_real4_6d

    subroutine CTA_Vector_SetVals_real4_7d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      real(4)                       , intent(in   )     ::  vals(:,:,:,:,:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetVals( hvec, vals, n, CTA_REAL, status )
    end subroutine CTA_Vector_SetVals_real4_7d

    subroutine CTA_Vector_SetVals_real8_1d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      real(8)                       , intent(in   )     ::  vals(:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetVals( hvec, vals, n, CTA_DOUBLE, status )
    end subroutine CTA_Vector_SetVals_real8_1d

    subroutine CTA_Vector_SetVals_real8_2d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      real(8)                       , intent(in   )     ::  vals(:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetVals( hvec, vals, n, CTA_DOUBLE, status )
    end subroutine CTA_Vector_SetVals_real8_2d

    subroutine CTA_Vector_SetVals_real8_3d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      real(8)                       , intent(in   )     ::  vals(:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetVals( hvec, vals, n, CTA_DOUBLE, status )
    end subroutine CTA_Vector_SetVals_real8_3d

    subroutine CTA_Vector_SetVals_real8_4d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      real(8)                       , intent(in   )     ::  vals(:,:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetVals( hvec, vals, n, CTA_DOUBLE, status )
    end subroutine CTA_Vector_SetVals_real8_4d

    subroutine CTA_Vector_SetVals_real8_5d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      real(8)                       , intent(in   )     ::  vals(:,:,:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetVals( hvec, vals, n, CTA_DOUBLE, status )
    end subroutine CTA_Vector_SetVals_real8_5d

    subroutine CTA_Vector_SetVals_real8_6d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      real(8)                       , intent(in   )     ::  vals(:,:,:,:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetVals( hvec, vals, n, CTA_DOUBLE, status )
    end subroutine CTA_Vector_SetVals_real8_6d

    subroutine CTA_Vector_SetVals_real8_7d( hvec, vals, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      real(8)                       , intent(in   )     ::  vals(:,:,:,:,:,:,:)
      integer                       , intent(in   )     ::  n
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetVals( hvec, vals, n, CTA_DOUBLE, status )
    end subroutine CTA_Vector_SetVals_real8_7d

    subroutine CTA_Vector_SetConstant_integer( hvec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      integer                       , intent(in   )     ::  val
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetConstant( hvec, val, CTA_INTEGER, status )
    end subroutine CTA_Vector_SetConstant_integer

    subroutine CTA_Vector_SetConstant_real4( hvec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      real(4)                       , intent(in   )     ::  val
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetConstant( hvec, val, CTA_REAL, status )
    end subroutine CTA_Vector_SetConstant_real4

    subroutine CTA_Vector_SetConstant_real8( hvec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hvec
      real(8)                       , intent(in   )     ::  val
      integer                       , intent(out  )     ::  status
      call CTA_Vector_SetConstant( hvec, val, CTA_DOUBLE, status )
    end subroutine CTA_Vector_SetConstant_real8

end module cta_f90_vector

