module cta_f90_treevector

  implicit none

  public

  !  \brief Create a tree-vector.
  ! 
  !  \param name    I  name of the tree-vector, this is a human readable
  !                    string used for (debug) output and not by the algorithms
  !                    itself
  !  \param tag     I  tag of this tree-vector
  !  \param treevec  O new tree-vector
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_Create
    subroutine CTA_TreeVector_Create( name, tag, treevec, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      character(len=*)              , intent(in   )     ::  name
      character(len=*)              , intent(in   )     ::  tag
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  treevec
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_Create
  end interface

  !  \brief Duplicate a tree-vector.
  ! 
  !  \note Duplication means that a new tree-vector is created that is identical to
  !  the originating tree-vector. All data in the original tree-vector is also copied.
  ! 
  !  \param treevec1  I  handle of treevector to be duplicated
  !  \param treevec2  O  receives handle to duplicate
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_Duplicate
    subroutine CTA_TreeVector_Duplicate( treevec1, treevec2, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec1
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  treevec2
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_Duplicate
  end interface

  !  \brief Define a tree-vector to be a concatination of other tree-vectors.
  ! 
  !  \note The concatenation is done by reference (handle). The sub-tree-vectors that
  !        are concatenated are not copied.
  ! 
  !  \param treevec1  I  tree-vector that will be concatenation of the sub-tree-vectors provided in parameter treevecs
  !  \param treevecs  I  array of the sub-tree-vectors
  !  \param ntreevecs I  number of sub-tree-vectors in treevecs
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_Conc
    subroutine CTA_TreeVector_Conc( treevec1, treevecs, ntreevecs, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec1
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevecs(*)
      integer                       , intent(in   )     ::  ntreevecs
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_Conc
  end interface

  !  \brief Get the handle of a sub-tree-vectors using its tag.
  ! 
  !  \note This is done by reference (handle). The handle of the
  !        returned sub-tree-vector is not a copy
  ! 
  !  \param treevec   I  Tree-vector
  !  \param tag       I  tag of the requested sub-tree-vector
  !  \param subtreevec O  receives handle of the requested sub-tree-vectors, this is by reference, not a copy
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_GetSubTreeVec
    subroutine CTA_TreeVector_GetSubTreeVec( treevec, tag, subtreevec, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      character(len=*)              , intent(in   )     ::  tag
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  subtreevec
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_GetSubTreeVec
  end interface

  !  \brief Get the tag of a sub-tree-vector using its index (starting with 0).
  ! 
  !  \param treevec   I  Tree-vector
  !  \param index     I  index of the requested sub-tree-vector
  !  \param tag       O  String of standard length containnig the tag
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_GetSubTreeVecId
    subroutine CTA_TreeVector_GetSubTreeVecId( treevec, index, tag, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      integer                       , intent(in   )     ::  index
      character(len=*)              , intent(out  )     ::  tag(*)
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_GetSubTreeVecId
  end interface

  !  \brief Get the handle of a first-layer sub-tree-vector using its index.
  ! 
  !  \note The concatination is done by reference (handle). The handle of the
  !        returned sub-tree-vector is not a copy
  ! 
  !  \param treevec   I  Tree-vector
  !  \param index     I  index of requested sub-tree-vector. Note that the first sub-tree-vector has index 1.
  !  \param subtreevec O  receives handle of the requested sub-tree-vector, this is by reference, not a copy
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_GetSubTreeVecIndex
    subroutine CTA_TreeVector_GetSubTreeVecIndex( treevec, index, subtreevec, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      integer                       , intent(in   )     ::  index
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  subtreevec
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_GetSubTreeVecIndex
  end interface

  !  \brief Get number of sub-treevectors
  ! 
  !  \note In case of a leaf 0 is returned
  ! 
  !  \param treevec      I  Tree-vector
  !  \param numSubTrees  O  Number of sub-treevectors
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_GetNumSubTree
    subroutine CTA_TreeVector_GetNumSubTree( treevec, numSubTrees, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      integer                       , intent(out  )     ::  numSubTrees
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_GetNumSubTree
  end interface

  !  \brief Get the tag of the tree-vector.
  ! 
  !  Note tag should be large enough to hold the result
  !       length of CTA_STRLEN_TAG is always save (no internal protection)
  ! 
  !  \param treevec   I  Tree-vector
  !  \param tag       O  receives the tag of the requested sub-tree-vector (see note)
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_GetTag
    subroutine CTA_TreeVector_GetTag( treevec, tag, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      character(len=*)              , intent(out  )     ::  tag
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_GetTag
  end interface

  !  \brief Set the values of the tree-vector
  ! 
  !  \note This operation is only possible when all data elements in the tree-vector
  !        are of the same type and the size of the tree-vector corresponds to the
  !        size of the input vector.
  ! 
  !  \param treevec   IO TreeVector
  !  \param hvec      I  handle of the vector containing new values (see note)
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_SetVec
    subroutine CTA_TreeVector_SetVec( treevec, hvec, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_SetVec
  end interface

  !  \brief Get the values of the tree-vector.
  ! 
  !  \note This operation is only possible when all data elements in the tree-vector
  !        are of the same type and the size of the tree-vector corresponds to the
  !        vector size.
  ! 
  !  \param treevec   I  Tree-vector
  !  \param hvec      O  Vector that is receiving the values; must exist before calling
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_GetVec
    subroutine CTA_TreeVector_GetVec( treevec, hvec, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hvec
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_GetVec
  end interface

  !  \brief Axpy operation between two tree-vectors.
  ! 
  !  \note  Axpy: y=alpha*x+y. Add alpha times tree-vector x to
  !               this tree-vector (y).
  ! 
  !  \param y         IO Tree-vector (y)
  !  \param alpha     I  scalar
  !  \param x         I  Tree-vector (x)
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_Axpy
    subroutine CTA_TreeVector_Axpy( y, alpha, x, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  y
      real(8)                       , intent(in   )     ::  alpha
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  x
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_Axpy
  end interface

  !  \brief Compute dot product of two tree-vectors.
  ! 
  !  \note  dotprod = sum[all i]  (treevec1_i * treevec2_i)
  ! 
  !  \param treevec1  I  first tree-vector
  !  \param treevec2  I  second tree-vector
  !  \param dotprod   O  receives the dot product
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_Dot
    subroutine CTA_TreeVector_Dot( treevec1, treevec2, dotprod, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec1
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec2
      real(8)                       , intent(out  )     ::  dotprod
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_Dot
  end interface

  !  \brief Compute the 2-norm of a tree-vector.
  ! 
  !  \param treevec1  I  Tree-vector
  !  \param nrm2      O  receives the 2-norm
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_Nrm2
    subroutine CTA_TreeVector_Nrm2( treevec1, nrm2, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec1
      real(8)                       , intent(out  )     ::  nrm2
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_Nrm2
  end interface

  !  \brief Copy a tree-vector
  ! 
  !  \note  The two tree-vectors must be compatible: same structure and datatypes.
  ! 
  !  \param treevec1   I  sending tree-vector
  !  \param treevec2   O  receiving tree-vector
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_Copy
    subroutine CTA_TreeVector_Copy( treevec1, treevec2, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec1
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  treevec2
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_Copy
  end interface

  !  \brief Set whole tree-vector equal to a constant value.
  ! 
  !  \note  This method can only be used if all elements of the tree-vector
  !         have the same data type.
  ! 
  !  \param treevec  IO TreeVector
  !  \param val      I  value to set
  !  \param datatype I  data type of val, must be same as data type of tree-vector
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_SetConstant
    module procedure CTA_TreeVector_SetConstant_integer
    module procedure CTA_TreeVector_SetConstant_real4
    module procedure CTA_TreeVector_SetConstant_real8
  end interface

  !  \brief Scale tree-vector.
  ! 
  !  \param treevec   IO handle of tree-vector
  !  \param alpha    I  scalar
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_Scal
    subroutine CTA_TreeVector_Scal( treevec, alpha, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      real(8)                       , intent(in   )     ::  alpha
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_Scal
  end interface

  !  \brief Set all values of the tree-vector.
  ! 
  !  \note  This method can only be used if all elements of the tree-vector
  !         are of the same data type.
  ! 
  !  \param treevec  IO Tree-vector
  !  \param val      I  values to be set
  !  \param nval     I  number of elements in val
  !  \param datatype I  data type of *val, must be the same as data type of elements in tree-vector
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_SetVals
    module procedure CTA_TreeVector_SetVals_integer_1d
    module procedure CTA_TreeVector_SetVals_integer_2d
    module procedure CTA_TreeVector_SetVals_integer_3d
    module procedure CTA_TreeVector_SetVals_integer_4d
    module procedure CTA_TreeVector_SetVals_integer_5d
    module procedure CTA_TreeVector_SetVals_integer_6d
    module procedure CTA_TreeVector_SetVals_integer_7d
    module procedure CTA_TreeVector_SetVals_real4_1d
    module procedure CTA_TreeVector_SetVals_real4_2d
    module procedure CTA_TreeVector_SetVals_real4_3d
    module procedure CTA_TreeVector_SetVals_real4_4d
    module procedure CTA_TreeVector_SetVals_real4_5d
    module procedure CTA_TreeVector_SetVals_real4_6d
    module procedure CTA_TreeVector_SetVals_real4_7d
    module procedure CTA_TreeVector_SetVals_real8_1d
    module procedure CTA_TreeVector_SetVals_real8_2d
    module procedure CTA_TreeVector_SetVals_real8_3d
    module procedure CTA_TreeVector_SetVals_real8_4d
    module procedure CTA_TreeVector_SetVals_real8_5d
    module procedure CTA_TreeVector_SetVals_real8_6d
    module procedure CTA_TreeVector_SetVals_real8_7d
  end interface

  !  \brief Get all values of the tree-vector.
  ! 
  !  \note  This method can only be used if all elements of the tree-vector
  !         are of the same data type.
  ! 
  !  \param treevec  I  Tree-vector
  !  \param val      O  receives the values
  !  \param nval     I  number of elements in val
  !  \param datatype I  data type of *val, must be the same as data type of elements in tree-vector
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_GetVals
    module procedure CTA_TreeVector_GetVals_integer_1d
    module procedure CTA_TreeVector_GetVals_integer_2d
    module procedure CTA_TreeVector_GetVals_integer_3d
    module procedure CTA_TreeVector_GetVals_integer_4d
    module procedure CTA_TreeVector_GetVals_integer_5d
    module procedure CTA_TreeVector_GetVals_integer_6d
    module procedure CTA_TreeVector_GetVals_integer_7d
    module procedure CTA_TreeVector_GetVals_real4_1d
    module procedure CTA_TreeVector_GetVals_real4_2d
    module procedure CTA_TreeVector_GetVals_real4_3d
    module procedure CTA_TreeVector_GetVals_real4_4d
    module procedure CTA_TreeVector_GetVals_real4_5d
    module procedure CTA_TreeVector_GetVals_real4_6d
    module procedure CTA_TreeVector_GetVals_real4_7d
    module procedure CTA_TreeVector_GetVals_real8_1d
    module procedure CTA_TreeVector_GetVals_real8_2d
    module procedure CTA_TreeVector_GetVals_real8_3d
    module procedure CTA_TreeVector_GetVals_real8_4d
    module procedure CTA_TreeVector_GetVals_real8_5d
    module procedure CTA_TreeVector_GetVals_real8_6d
    module procedure CTA_TreeVector_GetVals_real8_7d
  end interface

  !  \brief Set single value of the tree-vector.
  ! 
  !  \param treevec  IO Tree-Vector
  !  \param i        I  index of value in tree-vector
  !  \param val      I  value to be set
  !  \param datatype I  data type of *val, must be the same as data type of element in tree-vector
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_SetVal
    module procedure CTA_TreeVector_SetVal_integer
    module procedure CTA_TreeVector_SetVal_real4
    module procedure CTA_TreeVector_SetVal_real8
  end interface

  !  \brief Get single value of the tree-vector.
  ! 
  !  \param treevec  I  Tree-vector
  !  \param i        I  index in value in tree-vector
  !  \param val      O  returned value
  !  \param datatype I  data type of *val, must be the same as data type of element in tree-vector
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_GetVal
    module procedure CTA_TreeVector_GetVal_integer
    module procedure CTA_TreeVector_GetVal_real4
    module procedure CTA_TreeVector_GetVal_real8
  end interface

  !  \brief Get size of tree-vector.
  ! 
  !  \param treevec  I  Tree-vector
  !  \param n        O  receives size of tree-vector
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_GetSize
    subroutine CTA_TreeVector_GetSize( treevec, n, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      integer                       , intent(out  )     ::  n
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_GetSize
  end interface

  !  \brief Export tree-vector.
  ! 
  !  Can export tree-vector to file or pack object.\n
  !  usrdata must contain a handle of the file or pack object to be used.\n
  !  Dependency: CTA_Vector_Export()
  ! 
  ! 
  !  \param treevec  I  Tree-vector
  !  \param usrdata  I  export properties
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_Export
    subroutine CTA_TreeVector_Export( treevec, usrdata, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  usrdata
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_Export
  end interface

  !  \brief Import Tree-vector.
  ! 
  !  Can import tree-vector from file or pack object.\n
  !  usrdata must contain a handle of the file or pack object to be used.\n
  !  Dependency: CTA_Vector_Import()
  ! 
  ! 
  !  \param treevec  I  Tree-vector
  !  \param usrdata  I  import properties
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_Import
    subroutine CTA_TreeVector_Import( treevec, usrdata, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  usrdata
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_Import
  end interface

  !  \brief Import Tree-vector as flat vector.
  ! 
  !  Can import tree-vector from netcdf file.\n
  !  usrdata must contain a handle of the file .\n
  !  Dependency: CTA_Vector_VImport()
  ! 
  ! 
  !  \param treevec  I  Tree-vector
  !  \param usrdata  I  import properties
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_VImport
    subroutine CTA_TreeVector_VImport( treevec, usrdata, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  usrdata
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_VImport
  end interface

  !  \brief Free Tree-vector.
  ! 
  !  \param treevec     I  handle of tree-vector
  !  \param recursive  I  also free all sub-tree-vectors, yes: CTA_TRUE or no: CTA_FALSE
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_Free
    subroutine CTA_TreeVector_Free( treevec, recursive, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec(*)
      integer                       , intent(in   )     ::  recursive
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_Free
  end interface

  !  \brief Print tree-vector information.
  ! 
  !  Gives following information:\n\n
  !   Tree-vector information:\n
  !   tag: [tag]\n
  !   nsubtreevecs: [number of sub-tree-vectors]\n
  ! 
  !  If nsubtreevecs > 0: recursively prints all sub-tree-vectors
  !  Else prints:\n
  !  leaf: yes\n
  !  tree-vector size (leaf)
  ! 
  !  \param treevec     I  tree-vector
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_Info
    subroutine CTA_TreeVector_Info( treevec, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_Info
  end interface

  !  \brief Perform the matrix multiplication C:=alpha*op(A)*op(B)+beta*C
  ! where op(X)=X, X^T. However C and A are matrices of wich the columns are
  ! tree-vectors
  ! 
  !   \param sC     IO array of tree-vector (matrix C)
  !   \param nc     I  number of columns of C (dimension of sC)
  !   \param transa I  transpose flag CTA_TRUE/CTA_FALSE for matrix A (not supported)
  !   \param transb I  transpose flag CTA_TRUE/CTA_FALSE for matrix B
  !   \param alpha  I  scalar
  !   \param sA     I  handle of matrix A
  !   \param na     I  number of columns of A (dimension of sA)
  !   \param mB     I  handle of matrix B
  !   \param beta   I  scalar
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_Gemm
    subroutine CTA_TreeVector_Gemm( sC, nc, transa, transb, alpha, sA, na, mB, beta, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  sC
      integer                       , intent(in   )     ::  nc
      integer                       , intent(in   )     ::  transa
      integer                       , intent(in   )     ::  transb
      real(8)                       , intent(in   )     ::  alpha
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  sA(*)
      integer                       , intent(in   )     ::  na
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  mB
      real(8)                       , intent(in   )     ::  beta
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_Gemm
  end interface

  !  \brief Perform given operation on all leafs of the treevector
  ! 
  !   \param treevec1 I  handle of first COSTA tree-vector
  !   \param treevec2 I  handle of second COSTA tree-vector
  !   \param treevec  I  handle of a COSTA tree-vector
  !   \param op       I  operation to perform on the leafs
  !   \param arg      I  additional argument of operation
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_OpOnLeafs
    subroutine CTA_TreeVector_OpOnLeafs( treevec1, treevec2, op, arg, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec1
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec2
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  op
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  arg
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_OpOnLeafs
  end interface

  !  \brief Elementwise division of two vectors
  !   \note y:=y./x
  ! 
  !   \param y       I  handle of a COSTA tree-vector (y)
  !   \param x       I  handle of a COSTA tree-vector (y)
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_ElmDiv
    subroutine CTA_TreeVector_ElmDiv( y, x, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  y
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  x
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_ElmDiv
  end interface

  !  \brief Elementwise multiplication of two vectors
  !   \note y:=y.*x
  ! 
  !   \param y       I  handle of a COSTA tree-vector (y)
  !   \param x       I  handle of a COSTA tree-vector (y)
  !   \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_ElmProd
    subroutine CTA_TreeVector_ElmProd( y, x, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  y
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  x
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_ElmProd
  end interface

  !  \brief Elementwise sqare root
  !   \note y:=sqrt(y)
  ! 
  !   \param y       I  handle of a COSTA tree-vector (y)
  !   \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_ElmSqrt
    subroutine CTA_TreeVector_ElmSqrt( y, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  y
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_ElmSqrt
  end interface

  !  \brief Set nocompute flag of a sub-tree vector
  ! 
  !   When this flag is set, the values of the sub-treevector will
  !   be ignored in all basic vector operations (including asking the
  !   total length of the tree-vector). This propertie is used for
  !   additionally adding some meta information
  ! 
  !  \note the nocompute flag is set at the level of the parent!
  !  so the "isolated" sub-treevector can be used in basic vector
  !  operations.
  ! 
  !   \param x       I  handle of a COSTA tree-vector (y)
  !   \param tag     I  tag of sub-treevector
  !   \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_TreeVector_SetSubTreeNocompute
    subroutine CTA_TreeVector_SetSubTreeNocompute( x, tag, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  x
      character(len=*)              , intent(in   )     ::  tag
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_SetSubTreeNocompute
  end interface

  !  \brief Increase the reference count of a treevector and all subtrevectors
  ! 
  !  \param treevec  I handle of a COSTA tree-vector
  !   \param status O error status: CTA_OK if successful
  ! 
  !
  interface CTA_F90_TreeVector_IncRefCount
    subroutine CTA_TreeVector_IncRefCount( treevec, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      integer                       , intent(out  )     ::  status
    end subroutine CTA_TreeVector_IncRefCount
  end interface


contains

    subroutine CTA_TreeVector_SetConstant_integer( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      integer                       , intent(in   )     ::  val
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetConstant( treevec, val, CTA_INTEGER, status )
    end subroutine CTA_TreeVector_SetConstant_integer

    subroutine CTA_TreeVector_SetConstant_real4( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      real(4)                       , intent(in   )     ::  val
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetConstant( treevec, val, CTA_REAL, status )
    end subroutine CTA_TreeVector_SetConstant_real4

    subroutine CTA_TreeVector_SetConstant_real8( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      real(8)                       , intent(in   )     ::  val
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetConstant( treevec, val, CTA_DOUBLE, status )
    end subroutine CTA_TreeVector_SetConstant_real8

    subroutine CTA_TreeVector_SetVals_integer_1d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      integer                       , intent(in   )     ::  val(:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetVals( treevec, val, size(val), CTA_INTEGER, status )
    end subroutine CTA_TreeVector_SetVals_integer_1d

    subroutine CTA_TreeVector_SetVals_integer_2d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      integer                       , intent(in   )     ::  val(:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetVals( treevec, val, size(val), CTA_INTEGER, status )
    end subroutine CTA_TreeVector_SetVals_integer_2d

    subroutine CTA_TreeVector_SetVals_integer_3d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      integer                       , intent(in   )     ::  val(:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetVals( treevec, val, size(val), CTA_INTEGER, status )
    end subroutine CTA_TreeVector_SetVals_integer_3d

    subroutine CTA_TreeVector_SetVals_integer_4d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      integer                       , intent(in   )     ::  val(:,:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetVals( treevec, val, size(val), CTA_INTEGER, status )
    end subroutine CTA_TreeVector_SetVals_integer_4d

    subroutine CTA_TreeVector_SetVals_integer_5d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      integer                       , intent(in   )     ::  val(:,:,:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetVals( treevec, val, size(val), CTA_INTEGER, status )
    end subroutine CTA_TreeVector_SetVals_integer_5d

    subroutine CTA_TreeVector_SetVals_integer_6d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      integer                       , intent(in   )     ::  val(:,:,:,:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetVals( treevec, val, size(val), CTA_INTEGER, status )
    end subroutine CTA_TreeVector_SetVals_integer_6d

    subroutine CTA_TreeVector_SetVals_integer_7d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      integer                       , intent(in   )     ::  val(:,:,:,:,:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetVals( treevec, val, size(val), CTA_INTEGER, status )
    end subroutine CTA_TreeVector_SetVals_integer_7d

    subroutine CTA_TreeVector_SetVals_real4_1d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      real(4)                       , intent(in   )     ::  val(:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetVals( treevec, val, size(val), CTA_REAL, status )
    end subroutine CTA_TreeVector_SetVals_real4_1d

    subroutine CTA_TreeVector_SetVals_real4_2d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      real(4)                       , intent(in   )     ::  val(:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetVals( treevec, val, size(val), CTA_REAL, status )
    end subroutine CTA_TreeVector_SetVals_real4_2d

    subroutine CTA_TreeVector_SetVals_real4_3d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      real(4)                       , intent(in   )     ::  val(:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetVals( treevec, val, size(val), CTA_REAL, status )
    end subroutine CTA_TreeVector_SetVals_real4_3d

    subroutine CTA_TreeVector_SetVals_real4_4d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      real(4)                       , intent(in   )     ::  val(:,:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetVals( treevec, val, size(val), CTA_REAL, status )
    end subroutine CTA_TreeVector_SetVals_real4_4d

    subroutine CTA_TreeVector_SetVals_real4_5d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      real(4)                       , intent(in   )     ::  val(:,:,:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetVals( treevec, val, size(val), CTA_REAL, status )
    end subroutine CTA_TreeVector_SetVals_real4_5d

    subroutine CTA_TreeVector_SetVals_real4_6d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      real(4)                       , intent(in   )     ::  val(:,:,:,:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetVals( treevec, val, size(val), CTA_REAL, status )
    end subroutine CTA_TreeVector_SetVals_real4_6d

    subroutine CTA_TreeVector_SetVals_real4_7d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      real(4)                       , intent(in   )     ::  val(:,:,:,:,:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetVals( treevec, val, size(val), CTA_REAL, status )
    end subroutine CTA_TreeVector_SetVals_real4_7d

    subroutine CTA_TreeVector_SetVals_real8_1d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      real(8)                       , intent(in   )     ::  val(:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetVals( treevec, val, size(val), CTA_DOUBLE, status )
    end subroutine CTA_TreeVector_SetVals_real8_1d

    subroutine CTA_TreeVector_SetVals_real8_2d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      real(8)                       , intent(in   )     ::  val(:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetVals( treevec, val, size(val), CTA_DOUBLE, status )
    end subroutine CTA_TreeVector_SetVals_real8_2d

    subroutine CTA_TreeVector_SetVals_real8_3d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      real(8)                       , intent(in   )     ::  val(:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetVals( treevec, val, size(val), CTA_DOUBLE, status )
    end subroutine CTA_TreeVector_SetVals_real8_3d

    subroutine CTA_TreeVector_SetVals_real8_4d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      real(8)                       , intent(in   )     ::  val(:,:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetVals( treevec, val, size(val), CTA_DOUBLE, status )
    end subroutine CTA_TreeVector_SetVals_real8_4d

    subroutine CTA_TreeVector_SetVals_real8_5d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      real(8)                       , intent(in   )     ::  val(:,:,:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetVals( treevec, val, size(val), CTA_DOUBLE, status )
    end subroutine CTA_TreeVector_SetVals_real8_5d

    subroutine CTA_TreeVector_SetVals_real8_6d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      real(8)                       , intent(in   )     ::  val(:,:,:,:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetVals( treevec, val, size(val), CTA_DOUBLE, status )
    end subroutine CTA_TreeVector_SetVals_real8_6d

    subroutine CTA_TreeVector_SetVals_real8_7d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      real(8)                       , intent(in   )     ::  val(:,:,:,:,:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetVals( treevec, val, size(val), CTA_DOUBLE, status )
    end subroutine CTA_TreeVector_SetVals_real8_7d

    subroutine CTA_TreeVector_GetVals_integer_1d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      integer                       , intent(out  )     ::  val(:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_GetVals( treevec, val, size(val), CTA_INTEGER, status )
    end subroutine CTA_TreeVector_GetVals_integer_1d

    subroutine CTA_TreeVector_GetVals_integer_2d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      integer                       , intent(out  )     ::  val(:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_GetVals( treevec, val, size(val), CTA_INTEGER, status )
    end subroutine CTA_TreeVector_GetVals_integer_2d

    subroutine CTA_TreeVector_GetVals_integer_3d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      integer                       , intent(out  )     ::  val(:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_GetVals( treevec, val, size(val), CTA_INTEGER, status )
    end subroutine CTA_TreeVector_GetVals_integer_3d

    subroutine CTA_TreeVector_GetVals_integer_4d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      integer                       , intent(out  )     ::  val(:,:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_GetVals( treevec, val, size(val), CTA_INTEGER, status )
    end subroutine CTA_TreeVector_GetVals_integer_4d

    subroutine CTA_TreeVector_GetVals_integer_5d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      integer                       , intent(out  )     ::  val(:,:,:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_GetVals( treevec, val, size(val), CTA_INTEGER, status )
    end subroutine CTA_TreeVector_GetVals_integer_5d

    subroutine CTA_TreeVector_GetVals_integer_6d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      integer                       , intent(out  )     ::  val(:,:,:,:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_GetVals( treevec, val, size(val), CTA_INTEGER, status )
    end subroutine CTA_TreeVector_GetVals_integer_6d

    subroutine CTA_TreeVector_GetVals_integer_7d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      integer                       , intent(out  )     ::  val(:,:,:,:,:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_GetVals( treevec, val, size(val), CTA_INTEGER, status )
    end subroutine CTA_TreeVector_GetVals_integer_7d

    subroutine CTA_TreeVector_GetVals_real4_1d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      real(4)                       , intent(out  )     ::  val(:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_GetVals( treevec, val, size(val), CTA_REAL, status )
    end subroutine CTA_TreeVector_GetVals_real4_1d

    subroutine CTA_TreeVector_GetVals_real4_2d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      real(4)                       , intent(out  )     ::  val(:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_GetVals( treevec, val, size(val), CTA_REAL, status )
    end subroutine CTA_TreeVector_GetVals_real4_2d

    subroutine CTA_TreeVector_GetVals_real4_3d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      real(4)                       , intent(out  )     ::  val(:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_GetVals( treevec, val, size(val), CTA_REAL, status )
    end subroutine CTA_TreeVector_GetVals_real4_3d

    subroutine CTA_TreeVector_GetVals_real4_4d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      real(4)                       , intent(out  )     ::  val(:,:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_GetVals( treevec, val, size(val), CTA_REAL, status )
    end subroutine CTA_TreeVector_GetVals_real4_4d

    subroutine CTA_TreeVector_GetVals_real4_5d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      real(4)                       , intent(out  )     ::  val(:,:,:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_GetVals( treevec, val, size(val), CTA_REAL, status )
    end subroutine CTA_TreeVector_GetVals_real4_5d

    subroutine CTA_TreeVector_GetVals_real4_6d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      real(4)                       , intent(out  )     ::  val(:,:,:,:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_GetVals( treevec, val, size(val), CTA_REAL, status )
    end subroutine CTA_TreeVector_GetVals_real4_6d

    subroutine CTA_TreeVector_GetVals_real4_7d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      real(4)                       , intent(out  )     ::  val(:,:,:,:,:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_GetVals( treevec, val, size(val), CTA_REAL, status )
    end subroutine CTA_TreeVector_GetVals_real4_7d

    subroutine CTA_TreeVector_GetVals_real8_1d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      real(8)                       , intent(out  )     ::  val(:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_GetVals( treevec, val, size(val), CTA_DOUBLE, status )
    end subroutine CTA_TreeVector_GetVals_real8_1d

    subroutine CTA_TreeVector_GetVals_real8_2d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      real(8)                       , intent(out  )     ::  val(:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_GetVals( treevec, val, size(val), CTA_DOUBLE, status )
    end subroutine CTA_TreeVector_GetVals_real8_2d

    subroutine CTA_TreeVector_GetVals_real8_3d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      real(8)                       , intent(out  )     ::  val(:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_GetVals( treevec, val, size(val), CTA_DOUBLE, status )
    end subroutine CTA_TreeVector_GetVals_real8_3d

    subroutine CTA_TreeVector_GetVals_real8_4d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      real(8)                       , intent(out  )     ::  val(:,:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_GetVals( treevec, val, size(val), CTA_DOUBLE, status )
    end subroutine CTA_TreeVector_GetVals_real8_4d

    subroutine CTA_TreeVector_GetVals_real8_5d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      real(8)                       , intent(out  )     ::  val(:,:,:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_GetVals( treevec, val, size(val), CTA_DOUBLE, status )
    end subroutine CTA_TreeVector_GetVals_real8_5d

    subroutine CTA_TreeVector_GetVals_real8_6d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      real(8)                       , intent(out  )     ::  val(:,:,:,:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_GetVals( treevec, val, size(val), CTA_DOUBLE, status )
    end subroutine CTA_TreeVector_GetVals_real8_6d

    subroutine CTA_TreeVector_GetVals_real8_7d( treevec, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      real(8)                       , intent(out  )     ::  val(:,:,:,:,:,:,:)
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_GetVals( treevec, val, size(val), CTA_DOUBLE, status )
    end subroutine CTA_TreeVector_GetVals_real8_7d

    subroutine CTA_TreeVector_SetVal_integer( treevec, i, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      integer                       , intent(in   )     ::  i
      integer                       , intent(in   )     ::  val
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetVal( treevec, i, val, CTA_INTEGER, status )
    end subroutine CTA_TreeVector_SetVal_integer

    subroutine CTA_TreeVector_SetVal_real4( treevec, i, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      integer                       , intent(in   )     ::  i
      real(4)                       , intent(in   )     ::  val
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetVal( treevec, i, val, CTA_REAL, status )
    end subroutine CTA_TreeVector_SetVal_real4

    subroutine CTA_TreeVector_SetVal_real8( treevec, i, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  treevec
      integer                       , intent(in   )     ::  i
      real(8)                       , intent(in   )     ::  val
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_SetVal( treevec, i, val, CTA_DOUBLE, status )
    end subroutine CTA_TreeVector_SetVal_real8

    subroutine CTA_TreeVector_GetVal_integer( treevec, i, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_INTEGER
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      integer                       , intent(in   )     ::  i
      integer                       , intent(out  )     ::  val
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_GetVal( treevec, i, val, CTA_INTEGER, status )
    end subroutine CTA_TreeVector_GetVal_integer

    subroutine CTA_TreeVector_GetVal_real4( treevec, i, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_REAL
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      integer                       , intent(in   )     ::  i
      real(4)                       , intent(out  )     ::  val
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_GetVal( treevec, i, val, CTA_REAL, status )
    end subroutine CTA_TreeVector_GetVal_real4

    subroutine CTA_TreeVector_GetVal_real8( treevec, i, val, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      use CTA_F90_Parameters, only : CTA_DOUBLE
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  treevec
      integer                       , intent(in   )     ::  i
      real(8)                       , intent(out  )     ::  val
      integer                       , intent(out  )     ::  status
      call CTA_TreeVector_GetVal( treevec, i, val, CTA_DOUBLE, status )
    end subroutine CTA_TreeVector_GetVal_real8

end module cta_f90_treevector

