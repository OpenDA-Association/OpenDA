module cta_f90_matrix

  implicit none

  public

  !  \brief Create a new class (=implementation) of a COSTA matrix component.
  ! 
  !  \param name     I  name of the new matrix class
  !  \param h_func   I  COSTA function handles for functions that implement class,
  !                     missing functions must have value CTA_NULL
  !  \param hmatcl   O  receives handle of new matrix class
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Matrix_DefineClass
    subroutine CTA_Matrix_DefineClass( name, h_func, hmatcl, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      character(len=*)              , intent(in   )     ::  name
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  h_func(*)
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hmatcl
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Matrix_DefineClass
  end interface

  !  \brief Duplicate a matrix instance.
  ! 
  !  \note Only size, data type and class type are duplicated, the values are not
  !        copied.
  ! 
  !  \param hmat1 I  handle of matrix to be duplicated
  !  \param hmat2 O  receives handle of duplicate matrix, empty handle before calling
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Matrix_Duplicate
    subroutine CTA_Matrix_Duplicate( hmat1, hmat2, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmat1
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hmat2
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Matrix_Duplicate
  end interface

  !  \brief Create a new matrix.
  ! 
  !  \note
  ! 
  !  \param hmatcl   I  matrix class of new matrix
  !  \param m        I  number of rows
  !  \param n        I  number of columns
  !  \param datatype I  datatype of elements in matrix
  !  \param userdata IO userdata for creation (depends on class)
  !  \param hmat     O  receives handle of new matrix
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Matrix_Create
    subroutine CTA_Matrix_Create( hmatcl, m, n, datatype, userdata, hmat, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmatcl
      integer                       , intent(in   )     ::  m
      integer                       , intent(in   )     ::  n
      integer                       , intent(in   )     ::  datatype
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  userdata
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  hmat
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Matrix_Create
  end interface

  !  \brief Get datatype of matrix
  ! 
  !  \note
  ! 
  !  \param hmat     I  handle of matrix
  !  \param datatype O  receives data type of matrix
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Matrix_GetDatatype
    subroutine CTA_Matrix_GetDatatype( hmat, datatype, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmat
      integer                       , intent(out  )     ::  datatype
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Matrix_GetDatatype
  end interface

  !  \brief Get copy of all values in matrix.
  ! 
  !  \note The elements in the matrix are returned column-wise
  !        (FORTRAN matrix representation)
  ! 
  !  \param hmat     I  handle of matrix
  !  \param vals     O  copy of values in matrix
  !  \param m        I  number of rows of vals (must be the same as for the matrix)
  !  \param n        I  number of columns of vals (must be the same as for the matrix)
  !  \param datatype I  data type of *vals, must be the same as data type of matrix elements
  !  \param status O error status: CTA_OK if successful
  !
  !interface CTA_F90_Matrix_GetVals
  !  subroutine CTA_Matrix_GetVals( hmat, vals, m, n, datatype, status )
  !    use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
  !    integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmat
  !    void                          , intent(out  )     ::  vals
  !    integer                       , intent(in   )     ::  m
  !    integer                       , intent(in   )     ::  n
  !    integer                       , intent(in   )     ::  datatype
  !    integer                       , intent(out  )     ::  status
  !  end subroutine CTA_Matrix_GetVals
  !end interface

  !  \brief Get copy of single value in the matrix.
  ! 
  !  \note Counting of the indices starts from 1.
  !  \param hmat     I  handle of matrix
  !  \param val      O  receives copy of value in matrix
  !  \param m        I  row index of value to be copied
  !  \param n        I  column index of value to be copied
  !  \param datatype I  data type of *val, must be the same as data type of matrix elements
  !  \param status O error status: CTA_OK if successful
  !
  !interface CTA_F90_Matrix_GetVal
  !  subroutine CTA_Matrix_GetVal( hmat, val, m, n, datatype, status )
  !    use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
  !    integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmat
  !    void                          , intent(out  )     ::  val
  !    integer                       , intent(in   )     ::  m
  !    integer                       , intent(in   )     ::  n
  !    integer                       , intent(in   )     ::  datatype
  !    integer                       , intent(out  )     ::  status
  !  end subroutine CTA_Matrix_GetVal
  !end interface

  !  \brief Set whole matrix to one single value.
  ! 
  !  \note
  !  \param hmat     IO handle of matrix
  !  \param val      I  value that must be set
  !  \param datatype I  data type of *val, must be the same as data type of matrix elements
  !  \param status O error status: CTA_OK if successful
  !
  !interface CTA_F90_Matrix_SetConstant
  !  subroutine CTA_Matrix_SetConstant( hmat, val, datatype, status )
  !    use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
  !    integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hmat
  !    void                          , intent(in   )     ::  val(*)
  !    integer                       , intent(in   )     ::  datatype
  !    integer                       , intent(out  )     ::  status
  !  end subroutine CTA_Matrix_SetConstant
  !end interface

  !  \brief Set values of a single column of the matrix.
  ! 
  !  \note Counting of the indices starts from 1.
  !  \param hmat     IO handle of matrix
  !  \param n        I  index of matrix column to set
  !  \param hvec     I  handle of COSTA vector the values have to be set to, length must be the same as number of rows of matrix
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Matrix_SetCol
    subroutine CTA_Matrix_SetCol( hmat, n, hvec, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hmat
      integer                       , intent(in   )     ::  n
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hvec
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Matrix_SetCol
  end interface

  !  \brief Set all values of the matrix.
  ! 
  !  \note The elements in vals should be column-wise
  !        (FORTRAN matrix representation)
  !  \param hmat     IO handle of matrix
  !  \param vals     I  copy of values in matrix
  !  \param m        I  number of rows of vals (must be the same as for the matrix)
  !  \param n        I  number of columns of vals (must be the same as for the matrix)
  !  \param datatype I  data type of vals
  !  \param status O error status: CTA_OK if successful
  !
  !interface CTA_F90_Matrix_SetVals
  !  subroutine CTA_Matrix_SetVals( hmat, vals, m, n, datatype, status )
  !    use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
  !    integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hmat
  !    void                          , intent(in   )     ::  vals(*)
  !    integer                       , intent(in   )     ::  m
  !    integer                       , intent(in   )     ::  n
  !    integer                       , intent(in   )     ::  datatype
  !    integer                       , intent(out  )     ::  status
  !  end subroutine CTA_Matrix_SetVals
  !end interface

  !  \brief Set a single value in the matrix.
  ! 
  !  \param hmat     IO handle of matrix
  !  \param val      I  value to be set at position (m,n)
  !  \param m        I  row index
  !  \param n        I  column index
  !  \param datatype I  data type of *val, must be the same as data type of matrix elements
  !  \param status O error status: CTA_OK if successful
  !
  !interface CTA_F90_Matrix_SetVal
  !  subroutine CTA_Matrix_SetVal( hmat, val, m, n, datatype, status )
  !    use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
  !    integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hmat
  !    void                          , intent(in   )     ::  val(*)
  !    integer                       , intent(in   )     ::  m
  !    integer                       , intent(in   )     ::  n
  !    integer                       , intent(in   )     ::  datatype
  !    integer                       , intent(out  )     ::  status
  !  end subroutine CTA_Matrix_SetVal
  !end interface

  !  \brief Export a matrix.
  ! 
  !  \note CTA_DEFAULT_MATRIX supports exporting to:\n
  !        file (usedoc is handle of COSTA file)\n
  ! 
  !  \param hmat     I  handle of matrix
  !  \param usedoc   I  configuration of output
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Matrix_Export
    subroutine CTA_Matrix_Export( hmat, usedoc, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmat
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  usedoc
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Matrix_Export
  end interface

  !  \brief Perform a rank 1 operation A:=alpha*x*y'+A
  ! 
  !  \param hmat     IO handle of matrix A
  !  \param alpha    I  scalar
  !  \param vx       I  vector x
  !  \param vy       I  vector y
  !  \param status O error status: CTA_OK if successful
  !  \note it is allowed that are the same object (*vx==*vy)
  !
  interface CTA_F90_Matrix_Ger
    subroutine CTA_Matrix_Ger( hmat, alpha, vx, vy, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hmat
      real(8)                       , intent(in   )     ::  alpha
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  vx
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  vy
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Matrix_Ger
  end interface

  !  \brief Compute inverse of a square matrix A:=inv(A)
  ! 
  !  \param hmat     IO handle of matrix A
  !  \param status O error status: CTA_OK if successful
  ! 
  !
  interface CTA_F90_Matrix_Inv
    subroutine CTA_Matrix_Inv( hmat, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hmat
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Matrix_Inv
  end interface

  !  \brief Perform the matrix multiplication y:=alpha*OP(A)*x+beta*y
  !    where op(X)=X, X^T
  ! 
  !  \param hmat     I  handle of matrix (A from the equation above)
  !  \param trans    I  transpose flag CTA_TRUE/CTA_FALSE for matrix A
  !  \param alpha    I  scalar
  !  \param vx       I  vector x
  !  \param beta     I  scalar
  !  \param vy       IO vector y
  !  \param status O error status: CTA_OK if successful
  !  \note it is allowed that vectors are the same object (*vx==*vy)
  !
  interface CTA_F90_Matrix_Gemv
    subroutine CTA_Matrix_Gemv( hmat, trans, alpha, vx, beta, vy, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  hmat
      integer                       , intent(in   )     ::  trans
      real(8)                       , intent(in   )     ::  alpha
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  vx
      real(8)                       , intent(in   )     ::  beta
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  vy
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Matrix_Gemv
  end interface

  !  \brief Perform the matrix multication C:=alpha*op(A)*op(B)+beta*C
  ! where op(X)=X, X^T
  ! 
  !   \param mC     IO handle of matrix C
  !   \param transa I  transpose flag CTA_TRUE/CTA_FALSE for matrix A
  !   \param transb I  transpose flag CTA_TRUE/CTA_FALSE for matrix A
  !   \param alpha  I  scalar
  !   \param mA     I  handle of matrix A
  !   \param mB     I  handle of matrix B
  !   \param beta   I  scalar
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Matrix_Gemm
    subroutine CTA_Matrix_Gemm( mC, transa, transb, alpha, mA, mB, beta, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  mC
      integer                       , intent(in   )     ::  transa
      integer                       , intent(in   )     ::  transb
      real(8)                       , intent(in   )     ::  alpha
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  mA
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  mB
      real(8)                       , intent(in   )     ::  beta
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Matrix_Gemm
  end interface

  !  \brief Perform the matrix addition Y:=alpha*X+Y
  ! 
  !   \param mY     IO handle of matrix Y
  !   \param alpha  I  scalar
  !   \param mX     I  handle of matrix X
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Matrix_Axpy
    subroutine CTA_Matrix_Axpy( mY, alpha, mX, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  mY
      real(8)                       , intent(in   )     ::  alpha
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  mX
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Matrix_Axpy
  end interface

  !  \brief Computes the eigenvalues and optionally the eigenvectors
  !          of a general matrix A
  ! 
  !   The computed eigenvectors are normalized to have Euclidean norm
  !   equal to 1 and largest component real.
  ! 
  !   \param A     I Matrix A
  !   \param eigvals  O Vector with eigenvalues
  !   \param eigvecs  O Matrix with eigenvalues. The eigenvectors are
  !                     not computed when  eigvecs in CTA_NULL on entry
  !  \param status O error status: CTA_OK if successful
  !  \note the eigenvalues can be complex. Since COSTA does not yet support
  !        complex vectors, only the real part of the eigenvalues is
  !        returned.
  !
  interface CTA_F90_Matrix_EigVals
    subroutine CTA_Matrix_EigVals( A, eigvals, eigvecs, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  A
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  eigvals
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  eigvecs
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Matrix_EigVals
  end interface

  !  \brief Free the matrix object
  ! 
  !  \Note hmat=CTA_NULL is allowed
  ! 
  !  \param hmat     IO handle of matrix, replaced by CTA_NULL on return.
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Matrix_Free
    subroutine CTA_Matrix_Free( hmat, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(inout)     ::  hmat
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Matrix_Free
  end interface


end module cta_f90_matrix

