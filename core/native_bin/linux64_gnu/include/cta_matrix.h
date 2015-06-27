/*
COSTA: Problem solving environment for data assimilation
Copyright (C) 2005  Nils van Velzen

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
\file  cta_matrix.h
\brief The interface description of the COSTA matrix component. For user implementation see cta_usr_matrix.h.

CTA_Matrix is the default class implementation for matrices.
*/

#ifndef CTA_MATRIX_H
#define CTA_MATRIX_H
#include "cta_system.h"
#include "cta_handles.h"
#include "cta_datatypes.h"
#include "cta_functions.h"
#include "cta_vector.h"

/* Function Handle */
typedef CTA_Handle CTA_Matrix;
typedef CTA_Handle CTA_MatClass;

/* parameters for different functions */
#define CTA_MATRIX_CREATE_SIZE ( 1)
#define CTA_MATRIX_CREATE_INIT ( 2)
#define I_CTA_MATRIX_GETVALS     ( 4) 
#define I_CTA_MATRIX_GETVAL      ( 5) 
#define I_CTA_MATRIX_SETCOL      ( 6)
#define I_CTA_MATRIX_SETVALS     ( 7)
#define I_CTA_MATRIX_SETVAL      ( 8)
#define I_CTA_MATRIX_SETCONST    ( 9) 
#define I_CTA_MATRIX_FREE        (10)
#define I_CTA_MATRIX_EXPORT      (11)
#define I_CTA_MATRIX_GER         (12)
#define I_CTA_MATRIX_INV         (13)
#define I_CTA_MATRIX_GEMV        (14)
#define I_CTA_MATRIX_GEMM        (15)
#define I_CTA_MATRIX_AXPY        (16)
#define CTA_MATRIX_NUMFUNC     (17)

#ifdef __cplusplus
extern "C" {
#endif

/** \brief Create a new class (=implementation) of a COSTA matrix component.
 *
 * \param name     I  name of the new matrix class
 * \param h_func   I  COSTA function handles for functions that implement class,
 *                    missing functions must have value CTA_NULL
 * \param hmatcl   O  receives handle of new matrix class
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Matrix_DefineClass(const char *name,
                           const CTA_Func h_func[CTA_MATRIX_NUMFUNC],
                           CTA_MatClass *hmatcl);

/** \brief Duplicate a matrix instance.
 *
 * \note Only size, data type and class type are duplicated, the values are not
 *       copied.
 *
 * \param hmat1 I  handle of matrix to be duplicated
 * \param hmat2 O  receives handle of duplicate matrix, empty handle before calling
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Matrix_Duplicate(CTA_Matrix hmat1, CTA_Matrix *hmat2);

/** \brief Create a new matrix.
 *
 * \note
 *
 * \param hmatcl   I  matrix class of new matrix
 * \param m        I  number of rows
 * \param n        I  number of columns
 * \param datatype I  datatype of elements in matrix
 * \param userdata IO userdata for creation (depends on class)
 * \param hmat     O  receives handle of new matrix
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Matrix_Create(CTA_MatClass hmatcl, const int m, const int n,
                      CTA_Datatype datatype, CTA_Handle userdata, CTA_Matrix *hmat);

/* *\brief Get size of matrix.
 *
 * \note
 *
 * \param hmat  I  handle of matrix
 * \param m     O  receives number of rows
 * \param n     O  receives number of columns
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Matrix_GetSize(CTA_Matrix hmat, int *m, int *n);

/** \brief Get datatype of matrix
 *
 * \note 
 *
 * \param hmat     I  handle of matrix
 * \param datatype O  receives data type of matrix
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Matrix_GetDatatype(CTA_Matrix hmat, CTA_Datatype *datatype);

/** \brief Get copy of all values in matrix.
 *
 * \note The elements in the matrix are returned column-wise
 *       (FORTRAN matrix representation)
 *
 * \param hmat     I  handle of matrix
 * \param vals     O  copy of values in matrix
 * \param m        I  number of rows of vals (must be the same as for the matrix)
 * \param n        I  number of columns of vals (must be the same as for the matrix)
 * \param datatype I  data type of *vals, must be the same as data type of matrix elements
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Matrix_GetVals(CTA_Matrix hmat, void *vals, int  m, int  n,
                       CTA_Datatype datatype);

/** \brief Get copy of single value in the matrix.
 *
 * \note Counting of the indices starts from 1.
 * \param hmat     I  handle of matrix
 * \param val      O  receives copy of value in matrix
 * \param m        I  row index of value to be copied
 * \param n        I  column index of value to be copied
 * \param datatype I  data type of *val, must be the same as data type of matrix elements
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Matrix_GetVal (CTA_Matrix hmat, void *val , int  m, int  n,
                       CTA_Datatype datatype);

/** \brief Set whole matrix to one single value.
 *
 * \note
 * \param hmat     IO handle of matrix
 * \param val      I  value that must be set
 * \param datatype I  data type of *val, must be the same as data type of matrix elements
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Matrix_SetConstant(CTA_Matrix hmat, void *val, CTA_Datatype datatype);

/** \brief Set values of a single column of the matrix.
 *
 * \note Counting of the indices starts from 1.
 * \param hmat     IO handle of matrix
 * \param n        I  index of matrix column to set
 * \param hvec     I  handle of COSTA vector the values have to be set to, length must be the same as number of rows of matrix
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Matrix_SetCol(CTA_Matrix hmat, int  n, CTA_Vector hvec);

/** \brief Set all values of the matrix.
 *
 * \note The elements in vals should be column-wise
 *       (FORTRAN matrix representation)
 * \param hmat     IO handle of matrix
 * \param vals     I  copy of values in matrix
 * \param m        I  number of rows of vals (must be the same as for the matrix)
 * \param n        I  number of columns of vals (must be the same as for the matrix)
 * \param datatype I  data type of vals
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Matrix_SetVals(CTA_Matrix hmat, void *vals, int  m, int  n,
                       CTA_Datatype datatype);

/** \brief Set a single value in the matrix.
 *
 * \param hmat     IO handle of matrix
 * \param val      I  value to be set at position (m,n)
 * \param m        I  row index
 * \param n        I  column index
 * \param datatype I  data type of *val, must be the same as data type of matrix elements
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Matrix_SetVal(CTA_Matrix hmat, void *val, int  m, int  n,
                       CTA_Datatype datatype);

/** \brief Export a matrix.
 *
 * \note CTA_DEFAULT_MATRIX supports exporting to:\n
 *       file (usedoc is handle of COSTA file)\n
 *
 * \param hmat     I  handle of matrix
 * \param usedoc   I  configuration of output
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Matrix_Export(CTA_Matrix hmat, CTA_Handle usedoc);


/** \brief Perform a rank 1 operation A:=alpha*x*y'+A
 *
 * \param hmat     IO handle of matrix A
 * \param alpha    I  scalar
 * \param vx       I  vector x
 * \param vy       I  vector y
 * \return error status: CTA_OK if successful
 * \note it is allowed that are the same object (*vx==*vy)
 */
CTAEXPORT int CTA_Matrix_Ger(CTA_Matrix hmat, double alpha, CTA_Vector vx,
                      CTA_Vector vy);


/** \brief Compute inverse of a square matrix A:=inv(A)
 *
 * \param hmat     IO handle of matrix A
 * \return error status: CTA_OK if successful
 *
 */
CTAEXPORT int CTA_Matrix_Inv(CTA_Matrix hmat);

/** \brief Perform the matrix multiplication y:=alpha*OP(A)*x+beta*y
 *   where op(X)=X, X^T
 *
 * \param hmat     I  handle of matrix (A from the equation above)
 * \param trans    I  transpose flag CTA_TRUE/CTA_FALSE for matrix A
 * \param alpha    I  scalar
 * \param vx       I  vector x
 * \param beta     I  scalar
 * \param vy       IO vector y
 * \return error status: CTA_OK if successful
 * \note it is allowed that vectors are the same object (*vx==*vy)
 */
CTAEXPORT int CTA_Matrix_Gemv(CTA_Matrix hmat, int trans, double alpha,
                              CTA_Vector vx, double beta,  CTA_Vector vy);

/** \brief Perform the matrix multication C:=alpha*op(A)*op(B)+beta*C
    where op(X)=X, X^T
 *
 *  \param mC     IO handle of matrix C
 *  \param transa I  transpose flag CTA_TRUE/CTA_FALSE for matrix A
 *  \param transb I  transpose flag CTA_TRUE/CTA_FALSE for matrix A
 *  \param alpha  I  scalar
 *  \param mA     I  handle of matrix A
 *  \param mB     I  handle of matrix B
 *  \param beta   I  scalar
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Matrix_Gemm(CTA_Matrix mC, int transa, int transb, double alpha,
                    CTA_Matrix mA, CTA_Matrix mB, double beta);

/** \brief Perform the matrix addition Y:=alpha*X+Y
 *
 *  \param mY     IO handle of matrix Y
 *  \param alpha  I  scalar
 *  \param mX     I  handle of matrix X
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Matrix_Axpy(CTA_Matrix mY, double alpha, CTA_Matrix mX);


/** \brief Computes the eigenvalues and optionally the eigenvectors
 *         of a general matrix A
 *
 *  The computed eigenvectors are normalized to have Euclidean norm
 *  equal to 1 and largest component real.
 *
 *  \param A     I Matrix A
 *  \param eigvals  O Vector with eigenvalues
 *  \param eigvecs  O Matrix with eigenvalues. The eigenvectors are
 *                    not computed when  eigvecs in CTA_NULL on entry
 * \return error status: CTA_OK if successful
 * \note the eigenvalues can be complex. Since COSTA does not yet support
 *       complex vectors, only the real part of the eigenvalues is
 *       returned.
 */
CTAEXPORT int CTA_Matrix_EigVals(CTA_Matrix A, CTA_Vector eigvals, CTA_Matrix eigvecs);


/** \brief Free the matrix object
 *
 * \Note hmat=CTA_NULL is allowed
 *
 * \param hmat     IO handle of matrix, replaced by CTA_NULL on return.
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Matrix_Free(CTA_Matrix *hmat);

#ifdef __cplusplus
}
#endif
#endif
