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



#ifndef CTA_MATRIX_BLAS_H
#define CTA_MATRIX_BLAS_H

#include "cta_f77blas.h"
#include "cta_f77lapack.h"
#include "cta_datatypes.h"
#include "cta_handles.h"
#include "cta_vector.h"
#include "cta_matrix.h"
#include "cta_functions.h"


/**
\file  cta_matrix_blas.h
\brief Interface description of the COSTA BLAS-matrix component (a standard implementation class of CTA_Matrix) that can be used for standard BLAS operations.

<i>BLAS matrix object data:</i>

\code
typedef struct {
CTA_Datatype datatype;
void *values;
int m;
int n;
int *perm;
BOOL inverse;
} CTAI_Matrix_blas;
\endcode
\n
*/




typedef struct {
CTA_Datatype datatype;
void *values;
int m;
int n;
int *perm;
BOOL inverse;
} CTAI_Matrix_blas;

#ifdef __cplusplus
extern "C" {
#endif

/** \brief Define the BLAS matrix class. Use the resulting class object when calling CTA_Matrix_Create().
 *
 * \param hmatcl    IO receives class BLAS matrix class object, must be empty before calling
 * \return no return value
 */
CTANOEXPORT void CTA_Matrix_blas_initialise(CTA_MatClass *hmatcl);


/** \brief Implementation that forms part of the create process.
 * 
 * Gives the memory size of a new BLAS matrix object.
 *
 * \param m        I  dimension, m (rows)
 * \param n        I  dimension, n (columns)
 * \param datatype I  data type of matrix elements
 * \param userdata IO user data (not being used)
 * \param retval   O  receives return value
 * \param memsize  O  receives the number of bytes which are necessary to store one
                      BLAS matrix class, with a pointer to the contents (data), but without the
                      contents themselves
 * \return no return value
 */
void CTAI_Matrix_Create_Size(int *m, int *n, CTA_Datatype *datatype,
                             CTA_Handle userdata, int *retval, int *memsize);
                             
/** \brief Implementation that forms part of the create process.
 *
 * Prepares a BLAS matrix object for use.
 *
 * \param x          IO pointer to object data of BLAS matrix
 * \param m          I  dimension, m (rows)
 * \param n          I  dimension, n (columns)
 * \param datatype   I  data type of the matrix elements
 * \param userdata   IO user data (not being used)
 * \param retval     O  receives return value
 * \return no return value
 */
void CTAI_Matrix_Create_Init( CTAI_Matrix_blas *x, int *m, int *n,
                              CTA_Datatype *datatype, CTA_Handle userdata, int *retval);


/** \brief Get all values from the BLAS matrix.
 *
 * \note At index CTA_MATRIX_GETVALS in the function list of the class descriptor.
 *
 * \param x          I  pointer to object data of BLAS matrix
 * \param vals       O  receives values; must exist before calling
 * \param m          I  number of rows of *vals, must be the same as number of rows in matrix
 * \param n          I  number of columns of *vals, must be the same as number of columns in matrix
 * \param datatype   I  data type of value; must be the same as data type of matrix elements
 * \param userdata   IO user data (not being used)
 * \param retval     O  receives return value
 * \return no return value
 */
void CTAI_Matrix_getvals( CTAI_Matrix_blas *x, void *vals, int *m,
                          int *n, CTA_Datatype *datatype, int *retval);


/** \brief Get a single value from the BLAS matrix.
 *
 * \param x          I  pointer to object data of BLAS matrix
 * \param val        O  receives value; must exist before calling
 * \param m          I  row index of matrix value to get
 * \param n          I  column index of matrix value to get
 * \param datatype   I  data type of val; must be the same as data type of matrix elements
 * \param retval     O  receives return value
 * \return no return value
 */
void CTAI_Matrix_getval ( CTAI_Matrix_blas *x, void *val, int *m,
                          int *n, CTA_Datatype *datatype, int *retval);


/** \brief Set column of BLAS matrix.
 *
 * \param x          IO pointer to object data of BLAS matrix
 * \param n          I  index of column to set
 * \param hvec       I  handle of sending column; dimensions must be compatible
 * \param retval     O  receives return value
 * \return no return value
 */
void CTAI_Matrix_setcol( CTAI_Matrix_blas *x, int *n, CTA_Vector *hvec, int *retval);


/** \brief Set all values of BLAS matrix.
 *
 * \param x          IO pointer to object data of BLAS matrix
 * \param vals       I  values to set to
 * \param m          I  number of rows of *vals, must be the same as number of rows in matrix
 * \param n          I  number of columns of *vals, must be the same as number of columns in matrix
 * \param datatype   I  data type of value; must be the same as data type of matrix elements
 * \param retval     O  receives return value
 * \return no return value
 */
void CTAI_Matrix_setvals( CTAI_Matrix_blas *x, void *vals, int *m, int *n,
                          CTA_Datatype *datatype, int *retval);


/** \brief Set a single value of BLAS matrix.
 *
 * \param x          IO pointer to object data of BLAS matrix
 * \param val        I  value to set to
 * \param m          I  row index of matrix value to set
 * \param n          I  column index of matrix value to set
 * \param datatype   I  data type of val; must be the same as data type of matrix elements
 * \param retval     O  receives return value
 * \return no return value
 */
void CTAI_Matrix_setval ( CTAI_Matrix_blas *x, void *val , int *m, int *n,
                          CTA_Datatype *datatype, int *retval);


/** \brief Set all BLAS matrix elements to constant value.
 *
 * \note At index CTA_MATRIX_SETCONST in the function list of the class descriptor.
 *
 * \param x          IO pointer to object data of BLAS matrix
 * \param val        I  constant value to set to
 * \param datatype   I  data type of val; must be the same as data type of matrix elements
 * \param retval     O  receives return value
 * \return no return value
 */
void CTAI_Matrix_setconst( CTAI_Matrix_blas *x, void *val,
                           CTA_Datatype *datatype, int *retval);


/** \brief Export BLAS matrix.
 *
 * \note Export a BLAS matrix to file (userdata is a handle of COSTA file) or
 *
 * \param x          I  pointer to object data of BLAS matrix
 * \param userdata   IO user data (see note)
 * \param retval     O  receives return value
 * \return no return value
 */
void CTAI_Matrix_Export(
         CTAI_Matrix_blas *x,
         CTA_Handle userdata,
         int *retval);


/** \brief Apply the BLAS operation GER: A=A+(alpha)x(y(T))
 * 
 * i.e. for matrix A, vectors x and y and scalar alpha
 *
 * \param A           IO pointer to object data of BLAS matrix A; must exist before calling
 * \param alpha       I  scalar
 * \param vx          I  handle of vector x
 * \param vy          I  handle of vector y
 * \param retval      O  receives return value
 * \return no return value
 */
void CTAI_Matrix_Ger( CTAI_Matrix_blas *A, double *alpha, CTA_Vector *vx,
                      CTA_Vector *vy, int *retval);


/** \brief Invert BLAS matrix.
 *
 * \param A        IO pointer to object data of BLAS matrix
 * \param retval   O  receives return value
 * \return no return value
 */
void CTAI_Matrix_Inv( CTAI_Matrix_blas *A, int *retval);


/** \brief Apply the BLAS operation GEMV: A=(alpha)Ax+(beta)y
 *
 * i.e. for matrix A, vectors x (optionally transposed) and y and scalars alpha and beta
 *
 * \param A        IO pointer to object data of BLAS matrix
 * \param transa   I  transpose flag for matrix A (CTA_TRUE for transposing or CTA_FALSE otherwise)
 * \param alpha    I  scalar
 * \param vx       I  handle of vector x
 * \param beta     I  scalar
 * \param vy       I  handle of vector y
 * \param retval   O  receives return value
 * \return no return value
 */
void CTAI_Matrix_Gemv( CTAI_Matrix_blas *A, int *trans, double *alpha, CTA_Vector *vx,
                      double *beta, CTA_Vector *vy, int *retval);


/** \brief Apply the BLAS operation GEMM: C=(alpha)AB+(beta)C
 *
 * i.e. for matrices A,B (both optionally transposed) and C, scalars alpha and beta
 *
 * \param C        IO pointer to object data of BLAS matrix C
 * \param transa   I  transpose flag for matrix A (CTA_TRUE for transposing or CTA_FALSE otherwise)
 * \param transb   I  transpose flag for matrix B (CTA_TRUE for transposing or CTA_FALSE otherwise)
 * \param alpha    I  scalar
 * \param A        I  pointer to object data of BLAS matrix A
 * \param B        I  pointer to object data of BLAS matrix B
 * \param beta     I  scalar
 * \param retval   O  receives return value
 * \return no return value
 */
void CTAI_Matrix_Gemm( CTAI_Matrix_blas *C, int *transa, int *transb, double *alpha,
         CTAI_Matrix_blas *A, CTAI_Matrix_blas *B, double *beta, int *retval);


/** \brief Apply the BLAS operation AXPY: Y=Y+(alpha)X
 *
 * i.e. for matrices X and Y, scalar alpha
 *
 * \param y        IO pointer to object data of BLAS matrix y
 * \param alpha    I  scalar
 * \param x        I  pointer to object data of BLAS matrix x
 * \param retval   O  receives return value
 * \return no return value
 */
void CTAI_Matrix_Axpy( CTAI_Matrix_blas *y, double *alpha, CTAI_Matrix_blas *x, int *retval);


/** \brief Free the BLAS matrix object data and associated resources.
 *
 * \param x          IO pointer to object data of BLAS matrix
 * \param retval     O  receives return value
 * \return no return value
 */
void CTAI_Matrix_free( CTAI_Matrix_blas *x, int *retval);

#ifdef __cplusplus
}
#endif


#endif
