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
\file  cta_usr_matrix.h

\brief In this file a description is given of the interface of user matrix functions.
When creating your own user matrix class use the following as template.

The Usr_Matrix to which is being referred in this template can be substituted by your own user matrix.

<b>Step 1</b>: for creating your own user matrix class call the function CTA_Matrix_DefineClass().

Example:

\code
typedef CTA_Handle CTA_MatrixClass;

CTA_Func h_func[CTA_MATRIX_NUMFUNC];
CTA_MatrixClass my_own_matrix_class;

ierr=CTA_Func_Create(" ",&usr_matrix_create_size, hintf, &h_func[CTA_MATRIX_CREATE_SIZE]);
//...for all implementation functions...

CTA_Matrix_DefineClass("classname", h_func, &my_own_matrix_class);\endcode



Making a user matrix class involves the implementation of the following functions:

CTA_MATRIX_CREATE_SIZE \n
CTA_MATRIX_CREATE_INIT \n
CTA_MATRIX_GETVALS     \n
CTA_MATRIX_GETVAL      \n
CTA_MATRIX_SETCOL      \n
CTA_MATRIX_SETVALS     \n
CTA_MATRIX_SETVAL      \n
CTA_MATRIX_SETCONST    \n
CTA_MATRIX_FREE        \n
CTA_MATRIX_EXPORT      \n
CTA_MATRIX_GER         \n
CTA_MATRIX_INV         \n
CTA_MATRIX_GEMV        \n
CTA_MATRIX_GEMM        \n
CTA_MATRIX_AXPY        \n
CTA_MATRIX_NUMFUNC     

For creating an implementation function see documentation of CTA_Func_Create().

<b>Step 2</b>: to create an object of the newly defined matrix class call CTA_Matrix_Create() in the
same way as creating a CTA_Matrix but with a different class handle, i.e. the user class handle from step 1 above.

Example:

\code
Usr_Matrix usrmat; //user matrix object
int n = 10;
int m = 10;
CTA_Datatype datatype = CTAI_String2Type("CTA_STRING");
CTA_Handle userdata = CTA_NULL;
CTA_Matrix_Create(my_own_matrix_class, m, n, datatype, &userdata, &usrmat);
\endcode
\n
<b>Note 1: </b> with object data is meant only the object itself including pointer(s) to its contents, but
not the contents of the matrix.\n\n
<b>Note 2: </b> matrix indices start from 1, m and n for rows and columns respectively.\n\n
*/

/* parameters for different functions */
//#define CTA_MATRIX_CREATE_SIZE ( 1)
//#define CTA_MATRIX_CREATE_INIT ( 2)
//#define CTA_MATRIX_GETVALS     ( 4)
//#define CTA_MATRIX_GETVAL      ( 5)
//#define CTA_MATRIX_SETCOL      ( 6)
//#define CTA_MATRIX_SETVALS     ( 7)
//#define CTA_MATRIX_SETVAL      ( 8)
//#define CTA_MATRIX_SETCONST    ( 9)
//#define CTA_MATRIX_FREE        (10)
//#define CTA_MATRIX_EXPORT      (11)
//#define CTA_MATRIX_GER         (12)
//#define CTA_MATRIX_INV         (13)
//#define CTA_MATRIX_GEMV        (14)
//#define CTA_MATRIX_GEMM        (15)
//#define CTA_MATRIX_AXPY        (16)
//#define CTA_MATRIX_NUMFUNC     (17)




//#define CTA_MATRIX_CREATE_SIZE ( 1)

#ifdef __cplusplus
extern "C" {
#endif


/** \brief Implementation that forms part of the create process.
 * 
 * Must give the memory size of a new user matrix object.
 *
 * Example: 
 *  \code
//in header file:
typedef struct {
   //your own user object data goes here...
}USR_MATRIX;

//user implementation:
void usr_matrix_create_size(...){
   *memsize = sizeof(USR_MATRIX);
   *retval = CTA_OK;
}
 \endcode
 * \note At index CTA_MATRIX_CREATE_SIZE in the function list of the class descriptor.
 *
 * \param m        I  dimension, m (rows)
 * \param n        I  dimension, n (columns)
 * \param datatype I  data type of matrix elements
 * \param userdata IO user data
 * \param retval   O  must receive return value of user implementation function
 * \param memsize  O  must receive the number of bytes which are necessary to store one
                      user matrix class, with a pointer to the contents (data), but without the
                      contents themselves
 * \return no return value
 */
void usr_matrix_create_size(
   int *m,
   int *n,
   CTA_Datatype *datatype,
   void *userdata,
   int *retval,
   int *memsize
   );


//#define CTA_MATRIX_CREATE_INIT ( 2)
/** \brief Implementation that forms part of the create process.
 *
 * The user matrix object needs to be made ready for use.
 *
 * \note At index CTA_MATRIX_CREATE_INIT in the function list of the class descriptor.
 *
 * \param objectdata IO pointer to object data of user matrix
 * \param m          I  dimension, m (rows)
 * \param n          I  dimension, n (columns)
 * \param datatype   I  data type of the matrix elements
 * \param *userdata   IO user data
 * \param retval     O  must receive return value of user implementation function
 * \return no return value
 */
void usr_matrix_create_init(
   Usr_Matrix *objectdata,
   int *m,
   int *n,
   CTA_Datatype *datatype,
   void *userdata,
   int *retval
   );


//#define CTA_MATRIX_GETVALS     ( 4)
/** \brief Implementation for getting all values from the user matrix.
 *
 * \note At index CTA_MATRIX_GETVALS in the function list of the class descriptor.
 *
 * \param objectdata I  pointer to object data of user matrix
 * \param vals       O  must receive values; must exist before calling
 * \param m          I  number of rows of *vals, must be the same as number of rows in matrix
 * \param n          I  number of columns of *vals, must be the same as number of columns in matrix
 * \param datatype   I  data type of value; must be the same as data type of matrix elements
 * \param *userdata   IO user data
 * \param retval     O  must receive return value of user implementation function
 * \return no return value
 */
void usr_matrix_getvals(
   Usr_Matrix *objectdata,
   void *vals,
   int *m,
   int *n,
   CTA_Datatype *datatype,
   void *userdata,
   int *retval
   );


//#define CTA_MATRIX_GETVAL      ( 5)
/** \brief Implementation for getting a single value from the user matrix.
 *
 * \note At index CTA_MATRIX_GETVAL in the function list of the class descriptor.
 *
 * \param objectdata I  pointer to object data of user matrix
 * \param val        O  must receive value; must exist before calling
 * \param m          I  row index of matrix value to get
 * \param n          I  column index of matrix value to get
 * \param datatype   I  data type of val; must be the same as data type of matrix elements
 * \param *userdata   IO user data
 * \param retval     O  must receive return value of user implementation function
 * \return no return value
 */
void usr_matrix_getval(
   Usr_Matrix *objectdata,
   void *val,
   int *m,
   int *n,
   CTA_Datatype *datatype,
   void *userdata,
   int *retval
   );


//#define CTA_MATRIX_SETCOL      ( 6)
/** \brief Implementation for setting column of user matrix.
 *
 * \note At index CTA_MATRIX_SETCOL in the function list of the class descriptor.
 *
 * \param objectdata IO pointer to object data of user matrix
 * \param n          I  index of column to set
 * \param hvec       I  handle of sending column; dimensions must be compatible
 * \param retval     O  must receive return value of user implementation function
 * \return no return value
 */
void usr_matrix_setcol(
   Usr_Matrix *objectdata,
   int *n,
   CTA_Vector *hvec,
   int *retval
  );


//#define CTA_MATRIX_SETVALS     ( 7)
/** \brief Implementation for setting all values of user matrix.
 *
 * \note At index CTA_MATRIX_SETVALS in the function list of the class descriptor.
 *
 * \param objectdata IO pointer to object data of user matrix
 * \param vals       I  values to set to
 * \param m          I  number of rows of *vals, must be the same as number of rows in matrix
 * \param n          I  number of columns of *vals, must be the same as number of columns in matrix
 * \param datatype   I  data type of value; must be the same as data type of matrix elements
 * \param *userdata   IO user data
 * \param retval     O  must receive return value of user implementation function
 * \return no return value
 */
void usr_matrix_setvals(
   Usr_Matrix *objectdata,
   void *vals,
   int *m,
   int *n,
   CTA_Datatype *datatype,
   void *userdata,
   int *retval
  );
  

//#define CTA_MATRIX_SETVAL      ( 8)
/** \brief Implementation for setting a single value of user matrix.
 *
 * \note At index CTA_MATRIX_SETVAL in the function list of the class descriptor.
 *
 * \param objectdata IO pointer to object data of user matrix
 * \param val        I  value to set to
 * \param m          I  row index of matrix value to set
 * \param n          I  column index of matrix value to set
 * \param datatype   I  data type of val; must be the same as data type of matrix elements
 * \param *userdata   IO user data
 * \param retval     O  must receive return value of user implementation function
 * \return no return value
 */
void usr_matrix_setval(
   Usr_Matrix *objectdata,
   void *val,
   int *m,
   int *n,
   CTA_Datatype *datatype,
   void *userdata,
   int *retval
  );

//#define CTA_MATRIX_SETCONST    ( 9)
/** \brief Implementation for setting all user matrix elements to constant value.
 *
 * \note At index CTA_MATRIX_SETCONST in the function list of the class descriptor.
 *
 * \param objectdata IO pointer to object data of user matrix
 * \param val        I  constant value to set to
 * \param datatype   I  data type of val; must be the same as data type of matrix elements
 * \param *userdata   IO user data
 * \param retval     O  must receive return value of user implementation function
 * \return no return value
 */
void usr_matrix_setconst(
   Usr_Matrix *objectdata,
   void *val,
   CTA_Datatype *datatype,
   void *userdata,
   int *retval
  );

//#define CTA_MATRIX_FREE        (10)
/** \brief Implementation for freeing the object data and associated resources.
 *
 * \note At index CTA_MATRIX_FREE in the function list of the class descriptor.
 *
 * \param objectdata IO pointer to object data of user matrix
 * \param *userdata   IO user data
 * \param retval     O  must receive return value of user implementation function
 * \return no return value
 */
void usr_matrix_free(
   Usr_Matrix *objectdata,
   CTA_Handle *userdata,
   int *retval
   );

//#define CTA_MATRIX_EXPORT      (11)
/** \brief Implementation for exporting user matrix.
 *
 * \note At index CTA_MATRIX_EXPORT in the function list of the class descriptor.
 *
 * \param objectdata I  pointer to object data of user matrix
 * \param *userdata   IO user data
 * \param retval     O  must receive return value of user implementation function
 * \return no return value
 */
void usr_matrix_export(
         Usr_Matrix *objectdata,
         CTA_Handle *userdata,
         int *retval);
         

//#define CTA_MATRIX_GER         (12)
/** \brief Implementation for applying the BLAS operation GER: A=A+(alpha)x(y(T))
 * 
 * i.e. for matrix A, vectors x and y and scalar alpha
 *
 * \note At index CTA_MATRIX_GER in the function list of the class descriptor.
 *
 * \param A           IO pointer to object data of user matrix A; must exist before calling
 * \param alpha       I  scalar
 * \param vx          I  handle of vector x
 * \param vy          I  handle of vector y
 * \param retval      O  must receive return value of user implementation function
 * \return no return value
 */
void usr_matrix_ger(
         Usr_Matrix *A,
         double *alpha,
         CTA_Vector *vx,
         CTA_Vector *vy,
         int *retval);

//#define         (13)
/** \brief Implementation for inverting user matrix.
 *
 * \note At index CTA_MATRIX_INV in the function list of the class descriptor.
 *
 * \param A        IO pointer to object data of user matrix
 * \param retval   O  must receive return value of user implementation function
 * \return no return value
 */
void usr_matrix_inv(
   Usr_Matrix *A,
   int *retval
   );

//#define CTA_MATRIX_GEMV        (14)
/** \brief Implementation for applying the BLAS operation GEMV: A=(alpha)Ax+(beta)y
 *
 * i.e. for matrix A, vectors x (optionally transposed) and y and scalars alpha and beta
 *
 * \note At index CTA_MATRIX_GEMV in the function list of the class descriptor.
 *
 * \param A        IO pointer to object data of user matrix
 * \param trans    I  transpose flag CTA_TRUE/CTA_FALSE for matrix A
 * \param alpha    I  scalar
 * \param vx       I  handle of vector x
 * \param beta     I  scalar
 * \param vy       I  handle of vector y
 * \param retval   O  must receive return value of user implementation function
 * \return no return value
 */
void usr_matrix_gemv(
         Usr_Matrix *A,
         int *trans,
         double *alpha,
         CTA_Vector *vx,
         double *beta,
         CTA_Vector *vy,
         int *retval);

//#define CTA_MATRIX_GEMM        (15)
/** \brief Implementation for applying the BLAS operation GEMM: C=(alpha)AB+(beta)C
 *
 * i.e. for matrices A,B (both optionally transposed) and C, scalars alpha and beta
 *
 * \note At index CTA_MATRIX_GEMM in the function list of the class descriptor.
 *
 * \param C        IO pointer to object data of user matrix C
 * \param transa   I  transpose flag for matrix A (CTA_TRUE for transposing or CTA_FALSE otherwise)
 * \param transb   I  transpose flag for matrix B (CTA_TRUE for transposing or CTA_FALSE otherwise)
 * \param alpha    I  scalar
 * \param A        I  pointer to object data of user matrix A
 * \param B        I  pointer to object data of user matrix B
 * \param beta     I  scalar
 * \param retval   O  must receive return value of user implementation function
 * \return no return value
 */
void usr_matrix_gemm(
         Usr_Matrix *C,
         int *transa,
         int *transb,
         double *alpha,
         Usr_Matrix *A,
         Usr_Matrix *B,
         double *beta,
         int *retval);

//#define CTA_MATRIX_AXPY        (16)
/** \brief Implementation for applying the BLAS operation AXPY: Y=Y+(alpha)X
 *
 * i.e. for matrices X and Y, scalar alpha
 *
 * \note At index CTA_MATRIX_AXPY in the function list of the class descriptor.
 *
 * \param y        IO pointer to object data of user matrix y
 * \param alpha    I  scalar
 * \param x        I  pointer to object data of user matrix x
 * \param retval   O  must receive return value of user implementation function
 * \return no return value
 */
void usr_matrix_axpy(
   Usr_Matrix *y,
   double *alpha,
   Usr_Matrix *x,
   int *retval
  );

#ifdef __cplusplus
}
#endif
