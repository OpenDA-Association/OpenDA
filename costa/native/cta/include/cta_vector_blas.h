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

#ifndef CTA_VECTOR_BLAS_H
#define CTA_VECTOR_BLAS_H

#include "cta_f77blas.h"
#include "cta_datatypes.h"
#include "cta_handles.h"
#include "cta_vector.h"
#include "cta_functions.h"
#include "cta_pack.h"

/**
\file  cta_vector_blas.h
\brief Interface description of the COSTA BLAS-vector component (a standard implementation class of CTA_Vector) that can be used for standard BLAS operations.


<i>BLAS matrix object data:</i>

\code
typedef struct {
CTA_Datatype datatype;
void *values;
int n;
int size;
} CTAI_Vector_blas;
\endcode
\n


*/

typedef struct {
CTA_Datatype datatype;
void *values;
int n;                                  // total number of elements
int nDimensions;                        // total number of dimensions
int dimensions[7];                      // number of elements in each dimension
int size;
} CTAI_Vector_blas;

#ifdef __cplusplus
extern "C" {
#endif
/** \brief Define the BLAS vector class. Use the resulting class object when calling CTA_Vector_Create().
 *
 * \param hveccl    IO receives class BLAS vector class object, must be empty before calling
 * \return no return value
 */
CTANOEXPORT void CTA_Vector_blas_initialise(CTA_VecClass *hveccl);


/** \brief Part of the create process, get size of a new object.
 *
 * \param n        O  receives the size of a new vector blas object
 * \param datatype I  data type
 * \param usrdata  IO user data (not being used)
 * \param retval   O  receives return value of the function, CTA_OK if succesful
 * \param memsize  O  the number of bytes which are necessary to store one
                      CTAI_Vector_blas, with a pointer to the contents (data), but without the
                      contents themselves
 * \return no return value
 */
void CTAI_Vector_Create_Size(int *n, CTA_Datatype *datatype,
                            CTA_Handle *usrdata, int *retval, int *memsize);


/** \brief Part of the create process, initialize vector object.
 *
 * \param x          IO pointer to object data of user vector
 * \param n          I  size of vector data
 * \param datatype   I  data type of the vector elements
 * \param usrdata    IO user data (not being used)
 * \param retval     O  receives return value
 * \return no return value
 */
void CTAI_Vector_Create_Init(CTAI_Vector_blas *x, int *n,
                             CTA_Datatype *datatype,
                             CTA_Handle *usrdata, int *retval);


/** \brief Retrieve all values of the BLAS vector.
 *
 * \param x        I   pointer to object data of the vector
 * \param vals     O   receives the values of the vector
 * \param n        I   number of elements in the vals buffer, must equal number of elements in BLAS vector
 * \param datatype I   data type of elements of vals, must be the same as data type of BLAS vector x
 * \param retval   O   receives return value
 * \return no return value
 */
void CTAI_Vector_getvals(CTAI_Vector_blas *x, void *vals, int *n, CTA_Datatype *datatype, int *retval);


/** \brief Retrieve a single value of the BLAS vector at given index.
 *
 * \param x        I  pointer to object data of BLAS vector
 * \param i        I  index of value to retrieve
 * \param val      O  receives value; must exist before calling
 * \param datatype I  data type of *val, must be the same as data type of BLAS vector x
 * \param retval   O  receives return value
 * \return no return value
 */
void CTAI_Vector_getval(CTAI_Vector_blas *x, int *i, void *val, CTA_Datatype *datatype, int *retval);


/** \brief Set all values of a BLAS vector.
 *
 * \param x        I   pointer to object data of BLAS vector
 * \param vals     I   new values
 * \param n        I   number of elements in the vals buffer, must equal number of elements in BLAS vector
 * \param datatype I   data type of elements of vals, must be the same as data type of BLAS vector x
 * \param retval   O   receives return value
 * \return no return value
 */
void CTAI_Vector_setvals(CTAI_Vector_blas *x, void *vals, int *n, CTA_Datatype *datatype, int *retval);


/** \brief Set a single value of a BLAS vector.
 *
 * \param x        I  pointer to object data of BLAS vector
 * \param i        I  index of value to set
 * \param val      I  new data of value
 * \param datatype I  data type of *val, must be the same as data type of BLAS vector x
 * \param retval   O  receives return value
 * \return no return value
 */
void CTAI_Vector_setval(CTAI_Vector_blas *x, int *i, void *val, CTA_Datatype *datatype, int *retval);


/** \brief Set all values of BLAS vector to the same value.
 *
 * \param x        I  pointer to object data of BLAS vector
 * \param val      I  value to set vector elements to
 * \param datatype I  data type of elements of vals, must be the same as data type of BLAS vector x
 * \param retval   O  receives return value
 * \return no return value
 */
void CTAI_Vector_setconst(CTAI_Vector_blas *x, void *val, CTA_Datatype *datatype, int *retval);


/** \brief Scale the BLAS vector: x=ax
 *
 * \param x        IO pointer to object data of user vector
 * \param alpha    I  scalar
 * \param retval   O  receives return value
 * \return no return value
 */
void CTAI_Vector_scal(CTAI_Vector_blas *x, double *alpha, int *retval);


/** \brief Copy the BLAS vector.
 *
 * \param x        I  pointer to object data of a BLAS vector
 * \param y        O  pointer to object data of BLAS vector y that receives copy of x; must exist before calling
 * \param retval   O  receives return value
 * \return no return value
 */
void CTAI_Vector_copy(CTAI_Vector_blas *x, CTAI_Vector_blas *y, int *retval);


/** \brief Apply the BLAS operation axpy: y=y+a*x
 *
 * \param y        IO pointer to object data of BLAS vector y; must exist before calling
 * \param alpha    I  scalar
 * \param x        I  pointer to object data of BLAS vector x
 * \param retval   O  receives return value
 * \return no return value
 */
void CTAI_Vector_axpy(CTAI_Vector_blas *y, double *alpha, CTAI_Vector_blas *x, int *retval);


/** \brief Calculate the dot product of two BLAS vectors: x.y
 *
 * \param y        I  pointer to object data of BLAS vector y
 * \param x        I  pointer to object data of BLAS vector x
 * \param dotprod  O  receives the dot product
 * \param retval   O  receives return value
 * \return no return value
 */
void CTAI_Vector_dot(CTAI_Vector_blas *x, CTAI_Vector_blas *y, double *dotprod, int *retval);


/** \brief Calculate the 2-norm of a BLAS vector
 *
 * \param x        I  pointer to object data of BLAS vector
 * \param norm2    O  receives 2-norm
 * \param retval   O  receives return value
 * \return no return value
 */
void CTAI_Vector_nrm2(CTAI_Vector_blas *x, double *norm2, int *retval);


/** \brief Get the index of the maximum vector value. First index is 1.
 *
 * \param x        I  pointer to object data of BLAS vector
 * \param iloc     O  receives index of maximum value
 * \param retval   O  receives return value
 * \return no return value
 */
void CTAI_Vector_amax(CTAI_Vector_blas *x, int *iloc, int *retval);


/** \brief Get the maximum length in case of BLAS vector with string elements.
 *
 * \param x        I  pointer to object data of BLAS vector
 * \param maxlen   O  receives maximum length
 * \param retval   O  receives return value
 * \return no return value
 */
void CTAI_Vector_GetMaxLen(CTAI_Vector_blas *x, int *maxlen, int *retval);

/** \brief Export a BLAS vector.
 *
 * \note Supports exporting float, double, int and CTA_String values.\n
 *       Possibilities:
 *       Export to file (usrdata contains handle of COSTA file).\n
 *       Export to pack object (usrdata contains handle of COSTA pack object).\n
 * 
 *
 * \param x        I  pointer to object data of BLAS vector
 * \param usrdata  IO user data
 * \param retval   O  receives return value
 * \return no return value
 */
void CTAI_Vector_export(CTAI_Vector_blas *x, CTA_Handle *usrdata, int *retval);


/** \brief Import a BLAS vector.
 *
 * \note Supported:\n
      Import from pack object (usrdata contains handle of COSTA pack object).\n
 *
 * \param x            O  pointer to object data of BLAS vector that receives import result
 * \param usrdata     IO user data
 * \param retval       O  receives return value
 * \return no return value
 */
void CTAI_Vector_import(CTAI_Vector_blas *x, CTA_Handle *usrdata, int *retval);


/** \brief Print table. TODO
 *
 * \param table        IO
 * \param ncolumns     I  number of columns
 * \param formats      I  user vector
 * \param retval       O  receives return value
 * \return no return value
 */
void CTAI_Vector_Print_Table(CTAI_Vector_blas **table, int *ncolumns,
                      CTAI_Vector_blas * formats,     int *retval);


/** \brief Append single value to the BLAS vector.
 *
 * \param x            IO pointer to object data of BLAS vector
 * \param val          I  value to append
 * \param datatype     I  data type of value to append, must be the same as data type of user vector x
 * \param retval       O  receives return value
 * \return no return value
 */
void CTAI_Vector_appendval(CTAI_Vector_blas *x, void *val, CTA_Datatype *datatype,
                           int *retval);


/** \brief Free the BLAS vector object data and associated resources.
 *
 * \param x        IO pointer to object data of BLAS vector
 * \param retval   O  receives return value
 * \return no return value
 */
void CTAI_Vector_free(CTAI_Vector_blas *x, int *retval);


/** \brief Element-wise division of BLAS vector.
 *
 * \param y            IO pointer to object data of receiving BLAS vector
 * \param x            I  pointer to object data of user vector x
 * \param retval       O  receives return value
 * \return no return value
 */
void CTAI_Vector_elmdiv(CTAI_Vector_blas *y, CTAI_Vector_blas *x, int *retval);

/** \brief Element-wise product of BLAS vector.
 *
 * \param y            IO pointer to object data of receiving BLAS vector
 * \param x            I  pointer to object data of user vector x
 * \param retval       O  receives return value
 * \return no return value
 */
void CTAI_Vector_elmprod(CTAI_Vector_blas *y, CTAI_Vector_blas *x, int *retval);

/** \brief Element-wise square root of BLAS vector.
 *
 * \param y            IO pointer to object data of receiving BLAS vector
 * \param retval       O  receives return value
 * \return no return value
 */
void CTAI_Vector_elmsqrt(CTAI_Vector_blas *y, int *retval);


#ifdef __cplusplus
}
#endif
#endif

