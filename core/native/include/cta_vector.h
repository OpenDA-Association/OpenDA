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
\file  cta_vector.h
\brief Interface description of the default COSTA vector componennt. For user implementation see cta_usr_vector.h.

Basic functions and utilities for vector operations on CTA_VECTOR objects.
*/

#ifndef CTA_VECTOR_H
#define CTA_VECTOR_H
#include "cta_system.h"
#include "cta_handles.h"
#include "cta_datatypes.h"
#include "cta_functions.h"

/* Function Handle */
typedef CTA_Handle CTA_Vector;
typedef CTA_Handle CTA_VecClass;

/* parameters for different functions */
#define I_CTA_VECTOR_CREATE_SIZE ( 1)
#define I_CTA_VECTOR_CREATE_INIT ( 2)
#define I_CTA_VECTOR_GETVALS     ( 3)
#define I_CTA_VECTOR_SETVALS     ( 4)
#define I_CTA_VECTOR_SETCONST    ( 5)
#define I_CTA_VECTOR_SCAL        ( 6)
#define I_CTA_VECTOR_COPY        ( 7)
#define I_CTA_VECTOR_AXPY        ( 8)
#define I_CTA_VECTOR_DOT         ( 9)
#define I_CTA_VECTOR_NRM2        (10)
#define I_CTA_VECTOR_AMAX        (11)
#define I_CTA_VECTOR_GETMAXLEN   (12)
#define I_CTA_VECTOR_FREE        (13)
#define I_CTA_VECTOR_GETVAL      (14)
#define I_CTA_VECTOR_SETVAL      (15)
#define I_CTA_VECTOR_EXPORT      (16)
#define I_CTA_VECTOR_PRINT_TABLE (17)
#define I_CTA_VECTOR_APPENDVAL   (18)
#define I_CTA_VECTOR_ELMDIV      (19)
#define I_CTA_VECTOR_IMPORT      (20)
#define I_CTA_VECTOR_ELMPROD     (21)
#define I_CTA_VECTOR_ELMSQRT     (22)
#define I_CTA_VECTOR_NUMFUNC     (23)

// CTA_Vector_Create                Create a new COSTA-vector:
// CTA_Vector_DefineClass           ...

/* function interfaces */
#ifdef __cplusplus
extern "C" {
#endif
/** \brief Create a new class (=implementation) of a COSTA vector component.
 *
 * \param name     I  name of the new vector class
 * \param h_func   I  COSTA function handles for functions that implement class.
 *                    Missing functions must have value CTA_NULL
 * \param hveccl   O  receives handle of new vector class
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_DefineClass(
   const char *name,
   const CTA_Func h_func[I_CTA_VECTOR_NUMFUNC],
   CTA_VecClass     *hveccl
   );

/** \brief Duplicate a vector object.
 *
 * \note Only size, data type and type (class) are duplicated, the content is not
 *       copied.
 *
 * \param hvector1  I  handle of vector to be duplicated
 * \param hvector2  O  receives handle of new duplicate vector, empty before calling
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_Duplicate(CTA_Vector hvector1, CTA_Vector *hvector2);

/** \brief Create a new vector.
 *
 * \note
 *
 * \param hveccl   I  vector class of new vector
 * \param n        I  number of elements
 * \param datatype I  data type of elements in vector
 * \param userdata IO user data for creation (depends on class)
 * \param hvector  O  receives handle of new vector
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_Create(CTA_VecClass hveccl, const int n, CTA_Datatype datatype,
                      CTA_Handle userdata, CTA_Vector *hvector);

/** \brief Get size of vector.
 *
 * \param hvec  I  handle of vector
 * \param n     O  receives number of elements
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_GetSize(CTA_Vector hvec, int *n);

/** \brief Get a copy of a single element in the vector.
 *
 * \param hvec     I  handle of vector
 * \param i        I  index of element
 * \param vals     O  receives copy of value in vector, must exist before calling
 * \param datatype I  data type of *vals, must be the same as data type of elements in vector
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_GetVal(CTA_Vector hvec, int  i, void *vals,
                      CTA_Datatype datatype);

/** \brief Get a copy of all elements in the vector.
 *
 * \param hvec     I  handle of vector
 * \param vals     O  receives copy of all elements in vector, must exist before calling
 * \param n        I  length of array vals
 * \param datatype I  data type of *vals, must be the same as data type of elements in vector
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_GetVals(CTA_Vector hvec, void *vals, int  n,
                       CTA_Datatype datatype);

/** \brief Set a copy of an element in the vector.
 *
 * \note The value is copied in the vector.
 *
 * \param hvec     IO handle of vector
 * \param i        I  index of element
 * \param val      I  new value that is copied into element at given index
 * \param datatype I  data type of *val, must be the same as data type of elements in vector
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_SetVal(CTA_Vector hvec, int  i, void *val,
                      CTA_Datatype datatype);

/** \brief Append a copy of an element to a vector.
 *
 * \note The value is copied into the vector, size of vector is increased.
 *
 * \param hvec     IO handle of vector
 * \param val      I  value that needs to be added to vector
 * \param datatype I  data type of *val, must be the same as data type of elements in vector
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_AppendVal(CTA_Vector hvec, const void *val, CTA_Datatype datatype);

/** \brief Set all elementes in the vector.
 *
 * \note The values are copied in the vector.
 *
 * \param hvec     IO handle of vector
 * \param vals     I  values that need to be copied to vector
 * \param n        I  number of elements in vals
 * \param datatype I  data type of *vals, must be the same as data type of elements in vector
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_SetVals(CTA_Vector hvec, void *vals, int  n,
                       CTA_Datatype datatype);

/** \brief Scale a vector.
 *
 * \note scale: x=alpha*x
 *
 * \param hvec     IO handle of vector
 * \param alpha    I  scalar
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_Scal(CTA_Vector hvec, double alpha);

/** \brief Copy a vector
 *
 * \note This function copies the vector content (y=x), but does not make a new vector.
 *
 * \param hvec_x  I  handle of sending vector
 * \param hvec_y  O  handle of receiving vector
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_Copy(CTA_Vector hvec_x, CTA_Vector hvec_y);

/** \brief Operation axpy for two vectors x and y.
 *
 * \note axpy: y=alpha*x+y
 *
 * \param hvec_y  IO handle of vector y
 * \param alpha   I  scalar
 * \param hvec_x  I  handle of vector x
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_Axpy(CTA_Vector hvec_y, double alpha, CTA_Vector hvec_x);

/** \brief Dot product operation of two vectors.
 *
 * \note dot: dotprod=x^t*y
 *
 * \param hvec_x  I  handle of vector x
 * \param hvec_y  I  handle of vector y
 * \param dotprod O  receives dot product
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_Dot(CTA_Vector hvec_x, CTA_Vector hvec_y, double *dotprod);

/** \brief Compute 2-norm of vector.
 *
 * \note 2-norm: sqrt(x^t*x)
 *
 * \param hvec_x  I  handle of vector x
 * \param norm2   O  receives 2-norm
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_Nrm2(CTA_Vector hvec_x, double *norm2);

/** \brief Find index of element in vector with largest absolute value
 *
 * \param hvec_x  I  handle of vector
 * \param iloc    O  receives index of largest absolute value
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_Amax(CTA_Vector hvec_x, int *iloc);

/** \brief Find largest length of elements in vector.
 *
 * \note e.g. length of string in a vector of strings
 *
 * \param hvec_x  I  handle of vector
 * \param maxlen  O  receives largest length of elements in vector
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_GetMaxLen(
   CTA_Vector hvec_x,  /* Handle of the first vector  */
   int *maxlen
   );

/** \brief Export a vector to file, stdout or pack object.
 *
 * \Note CTA_DEFAULT_VECTOR supports exporting to:\n
 *       file (usrdata a handle of COSTA file)\n
 *       pack object (usrdata a handle of COSTA pack object)\n
 *
 * \param hvec     I  handle of vector
 * \param usrdata  I  configuration of output
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_Export(
   CTA_Vector hvec, /* Handle of the vector */
   CTA_Handle usrdata
   );

/** \brief Import a vector.
 *
 * \note CTA_DEFAULT_VECTOR supports importing from pack object (usrdata[0] a pack handle).
 * \note Data type and size of vector can be changed due to this action
 *
 * \param hvec     I  handle of vector
 * \param usrdata  I  configuration of output
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_Import( CTA_Vector hvec, CTA_Handle usrdata);


/** \brief Print table, each column built up by a vector.
 *
 * \note CTA_DEFAULT_VECTOR    TODO
 *
 * \param table    I  array of vector handles, these vectors form the table to be filled
 * \param ncolumns I  number of columns in table
 *                    (number of vector handles in table)
 * \param vformats I  handle of vector of string (size ncolumns) containing the
 *                    formats for printing each column (C-format)
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_Print_Table(CTA_Vector* table, int ncolumns,
                           CTA_Vector vformats);

/** \brief Free the vector object.
 *
 *
 * \Note hvec_x=CTA_NULL is allowed
 *
 * \param hvec_x  IO handle of vector, replaced by CTA_NULL on return
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_Free(CTA_Vector *hvec_x);


/** \brief Get type of vector.
 *
 * \param hvec     I  handle of vector
 * \param datatype O  receives data type of vector
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_GetDatatype(
   CTA_Vector hvec,         /* Handle of the vector */
   CTA_Datatype *datatype   /* Returned data type   */
   );

/** \brief Set whole vector to one single value.
 *
 * \Note hvec=CTA_NULL is allowed, function returns error code
 *
 * \param hvec     IO handle of vector
 * \param val      I  value that must be set
 * \param datatype I  data type of *val, must be the same as data type of elements in vector
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_SetConstant(
   CTA_Vector hvec,  /* Handle of the vector */
   void *val,        /* value that must be set */
   CTA_Datatype datatype /* Data type */
   );

/** \brief Element wise division
 *
 * \note y=x./y
 *
 * \param hvec_y  IO handle of vector y
 * \param hvec_x  I  handle of vector x
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_ElmDiv(CTA_Vector hvec_y, CTA_Vector hvec_x);


/** \brief Element wise product
 *
 * \note y=x.y
 *
 * \param hvec_y  IO handle of vector y
 * \param hvec_x  I  handle of vector x
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_ElmProd(CTA_Vector hvec_y, CTA_Vector hvec_x);

/** \brief Element wise square root
 *
 * \note y=y.^0.5
 *
 * \param  hvec_x  IO handle of vector y
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Vector_ElmSqrt(CTA_Vector hvec_x);




#ifdef __cplusplus
}
#endif
#endif
