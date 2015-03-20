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

/* parameters for different functions */
#define CTA_VECTOR_CREATE_SIZE ( 1)
#define CTA_VECTOR_CREATE_INIT ( 2)
#define CTA_VECTOR_GETVALS     ( 3)
#define CTA_VECTOR_SETVALS     ( 4)
#define CTA_VECTOR_SETCONST    ( 5)
#define CTA_VECTOR_SCAL        ( 6)
#define CTA_VECTOR_COPY        ( 7)
#define CTA_VECTOR_AXPY        ( 8)
#define CTA_VECTOR_DOT         ( 9)
#define CTA_VECTOR_NRM2        (10)
#define CTA_VECTOR_AMAX        (11)
#define CTA_VECTOR_GETMAXLEN   (12)
#define CTA_VECTOR_FREE        (13)
#define CTA_VECTOR_GETVAL      (14)
#define CTA_VECTOR_SETVAL      (15)
#define CTA_VECTOR_EXPORT      (16)
#define CTA_VECTOR_PRINT_TABLE (17)
#define CTA_VECTOR_APPENDVAL   (18)
#define CTA_VECTOR_ELMDIV      (19)
#define CTA_VECTOR_IMPORT      (20)
#define CTA_VECTOR_NUMFUNC     (21)

/**
\file  cta_usr_vector.h

\brief In this file a description is given of the interface of user vector functions.
When creating your own vector class use the following as template.

The Usr_Vector to which is being referred in this template can be substituted by your own user vector object.

<b>Step 1</b>: for creating your own user vector class call the function CTA_Vector_DefineClass().

Example:

\code
typedef CTA_Handle CTA_VectorClass;

CTA_Func h_func[CTA_VECTOR_NUMFUNC];
CTA_VectorClass my_own_vector_class;

ierr=CTA_Func_Create(" ",&usr_vector_create_size, hintf, &h_func[CTA_VECTOR_CREATE_SIZE]);
//...for all implementation functions...

CTA_Vector_DefineClass("classname", h_func, &my_own_vector_class);\endcode

Making a user vector class involves the implementation of the following functions:

<code>CTA_VECTOR_CREATE_SIZE \n
CTA_VECTOR_CREATE_INIT \n
CTA_VECTOR_GETVALS     \n
CTA_VECTOR_SETVALS     \n
CTA_VECTOR_SETCONST    \n
CTA_VECTOR_SCAL        \n
CTA_VECTOR_COPY        \n
CTA_VECTOR_AXPY        \n
CTA_VECTOR_DOT         \n
CTA_VECTOR_NRM2        \n
CTA_VECTOR_AMAX        \n
CTA_VECTOR_GETMAXLEN   \n
CTA_VECTOR_FREE        \n
CTA_VECTOR_GETVAL      \n
CTA_VECTOR_SETVAL      \n
CTA_VECTOR_EXPORT      \n
CTA_VECTOR_PRINT_TABLE \n
CTA_VECTOR_APPENDVAL   \n
CTA_VECTOR_ELMDIV      \n
CTA_VECTOR_IMPORT      \n</code>

For creating an implementation function see documentation of CTA_Func_Create().

<b>Step 2</b>: to create an object of the newly defined vector class call CTA_Vector_Create() in the
same way as creating a CTA_Vector but with a different class handle, i.e. the user class handle from step 1 above.

Example:

\code
Usr_Vector usrvec; //user vector object
int nelements = 10;
CTA_Datatype datatype = CTAI_String2Type("CTA_STRING");
CTA_Handle userdata = CTA_NULL;
CTA_Vector_Create(my_own_vector_class, nelements, datatype, &userdata, &usrvec);
\endcode
\n
<b>Note 1: </b> with object data is meant only the object itself including pointer(s) to its contents, but
not the contents of the vector.\n\n
<b>Note 2: </b> vector indices start from 1.\n\n
*/


//CTA_VECTOR_CREATE_SIZE ( 1)
/** \brief Implementation that forms part of the create process.
 * 
 * Must give the memory size of a new object.
 *
 * Example:
 *  \code
//in header file:
typedef struct {
   //your own user object data goes here...
}USR_VECTOR;

//user implementation:
void usr_vector_create_size(...){
   *memsize = sizeof(USR_VECTOR);
   *retval = CTA_OK;
}
 \endcode
 *
 * \note At index CTA_VECTOR_CREATE_SIZE in the function list of the class descriptor.
 *
 * \param n        I  dimension
 * \param datatype I  data type of the vector elements
 * \param userdata IO user data
 * \param retval   O  must receive return value of user implementation function
 * \param memsize  O  must receive the number of bytes which are necessary to store one
                      user vector class, with a pointer to the contents (data), but without the
                      contents themselves
 * \return no return value
 */
void usr_vector_create_size(int *n, CTA_Datatype *datatype,
                            void *userdata, int *retval, int *memsize);


//CTA_VECTOR_CREATE_INIT ( 2)
/** \brief Implementation that forms part of the create process.
 *
 * The user vector object needs to be made ready for use.
 *
 * \note At index CTA_VECTOR_CREATE_INIT in the function list of the class descriptor.
 *
 * \param objectdata IO pointer to object data of user vector
 * \param n          I  size of vector data
 * \param datatype   I  data type of the vector elements
 * \param userdata   IO user data
 * \param retval     O  must receive return value of user implementation function
 * \return no return value
 */
void usr_vector_create_init(Usr_Vector *objectdata, int *n, CTA_Datatype *datatype,
                            void *userdata, int *retval);


//CTA_VECTOR_GETVALS     ( 3)
/** \brief Implementation for retrieving all values of the user vector.
 *
 * \note At index CTA_VECTOR_GETVALS in the function list of the class descriptor.
 *
 * \param x        I   pointer to object data of the user vector
 * \param vals     O   must receive the values of the user vector
 * \param n        I   number of elements in the vals buffer, must equal number of elements in user vector
 * \param datatype I   data type of elements of vals, must be the same as data type of user vector x
 * \param userdata IO  user data
 * \param retval   O   must receive return value of user implementation function
 * \return no return value
 */
void usr_vector_getvals(Usr_Vector *x, void *vals, int *n, CTA_Datatype *datatype,
                         void *userdata, int *retval);


//CTA_VECTOR_SETVALS     ( 4)
/** \brief Implementation for setting all values of the user vector.
 *
 * \note At index CTA_VECTOR_SETVALS in the function list of the class descriptor.
 *
 * \param x        I   pointer to object data of user vector
 * \param vals     I   new values
 * \param n        I   number of elements in the vals buffer, must equal number of elements in user vector
 * \param datatype I   data type of elements of vals, must be the same as data type of user vector x
 * \param userdata IO  user data
 * \param retval   O   must receive return value of user implementation function
 * \return no return value
 */
void usr_vector_setvals(Usr_Vector *x, void *vals, int *n, CTA_Datatype *datatype,
                         void *userdata, int *retval);


//CTA_VECTOR_SETCONST    ( 5)
/** \brief Implementation for setting all values of user vector to same value.
 *
 * \note At index CTA_VECTOR_SETCONST in the function list of the class descriptor.
 *
 * \param x        I  pointer to object data of user vector
 * \param val      I  value to set vector elements to
 * \param datatype I  data type of elements of vals, must be the same as data type of user vector x
 * \param userdata IO user data
 * \param retval   O  must receive return value of user implementation function
 * \return no return value
 */
void usr_vector_setconst(Usr_Vector *x, void *val, CTA_Datatype *datatype,
                          void *userdata, int *retval);


//CTA_VECTOR_SCAL        ( 6)
/** \brief Implementation for scaling the user vector: x=ax
 *
 * \note At index CTA_VECTOR_SCAL in the function list of the class descriptor.
 *
 * \param x        IO pointer to object data of user vector
 * \param alpha    I  scaling factor
 * \param userdata IO user data
 * \param retval   O  must receive return value of user implementation function
 * \return no return value
 */
void usr_vector_scal(Usr_Vector *x, double *alpha, void *userdata, int *retval);


//CTA_VECTOR_COPY        ( 7)
/** \brief Implementation for copying the user vector.
 *
 * \note At index CTA_VECTOR_COPY in the function list of the class descriptor.
 *
 * \param x        I  pointer to object data of a user vector
 * \param y        O  pointer to object data of user vector y that must receive copy of x; must exist before calling
 * \param userdata IO user data
 * \param retval   O  must receive return value of user implementation function
 * \return no return value
 */
void usr_vector_copy(Usr_Vector *x, Usr_Vector *y, void *userdata,
                      int *retval);


//CTA_VECTOR_AXPY        ( 8)
/** \brief Implementation for applying the BLAS operation axpy: y=y+ax
 *
 * \note At index CTA_VECTOR_AXPY in the function list of the class descriptor.
 *
 * \param y        IO pointer to object data of user vector y; must exist before calling
 * \param alpha    I  scaling factor
 * \param x        I  pointer to object data of user vector x
 * \param userdata IO user data
 * \param retval   O  must receive return value of user implementation function
 * \return no return value
 */
void usr_vector_axpy(Usr_Vector *y, double *alpha, Usr_Vector *x,
                      void *userdata, int *retval);

//CTA_VECTOR_DOT         ( 9)
/** \brief Implementation for calculating the dot product of two user vectors x.y
 *
 * \note At index CTA_VECTOR_DOT in the function list of the class descriptor.
 *
 * \param y        I  pointer to object data of user vector y
 * \param x        I  pointer to object data of user vector x
 * \param userdata IO user data
 * \param dotprod  O  must receive the dot product
 * \param retval   O  must receive return value of user implementation function
 * \return no return value
 */
void usr_vector_dot(Usr_Vector *x, Usr_Vector *y, void *userdata,
                     double *dotprod, int *retval);

//CTA_VECTOR_NRM2        (10)
/** \brief Implementation for calculating the 2-norm of a user vector
 *
 * \note At index CTA_VECTOR_NRM2 in the function list of the class descriptor.
 *
 * \param x        I  pointer to object data of user vector
 * \param userdata IO user data
 * \param norm2    O  must receive the 2-norm
 * \param retval   O  must receive return value of user implementation function
 * \return no return value
 */
void usr_vector_nrm2(Usr_Vector *x, void *userdata, double *norm2, int *retval);

//CTA_VECTOR_AMAX        (11)
/** \brief Implementation for getting the index of the element with the largest maximum value. First index is 1.
 *
 * \note At index CTA_VECTOR_AMAX in the function list of the class descriptor.
 *
 * \param x        I  pointer to object data of user vector
 * \param userdata IO user data
 * \param iloc     O  must receive index of maximum value
 * \param retval   O  must receive return value of user implementation function
 * \return no return value
 */
void usr_vector_amax(Usr_Vector *x, void *userdata, int *iloc, int *retval);


//CTA_VECTOR_GETMAXLEN   (12)
/** \brief Implementation for getting the maximum length in case of vector with string elements. 
 *
 * \note At index CTA_VECTOR_GETMAXLEN in the function list of the class descriptor.
 *
 * \param x        I  pointer to object data of user vector
 * \param userdata IO user data
 * \param maxlen   O  must receive maximum length
 * \param retval   O  must receive return value of user implementation function
 * \return no return value
 */
void usr_vector_getmaxlen(Usr_Vector *x, void *userdata, int *maxlen, int *retval);


//CTA_VECTOR_FREE        (13)
/** \brief Implementation for freeing the user vector.
 *
 * \note At index CTA_VECTOR_FREE in the function list of the class descriptor.
 *
 * \param x        IO pointer to object data of user vector
 * \param userdata IO user data
 * \param retval   O  must receive return value of user implementation function
 * \return no return value
 */
void usr_vector_free(Usr_Vector *x, void *userdata, int *retval);


//CTA_VECTOR_GETVAL      (14)
/** \brief Implementation for retrieving a single value of user vector at given index
 *
 * \note At index CTA_VECTOR_GETVAL in the function list of the class descriptor.
 *
 * \param x        I  pointer to object data of user vector
 * \param i        I  index of value to retrieve
 * \param val      O  must receive value; must exist before calling
 * \param datatype I  data type of *val, must be the same as data type of user vector x
 * \param userdata IO user data
 * \param retval   O  must receive return value of user implementation function
 * \return no return value
 */
void usr_vector_getval(Usr_Vector *x, int *i, void *val, CTA_Datatype *datatype,
                        void *userdata, int *retval);


//CTA_VECTOR_SETVAL      (15)
/** \brief Implementation for setting a single value of a user vector.
 *
 * \note At index CTA_VECTOR_SETVAL in the function list of the class descriptor.
 *
 * \param x        I  pointer to object data of user vector
 * \param i        I  index of value to set
 * \param val      I  new data of value
 * \param datatype I  data type of *val, must be the same as data type of user vector x
 * \param userdata IO user data
 * \param retval   O  must receive return value of user implementation function
 * \return no return value
 */
void usr_vector_setval(Usr_Vector *x, int *i, void *val, CTA_Datatype *datatype,
                        void *userdata, int *retval);


//CTA_VECTOR_EXPORT      (16)
/** \brief Implementation for exporting a user vector.
 *
 * \note At index CTA_VECTOR_EXPORT in the function list of the class descriptor.
 * \n For example implementation for exporting a user vector to a pack object or to screen.
 *
 * \param x        I  pointer to object data of user vector
 * \param userdata IO user data
 * \param retval   O  must receive return value of user implementation function
 * \return no return value
 */
void usr_vector_export(Usr_Vector *x, void userdata, int *retval);


//CTA_VECTOR_PRINT_TABLE (17)
/** \brief Implementation for printing a table.
 *
 * \note At index CTA_VECTOR_PRINT_TABLE in the function list of the class descriptor.
 *
 * \param table        IO array of vectors that forms the table to be filled
 * \param ncolumns     I  number of columns
 * \param formats      I  user vector
 * \param retval       O  must receive return value of user implementation function
 * \return no return value
 */
void usr_vector_print_table(Usr_Vector **table, int *ncolumns,
                      Usr_Vector * formats,     int *retval);


//CTA_VECTOR_APPENDVAL   (18)
/** \brief Implementation for appending a single value to the user vector.
 *
 * \note At index CTA_VECTOR_APPENDVAL in the function list of the class descriptor.
 *
 * \param x            IO pointer to object data of user vector
 * \param val          I  value to append
 * \param datatype     I  data type of value to append, must be the same as data type of user vector x
 * \param retval       O  must receive return value of user implementation function
 * \return no return value
 */
void usr_vector_appendval(Usr_Vector *x, void *val, CTA_Datatype *datatype,
                           int *retval);


//CTA_VECTOR_ELMDIV      (19)
/** \brief Implementation for element-wise division of user vector.
 *
 * \note At index CTA_VECTOR_ELMDIV in the function list of the class descriptor.
 *
 * \param y            IO pointer to object data of receiving user vector
 * \param x            I  pointer to object data of user vector x
 * \param userdata     IO user data
 * \param retval       O  must receive return value of user implementation function
 * \return no return value
 */
void usr_vector_elmdiv(Usr_Vector *y, CTAI_Vector_blas *x,
                      void *userdata, int *retval);



//CTA_VECTOR_IMPORT      (20)
/** \brief Implementation for importing a user vector.
 *
 * \note At index CTA_VECTOR_IMPORT in the function list of the class descriptor.
 *
 * \param x            O  pointer to object data of user vector that must receive import result
 * \param userdata     IO user data
 * \param retval       O  must receive return value of user implementation function
 * \return no return value
 */
void usr_vector_import(Usr_Vector *x, void userdata, int *retval);











