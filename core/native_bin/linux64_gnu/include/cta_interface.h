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
\file  cta_interface.h
\brief Description of the COSTA interface componennt used for
communication with user implementation classes.

To create an object of your user implementation go through the following steps:

1: Create a new class (=implementation) of a COSTA component by calling the
appropriate DefineClass() function providing the name of the class, a matrix with
CTA_Function objects describing the user functions.

example:

<code>void Usr_Object_initialise(CTA_MatClass *hmatcl){
   CTA_Intf hintf=0;\n
   CTA_Func h_func[CTA_MATRIX_NUMFUNC];\n
//   CTA_Func h_create_size;\n
//   CTA_Func h_Create_Init;\n
   int ierr;\n

   ierr=CTA_Func_Create(" ",&CTAI_Matrix_Create_Size, hintf,&h_func[CTA_MATRIX_CREATE_SIZE]);\n
   ierr=CTA_Func_Create(" ",&CTAI_Matrix_Create_Init, hintf,&h_func[CTA_MATRIX_CREATE_INIT]);\n
   ierr=CTA_Func_Create(" ",&CTAI_Matrix_getvals,     hintf,&h_func[CTA_MATRIX_GETVALS]    );\n
   ierr=CTA_Func_Create(" ",&CTAI_Matrix_getval,      hintf,&h_func[CTA_MATRIX_GETVAL]     );\n
   ierr=CTA_Func_Create(" ",&CTAI_Matrix_setcol,     hintf,&h_func[CTA_MATRIX_SETCOL]    );\n
   ierr=CTA_Func_Create(" ",&CTAI_Matrix_setvals,     hintf,&h_func[CTA_MATRIX_SETVALS]    );\n
   ierr=CTA_Func_Create(" ",&CTAI_Matrix_setval,      hintf,&h_func[CTA_MATRIX_SETVAL]     );\n
   ierr=CTA_Func_Create(" ",&CTAI_Matrix_setconst,    hintf,&h_func[CTA_MATRIX_SETCONST]   );\n
   ierr=CTA_Func_Create(" ",&CTAI_Matrix_Export,      hintf,&h_func[CTA_MATRIX_EXPORT]     );\n
   ierr=CTA_Func_Create(" ",&CTAI_Matrix_Ger,         hintf,&h_func[CTA_MATRIX_GER]        );\n
   ierr=CTA_Func_Create(" ",&CTAI_Matrix_Inv,         hintf,&h_func[CTA_MATRIX_INV]        );\n
   ierr=CTA_Func_Create(" ",&CTAI_Matrix_Gemv,        hintf,&h_func[CTA_MATRIX_GEMV]        );\n
   ierr=CTA_Func_Create(" ",&CTAI_Matrix_Gemm,        hintf,&h_func[CTA_MATRIX_GEMM]        );\n
   ierr=CTA_Func_Create(" ",&CTAI_Matrix_Axpy,        hintf,&h_func[CTA_MATRIX_AXPY]        );\n
   ierr=CTA_Func_Create(" ",&CTAI_Matrix_free,        hintf,&h_func[CTA_MATRIX_FREE]       );\n

   ierr=CTA_Object_DefineClass("cta_matrix_blas",h_func,hmatcl);\n
}</code>

2: use the resulting class object for creating the user object in following way:

<code>int CTA_Matrix_Create(classobject, ..., *userdata, Usr_Object *usrObject);</code>

*/

#ifndef CTA_INTERFACE_H
#define CTA_INTERFACE_H
#include "cta_system.h"
#include "cta_datatypes.h"
#include "cta_handles.h"

CTAEXPORT typedef int CTA_Intf;

#ifdef __cplusplus
extern "C" {
#endif

/** \brief Create a new interface
 *
 * \param name     I  name of the new interface
 * \param argtyp   I  list with the data types of arguments
 * \param narg     I  number of arguments of interface
 * \param hintf    O  receives the new COSTA interface handle
 * \return error status: CTA_OK
 */
CTAEXPORT int CTA_Intf_Create(const char *name, const CTA_Datatype *argtyp,
                    const int narg,CTA_Intf *hintf);

/** \brief Free an interface
 *
 *  \note Freeing CTA_NULL is allowed.
 *
 *  \param hintf  IO  handle of interface, replaced by CTA_NULL on return
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE, CTA_INCOMPATIBLE_HANDLE 
 */
CTAEXPORT int CTA_Intf_Free(CTA_Intf *hintf);

/** \brief Match two interfaces for compatibility argumentlist-argumentlist
 *
 *  \note Two interfaces are compatible if all arguments have the same datatype,
 *        CTA_VOID is compatible with all other arguments except for CTA_FSTRING
 *
 *  \param argtyp1  I  list with the data types of arguments of first interface
 *  \param narg1    I  number of arguments in first interface
 *  \param argtyp2  I  list with the data types of arguments of second interface
 *  \param narg2    I  number of arguments in second interface
 *  \param flag     O  receives TRUE if interfaces are compatible FALSE ortherwise
 *  \return error status: CTA_OK
 */
CTAEXPORT int CTA_Intf_Match_aa(const CTA_Datatype *argtyp1, const int narg1, 
                      const CTA_Datatype *argtyp2, const int narg2,
                      BOOL *flag);

/** \brief Match two interfaces for compatibility handle-argumentlist
 *
 *  \note Two interfaces are compatible if all arguments have the same datatype,
 *        CTA_VOID is compatible with all other arguments except for CTA_FSTRING
 *
 *  \param hintf1   I  handle of first interface
 *  \param argtyp2  I  list with the data types of arguments of second interface
 *  \param narg2    I  number of arguments in second interface
 *  \param flag     O  receives TRUE if interfaces are compatible FALSE ortherwise
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE, CTA_INCOMPATIBLE_HANDLE
 */
CTAEXPORT int CTA_Intf_Match_ha(const CTA_Intf hintf1,
                      const CTA_Datatype *argtyp2, const int narg2, BOOL *flag);

/** \brief Match two interfaces for compatibility handle-handle
 *
 *  \note two interfaces are compatible if all arguments have the same datatype,
 *        CTA_VOID is compatible with all other arguments except for CTA_FSTRING
 *
 *  \param hintf1   I  handle of first interface
 *  \param hintf2   I  handle of second interface
 *  \param flag     O  receives TRUE if interfaces are compatible FALSE ortherwise
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE, CTA_INCOMPATIBLE_HANDLE 
 */
CTAEXPORT int CTA_Intf_Match_hh(const CTA_Intf hintf1, const CTA_Intf hintf2, BOOL *flag);

#ifdef __cplusplus
}
#endif
#endif

