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
\file  cta_functions.h
\brief Functions for working with CTA_Function objects. CTA_Function objects can be used to address functions.
*/

#ifndef CTA_FUNCTIONS_H
#define CTA_FUNCTIONS_H
#include "cta_system.h"
#include "cta_datatypes.h"
#include "cta_handles.h"
#include "cta_interface.h"
#include "ctai_xml.h"

/* Function Handle */
typedef CTA_Handle CTA_Func;

#ifdef __cplusplus
extern "C" {
#endif


/** \brief Create a new COSTA function.
 *
 *  \note Argument name is only used for debugging and output.
 *
 *  \param name     I  name of the new function for debugging purposes
 *  \param function I  pointer to function that has to be associated
 *                     with new COSTA function
 *  \param hintf    I  handle of associated interface
 *  \param hfunc    O  receives handle of created COSTA function
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_Func_Create(const char *name, CTA_Function *function , 
                    const CTA_Intf hintf, CTA_Func *hfunc);

/** \brief Create a COSTA function loaded from dynamic library
*
*

*  \param libraryName  I  Name of the dynamic library excluding the extention (.dll, .so, .dylib) eg "libmyroutines"
*  \param functionName I  Name of the function to load. 
*  \param name         I  Name of the function used inside the OpenDA administration
*  \param id           I  Name (id) of the handle that is created (needed when stored as part of a tree
*  \return             Handle to function or CTA_NULL in case of an error.
*
*  \note When the load of the function fails, some decorated, typical Fortran variations are tried as well
*
*/
CTANOF90 CTAEXPORT CTA_Func CTA_CreateFuncDynamicLib(char *libraryName, char *functionName, char *name, char *id);

/** \brief Duplicates a user defined function
 *
 * \param hfunc      I COSTA user function handle
 * \param hdupl      I duplication of hfunc
 * \return error status: CTA_OK
 */
CTAEXPORT int CTA_Func_Duplicate(CTA_Func hfunc, CTA_Func *hdupl);



/** \brief Free a new COSTA function.
 *
 *  \note 
 *
 *  \param hfunc  IO handle of COSTA function, replaced by CTA_NULL on return
 *  \return CTA_OK if sucessful
 */
CTAEXPORT int CTA_Func_Free(CTA_Func *hfunc);

/** \brief Get interface of COSTA function.
 *
 *  \note For performance reasons, the interface is not a copy but a handle
 *        to the actual interface, it should NOT be freed by the calling routine!
 *
 *  \param hfunc  I  handle of COSTA function
 *  \param hintf  O  receives handle of interface of function
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_Func_GetIntf(const CTA_Func hfunc, CTA_Intf *hintf);

/** \brief Get function pointer of function
 *
 *  \note There is no FORTRAN verion of this function available
 *
 *  \param hfunc     I  handle of COSTA function.
 *  \param function  O  receives pointer to function
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_Func_GetFunc(const CTA_Func hfunc, CTA_Function **function);

/** \brief Get name of function
 *
 *  \note Future versions will return a COSTA string handle.
 *
 *  \param hfunc     I  handle of COSTA function.
 *  \param name      O  handle of string object that is to receive function name, must exist before calling
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_Func_GetName(const CTA_Func hfunc, CTA_String name);

/** \brief Set userdata of function
 *
 *  \note Frees existing user data and replaces it with userdata
 *
 *  \param hfunc     IO handle of COSTA function.
 *  \param userdata  I  new userdata handles
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_Func_SetUserdata(const CTA_Func hfunc, const CTA_Handle userdata );

/** \brief Get userdata of function
 *
 *  \param hfunc     I  handle of COSTA function.
 *  \param userdata  O  userdata handle
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_Func_GetUserdata(const CTA_Func hfunc, CTA_Handle userdata );

/** \brief Create a COSTA function from XML
*          (load from dynamic load library). 
*
*  \param cur_node  I  Current XML node 
*  \return             Handle to create or CTA_NULL in case of an error.
*/
CTANOF90 CTAEXPORT CTA_Func CTAI_XML_CreateFunc(xmlNode *cur_node);



#ifdef __cplusplus
}
#endif
#endif

