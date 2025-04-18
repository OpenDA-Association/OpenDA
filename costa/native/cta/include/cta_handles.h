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
\file  cta_handles.h
\brief Description of functions for working with CTA_Handle objects that are being used to access COSTA objects.
*/

#ifndef CTA_HANDLES_H
#define CTA_HANDLES_H

#include "cta_datatypes.h"

// Constants

// Derived types
CTAEXPORT typedef int CTA_Handle;

/* String instance handle - needed for GetName() */
CTAEXPORT typedef CTA_Handle CTA_String;

#ifdef __cplusplus
extern "C" {
#endif

// Interfaces
/** \brief Create a new COSTA handle
 *
 * \param name     I  name associated with handle
 * \param datatype I  data type of handle
 * \param data     I  block of data associated to handle
 * \param handle   O  receives COSTA handle
 * \return error status: CTA_OK
 */
CTAEXPORT int CTA_Handle_Create(const char *name, const CTA_Datatype datatype,
                       void *data, CTA_Handle *handle);

/** \brief Free a COSTA handle
 *
 *  \note The data part of the handle is NOT freed.
 *
 *  \param handle IO handle that is to be freed, replaced by CTA_NULL on return.
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE
 */
CTAEXPORT int CTA_Handle_Free(CTA_Handle *handle);

/** \brief Free a COSTA handle
 *
 *  \note Calls datatype-specific free methods, if available.
 *
 *  \param handle IO handle that must be freed, replaced by CTA_NULL on return.
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE
 */
CTAEXPORT int CTA_Handle_Free_All(CTA_Handle *handle);

/** \brief Check whether a handle is valid and checks type
 *
 *  \note The handle CTA_NULL is not valid.
 *
 * \param handle   I  COSTA handle
 * \param datatype I  data type to compare handle with
 * \return error status: CTA_OK, CTA_ILLEGAL_HANDLE, CTA_INCOMPATIBLE_HANDLE 
 *
 */
CTAEXPORT int CTA_Handle_Check(const CTA_Handle handle,const CTA_Datatype datatype);

/** \brief Get pointer to data element of handle
 *
 *  \param handle I  COSTA handle
 *  \param data   O  receives pointer to data element
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE
 */
CTAEXPORT int CTA_Handle_GetData(const CTA_Handle handle, void **data);

/** \brief Get the value the handle points to
 *
 *  \param handle 	I  COSTA handle
 *  \param value   	O  receives pointer to data element
 *  \param datatype     I  specify the data type of *value, must be the same as data type of handle
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE, CTA_INCOMPATIBLE_HANDLE
 */
CTAEXPORT int CTA_Handle_GetValue(const CTA_Handle handle, void *value, CTA_Datatype datatype);

/** \brief Get name associated with handle
 *
 *  \param handle   I  COSTA handle
 *  \param hname    O  receives name of data type
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE
 */
CTAEXPORT int CTA_Handle_GetName(const CTA_Handle handle, CTA_String hname);

/** \brief Get data type associated with handle
 *
 *  \param handle   I  COSTA handle
 *  \param datatype O  receives data type of handle
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE
 */
CTAEXPORT int CTA_Handle_GetDatatype(const CTA_Handle handle, CTA_Datatype *datatype);

/** \brief Set name associated with handle (internal use only!)
 *
 *  \param handle   IO COSTA handle
 *  \param name     I  COSTA string
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE
 */
CTAEXPORT int CTAI_Handle_SetName(const CTA_Handle handle, const char *name);

/** \brief Find a handle by name and data type in the COSTA handle administration
 *
 *  \param sname    I  name of handle
 *  \param datatype I  data type of handle
 *  \param handlenr O  receives the handle (only if return value is CTA_OK)
   *  \return error status: CTA_OK, CTA_HANDLE_NOT_FOUND
 */
CTAEXPORT int CTA_Handle_Find(CTA_String sname, CTA_Datatype datatype, int *handlenr);

/** \brief Print overview of all COSTA handles 
 *
   *  \return error status: CTA_OK
 */
CTAEXPORT int CTA_Handle_Printall();

/** \brief Counts all handles sorts them by type and prints overview.
 * This function can be usefull for detecting memory leaks that are 
 * the result of not freeing instances of COSTA objects costa objects.
 *
 *  \param location I String to indicate location of call
 *  \return error status: CTA_OK
 */
CTAEXPORT int CTA_Handle_PrintInfo(const char *location);

/** \brief Get the reference count of the handle
 *
 *  \param handle   I COSTA handle
 *  \param refCount O reference count of handle
 *  \return error status: CTA_OK
 *
 */
CTAEXPORT int CTA_Handle_GetRefCount(const CTA_Handle handle, int *refCount);

/** \brief Increase the reference count of the handle
 *
 *  \param handle   I COSTA handle
 *  \return error status: CTA_OK
 *
 */ 
CTAEXPORT int CTA_Handle_IncRefCount(const CTA_Handle handle);

/** \brief Decrease the reference count of the handle
 *
 *  \param handle   I COSTA handle
 *  \return error status: CTA_OK
 *
 */   
CTAEXPORT int CTA_Handle_DecrRefCount(const CTA_Handle handle);
   

#ifdef __cplusplus
}
#endif
#endif

