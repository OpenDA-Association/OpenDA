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
\file  ctai_handles.h
\brief Utility functions for working with CTA_Handle objects.

\note This is just a set of utility routines it does not define any COSTA component
*/
#ifndef NL_VORTECH_CTAI_HANDLES_H
#define NL_VORTECH_CTAI_HANDLES_H

#ifdef __cplusplus
extern "C" {
#endif

/** \brief Get data type of object the given handle is referring to (internal use only!)
 *
 *  \param handle I handle of which to get the data type
 *  \return data type associated with handle (no error codes)
 */
CTA_Datatype CTAI_Handle_GetDatatype(const CTA_Handle handle);


/** \brief Get name associated with the handle (internal use only!)
 *
 *  \param handle I handle to get name of
 *  \return pointer to name associated with the given handle
 */
const char *CTAI_Handle_GetName(const CTA_Handle handle);


/** \brief Get data belonging to the handle (internal use only!)
 *
 *  \param handle I handle of which to get the data
 *  \return constant pointer to the data belonging to the handle
 */
const void *CTAI_Handle_GetData(const CTA_Handle handle);


/** \brief Set name associated with the handle (internal use only!)
 *
 *  \param handle IO handle of which to set the name
 *  \param name   I  new name associated with handle
 *  \return CTA_OK is succesful
 */
int CTAI_Handle_SetName(const CTA_Handle handle, const char *name);

#ifdef __cplusplus
}
#endif
#endif

