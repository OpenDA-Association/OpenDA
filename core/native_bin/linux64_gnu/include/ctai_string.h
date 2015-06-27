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
\file  ctai_string.h

\brief Internal string utility functions
*/
#ifndef NL_VORTECH_CTAI_STRING_H
#define NL_VORTECH_CTAI_STRING_H

#ifdef __cplusplus
extern "C" {
#endif
/** \brief Get a pointer to the contents of the string (internal use only!)
 *
 *  \param hstring I handle of string object
 *  \return pointer to string contents
 */
char *CTAI_String_GetPtr(CTA_String hstring);

/** \brief Allocate a string, created from a COSTA string (internal use only!).
 *
 *  \param hstr I handle of string object to use
 *  \return pointer to C string allocated by this function
 */
char *CTAI_String_Allocate(CTA_String hstr);

#ifdef __cplusplus
}
#endif
#endif
