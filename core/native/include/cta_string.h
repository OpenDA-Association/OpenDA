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
\file  cta_string.h
\brief Interface description of the default COSTA string component.

Utilities including most of the basic string operations and access to the string data itself.
*/

#ifndef CTA_STRING_H
#define CTA_STRING_H
#include "cta_system.h"
#include "cta_handles.h"
#include "cta_datatypes.h"

/* String instance handle declared in cta_handles.h */
#ifdef __cplusplus
extern "C" {
#endif

/** \brief Create a new COSTA string instance.
 *
 *  \param hstring  O  handle of created string
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_String_Create(CTA_String *hstring);

/** \brief Create a new COSTA string that is a copy of an existing one
 *
 *  \param hto    O  receives the handle of the created string
 *  \param hfrom  I  handle of string to copy
 *
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_String_Copy(CTA_String *hto, CTA_String hfrom);

/** \brief Free the COSTA string instance.
 *
 *  \note
 *
 *  \param hstring  IO handle of the string instance, replaced by CTA_NULL on return
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_String_Free(CTA_String *hstring);

/** \brief Get the number of characters in string.
 *
 *  \note The returned length is the number of characters excluding the 
 *        0-character.
 *
 *  \param hstring  I  handle of the string
 *  \param len      O  receives the number of characters in string
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_String_GetLength(CTA_String hstring, int *len);

/** \brief Set the string to new content.
 *
 *  \param hstring  IO handle of the string
 *  \param str      I  new content
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_String_Set(CTA_String hstring, const char *str);

/** \brief Get a copy of the string.
 *
 *  \note It is the responsibility of the caller making str large enough to
 *        hold the string and trailing 0-character.
 *
 *  \param hstring  I  handle of the string
 *  \param str      O  buffer that receives a copy of the string including trailing 0-character
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_String_Get(CTA_String hstring, char *str);

/** \brief Get the (scalar) value of a string
 *
 *  \note It is the responsibility of the caller that parameter value is large enough to
 *        hold the value as specified by the datatype.
 *
 *  \param hstring  I  handle of the string
 *  \param value    O  receives the value
 *  \param datatype I  data type of value
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_String_GetValue(CTA_String hstring, void *value, CTA_Datatype datatype);


/** \brief Create new string that is a concatination of existing strings.
 *
 *  \param istring  IO handle of the string (first string in concatination)
 *                     and whole concatinated string on return
 *  \param xstring  I  handle of the second string (extension string)
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_String_Conc(CTA_String istring, CTA_String xstring);


/** \brief Get a pointer to the contents of the string (INTERNAL USE)
 *
 *  \param hstring  I  handle of the string
 *  \return pointer to the string contents
 */
CTAEXPORT char* CTAI_String_GetPtr(CTA_String hstring);


/** \brief Imports string.
 *
 *  Supports: pack objects (usrdata must be handle of pack object to import from)
 *
 *  \param hstring  IO handle of the string
 *  \param usrdata  I  configuration of import
 *  \return CTA_OK if successful
 *
 *  \note Only CTA_Pack is currently supported fot usrdata
 */
CTAEXPORT int CTA_String_Import(CTA_String hstring, CTA_Handle usrdata);

/** \brief Exports length of string and string itself.
 *
 *  Supports: pack objects (usrdata must be handle of pack object to export to)
 *
 *  \param hstring  I  handle of the string
 *  \param usrdata  IO configuration of export
 *  \return CTA_OK if successful
 *
 *  \note Only CTA_Pack is currently supported fot usrdata
 */
CTAEXPORT int CTA_String_Export(CTA_String hstring, CTA_Handle usrdata);

/** \brief Check whether string is equal to COSTA string.
 *
 *
 *  \param hstring  I  handle of the string
 *  \param str0     I  string to compare hsting with
 *  \return CTA_TRUE/CTA_FALSE 
 *
 *  \note Only CTA_Pack is currently supported fot usrdata
 */
CTAEXPORT int CTA_String_Equals_Char(CTA_String hstring, const char *str0);

/** \brief Check whether two COSTA strings are equal.
 *
 *
 *  \param hstring1  I  handle of first string
 *  \param hstring2  I  handle of second string
 *  \return CTA_TRUE/CTA_FALSE 
 *
 *  \note Only CTA_Pack is currently supported fot usrdata
 */
CTAEXPORT int CTA_Strings_Equal(CTA_String hstring1, CTA_String hstring2);


/** \brief Create a duplication of a COSTA string 
 *
 *  \param hfrom  I  handle of string to copy
 *  \param hto    O  handle of created string
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_String_Duplicate(CTA_String hfrom, CTA_String *hto);

/*=====================================================================*/
#ifdef __cplusplus
}
#endif
#endif

