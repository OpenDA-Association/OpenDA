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
\file  ctai_vector.h
\brief Utilities for working with CTA_Vector objects.
*/

#ifndef NL_VORTECH_CTAI_VECTOR_H
#define NL_VORTECH_CTAI_VECTOR_H

#ifdef __cplusplus
extern "C" {
#endif
/** \brief Count the number of handles with the given name in the vector (internal use only!)
 *
 * \param hvec   I  vector in which to search
 * \param name   I  name of handle(s) to find
 *
 * \return number of handles counted or -1 in case of an error.
 */
int CTAI_Vector_CountHandles(CTA_Vector hvec, const char* name);


/** \brief Find the handle with the given name (internal use only!)
 *
 *
 * \param hvec     I  vector in which to search
 * \param name     I  name of handle to find
 *
 * \return handle or CTA_NULL if not found or in case of an error
 */
CTA_Handle CTAI_Vector_FindHandle(CTA_Vector hvec, const char* name);


/** \brief Get vector value at index i as C string (internal use only!)
 *
 * \note Free pointer after use.
 *
 * \param hvec I  vector
 * \param i    I  index of value
 *
 * \return string representation of value at index i of vector
 */
const char *CTAI_Vector_GetStringVal(CTA_Vector hvec, int  i);


/** \brief Set vector value at index i giving a C string to be converted to given data type (internal use only!)
 *
 * \param hvec      IO handle of vector
 * \param i         I  index of value to set
 * \param val       I  string representation of value
 * \param datatype  I  data type of value
 *
 * \return CTA_OK if succesful or CTA_ILLEGAL_DATATYPE if invalid data type
 */
int CTAI_Vector_SetStringVal(CTA_Vector hvec, int  i, const char *val, CTA_Datatype datatype);

/** \brief Create a COSTA vector. 
*
*  \param cur_node  I  Current XML node 
*  \return             Handle to create or CTA_NULL in case of an error.
*/
CTA_Vector CTAI_XML_CreateVector(xmlNode *cur_node);

/** \brief Generate XML from one COSTA vector
*
*  \param hvec   I  handle of a COSTA vector
*  \param writer I  the XML text writer
*/
//void CTAI_XML_WriteVector(CTA_Vector hvec, xmlTextWriter *writer);
  void CTAI_XML_WriteVector(CTA_Vector hvec, const char *id, const char *caption, CTA_Metainfo minfo, int level, xmlTextWriter *writer);


/** \brief Create a COSTA vector from XML. 
*
*  \param cur_node  I  Current XML node 
*  \param minfo     I/O  Meta info object
*  \return             Handle to create or CTA_NULL in case of an error.
*/
CTA_Vector CTAI_XML_CreateVector_New(xmlNode *cur_node, CTA_Metainfo minfo);


#ifdef __cplusplus
}
#endif
#endif
