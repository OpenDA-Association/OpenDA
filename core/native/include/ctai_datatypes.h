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
\file ctai_datatypes.h
\brief Utility functions for working with data types.

\note This is just a set of utility routines it does not define any COSTA component.

*/


#ifndef NL_VORTECH_CTAI_DATA_TYPS_H
#define NL_VORTECH_CTAI_DATA_TYPS_H

#ifdef __cplusplus
extern "C" {
#endif
/** \brief Get a CTA_Datatype object describing the data type given in string representation
 *
 *  \param dt I description of data type in string form
 *  \return CTA_Datatype describing the data type, CTA_NULL if data type is unknown
 */
CTA_Datatype CTAI_String2Type(const char *dt);



/** \brief Get string representation of CTA_Datatype object
 *
 *  \param datatype I CTA_Datatype object describing the data type
 *  \return pointer to string containing description of data type ("CTA_ILLEGAL_DATATYPE" if unknown)
 */
const char *CTAI_Type2String(CTA_Datatype datatype);
#ifdef __cplusplus
}
#endif
#endif
