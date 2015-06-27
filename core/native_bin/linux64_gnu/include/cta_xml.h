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
\file  cta_xml.h
\brief Utilities for XML access: reading/writing of trees from/to a XML file.
\note This is just a set of utility routines it does not define any COSTA component

*/

#ifndef CTA_XML_H
#define CTA_XML_H
#include "cta_system.h"
#include "cta_tree.h"
#include "cta_string.h"

/* tree functions */
#ifdef __cplusplus
extern "C" {
#endif
/** \brief Read a COSTA XML file into a new tree.
 *
 *  \param hfname  I  file name of XML file to read
 *  \param hroot   O  handle of a new COSTA tree
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_XML_Read(CTA_String hfname, CTA_Tree *hroot);


/** \brief Write a tree to a COSTA XML file
 *
 *  \param hfname  I  file name of XML file to write
 *  \param hroot   I  handle of a COSTA tree
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_XML_Write(CTA_String hfname, CTA_Tree hroot);

#ifdef __cplusplus
}
#endif
#endif /* CTA_XML_H */
