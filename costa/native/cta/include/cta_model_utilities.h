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
\file  cta_model_utilities.h
\brief A set of utilities that simplify the creation of COSTA model components.

A set of utility routines. These routines are originally designed for
the rapid development of COSTA model components. But their use is not limited
to model components.

\note This is just a set of utility routines it does not define any COSTA component

*/


#ifndef CTA_MODEL_UTILITIES_H
#define CTA_MODEL_UTILITIES_H
#include "cta_system.h"
#include "cta_handles.h"
#include "cta_datatypes.h"
#include "cta_tree.h"
#include "cta_string.h"

#ifdef __cplusplus
extern "C" {
#endif

/** \brief Handles model configuration tree or name of input xml-file
 *
 * When a new instance of the a model component is created it needs some
 * input. The most convenient way is to provide the root to a COSTA-tree
 * with configuration information the name of a XML-configuration file
 * This routine will check whether the input handle is a COSTA-tree or
 * the name of a XML-configuration file. A COSTA-tree containing the content
 * of the XML-file is created when the input is the name of the XML-file.
 *
 * \note A COSTA-tree is created if the input is the name of an XML-file.
 *       the handle of the input tree is returned otherwise. This means that 
 *       depending on the input, a tree is created. The routine will return whether
 *       the returned tree is created by this routine. The caller is responsible 
 *       for freeing the tree when necessary. 
 *
 * \param hinput   I  handle of tree (CTA_Tree) with model input or string
 *                    (CTA_Sring) with name of xml-input file
 * \param tinput   O  receives handle of tree (CTA_Tree) with model input
 * \param cleanup  O  receives flag (CTA_TRUE/CTA_FALSE) indicating whether tinput is
 *                    created and must be freed by the caller
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Model_Util_InputTree(CTA_Handle hinput, CTA_Tree *tinput, int *cleanup);

#ifdef __cplusplus
}
#endif
#endif


   
