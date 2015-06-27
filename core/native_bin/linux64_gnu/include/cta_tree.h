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
\file  cta_tree.h

\brief Interface description of the COSTA tree component.

Store data in a CTA_Tree object in tree form. Access the members in the following way:
 <I>branch1\\subbranch\\member</I> or <I>branch1/branch2/member</I>
*/

#ifndef CTA_TREE_H
#define CTA_TREE_H
#include "cta_system.h"
#include "cta_handles.h"
#include "cta_datatypes.h"

/* Tree instance handle */
typedef CTA_Handle CTA_Tree;
#ifdef __cplusplus
extern "C" {
#endif
/** \brief Create a new COSTA tree instance 
 *
 *  \note
 *
 *  \param htree  O  receives handle of created tree
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_Tree_Create(CTA_Tree *htree);


/** \brief Free the COSTA tree instance 
 *
 *  \note 
 *
 *  \param htree  IO handle of the tree instance, replaced by CTA_NULL on return
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_Tree_Free(CTA_Tree *htree);


/** \brief Add a COSTA handle to the COSTA tree
 *
 *  \note
 *
 *  \param htree  IO handle of the tree object (parent)
 *  \param name   I  name of the COSTA item
 *  \param hitem  I  handle of the COSTA item to add (do not free the object after adding it to the tree)
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_Tree_AddHandle(CTA_Tree htree, const char *name, CTA_Handle hitem);


/** \brief Count the number of COSTA handles specified by the given path.
 *
 *  \param htree  I  handle of the tree object
 *  \param path   I  path of the item, separated by / or \\
 *  \param count  O  receives the number of items found
 *  \return CTA_OK if successful or CTA_ITEM_NOT_FOUND in case of not found
 */
CTAEXPORT int CTA_Tree_CountHandles(CTA_Tree htree, CTA_String path, int *count);

/** \brief Count the number of COSTA handles specified by the given path.
 *
 *  \param htree  I  handle of the tree object
 *  \param path   I  path of the item, separated by / or \
 *  \param count  O  receives the number of items found
 *  \return CTA_OK if successful or CTA_ITEM_NOT_FOUND in case of not found
 */
CTAEXPORT int CTA_Tree_CountHandlesStr(CTA_Tree htree, char *path, int *count);


/** \brief Get a COSTA handle from the COSTA tree (by path)
 *
 *  \note In case of trees with default values, returns the default value.
 *  \note The returned handle must not be freed.
 *
 *  \param htree  I  handle of the tree object
 *  \param path   I  path of the item, separated by / or \\
 *  \param hitem  O  receives the handle of the COSTA item, or CTA_NULL in case not found, do not free this handle.
 *  \return CTA_OK if successful or CTA_ITEM_NOT_FOUND in case of not found
 */
CTAEXPORT int CTA_Tree_GetHandle(CTA_Tree htree, CTA_String path, CTA_Handle *hitem);


/** \brief Get the value of a COSTA handle from the COSTA tree (by path)
 *
 *  \note In case of trees with default values, returns the default value.
 *
 *  \param htree    I  handle of the tree object
 *  \param path     I  COSTA string describing path of the item, separated by / or \
 *  \param value    O  receives the value of the COSTA item, or CTA_NULL in case of not found
 *  \param datatype I  data type of parameter value, must be the same as item in tree
 *  \return CTA_OK if successful or CTA_ITEM_NOT_FOUND in case of not found
 */
CTAEXPORT int CTA_Tree_GetValue(CTA_Tree htree, CTA_String path, void *value, CTA_Datatype datatype);

/** \brief Get a COSTA handle from the COSTA tree (by path)
 *
 *  \note In case of trees with default values, returns the default value.
 *  \note The returned handle must not be freed.
 *
 *  \param htree  I  handle of the tree object
 *  \param str    I  C string describing path of the item, separated by / or \
 *  \param hitem  O  receives the handle of the COSTA item, or CTA_NULL in case of not found, do not free this handle
 *  \return CTA_OK if successful or CTA_ITEM_NOT_FOUND in case of not found
 */
CTAEXPORT int CTA_Tree_GetHandleStr(CTA_Tree htree, char* str, CTA_Handle *hitem);


/** \brief Get the value of a COSTA handle from the COSTA tree (by path)
 *
 *  \note In case of trees with default values, returns the default value.
 *
 *  \param htree    I  handle of the tree instance
 *  \param str      I  C string describing path of the item, separated by / or \
 *  \param value    O  receives the value of the COSTA item, or CTA_NULL in case of not found
 *  \param datatype I  data type of the value specified
 *  \return CTA_OK if successful or CTA_ITEM_NOT_FOUND in case of not found
 */
CTAEXPORT int CTA_Tree_GetValueStr(CTA_Tree htree, char* str, void *value, CTA_Datatype datatype);


/** \brief Count the number of elements on the current level of the COSTA tree
 *
 *  \param htree  I  handle of the tree level
 *  \param count  O  receives the number of elements on the current tree level
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_Tree_CountItems(CTA_Tree htree, int *count);


/** \brief Get a handle (by index) on the current level of the COSTA tree
 *
 *  \param htree  I  handle of the tree level
 *  \param index  I  index of the item to return, 1 <= index <= CTA_Tree_CountItems()
 *  \param hitem  O  receives handle of the item at given index
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_Tree_GetItem(CTA_Tree htree, int index, CTA_Handle *hitem);


/** \brief Get the value of a COSTA handle from the COSTA tree (by index)
 *
 *  \note In case of trees with default values, returns the default value.
 *
 *  \param htree    I  handle of the tree instance
 *  \param index    I  index of the item
 *  \param value    O  receives value of the COSTA item, or CTA_NULL in case of not found
 *  \param datatype I  data type of the value specified
 *  \return CTA_OK if successful or CTA_ITEM_NOT_FOUND in case not found
 */
CTAEXPORT int CTA_Tree_GetItemValue(CTA_Tree htree, int index, void *value, CTA_Datatype datatype);


/** \brief Print a COSTA tree to STDOUT
 *
 *  \note
 *
 *  \param htree  I  handle of the tree
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_Tree_Print(CTA_Tree htree);

/*=====================================================================*/
#ifdef __cplusplus
}
#endif
#endif
