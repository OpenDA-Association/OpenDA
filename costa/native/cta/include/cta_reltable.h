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
\file  cta_reltable.h
\brief Relation table component that defines a relation between two
       elements (ordered) sets of elements. 

  Relation tables are used to define a relation between elements in sets.
  Examples are the elements of a vector, matrix, tree vector or in a
  stochastic observer.

  The Relation table can be used for copying elements from one set to the
  other optionally using interpolation (not yet supported).

*/

#ifndef CTA_RELTABLE_H
#define CTA_RELTABLE_H
#include "cta_system.h"
#include "cta_handles.h"
#include "cta_datatypes.h"
#include "cta_vector.h"

/* Function Handle */
typedef CTA_Handle CTA_RelTable;
#ifdef __cplusplus
extern "C" {
#endif

/* functions */

/** \brief Create a relation table
 *
 * \param hreltable  O  created relation table
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_RelTable_Create(CTA_RelTable *hreltable);

/** \brief Free a relation table object.
 *
 * \param hreltable  IO relation table to be freed, 
 *                      value is set to CTA_NULL on return.
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_RelTable_Free(CTA_RelTable *hreltable);

/** \brief Copy elements according to relation table
 *
 * \note we currently only support copying of elements 
 *       between two vector instances. Other types of
 *       COSTA object will be supported when needed
 *       in later versions
 *
 * \param hreltable  I handle of relation table
 * \param hfrom      I Origin object to copy data from
 * \param hto        I Target object to copy data to
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_RelTable_Apply(CTA_RelTable hreltable, 
                       CTA_Handle hfrom, CTA_Handle hto);

/** \brief Copy elements according to inverse of relation table
 *
 * \note we currently only support copying of elements 
 *       between two vector instances. Other types of
 *       COSTA object will be supported when needed
 *       in later versions
 *
 * \param hreltable  I handle of relation table
 * \param hfrom      I Origin object to copy data from
 * \param hto        I Target object to copy data to
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_RelTable_ApplyInv(CTA_RelTable hreltable, 
                       CTA_Handle hfrom, CTA_Handle hto);


/** \brief Set a relation table
 *  A Set a relation table that defines a selection of elements
 *
 * \param hreltable  O relation table that is set
 * \param vselect    I (integer) vector with indices of elements
                       from the target set that are selected.
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_RelTable_SetSelect(CTA_RelTable hreltable, CTA_Vector vselect);

/** \brief Get the number of elements that are copied when the table is applied
 *
 *
 * \param hreltable  I  relation table
 * \param nelt     O  number of elements that are copied 
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_RelTable_Count(CTA_RelTable hreltable, int *nelt);


/** \brief Set a relation table that is combination of two 
 *         relation tables.
 *
 * Set a relation table that is the combination of two exisiting relation
 * tables. It is possible to use the inverse of the relation tables when
 * needed
 *.
 * A usefull application of this method is to create a relation table that
 * defines a relation between a subset of elements from set1 and a subset of
 * the elements of set2. In order to set a relation table of this kind first
 * create two relation tables:
 * hrel1 elements from set 1 that have a relation with the elements from set 2,
 * hrel2 elements from set 2 that have a relation with the elements from set 1
*
 * The combined relation table of hrel1 and inverse(hrel2) is a relation
 * table that spcifies the relation of a subset of elements from set1 and a
 * subset of elements from set2.
 *
 * \param hreltable  O relation table that is set
 * \param hrel1      I first relation table 
 * \param inverse1   I use inverse of hrel1 (CTA_TRUE/CTA_FALSE)
 * \param hrel2      I first relation table 
 * \param inverse2   I use inverse of hrel2 (CTA_TRUE/CTA_FALSE)
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_RelTable_SetTableCombine(CTA_RelTable hreltable, 
                                 CTA_RelTable hrel1, int inverse1, 
                                 CTA_RelTable hrel2, int inverse2 );




/** \brief Apply relation table to two components
 *
 * \param reltable (I)  Data of relation table
 * \param hrom     (I)  Source set of elements
 * \param hto      (IO) Target set (some will be overwitten)
 * \param iverse   (I)  CTA_TRUE/CTA_FALSE apply inverse table
 *
 * \return error status: CTA_OK if successful
 * \note Internal routine not a user routine
 */
int CTAI_RelTable_Apply(CTA_RelTable hreltable,
                       CTA_Handle hfrom, CTA_Handle hto, int inverse);



#ifdef __cplusplus
}
#endif
#endif
