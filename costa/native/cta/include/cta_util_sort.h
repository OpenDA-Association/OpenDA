/*
COSTA: Problem solving environment for data assimilation
Copyright (C) 2007  Nils van Velzen

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
\file  cta_util_sort.h
\brief Interface to the sorting utility functions of COSTA. 
       This header file handles the C/FORTRAN interfacing 
*/

#ifndef CTA_UTILSORT_H
#define CTA_UTILSORT_H
#include "cta_system.h"

#ifdef __cplusplus
extern "C" { 
#endif

/** \brief Sort an integer array using the Quicksort algorithm.
 *  An additional interger array is permutated in way as the unsorted array
 *
 *
 *  \param list       (input/output) array, dimension (N)
 *          On entry, the array to be sorted.
 *          On exit, list has been sorted into increasing order
 *
 * \param indx (input/output) array, dimension (N)

 *  \param n       (input) INTEGER
 *          The length of the array D.
 *
 */
CTAEXPORT void CTA_Util_IQSort2(int *list, int *indx, int n);

#ifdef __cplusplus
}
#endif
#endif
