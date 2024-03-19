/*
COSTA: Problem solving environment for data assimilation
Copyright (C) 2011  Nils van Velzen

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
\file  cta_mem.h
\brief Memory management routines. These routines are equivalent to standard C
       routines but allows us to do additional checking and debugging
*/

#ifndef CTA_MEM_H
#define CTA_MEM_H

#include <stdlib.h> 
#include "cta_system.h"

#ifdef __cplusplus
extern "C" {
#endif

/** \brief Free memory that has been allocated with CTA_Malloc or CTA_Realloc
 *
 * \param ptr     I  pointer to memory block to deallocate
 */
CTAEXPORT void CTA_Free(void *ptr);

/** \brief Allocates size bytes of memory and returns a pointer
 *         to the allocated memory.
 *
 * \param size  I  number of bytes to allocate
 *
 * \return pointer to allocated memory. NULL is returned when allocation fails
 */
CTANOF90 CTAEXPORT void* CTA_Malloc(size_t size);

/** \brief Changes the size of the allocated memory block pointed by ptr
 *         Values in ptr are copied to returned memoy block
 * \param size  I  pointer to memoryblock of which the size is changed
 * \param size  I  number of bytes of returned memory block
 *
 * \return pointer to allocated memory. NULL is returned when allocation fails
 */
CTANOF90 CTAEXPORT void* CTA_Realloc(void *ptr, size_t size);

#ifdef __cplusplus
}
#endif
#endif

