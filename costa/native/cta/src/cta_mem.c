/*
$URL: https://repos.deltares.nl/repos/openda/openda_1/public/trunk/core/native/src/cta/cta_handles.c $
$Revision: 2738 $, $Date: 2011-09-05 10:48:32 +0200 (Mon, 05 Sep 2011) $

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

#define DEBUG  0
#include <stdlib.h>
#include <stdio.h>
#include "cta_mem.h"



void CTA_Free(void *ptr){
   free(ptr);
}

void * CTA_Malloc(size_t size){
   void *ptr = malloc(size);
#ifdef DEBUG
   if (! ptr){
      printf("CTA_Malloc: Warning: malloc returns NULL. (size=%zd)\n",size);
   }
   else {
//      memset(ptr,'@', size);  
   }
#endif
  return ptr;

}

void * CTA_Realloc(void *ptr, size_t size){
   void *ptrOut = realloc(ptr, size);
#ifdef DEBUG
   if (! ptrOut){
      printf("CTA_Malloc: Warning: malloc returns NULL. (size=%zd)\n",size);
   }
#endif
  return ptrOut;
}



