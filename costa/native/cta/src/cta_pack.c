/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_pack.c $
$Revision: 2932 $, $Date: 2011-11-30 17:10:24 +0100 (Wed, 30 Nov 2011) $

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

#include <math.h>
#include <stdio.h>
#include "cta_mem.h"
#include "f_cta_utils.h"
#include "ctai.h"
#include "cta_pack.h"
#include "cta_errors.h"
#include "cta_message.h"

#define CTA_PACK_CREATE_F77      F77_CALL(cta_pack_create,CTA_PACK_CREATE)
#define CTA_PACK_FREE_F77        F77_CALL(cta_pack_free,CTA_PACK_FREE)
#define CTA_PACK_ADD_F77         F77_CALL(cta_pack_add,CTA_PACK_ADD)
#define CTA_PACK_GET_F77         F77_CALL(cta_pack_get,CTA_PACK_GET)
#define CTA_PACK_GETPTR_F77      F77_CALL(cta_pack_getptr,CTA_PACK_GETPTR)
#define CTA_PACK_GETLEN_F77      F77_CALL(cta_pack_getlen,CTA_PACK_GETLEN)
#define CTA_PACK_ADDCNT_F77      F77_CALL(cta_pack_addcnt,CTA_PACK_ADDCNT)
#define CTA_PACK_GETINDX_F77     F77_CALL(cta_pack_getindx,CTA_PACK_GETINDX)
#define CTA_PACK_SETINDX_F77     F77_CALL(cta_pack_setindx,CTA_PACK_SETINDX)

#define CLASSNAME "CTA_Pack"

/* Struct holding all data associated to a COSTA package */

typedef struct {
char *pack_array;
int lenpack;
int ip1;
int ip2;
} CTAI_Pack;


#undef METHOD
#define METHOD "Create"
int CTA_Pack_Create(int initsize, CTA_Pack *hpack){

   CTAI_Pack *pack;
   int retval;

   /* allocate memory for new pack object */
   pack=CTA_Malloc(sizeof(CTAI_Pack));
   pack->pack_array=NULL;
   pack->lenpack=0;
   pack->ip1=0;
   pack->ip2=0;

   /* Allocate new handle and return eror when unsuccesfull */
   retval=CTA_Handle_Create("pack",CTA_PACK,pack,hpack);
   if (retval) {
       CTA_WRITE_ERROR("Cannot create handle");
       return retval;
   }

   if (initsize>0) {
      pack->pack_array=CTA_Malloc(initsize);
      pack->lenpack=initsize;
   }

   return CTA_OK;
}

#undef METHOD
#define METHOD "Free"
int CTA_Pack_Free(CTA_Pack *hpack){

   CTAI_Pack *pack;
   int retval;

   if (*hpack==CTA_NULL) return CTA_OK;

   retval=CTA_Handle_Check((CTA_Handle) *hpack,CTA_PACK);
   if (retval!=CTA_OK)  {
       CTA_WRITE_ERROR("Handle is not a cta_pack handle");
       return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) *hpack,(void**) &pack);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   if (pack->pack_array) {free(pack->pack_array);}
   free(pack);
   retval=CTA_Handle_Free(hpack);
   return retval;

}

#undef METHOD
#define METHOD "Add"
int CTA_Pack_Add(CTA_Pack hpack, void *data, int lendat){

   CTAI_Pack *pack;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) hpack,CTA_PACK);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_pack handle");
       return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) hpack,(void**) &pack);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

//   printf("pack->lenpack=%d\n",pack->lenpack);
//   printf("lendat=%d",lendat);
//   printf("pack->ip2=%d");
   // Check whether buffer is long enough and reallocate when necessary
   if (pack->lenpack<lendat+pack->ip2) {
      pack->lenpack = (int)(pack->lenpack*1.5) + lendat;
      // printf("CTA_Pack_Add: reallocate new size %d\n", pack->lenpack);
      pack->pack_array=realloc(pack->pack_array, pack->lenpack);
   }

   // pack new data
   memcpy(pack->pack_array+pack->ip2, data, lendat);
   pack->ip2=pack->ip2+lendat;
   return CTA_OK;
}

#undef METHOD
#define METHOD "Get"
int CTA_Pack_Get(CTA_Pack hpack, void *data, int lendat){

   CTAI_Pack *pack;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) hpack,CTA_PACK);
   if (retval!=CTA_OK)  {
       CTA_WRITE_ERROR("Handle is not a cta_pack handle");
       return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) hpack,(void**) &pack);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   // Check whether requested data can be in buffer
   if (pack->ip1+lendat>pack->lenpack) {
      return CTA_BUFFER_TOO_SMALL;
   }

   // pack new data
   memcpy(data, pack->pack_array+pack->ip1, lendat);
   pack->ip1=pack->ip1+lendat;
   return CTA_OK;
}



#undef METHOD
#define METHOD "GetPtr"
char* CTA_Pack_GetPtr(CTA_Pack hpack){

   CTAI_Pack *pack;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) hpack,CTA_PACK);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_pack handle");
	   return NULL;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hpack,(void**) &pack);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return NULL;
   }

   return &(pack->pack_array[pack->ip1]);
}

#undef METHOD
#define METHOD "GetLen"
int CTA_Pack_GetLen(CTA_Pack hpack){

   CTAI_Pack *pack;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) hpack,CTA_PACK);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_pack handle");
	   return -1;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hpack,(void**) &pack);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return -1;
   }

   return pack->ip2-pack->ip1;
}

#undef METHOD
#define METHOD "AddCnt"
int CTA_Pack_AddCnt(CTA_Pack hpack, int lendat){
   CTAI_Pack *pack;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) hpack,CTA_PACK);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_pack handle");
	   return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) hpack,(void**) &pack);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   if (pack->ip2+lendat>pack->lenpack) {
      return CTA_BUFFER_TOO_SMALL;
   }

   pack->ip2=pack->ip2+lendat;

   return CTA_OK;
}


#undef METHOD
#define METHOD "Debug"
int CTA_Pack_Debug(CTA_Pack hpack){

   CTAI_Pack *pack;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) hpack,CTA_PACK);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_pack handle");
	   return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) hpack,(void**) &pack);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   // Check whether requested data can be in buffer
   printf("DEBUG OF PACK OBJECT %d\n",hpack );
   printf("geheugen adress %p\n",&pack);
   printf("ip1=%d lenpack=%d\n",pack->ip1, pack->lenpack);
   printf("END OF PACK OBJECT %d\n",hpack );

   return CTA_NULL;
}

#undef METHOD
#define METHOD "GEtIndx"
int CTA_Pack_GetIndx(CTA_Pack hpack, int *ip1, int *ip2){

   CTAI_Pack *pack;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) hpack,CTA_PACK);
   if (retval!=CTA_OK) {
	  CTA_WRITE_ERROR("Handle is not a cta_pack handle");
	  return retval;
   }
   
   retval=CTA_Handle_GetData((CTA_Handle) hpack,(void**) &pack);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Cannot retrieve handle data");
	  return retval;
   }

   *ip1=pack->ip1;
   *ip2=pack->ip2;

   return CTA_OK;
}


#undef METHOD
#define METHOD "SetIndex"
int CTA_Pack_SetIndx(CTA_Pack hpack, int ip1, int ip2){

   CTAI_Pack *pack;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) hpack,CTA_PACK);
   if (retval!=CTA_OK) {
	  CTA_WRITE_ERROR("Handle is not a cta_pack handle");
	  return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) hpack,(void**) &pack);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Cannot retrieve handle data");
	  return retval;
   }

//   printf("ip1, ip2 =%d %d\n",pack->ip1, pack->ip2);
   if (ip1==CTA_PACK_RESET) {
      pack->ip1=0;
   }
   else if (ip2==CTA_PACK_RESET) {
      pack->ip1=0;
      pack->ip2=0;
   }
   else {
      pack->ip2=ip2;
      pack->ip1=ip1;
   }

   return CTA_OK;
}


CTAEXPORT void CTA_PACK_CREATE_F77(int *initsize, int *hpack, int *ierr){
   *ierr=CTA_Pack_Create(*initsize, (CTA_Pack*) hpack);
}


CTAEXPORT void CTA_PACK_FREE_F77(int *hpack, int *ierr){
   *ierr=CTA_Pack_Free((CTA_Pack*) hpack);
}

CTAEXPORT void CTA_PACK_ADD_F77(int *hpack, void *data, int *lendat, int *ierr){
   *ierr=CTA_Pack_Add((CTA_Pack) *hpack, data, *lendat);
}

CTAEXPORT void CTA_PACK_GET_F77(int *hpack, void *data, int *lendat, int *ierr){
   *ierr=CTA_Pack_Get((CTA_Pack) *hpack, data, *lendat);
}


CTAEXPORT char* CTA_PACK_GETPTR_F77(int *hpack){
   return CTA_Pack_GetPtr((CTA_Pack) *hpack);
}

CTAEXPORT void CTA_PACK_GETLEN_F77(int *hpack, int *ierr){
   *ierr=CTA_Pack_GetLen((CTA_Pack) *hpack);
}

CTAEXPORT void CTA_PACK_ADDCNT_F77(int *hpack, int *lendat, int *ierr){
   *ierr=CTA_Pack_AddCnt((CTA_Pack) *hpack, *lendat);
}

CTAEXPORT void CTA_PACK_GETINDX_F77(int *hpack, int *ip1, int *ip2, int *ierr){
   *ierr=CTA_Pack_GetIndx((CTA_Pack) *hpack, ip1, ip2);
}

CTAEXPORT void CTA_PACK_SETINDX_F77(int *hpack, int *ip1, int *ip2, int *ierr){
   *ierr=CTA_Pack_SetIndx((CTA_Pack) *hpack, *ip1, *ip2);
}







