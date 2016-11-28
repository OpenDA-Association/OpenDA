/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_treevector.c $
$Revision: 3792 $, $Date: 2013-01-31 12:34:05 +0100 (Thu, 31 Jan 2013) $

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

#define IDEBUG (0)

#define ELMDIV  (1)
#define ELMPROD (2)

#include <stdio.h>
#include <string.h>
#if HAVE_LIBNETCDF
#include <netcdf.h>
#endif


#include <math.h>
#include "cta_mem.h"
#include "f_cta_utils.h"
#include "cta_errors.h"
#include "cta_defaults.h"
#include "cta_treevector.h"
#include "ctai_vector.h"
#include "ctai_datatypes.h"
#include "cta_handles.h"
#include "ctai_handles.h"
#include "cta_file.h"
#include "cta_metainfo.h"
#include "ctai_xml.h"
#include "cta_pack.h"
#include "cta_message.h"
#include "cta_metainfo.h"

#define CTA_TREEVECTOR_CREATE_F77              F77_CALL(cta_treevector_create,CTA_TREEVECTOR_CREATE)
#define CTA_TREEVECTOR_DUPLICATE_F77           F77_CALL(cta_treevector_duplicate,CTA_TREEVECTOR_DUPLICATE)
#define CTA_TREEVECTOR_CONC_F77                F77_CALL(cta_treevector_conc,CTA_TREEVECTOR_CONC)
#define CTA_TREEVECTOR_GETSUBTREEVEC_F77       F77_CALL(cta_treevector_getsubtreevec,CTA_TREEVECTOR_GETSUBTREEVEC)
#define CTA_TREEVECTOR_SETSUBTREENOCOMPUTE_F77 F77_CALL(cta_treevector_setsubtreenocompute,CTA_TREEVECTOR_SETSUBTREENOCOMPUTE)
#define CTA_TREEVECTOR_GETSUBTREEVECINDEX_F77  F77_CALL(cta_treevector_getsubtreevecindex,CTA_TREEVECTOR_GETSUBTREEVECINDEX)
#define CTA_TREEVECTOR_GETTAG_F77              F77_CALL(cta_treevector_gettag,CTA_TREEVECTOR_GETTAG)
#define CTA_TREEVECTOR_SETVEC_F77              F77_CALL(cta_treevector_setvec,CTA_TREEVECTOR_SETVEC)
#define CTA_TREEVECTOR_GETVEC_F77              F77_CALL(cta_treevector_getvec,CTA_TREEVECTOR_GETVEC)
#define CTA_TREEVECTOR_SETVAL_F77              F77_CALL(cta_treevector_setval,CTA_TREEVECTOR_SETVAL)
#define CTA_TREEVECTOR_GETVAL_F77              F77_CALL(cta_treevector_getval,CTA_TREEVECTOR_GETVAL)
#define CTA_TREEVECTOR_SETVALS_F77             F77_CALL(cta_treevector_setvals,CTA_TREEVECTOR_SETVALS)
#define CTA_TREEVECTOR_GETVALS_F77             F77_CALL(cta_treevector_getvals,CTA_TREEVECTOR_GETVALS)
#define CTA_TREEVECTOR_GETSIZE_F77             F77_CALL(cta_treevector_getsize,CTA_TREEVECTOR_GETSIZE)
#define CTA_TREEVECTOR_GETMETAINFO_F77         F77_CALL(cta_treevector_getmetainfo,CTA_TREEVECTOR_GETMETAINFO)
#define CTA_TREEVECTOR_SETMETAINFO_F77         F77_CALL(cta_treevector_setmetainfo,CTA_TREEVECTOR_SETMETAINFO)
#define CTA_TREEVECTOR_COPY_F77                F77_CALL(cta_treevector_copy,CTA_TREEVECTOR_COPY)
#define CTA_TREEVECTOR_AXPY_F77                F77_CALL(cta_treevector_axpy,CTA_TREEVECTOR_AXPY)
#define CTA_TREEVECTOR_DOT_F77                 F77_CALL(cta_treevector_dot,CTA_TREEVECTOR_DOT)
#define CTA_TREEVECTOR_NRM2_F77                F77_CALL(cta_treevector_nrm2,CTA_TREEVECTOR_NRM2)
#define CTA_TREEVECTOR_SETCONSTANT_F77         F77_CALL(cta_treevector_setconstant,CTA_TREEVECTOR_SETCONSTANT)
#define CTA_TREEVECTOR_SCAL_F77                F77_CALL(cta_treevector_scal,CTA_TREEVECTOR_SCAL)
#define CTA_TREEVECTOR_EXPORT_F77              F77_CALL(cta_treevector_export,CTA_TREEVECTOR_EXPORT)
#define CTA_TREEVECTOR_IMPORT_F77              F77_CALL(cta_treevector_import,CTA_TREEVECTOR_IMPORT)
#define CTA_TREEVECTOR_FREE_F77                F77_CALL(cta_treevector_free,CTA_TREEVECTOR_FREE)
#define CTA_TREEVECTOR_INFO_F77                F77_CALL(cta_treevector_info,CTA_TREEVECTOR_INFO)
#define CTA_TREEVECTOR_LIST_F77                F77_CALL(cta_treevector_list,CTA_TREEVECTOR_LIST)
#define CTA_TREEVECTOR_GEMM_F77                F77_CALL(cta_treevector_gemm,CTA_TREEVECTOR_GEMM)
#define CTA_TREEVECTOR_OPONLEAFS_F77           F77_CALL(cta_treevector_oponleafs,CTA_TREEVECTOR_OPONLEAFS)
#define CTA_TREEVECTOR_ELMSQRT_F77             F77_CALL(cta_treevector_elmsqrt,CTA_TREEVECTOR_ELMSQRT)
#define CTA_TREEVECTOR_ELMPROD_F77             F77_CALL(cta_treevector_elmprod,CTA_TREEVECTOR_ELMPROD)
#define CTA_TREEVECTOR_GETSUBTREEVECINDEX_F77  F77_CALL(cta_treevector_getsubtreevecindex,CTA_TREEVECTOR_GETSUBTREEVECINDEX)
#define CTA_TREEVECTOR_GETNUMSUBTREE_F77       F77_CALL(cta_treevector_getnumsubtree,CTA_TREEVECTOR_GETNUMSUBTREE)
#define CTA_TREEVECTOR_GETSUBTREEVECID_F77    F77_CALL(cta_treevector_getsubtreevecid,CTA_TREEVECTOR_GETSUBTREEVECID)


#define CLASSNAME "CTA_TreeVector"

/* --------------------------------------------------------------------- */

/* Struct holding all data associated to an COSTA Tree-vector */


typedef struct {
char tag[CTA_STRLEN_TAG];
char name[CTA_STRLEN_NAME];
int            size;
CTA_Vector     *v;
int            nsubtrees;
CTA_Metainfo   *metainfo;
CTA_TreeVector *treevecs;
BOOL           *nocompute;
} CTAI_TreeVec;


/* Local headers */

int CTAI_TreeVector_ExcludeFromVector(CTA_TreeVector treevec, BOOL *excludefv);



#undef METHOD
#define METHOD "Create"
int CTA_TreeVector_Create(const char *name, const char *tag, CTA_TreeVector *treevec){
   int retval;         //Return value of a call
   CTAI_TreeVec *data;   //Treevector specific data

   // Allocate data and set properties
   data=CTA_Malloc(sizeof(CTAI_TreeVec));

   // Set properties
   strcpy(data->name,name);
   strcpy(data->tag,tag);
   data->v=NULL;
   data->nsubtrees=0;
   data->treevecs=NULL;
   data->nocompute=NULL;

   // Allocate new handle and return error when unsuccesfull
   retval=CTA_Handle_Create("tree-vector",CTA_TREEVECTOR,data,treevec);
   if (retval) {
       CTA_WRITE_ERROR("Cannot create tree-vector");
       return retval;
   }

   data->metainfo=NULL;

   return CTA_OK;
};

#undef METHOD
#define METHOD "Duplicate"
int CTA_TreeVector_Duplicate(CTA_TreeVector treevec1, CTA_TreeVector *treevec2 ){
   int retval;         //Return value of a call
   CTAI_TreeVec *data1, *data2;   //Tree-vector specific data
   int istate; // counter over all substates
   const char *name;

   if (IDEBUG>10){printf("CTA_TreeVector_Duplicate start\n");}
   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec1,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) treevec1,(void*) &data1);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   name = CTAI_Handle_GetName((CTA_Handle) treevec1);
   if (IDEBUG>10){printf("CTA_TreeVector_Duplicate name of state '%s'\n",name);}
   if (IDEBUG>10){printf("CTA_TreeVector_Duplicate adress data1=%p\n",data1);}

   /* create new state vector and get data object */
   retval=CTA_TreeVector_Create(name, data1->tag, treevec2);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) *treevec2,(void*) &data2);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }


   /* check whether this is a leaf */
   if (data1->nsubtrees==0){

      if (IDEBUG>10){printf("CTA_TreeVector_Duplicate duplicate vector \n");}
      /* Duplicate and copy vector */

      if (data1->v) {
         data2->v=CTA_Malloc(sizeof(CTA_Vector));
         retval=CTA_Vector_Duplicate(*data1->v, data2->v);
         if (IDEBUG>10){printf("CTA_TreeVector_Duplicate retval Vector ok %d\n",retval);}

         if (retval!=CTA_OK) return retval;
      }
      if (data1->metainfo) {
        // make data2->metainfo.
        // *data2->metainfo has been made (and set at NULL) in cta_treevector_create.
        data2->metainfo=CTA_Malloc(sizeof(CTA_Metainfo));
        retval=CTA_Metainfo_Create(data2->metainfo);
        retval = CTA_Metainfo_Copy(*data1->metainfo,*data2->metainfo);
      }
   }

   else {
      if (IDEBUG>10){printf("CTA_TreeVector_Duplicate duplicate substates \n");}
      /* duplicate sub-states */
      data2->treevecs=CTA_Malloc(sizeof(CTA_TreeVector)*data1->nsubtrees);
      data2->nocompute=CTA_Malloc(sizeof(BOOL)*data1->nsubtrees);
      for (istate=0;istate<data1->nsubtrees;istate++){
         retval=CTA_TreeVector_Duplicate(data1->treevecs[istate],
                                    &data2->treevecs[istate] );
         if (retval!=CTA_OK) return retval;
         data2->nocompute[istate]=data1->nocompute[istate];
      }
      data2->nsubtrees=data1->nsubtrees;


      if (data1->metainfo) {
         data2->metainfo=CTA_Malloc(sizeof(CTA_Metainfo));
         retval=CTA_Metainfo_Create(data2->metainfo);
         retval = CTA_Metainfo_Copy(*data1->metainfo,*data2->metainfo);
      }

   }
   if (IDEBUG>10){printf("CTA_TreeVector_Duplicate end of function\n");}
   return CTA_OK;
};

/* ------------------------------------------------------------*/

int CTAI_axpy_grid(CTA_TreeVector y, CTAI_Gridm gridy, double alpha, CTA_TreeVector x, CTAI_Gridm gridx){
   CTAI_TreeVec *datax, *datay;   //Tree-vector specific data
   CTA_Vector vecx_to_y;
   int retval;

   retval=CTA_Handle_GetData((CTA_Handle) x,(void*) &datax);
   retval=CTA_Handle_GetData((CTA_Handle) y,(void*) &datay);

   retval=CTA_Vector_Duplicate(*datay->v, &vecx_to_y);



  /*  interpolate values of vector x on gridx towards gridy. That means the following:
     - for each point on gridy:
            use bi/trilinear interpolation using 4/8 gridpoints of gridx which surround
            gridpoints of y  */



  if (!datax->v) return CTA_TREEVECTORS_NOT_COMPATIBLE;
  retval = CTAI_Grid_Interpolate(gridy, gridx, *datax->v, vecx_to_y );



  /* use now the standard vector axpy   */

  if (!datay->v) return CTA_TREEVECTORS_NOT_COMPATIBLE;

  retval=CTA_Vector_Axpy(*datay->v, alpha, vecx_to_y);

  if (retval!=CTA_OK) return retval;

  /* free vectors */
  retval = CTA_Vector_Free(&vecx_to_y);


  return retval;
}

/* ------------------------------------------------------------*/


#undef METHOD
#define METHOD "Conc"
int CTA_TreeVector_Conc(CTA_TreeVector treevec1, CTA_TreeVector *treevecs, int nsubtrees){

   int retval;         //Return value of a call
   CTAI_TreeVec *data;   //Tree-vector specific data
   int istate;         //counter of the states


   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec1,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) treevec1,(void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* Check handles */
   for (istate=0;istate<nsubtrees;istate++){
      retval=CTA_Handle_Check((CTA_Handle) treevecs[istate],CTA_TREEVECTOR);
      if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   }
   /* Check whether concatenation is still possible */
   if (data->nsubtrees || data->treevecs){
      return CTA_CONCAT_NOT_POSSIBLE;
   }

   /* Add handles */
   data->treevecs=CTA_Malloc(sizeof(CTA_TreeVector)*nsubtrees);
   data->nocompute=CTA_Malloc(sizeof(BOOL)*nsubtrees);
   for (istate=0;istate<nsubtrees;istate++){
      data->treevecs[istate]=treevecs[istate];
      data->nocompute[istate]=FALSE;
   }
   data->nsubtrees=nsubtrees;

   return CTA_OK;
};


#undef METHOD
#define METHOD "GetSubTreeVec"
int CTA_TreeVector_GetSubTreeVec(CTA_TreeVector treevec, const char *tag,
                          CTA_TreeVector *hsubstate){

   int retval;         //Return value of a call
   CTAI_TreeVec *data;   //Tree-vector specific data
   int istate;         //counter of the states

   *hsubstate=CTA_NULL;
   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) treevec,(void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   if (strcmp(data->tag,tag)==0 || strcmp(".",tag)==0){
      *hsubstate=treevec;
      return CTA_OK;
   }

   /* look in all substates */

   for (istate=0;istate<data->nsubtrees;istate++){

      retval=CTA_TreeVector_GetSubTreeVec(data->treevecs[istate], tag,hsubstate);
      if (retval==CTA_OK) return CTA_OK;
   };

   if (IDEBUG>10){
      char msg[1024];
      strcpy(msg,"Cannot find Treevector Item : ");
      strncat(msg,tag,1023);
      CTA_WRITE_ERROR(msg);
   }
   return CTA_ITEM_NOT_FOUND;
};


#undef METHOD
#define METHOD "SetSubTreeNocompute"
int CTA_TreeVector_SetSubTreeNocompute(CTA_TreeVector treevec, const char *tag){
   int retval;         //Return value of a call
   CTAI_TreeVec *data;   //Tree-vector specific data
   int istate;         //counter of the states
   char tagsub[CTA_STRLEN_TAG];

   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) treevec,(void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   if (data->nsubtrees == 0) return CTA_OK;

   /* look in all substates */

   for (istate=0;istate<data->nsubtrees;istate++){
     retval = CTA_TreeVector_GetTag(data->treevecs[istate],tagsub);
     if (strcmp(tagsub,tag)==0){
       data->nocompute[istate] = TRUE;
     }

     /* look in substates of substate */
     retval=CTA_TreeVector_SetSubTreeNocompute(data->treevecs[istate], tag);
	 if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Error in CTA_TreeVector_SetSubTreeNocompute");
	   return retval;
   }
   };
   return CTA_OK;
}

#undef METHOD
#define METHOD "GetSubTreeVecIndex"
int CTA_TreeVector_GetSubTreeVecIndex(CTA_TreeVector treevec, int index,
                          CTA_TreeVector *hsubstate){
   int retval;         //Return value of a call
   CTAI_TreeVec *data;   //Tree-vector specific data
   char msg[1024];

   *hsubstate=CTA_NULL;
   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) treevec,(void*) &data);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Cannot retrieve handle data");
      return retval;
   }

   /* get the substate.Note that we have to subtract 1 because of our counting convention */
   if (index > 0 && index <= data->nsubtrees) {
     *hsubstate = data->treevecs[index-1];
     return CTA_OK;
     }
   if (IDEBUG>10){
      sprintf(msg,"Cannot find sub treevector with index :%d",index);
      CTA_WRITE_ERROR(msg);
   }
   return CTA_ITEM_NOT_FOUND;
};

#undef METHOD
#define METHOD "GetSubTreeVecId"
int CTA_TreeVector_GetSubTreeVecId(CTA_TreeVector treevec, int index, char tag[CTA_STRLEN_TAG]){
   int retval;         //Return value of a call
   CTAI_TreeVec *data;   //Tree-vector specific data

   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) treevec,(void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
 /* get the substate-id. Note that we DO NOT have to subtract 1 because of Java counting convention 
    (this method is only used by JAVA users!) */
     retval = CTA_TreeVector_GetTag(data->treevecs[index],tag);
   
   return retval;
};



#undef METHOD
#define METHOD "GetNumSubTree"
int CTA_TreeVector_GetNumSubTree(CTA_TreeVector treevec, int *numSubTrees){
   int retval;         //Return value of a call
   CTAI_TreeVec *data;   //Tree-vector specific data

   *numSubTrees=0;
   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) treevec,(void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* get number of substates */
   *numSubTrees=data->nsubtrees;
   return CTA_OK;
};


#undef METHOD
#define METHOD "GetTag"
int CTA_TreeVector_GetTag(CTA_TreeVector treevec, char *tag){

   int retval;         //Return value of a call
   CTAI_TreeVec *data;   //Tree-vector specific data

   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) treevec,(void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* Note this copy is not protected */
   strcpy(tag,data->tag);

   return CTA_OK;
};


#undef METHOD
#define METHOD "GetMetainfo"
int CTA_TreeVector_GetMetainfo(CTA_TreeVector treevec, CTA_Metainfo minfo){

   int retval;         //Return value of a call
   CTAI_TreeVec *data;   //Tree-vector specific data
   int rest;


   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec,CTA_TREEVECTOR);
   //      printf("cta_treevector_getmetainfo0:  %d \n ",retval);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) treevec,(void*) &data);
   // printf("cta_treevector_getmetainfo1:  %d \n ",retval);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   if (!data->metainfo ){
      printf("cta_treevector_getmetainfo: NO METAINFO!  \n ");
      return CTA_ILLEGAL_HANDLE;
   }


   retval=CTA_Handle_Check((CTA_Handle) *data->metainfo, CTA_METAINFO);
   //printf("cta_treevector_getmetainfo2:  %d \n ",retval);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }

   retval=CTA_Handle_Check((CTA_Handle) minfo, CTA_METAINFO);
   //printf("cta_treevector_getmetainfo3:  %d \n ",retval);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }

   retval = CTA_Metainfo_Copy(*data->metainfo, minfo);
   retval = CTA_Metainfo_GetRest(minfo,&rest);

   if (retval!=CTA_OK){
       CTA_WRITE_ERROR("cta_treevector_getmetainfo: error");
       return retval;
   }

   return CTA_OK;
};


#undef METHOD
#define METHOD "SetMetainfo"
int CTA_TreeVector_SetMetainfo(CTA_TreeVector treevec, CTA_Metainfo minfo){

   int retval;         //Return value of a call
   CTAI_TreeVec *data;   //Tree-vector specific data

   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec,CTA_TREEVECTOR);
   //   printf("cta_treevector_setmetainfo0:  %d \n ",retval);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) treevec,(void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   retval=CTA_Handle_Check((CTA_Handle) minfo, CTA_METAINFO);
   //printf("cta_treevector_setmetainfo1:  %d \n ",retval);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }

   /*  er wordt verondersteld dat metainfo van state nog niet is gevuld*/
   /* dus state_setmetainfo is eenmalig per state; anders copy gebruiken! */

   if (!data->metainfo){
     data->metainfo=CTA_Malloc(sizeof(CTA_Metainfo));
   }
     retval=CTA_Metainfo_Create(data->metainfo);
     //printf("cta_treevector_setmetainfo2:  %d \n ",retval);
       if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Error in CTA_Metainfo_Create");
	   return retval;
   }

   //dit kan niet met een toekenning; er is een functie cta_metainfo_copy nodig!!!

   retval = CTA_Metainfo_Copy(minfo,*data->metainfo);
   //printf("cta_treevector_setmetainfo3:  %d \n ",retval);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Error in CTA_Metainfo_Copy");
	   return retval;
   }

   //   data->metainfo = minfo;

   return CTA_OK;
};

/* ------------------------------------------------------- */
//int CTA_TreeVector_SetConnection(CTA_TreeVector treevec1, CTA_TreeVector treevec2){
//  int retval;         //Return value of a call
//  CTA_Metainfo minfo1, minfo2;
//  char  tag1[CTA_STRLEN_TAG],  tag2[CTA_STRLEN_TAG];
//
//  /* get both metainfos; checking occurs in called routine  */
//  retval = CTA_TreeVector_GetMetainfo(treevec1, minfo1);
//  if (retval!=CTA_OK) return retval;
//  retval = CTA_TreeVector_GetMetainfo(treevec2, minfo2);
//  if (retval!=CTA_OK) return retval;
//
//  retval = CTA_Metainfo_GetTag(minfo1, tag1);
//  if (retval!=CTA_OK) return retval;
//  retval = CTA_Metainfo_GetTag(minfo2, tag2);
//  if (retval!=CTA_OK) return retval;
//
//  /* let both metainfos connect to each other */
//  retval = CTA_Metainfo_SetBelongsTo(minfo1, tag2);
//  if (retval!=CTA_OK) return retval;
//  retval = CTA_Metainfo_SetBelongsTo(minfo2, tag1);
//  if (retval!=CTA_OK) return retval;
//
// /* put the changed metainfo back */
//  retval = CTA_TreeVector_SetMetainfo(treevec1, minfo1);
//  if (retval!=CTA_OK) return retval;
//  retval = CTA_TreeVector_SetMetainfo(treevec2, minfo2);
//  if (retval!=CTA_OK) return retval;
//
//  return CTA_OK;
//}

/* ------------------------------------------------------- */

// return the total number of leaves/vectors that form the state
int CTA_TreeVector_GetVecNumHandles(CTA_TreeVector treevec){
   return CTAI_TreeVec_GetVecNumHandles(treevec);
};



// gives a list of all tags of state and substates
int CTA_TreeVector_List(CTA_TreeVector treevec, CTA_Vector taglist ){
  int indx, retval;
  indx = 0;
  retval=CTAI_TreeVec_List(treevec, taglist, &indx);
   return retval;
};


// gives a list of all tags of state and substates
int CTAI_TreeVec_List(CTA_TreeVector treevec, CTA_Vector taglist, int *indx){
  int retval,istate;
  CTA_String str1;
  CTAI_TreeVec *data;   //Tree-vector specific data


   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec,CTA_TREEVECTOR);
   if (retval!=CTA_OK) return retval;
   retval=CTA_Handle_GetData((CTA_Handle) treevec,(void*) &data);
   if (retval!=CTA_OK) return retval;

   printf("State-vector information:\n");
   printf("tag     :%s\n",data->tag);

   retval = CTA_String_Create(&str1);
   retval = CTA_String_Set(str1,data->tag);

   retval = CTA_Vector_SetVal(taglist, *indx + 1, &str1,CTA_STRING);

   *indx = *indx + 1;

   for (istate=0;istate<data->nsubtrees;istate++){
     retval=CTAI_TreeVec_List(data->treevecs[istate], taglist, indx);
     if (retval!=CTA_OK) return retval;
   }

   return CTA_OK;
};










// return the total number of leaves/vectors that form the state
// do not count 'nocompute'-substates
int CTAI_TreeVec_GetVecNumHandles(CTA_TreeVector treevec){

   CTAI_TreeVec *data;   //Tree-vector specific data
   int nvec;
   int istate;

   CTA_Handle_GetData((CTA_Handle) treevec,(void*) &data);

   // We are in a leaf
   if (data->nsubtrees==0){
      return 1;
   } else {
      nvec=0;
      for (istate=0;istate<data->nsubtrees;istate++){
        if (data->nocompute[istate] == FALSE) {
          nvec=nvec+CTAI_TreeVec_GetVecNumHandles(data->treevecs[istate]);
        }
      }
      return nvec;
   }
};

int CTAI_TreeVec_GetVecHandles(CTA_TreeVector treevec, CTA_Vector **hvecs, int *indx){

   int retval;         //Return value of a call
   CTAI_TreeVec *data;   //Tree-vector specific data
   int istate;         //Counter over substates

   retval=CTA_Handle_GetData((CTA_Handle) treevec,(void*) &data);
   if (retval) return retval;

   // We are in a leaf
   if (data->nsubtrees==0){
      hvecs[*indx]=data->v;
      (*indx)++;

   } else {
      for (istate=0;istate<data->nsubtrees;istate++){
        if (data->nocompute[istate] == FALSE) {
         CTAI_TreeVec_GetVecHandles(data->treevecs[istate], hvecs, indx);
        }
      }
   }
   return CTA_OK;
};


#undef METHOD
#define METHOD "SetVec"
int CTA_TreeVector_SetVec(CTA_TreeVector treevec, CTA_Vector hvec){

   int retval;         //Return value of a call
   CTAI_TreeVec *data;   //Tree-vector specific data
   CTA_Vector **hvecs; //array with pointers to all vectors of substates
   int nvecs;          //number of sub vectors state is build of
   int ivec;           //counter over sub vectors
   int indx;
   void *values;       //values of hvec
   int nglob;          //dimension of vector hvec
   int nsub;           //dimension of a vector of a substate
   int size_type;      //C- sizeof of data items in hvec
   int ioff;           //offset in input vector to start of subvector
   CTA_Datatype datatype; //Data Type of hvec
   char *cvalues;

   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) treevec,(void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* we are in a leaf -> copy vector */
   if (data->nsubtrees==0) {
      if (data->v){
        /* copy vector */
        retval=CTA_Vector_Copy(hvec,*data->v);
        if (retval!=CTA_OK) {
	      CTA_WRITE_ERROR("Cannot copy vector");
	      return retval;
      }
      }else{
        /* duplicate vector */
        data->v=CTA_Malloc(sizeof(CTA_Vector));
        retval=CTA_Vector_Duplicate(hvec,data->v);
        if (retval!=CTA_OK) {
	      CTA_WRITE_ERROR("Cannot duplicate vector");
	      return retval;
      }
      }
   }else{
      /* we are not in a leaf, try to break vector up */
      /* This is only allowed if all vectors have already been filled-in */
      /* Note: the 'excludefromvector'-leafs are excluded */
      nvecs=CTAI_TreeVec_GetVecNumHandles(treevec);
      hvecs=CTA_Malloc(sizeof(CTA_Vector*)*nvecs);
      indx=0;
      retval=CTAI_TreeVec_GetVecHandles(treevec, hvecs, &indx);
      if (retval) {
	      CTA_WRITE_ERROR("Cannot get treevector handles");
	      return retval;
      }
      /* no NULL vectors are allowed */
      for (ivec=0;ivec<nvecs;ivec++){
         if (!hvecs[ivec]) return CTA_UNINITIALISED_SUBTREEVECTOR;
      }

      /* Copy values over all substates :*/

      /* First get values from hvec */
      retval=CTA_Vector_GetDatatype(hvec,&datatype);
      retval=CTA_Vector_GetSize(hvec,&nglob);
      if (retval) {
	      CTA_WRITE_ERROR("Cannot get size");
	      return retval;
      }
      retval=CTA_SizeOf(datatype,&size_type);
      if (retval) {
	      CTA_WRITE_ERROR("Cannot get sizeOf");
	      return retval;
      }
      values=CTA_Malloc(size_type*nglob);
      retval=CTA_Vector_GetVals(hvec,values,nglob,datatype);
      if (retval) {
	      CTA_WRITE_ERROR("Cannot get values");
	      return retval;
      }

      /* copy values into sub-vector */
      ioff=0;
      for (ivec=0;ivec<nvecs;ivec++){
         char *ptr;

         retval=CTA_Vector_GetSize(*hvecs[ivec],&nsub);
         if (retval) {
	        CTA_WRITE_ERROR("Cannot get size of vector");
	        return retval;
         }
         if (ioff+nsub>nglob) return CTA_INCOMPATIBLE_VECTORS;
         if (retval) return retval;
         cvalues=values;

         ptr=cvalues;
         retval=CTA_Vector_SetVals(*hvecs[ivec],ptr+ioff*size_type,nsub,
                                   datatype);
         if (retval) {
	        CTA_WRITE_ERROR("Cannot set values of vector");
	        return retval;
         }
         ioff=ioff+nsub;

      }
      if (ioff!=nglob) return CTA_INCOMPATIBLE_VECTORS;

      /* free work variables */
      free(values);
      free(hvecs);
   }
   return CTA_OK;
};

void zet999(double *vals, int n){

int i;

   for (i=0;i<n;i++){
      printf ("(%d) %lg\n",i,vals[i]);
   }

}

#undef METHOD
#define METHOD "GetVec"
int CTA_TreeVector_GetVec(CTA_TreeVector treevec, CTA_Vector hvec){

   int retval;         //Return value of a call
   CTAI_TreeVec *data;   //Tree-vector specific data
   CTA_Vector **hvecs; //array with pointers to all vectors of substates
   int nvecs;          //number of sub vectors state is build of
   int ivec;           //counter over sub vectors
   int indx;
   void *values;       //values of hvec
   int nglob;          //dimension of vector hveec
   int nsub;           //dimension of a vector of a substate
   int size_type;      //C- sizeof of data items in hvec
   int ioff;           //offset in input vector to start of subvector
   CTA_Datatype datatype; //Data Type of hvec

   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) treevec,(void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* we are in a leaf -> copy vector */
   if (data->nsubtrees==0) {
      if (data->v){
        /* copy vector */
        retval=CTA_Vector_Copy(*data->v,hvec);
        if (retval!=CTA_OK) {
	       CTA_WRITE_ERROR("Cannot copy vector");
	       return retval;
        }
      }
   }else{
      /* we are not in a leave, try to break vector up */
      /* This is only allowed if all vectors have already been filled-in */
      nvecs=CTAI_TreeVec_GetVecNumHandles(treevec);
      hvecs=CTA_Malloc(sizeof(CTA_Vector*)*nvecs);
      indx=0;
      retval=CTAI_TreeVec_GetVecHandles(treevec, hvecs, &indx);
      /* no NULL vecors are allowed */
      for (ivec=0;ivec<nvecs;ivec++){
         if (hvecs[ivec]==CTA_NULL) return CTA_UNINITIALISED_SUBTREEVECTOR;
      }

      /* Copy values over all substates :*/

      /* create array for holding values from hvec */
      retval=CTA_Vector_GetDatatype(hvec,&datatype);
      retval=CTA_Vector_GetSize(hvec,&nglob);
      if (retval) {
	     CTA_WRITE_ERROR("Cannot get size of vector");
	     return retval;
      }
      retval=CTA_SizeOf(datatype,&size_type);
      if (retval) {
	     CTA_WRITE_ERROR("Error in CTA_SizeOf");
	     return retval;
      }
      values=CTA_Malloc(size_type*nglob);

      /* copy values into sub-vector */
      ioff=0;
      for (ivec=0;ivec<nvecs;ivec++){
         char *ptr;
         retval=CTA_Vector_GetSize(*hvecs[ivec],&nsub);
         if (retval) {
	        CTA_WRITE_ERROR("Cannot get size of vector");
	        return retval;
         }
         if (ioff+nsub>nglob) return CTA_INCOMPATIBLE_VECTORS;
         if (retval) return retval;

         ptr=values;

         retval=CTA_Vector_GetVals(*hvecs[ivec],ptr+ioff*size_type,nsub,
                                 datatype);
         if (retval) {
	        CTA_WRITE_ERROR("Cannot get values of vector");
	        return retval;
         }
         ioff=ioff+nsub;
      }
      if (ioff!=nglob) return CTA_INCOMPATIBLE_VECTORS;


      /* set values in hvec */
//         zet999(values,nglob);
      retval=CTA_Vector_SetVals(hvec,values,nglob,datatype);
      if (retval) {
        CTA_WRITE_ERROR("Cannot set values of vector");
        return retval;
      }

      /* free work variables */
//      zet999(values,nglob);
      free(values);
      free(hvecs);
   }
   return CTA_OK;
};


#undef METHOD
#define METHOD "Copy"
int CTA_TreeVector_Copy(CTA_TreeVector treevec1, CTA_TreeVector treevec2){
// DIT IS EEN ERG KORT DOOR DE BOCHT GEDEELTELIJK INGEKLAPTE '
// SUBSTATES MOETEN OOK ONDERSTEUND WORDEN !!!!


   int retval;         //Return value of a call
   CTAI_TreeVec *data1, *data2;   //Tree-vector specific data
   int istate; // counter over all substates

   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec1,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) treevec1,(void*) &data1);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   retval=CTA_Handle_Check((CTA_Handle) treevec2,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) treevec2,(void*) &data2);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   // check if there is any meta-information; if yes, make a copy of it.



   /* check whether this is a leaf */
   if (data1->nsubtrees==0){
      if (data2->nsubtrees!=0) return CTA_TREEVECTORS_NOT_COMPATIBLE;

      /* copy vector */
      if (data1->v) {
         if (!data2->v) return CTA_TREEVECTORS_NOT_COMPATIBLE;
         retval=CTA_Vector_Copy(*data1->v, *data2->v);
         if (retval!=CTA_OK) {
            CTA_WRITE_ERROR("Cannot copy vector");
            return retval;
         }
      }

      /* copy metainfo of leaf */
      if (data1->metainfo) {
        // check if the other state has a metainfo
        //        if (!data2->metainfo) printf("leaf: data2 has no metainfo \n" );
        if (!data2->metainfo) return CTA_TREEVECTORS_NOT_COMPATIBLE;
        // assume that the other metainfo is of the right size and copy

        retval = CTA_Metainfo_Copy(*data1->metainfo,*data2->metainfo);
      }


   }
   else {    /* we are in a node. Copy the metainfo of the node, then the substates.*/


      if (data1->metainfo) {
        // check if the other state has a metainfo
        //  if (!data2->metainfo) printf("node: data2 has no metainfo \n" );
        if (!data2->metainfo) return CTA_TREEVECTORS_NOT_COMPATIBLE;
        // assume that the other metainfo is of the right size and copy

        retval = CTA_Metainfo_Copy(*data1->metainfo,*data2->metainfo);
      }


      /* copy sub-states */
      if (data1->nsubtrees!=data2->nsubtrees) return CTA_TREEVECTORS_NOT_COMPATIBLE;
      for (istate=0;istate<data1->nsubtrees;istate++){
         retval=CTA_TreeVector_Copy(data1->treevecs[istate],data2->treevecs[istate]);
         if (retval!=CTA_OK) {
            CTA_WRITE_ERROR("Cannot copy treevector");
            return retval;
         }
      }
   }
   return CTA_OK;
};




#undef METHOD
#define METHOD "SetConstant"
int CTA_TreeVector_SetConstant(CTA_TreeVector treevec, void *val,CTA_Datatype datatype){

   int retval;         //Return value of a call
   CTAI_TreeVec *data;   //Tree-vector specific data
   int istate; // counter over all substates
//   char name[CTA_STRLEN_NAME];

   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) treevec,(void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* check whether this is a leaf */
   if (data->nsubtrees==0){

      /* set constant vector */
     if (data->v) {
       retval=CTA_Vector_SetConstant(*data->v,val,datatype);
       if (retval!=CTA_OK) {
	      CTA_WRITE_ERROR("Error in CTA_Vector_SetConstant");
	      return retval;
       }
     }
   }
   else {
     /* copy sub-states */
     for (istate=0;istate<data->nsubtrees;istate++){
       if (data->nocompute[istate] == FALSE) {
         retval=CTA_TreeVector_SetConstant(data->treevecs[istate],val,datatype);
         if (retval!=CTA_OK) {
	        CTA_WRITE_ERROR("Error in CTA_TreeVector_SetConstant");
	        return retval;
         }
       }
     }
   }

   return CTA_OK;
};


#undef METHOD
#define METHOD "Scal"
int CTA_TreeVector_Scal(CTA_TreeVector treevec, double val){

   int retval;         //Return value of a call
   CTAI_TreeVec *data;   //Tree-vector specific data
   int istate; // counter over all substates
//   char name[CTA_STRLEN_NAME];

   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) treevec,(void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* check whether this is a leaf */
   if (data->nsubtrees==0){

      /* set constant vector */
      if (data->v) {
         retval=CTA_Vector_Scal(*data->v,val);
         if (retval!=CTA_OK) {
	        CTA_WRITE_ERROR("Error in CTA_Vector_Scal");
	        return retval;
         }
      }
   }
   else {
      /* copy sub-states */
     for (istate=0;istate<data->nsubtrees;istate++){
       if (data->nocompute[istate] == FALSE) {
         retval=CTA_TreeVector_Scal(data->treevecs[istate],val);
         if (retval!=CTA_OK) {
	        CTA_WRITE_ERROR("Error in CTA_TreeVector_Scal");
	        return retval;
         }
       }
     }
   }
   return CTA_OK;
};


#undef METHOD
#define METHOD "GetSetVal"
int CTAI_TreeVec_GetSetVal(CTA_TreeVector treevec1, int i, void *val,
                         CTA_Datatype datatype, BOOL lget ){
   int retval;         //Return value of a call
   CTAI_TreeVec *data1;  //Tree-vector specific data
   int istate;         // counter over all substates
   int nstate, offset;
   char msg[128];


   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec1,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) treevec1,(void*) &data1);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* check whether this is a leaf */
   if (data1->nsubtrees==0){
      /* Set value in vector vector */
      if (data1->v) {
         if (lget){
            retval=CTA_Vector_GetVal(*data1->v, i,val, datatype);
         } else {
            retval=CTA_Vector_SetVal(*data1->v, i,val, datatype);
         }
         if (retval!=CTA_OK) {
            sprintf(msg, "Error %d in getting or setting element %d vals", retval, i);
            CTA_WRITE_ERROR(msg);
	    return retval;
         }
      }
   }
   else {
      /* Find corresponding substate */
     offset=0;
     for (istate=0;istate<data1->nsubtrees;istate++){
       if (data1->nocompute[istate] == FALSE) {
         retval=CTA_TreeVector_GetSize(data1->treevecs[istate], &nstate);
         if (retval!=CTA_OK) {
	        CTA_WRITE_ERROR("Cannot get size of TreeVector");
	        return retval;
         }
         if (nstate>=i-offset) {
           retval=CTAI_TreeVec_GetSetVal(data1->treevecs[istate],i-offset, val,
                                         datatype, lget);
           return retval;
         }
         offset=offset+nstate;
       }
     }
   }
   return CTA_OK;
};


int CTA_TreeVector_GetVal(CTA_TreeVector treevec, int i, void *val,CTA_Datatype datatype){
   return CTAI_TreeVec_GetSetVal(treevec, i, val, datatype, TRUE );
};

int CTA_TreeVector_SetVal(CTA_TreeVector treevec, int i, void *val,CTA_Datatype datatype){
   return CTAI_TreeVec_GetSetVal(treevec, i, val, datatype, FALSE );
};




#undef METHOD
#define METHOD "SetVals"
int CTA_TreeVector_SetVals(CTA_TreeVector treevec, void *val,int nval, CTA_Datatype datatype){
// DIT IS EEN ERG KORT DOOR DE BOCHT VERSIE'
   CTA_Vector vwork;
   int retval;

   retval=CTA_Vector_Create(CTA_DEFAULT_VECTOR, nval, datatype, CTA_NULL, &vwork);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Cannot create vector");
       return retval;
   }
   retval=CTA_Vector_SetVals(vwork, val, nval, datatype);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Cannot set values in vector");
       return retval;
   }
   retval=CTA_TreeVector_SetVec(treevec,vwork);
   if (retval!=CTA_OK)  {
       CTA_WRITE_ERROR("Cannot set vector in vectortree");
       return retval;
   }
   retval=CTA_Vector_Free(&vwork);
   if (retval!=CTA_OK)  {
       CTA_WRITE_ERROR("Cannot free vector");
       return retval;
   }

   return CTA_OK;
};

#undef METHOD
#define METHOD "GetVals"
int CTA_TreeVector_GetVals(CTA_TreeVector treevec, void *val,int nval,CTA_Datatype datatype){
// DIT IS EEN ERG KORT DOOR DE BOCHT VERSIE'
   CTA_Vector vwork;
   int retval;
   char msg[128];

   retval=CTA_Vector_Create(CTA_DEFAULT_VECTOR, nval, datatype, CTA_NULL, &vwork);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }

   retval=CTA_TreeVector_GetVec(treevec,vwork);
   if (retval!=CTA_OK) {
       CTA_Vector_Free(&vwork);
       sprintf(msg,"Cannot get vector from treevector (length=%d) error code is %d", nval,retval);
       CTA_WRITE_ERROR(msg);
       return retval;
   }

   retval=CTA_Vector_GetVals(vwork, val, nval, datatype);
   if (retval!=CTA_OK) {
       CTA_Vector_Free(&vwork);
       CTA_WRITE_ERROR("Cannot get values from vector");
       return retval;
   }

   retval=CTA_Vector_Free(&vwork);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Cannot free vector");


       return retval;
   }
   return CTA_OK;
};

/* ----------------------------------------------------------------- */

#undef METHOD
#define METHOD "Axpy"
int CTA_TreeVector_Axpy(CTA_TreeVector y, double alpha, CTA_TreeVector x){
// DIT IS EEN ERG KORT DOOR DE BOCHT GEDEELTELIJK INGEKLAPTE '
// SUBSTATES MOETEN OOK ONDERSTEUND WORDEN !!!!


   int retval, retvalA;         //Return value of a call
   CTAI_TreeVec *datax, *datay;   //Tree-vector specific data
   int istate, jstate; // counter over all substates
   CTA_Metainfo minfox, minfoy;
   CTAI_Gridm hgridx, hgridy;
   char tagx[CTA_STRLEN_TAG], tagy[CTA_STRLEN_TAG];
   char xbelongsto[CTA_STRLEN_TAG], ybelongsto[CTA_STRLEN_TAG];
   CTA_TreeVector substate_x, substate_y;
   CTA_Datatype datatype;
   CTA_Vector vecy;
   int nx,ny, ierr;

   if (IDEBUG) {
      printf("Start of CTA_TreeVector_Axpy(%d,%f,%d)\n",y,alpha,x);
   }
   //
   //
   // printf("CTA_TreeVector_Axpy: y=\n");
   // retval=CTA_TreeVector_Export(y,CTA_FILE_STDOUT);

   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) x,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Argument x must be a CTA_TreeVector");
       return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) x,(void*) &datax);
   if (retval!=CTA_OK)
      {   printf("ERROR in CTA_TreeVector_Axpy: ");
          printf("Handle-getdata of argument x fails\n");
          return retval;
      }
   retval=CTA_Handle_Check((CTA_Handle) y,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
      char message[1024];
	  sprintf(message,"Argument y (handle=%d) must be a CTA_TreeVector\n",y);
	  CTA_WRITE_ERROR(message);
      return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) y,(void*) &datay);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Handle-getdata of argument y fails");
      return retval;
   }

/* FIRST RULE: if x is flat, y not flat, then if size(x)==size(y) do a crude axpy with one datatype only
  This is handy for steady-state filters, when KG has an unknown tree structure */
   retval=CTA_TreeVector_GetSize(x,&nx);
   retval=CTA_TreeVector_GetSize(y,&ny);
   if (datax->nsubtrees==0 && datay->nsubtrees > 0 && nx == ny){
	  retval=CTA_Vector_GetDatatype(*datax->v,&datatype);  // usually: cta_double
      retval=CTA_Vector_Create(CTA_DEFAULT_VECTOR, ny, datatype, CTA_NULL, &vecy);
      retval=CTA_TreeVector_GetVec(y,vecy);
      retval=CTA_Vector_Axpy(vecy, alpha,*datax->v);
      retval=CTA_TreeVector_SetVec(y,vecy);
	  retval=CTA_Vector_Free(&vecy);
	  return retval;
   }
/* END OF first rule ; now exploring other possibilities. */


   /* decide what to do according to metainfo */
   if (!datax->metainfo || !datay->metainfo ) {
   if (IDEBUG) {
     printf("CTA_TreeVector_Axpy: NO meta info in one (or both) of the treevectors %s and %s \n",
                  datax->tag, datay->tag);
   }
     /* check whether this is a leaf */
     if (datax->nsubtrees==0){

	   if (datay->nsubtrees!=0) {
             char message[1024];
	         sprintf(message,"Argument y (size %d, handle=%d) contains subtrees while x (size %d) is a leaf.\n",ny,y,nx);
	         CTA_WRITE_ERROR(message);			 
			 return CTA_TREEVECTORS_NOT_COMPATIBLE;
		 }

       /* axpy operation for vector */
       if (datax->v) {
         if (!datay->v) return CTA_TREEVECTORS_NOT_COMPATIBLE;
         retval=CTA_Vector_Axpy(*datay->v, alpha,*datax->v);
         if (retval!=CTA_OK) {
            CTA_WRITE_ERROR("Error in CTA_Vector_Axpy");
            return retval;
         }
       }
     }
     else {
       /* axpy operation for sub-states */
       if (datax->nsubtrees!=datay->nsubtrees){
           char message[1024];
	       sprintf(message,"arguments y and x have different number of subtrees \n");
	       CTA_WRITE_ERROR(message);
		   return CTA_TREEVECTORS_NOT_COMPATIBLE;
       }
       for (istate=0;istate<datax->nsubtrees;istate++){
         if (datax->nocompute[istate] == FALSE) {
           retval=CTA_TreeVector_Axpy(datay->treevecs[istate], alpha,datax->treevecs[istate]);
           if (retval!=CTA_OK) {
              CTA_WRITE_ERROR("Error in CTA_TreeVector_Axpy");
              return retval;
           }
         }
       }
     }
     return CTA_OK;
   }
   else {
      if (IDEBUG) {
         printf("CTA_TreeVector_Axpy meta info available for both ");
         printf("treevectors %s and %s \n",datax->tag, datay->tag);
               retval=CTA_TreeVector_GetSize(x,&nx);
               retval=CTA_TreeVector_GetSize(y,&ny);
               printf("sizes: x %d y %d \n",nx,ny);
      }

     /* ---------- metainfo exists for both x and y --------------  */

     retvalA = CTA_OK;
     retval=CTA_Metainfo_Create(&minfox);   retval=CTA_Metainfo_Create(&minfoy);

     retval = CTA_Metainfo_Copy(*datax->metainfo, minfox);
     retval = CTA_Metainfo_Copy(*datay->metainfo, minfoy);
     retval = CTA_Metainfo_GetGrid(minfox, &hgridx);
     retval = CTA_Metainfo_GetGrid(minfoy, &hgridy);
     retval = CTA_Metainfo_GetTag(minfox, tagx);
     retval = CTA_Metainfo_GetTag(minfoy, tagy);
     retval = CTA_Metainfo_GetBelongsTo(minfox, xbelongsto);
     retval = CTA_Metainfo_GetBelongsTo(minfoy, ybelongsto);

     /* -------------------------- */
     /* search for equal tags        */

     if (IDEBUG) {
        printf("CTA_TreeVector_Axpy: tagx=%s,tagy=%s\n",tagx, tagy);
     }

     if (0==strcmp(tagx, tagy)) { // y and x have same tag ; finished scanning!

     if (IDEBUG) {
        printf("CTA_TreeVector_Axpy: equal tags; now calling ctai_axpy_grid\n");
     }

       retvalA = CTAI_axpy_grid(y,hgridy,alpha,x,hgridx);
	   if (retvalA) {
		  char message[1024];
		  sprintf(message," ERROR cta_treevector_axpy: code %d  \n",retval);
	      CTA_WRITE_ERROR(message);
	   }

     }
     else {

       /* check if tag of y equals tag of substate of x */

       if (datax->nsubtrees != 0) {
         for (istate=0; istate<datax->nsubtrees;istate++){
           if (datax->nocompute[istate] == FALSE) {
             substate_x= datax->treevecs[istate];
             retval = CTA_TreeVector_Axpy(y, alpha, substate_x);
           }
           // retvalA = max(retvalA, retval);
         }
       }  // loop along substates of x


       /* check if tag of substate of y equals tag of x */
       if (datay->nsubtrees!=0) {

         for (jstate=0; jstate<datay->nsubtrees;jstate++){
           if (datay->nocompute[jstate] == FALSE) {
             substate_y= datay->treevecs[jstate];
             retval = CTA_TreeVector_Axpy(substate_y, alpha, x);
           }
           // retvalA = max(retvalA, retval);
         }
       }
     }
     /* free variables */
     ierr = CTA_Metainfo_Free(&minfox);
     if (IDEBUG>0) {printf("end of ctai_treevector_axpy: free metainfo %d\n",ierr);}
     ierr = CTA_Metainfo_Free(&minfoy);



     return retvalA;
   } // else metainfo exists


   return CTA_OK;
};


#undef METHOD
#define METHOD "ElmOp"
int CTA_TreeVector_ElmOp(CTA_TreeVector y, CTA_TreeVector x, int oper){
// DIT IS EEN ERG KORT DOOR DE BOCHT GEDEELTELIJK INGEKLAPTE '
// SUBSTATES MOETEN OOK ONDERSTEUND WORDEN !!!!


   int retval;                    //Return value of a call
   CTAI_TreeVec *datax, *datay;   //Tree-vector specific data
   int istate; // counter over all substates
   BOOL noInterpolation;

   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) x,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
       char message[1024];
	   sprintf(message, "ERROR in CTA_TreeVector_Axpy: argument x must be a CTA_TreeVector");
	   CTA_WRITE_ERROR(message);
       return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) x,(void*) &datax);
   if (retval!=CTA_OK) {
      char message[1024];
	  sprintf(message, "ERROR in CTA_TreeVector_Axpy: Handle-getdata of argument x fails");
      CTA_WRITE_ERROR(message);
	  return retval;
   }
   retval=CTA_Handle_Check((CTA_Handle) y,CTA_TREEVECTOR);
   if (retval!=CTA_OK){
	   char message[1024];
	   sprintf(message, "ERROR in CTA_TreeVector_Axpy: argument y must be a CTA_TreeVector");
       CTA_WRITE_ERROR(message);
       return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) y,(void*) &datay);
   if (retval!=CTA_OK) {
      char message[1024];
	  sprintf(message, "ERROR in CTA_TreeVector_Axpy: Handle-getdata of argument y fails");
	  CTA_WRITE_ERROR(message);
      return retval;
   }

   /* decide what to do according to metainfo */
   /* Note :if only one has meta info just continue without interpolation */
   /* If both meta info's are the same we can continue as well without interpolation */

   noInterpolation=TRUE;
   /* If both have meta info check whether we need interpolation */
   if (datax->metainfo && datay->metainfo){
      if (CTA_Metainfo_IsEqual(*(datax->metainfo),
                               *(datay->metainfo))==CTA_TRUE){
         noInterpolation=FALSE;
      }
   }

   if (noInterpolation) {

     /* check whether this is a leaf */
     if (datax->nsubtrees==0){
       if (datay->nsubtrees!=0) {
          printf("Error: Vector x is a leaf but vector y has %d children\n",datay->nsubtrees);
          return CTA_TREEVECTORS_NOT_COMPATIBLE; // hoeft niet!
       }

       /* axpy operation for vector */
       if (datax->v) {
         if (!datay->v) {
            printf("Error: Vector x is allocated (leaf) but vector v not\n");
            return CTA_TREEVECTORS_NOT_COMPATIBLE;
         }
         if (oper==ELMDIV){
            retval=CTA_Vector_ElmDiv(*datay->v, *datax->v);
         } else {
            retval=CTA_Vector_ElmProd(*datay->v, *datax->v);
         }
         if (retval!=CTA_OK) {
            CTA_WRITE_ERROR("Error in CTA_Vector_ElmDiv or CTA_VEctor_ElmProd");
            return retval;
         }
       }
     }
     else {
       /* axpy operation for sub-states */
       if (datax->nsubtrees!=datay->nsubtrees) {
          printf("Error: Number of sub-trees is not the same x(%d) y(%d)\n",
                 datax->nsubtrees,datay->nsubtrees);
          return CTA_TREEVECTORS_NOT_COMPATIBLE;
       }
       for (istate=0;istate<datax->nsubtrees;istate++){
         if (datax->nocompute[istate] == FALSE) {
           retval=CTA_TreeVector_ElmOp(datay->treevecs[istate], datax->treevecs[istate], oper);
           if (retval!=CTA_OK) {
             CTA_WRITE_ERROR("Error in CTA_TreeVector_ElmOp");
             return retval;
           }
         }
       }
     }
     return CTA_OK;
   }
   else {
      printf("Error: CTA_TreeVector_ElmDiv with meta info is not supported\n");
      return CTA_TREEVECTORS_NOT_COMPATIBLE;
   } // else metainfo exists


   return CTA_OK;
};


int CTA_TreeVector_ElmDiv(CTA_TreeVector y, CTA_TreeVector x){
   return CTA_TreeVector_ElmOp(y, x, ELMDIV);

};


int CTA_TreeVector_ElmProd(CTA_TreeVector y, CTA_TreeVector x){
   return CTA_TreeVector_ElmOp(y, x, ELMPROD);
};


/* ------------------------------------------------------------*/


#undef METHOD
#define METHOD "Dot"
int CTA_TreeVector_Dot(CTA_TreeVector treevec1, CTA_TreeVector treevec2, double *dotprod){
// DIT IS EEN ERG KORT DOOR DE BOCHT GEDEELTELIJK INGEKLAPTE '
// SUBSTATES MOETEN OOK ONDERSTEUND WORDEN !!!!


   int retval;         //Return value of a call
   CTAI_TreeVec *data1, *data2;   //Tree-vector specific data
   int istate; // counter over all substates
//   char name[CTA_STRLEN_NAME];
   double dotwrk; //dot product of substate


   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec1,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) treevec1,(void*) &data1);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   retval=CTA_Handle_Check((CTA_Handle) treevec2,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) treevec2,(void*) &data2);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   /* check whether this is a leaf */
   if (data1->nsubtrees==0){
      if (data2->nsubtrees!=0) return CTA_TREEVECTORS_NOT_COMPATIBLE;

      /* dot vector */
      if (data1->v) {
         if (!data2->v) return CTA_TREEVECTORS_NOT_COMPATIBLE;
         retval=CTA_Vector_Dot(*data1->v, *data2->v,dotprod);
         if (retval!=CTA_OK) {
            CTA_WRITE_ERROR("Error in using CTA_Vector_Dot");
            return retval;
         }
      }
   }
   else {
      /* dot sub-states */
      *dotprod=0.0;
      if (data1->nsubtrees!=data2->nsubtrees) return CTA_TREEVECTORS_NOT_COMPATIBLE;
      for (istate=0;istate<data1->nsubtrees;istate++){
        if (data1->nocompute[istate] == FALSE) {
          retval=CTA_TreeVector_Dot(data1->treevecs[istate],data2->treevecs[istate],
                                    &dotwrk);
          if (retval!=CTA_OK) {
             CTA_WRITE_ERROR("Error in using CTA_TreeVector_Dot");
             return retval;
          }
          *dotprod=*dotprod+dotwrk;
        }
      }
   }
   return CTA_OK;
};

#undef METHOD
#define METHOD "Nrm2"
int CTA_TreeVector_Nrm2(CTA_TreeVector treevec1, double *nrm2){

   int retval;         //Return value of a call
   CTAI_TreeVec *data1;  //Tree-vector specific data
   int istate;         // counter over all substates
   double nrmwrk; //dot product of substate


   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec1,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) treevec1,(void*) &data1);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* check whether this is a leaf */
   if (data1->nsubtrees==0){
      /* dot vector */
      if (data1->v) {
         retval=CTA_Vector_Nrm2(*data1->v,nrm2);
         if (retval!=CTA_OK) {
	        CTA_WRITE_ERROR("Error in using CTA_Vector_Nrm2");
	        return retval;
         }
      }
   }
   else {
      /* dot sub-states */
      *nrm2=0.0;
      for (istate=0;istate<data1->nsubtrees;istate++){
        if (data1->nocompute[istate] == FALSE) {
          retval=CTA_TreeVector_Nrm2(data1->treevecs[istate],&nrmwrk);
          if (retval!=CTA_OK) {
	         CTA_WRITE_ERROR("Error in using CTA_TreeVector_Nrm2");
	         return retval;
          }
          *nrm2=*nrm2+(nrmwrk*nrmwrk);
        }
      }
      *nrm2=sqrt(*nrm2);
   }
   return CTA_OK;
};


#undef METHOD
#define METHOD "GetSize"
int CTA_TreeVector_GetSize(CTA_TreeVector treevec, int *n){
   int retval;         //Return value of a call
   CTAI_TreeVec *data;   //Tree-vector specific data
   int nsub;
   int istate;

   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) treevec,(void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   // We are in a leaf
   if (data->nsubtrees==0){
      if (data->v){
         retval=CTA_Vector_GetSize(*data->v,n);
         if (retval) {
	        CTA_WRITE_ERROR("Error while using CTA_Vector_GetSize");
	        return retval;
         }
      }
      else {
         return CTA_UNINITIALISED_SUBTREEVECTOR;
      }
   } else {
      *n=0;
      for (istate=0;istate<data->nsubtrees;istate++){
        if (data->nocompute[istate] == FALSE) {
          retval=CTA_TreeVector_GetSize(data->treevecs[istate], &nsub);
          if (retval) {
	         CTA_WRITE_ERROR("Error while using CTA_TreeVector_GetSize");
	         return retval;
          }
          *n=*n+nsub;
        }
      }

   }
   return CTA_OK;
}

// This is a try-out function that writes a tree-vector in
// a NETCDF file

char *CTAI_AddNameToPath(const char *path,const char *name, const char *sep){
     size_t len1, len2, len3;
     char *retStr;

     len1=strlen(path);
     len2=strlen(name);
     len3=strlen(sep);
     retStr=CTA_Malloc(sizeof(char)*(len1+len2+len3+1));

      strcpy(retStr,path);
      strcat(retStr,name);
      strcat(retStr,sep);

      return retStr;
}

//-------------------------------------------
// small function for string manipulation 
int replace_char_of_string(char *s, char old, char new){

   char *p = s;

   while(*p){
      if(*p == old)
      *p = new;
      ++p;
   }
   return CTA_OK;
}
//--------------------------------------------

int CTAI_CTA_TreeVector_ToNetCDF(CTA_TreeVector treeVec, char *path, int ncid, int timeDimid, BOOL append, int ntime, BOOL isdefvar){
#if HAVE_LIBNETCDF

#define ERR(e) {printf("Error: %s\n", nc_strerror(e)); exit(2);}

   int nSize; //Size of the whole Tree-vector
   int retval;          //Return value of a call
   int varid;
   void *values;
   int dimids[5];
   int dimidsRev[5];

   char *nameDim1, *nameDim2, *nameDim3, *nameDim4;
   char *nameVar;
   char *nameVar_;
   char *extendedPath;
   char nameGrid[20];
   char tvtag[CTA_STRLEN_NAME];
   char modtag[CTA_STRLEN_NAME];
   
   /* Grid information */
   int nx;
   int ny;
   int nz;
   int nquant;
   double x_origin;
   double y_origin;
   double z_origin;
   double dx;
   double dy;
   double dz;

   CTA_Datatype datatype;
   CTA_Metainfo meta;
   CTA_Vector v;
   CTAI_TreeVec *data1;   //Tree-vector specific data
   int istate;          // counter over all substates
   int ndims[4];
   int numDims;
   int idim;
   int i;
   int size_type;
   int locIDEBUG = 0;
  
   size_t *start;
   size_t *count;

   if (IDEBUG>0) {printf("start treevector_netcdf_export; append, isdefvar %d %d\n",append,isdefvar);}

   nameDim1 = NULL;
   nameDim2 = NULL;
   nameDim3 = NULL;
   nameDim4 = NULL;
   nameVar_ = NULL;
   nameVar  = NULL;

   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treeVec,CTA_TREEVECTOR);
   if (retval!=CTA_OK) return retval;
   retval=CTA_Handle_GetData((CTA_Handle) treeVec,(void*) &data1);
   if (retval!=CTA_OK) return retval;

   /* if we are in a leaf */
   if (data1->nsubtrees==0){
      /* export the treevector to netCDF */
      if (data1->v){
         v=*data1->v;

         /* Get the size of the vector */
         retval=CTA_Vector_GetSize(v,&nSize);
         if (IDEBUG>0) {printf("nSize=%d %d\n",nSize,retval);}
         /* also get the datatype */
         retval=CTA_Vector_GetDatatype(v,&datatype);

         /* if we have meta information */
         if (data1->metainfo){
            meta=*data1->metainfo;
            retval=CTA_Metainfo_getRegGrid(meta, nameGrid, &nx, &ny, &nz, &x_origin, &y_origin, &z_origin, &dx, &dy, &dz);
            ndims[0]=nx;
            ndims[1]=ny;
            ndims[2]=nz;

            if (nx*ny*nz == nSize) {numDims=3;}

            if (nz<=1) numDims=2;
            if (ny<=1) numDims=1;

            if (numDims == 3  && nx*ny*nz < nSize) {
              nquant = nSize / (nx*ny*nz) ;
              if (nquant*nx*ny*nz != nSize) {
                printf("Error: grid dimension and state dimension do not match %d %d %d %d %d \n",nx,ny,nz,nSize,nquant);
                return CTA_DIMENSION_ERROR;
              }

              if (locIDEBUG>0) {printf("State is larger than product of grid dimensions. (%d %d %d) \n"
                "We assume the state can be divided into %d separate quantities \n",nx,ny,nz,nquant);}
              numDims=4;
              ndims[3]=nquant;

            }
         } else {
           if (locIDEBUG>0) {printf("We do not have meta information; so we  assume  1D array of size %d\n",nSize);}
            numDims=1;
            ndims[0]=nSize;
         }

         /* Now write the vector to netcdf */

         /* First construct the names of the dimensions */

         /* for safety reasons, underscores in tags are replaced by '-'
            since underscores play a dividing role in importing netcdf from scratch
            */
         strcpy(modtag,data1->tag);
         replace_char_of_string(modtag,'_','-');
         
         nameVar_=CTAI_AddNameToPath(path,modtag,"_");
         nameDim1=CTAI_AddNameToPath(nameVar_,"x","");
         nameDim2=CTAI_AddNameToPath(nameVar_,"y","");
         nameDim3=CTAI_AddNameToPath(nameVar_,"z","");
         nameDim4=CTAI_AddNameToPath(nameVar_,"n","");
         nameVar=CTAI_AddNameToPath(path,modtag,"");

         if (append || !isdefvar ){
            /* appending a state-vector; we need to get the dimensions from the file */
            /* and the data array */

            dimids[0]=timeDimid;
            if (numDims>=1){
               if ((retval = nc_inq_dimid(ncid, nameDim1, &dimids[1]))) ERR(retval);
            }
            if (numDims>=2){
               if ((retval = nc_inq_dimid(ncid, nameDim2, &dimids[2]))) ERR(retval);
            }
            if (numDims>=3){
               if ((retval = nc_inq_dimid(ncid, nameDim3, &dimids[3]))) ERR(retval);
            }
            if (numDims>=4){
               if ((retval = nc_inq_dimid(ncid, nameDim4, &dimids[4]))) ERR(retval);
            }
            if ((retval = nc_inq_varid(ncid, nameVar,  &varid    ))) ERR(retval);
         }
         else {
           if (locIDEBUG>0) {printf("%s: netcdf dimensions are defined: %d \n",data1->tag, numDims);}
         

         /* defining a tree-vector so we have to define the dimensions y*/
            retval = nc_redef(ncid);

            dimids[0]=timeDimid;
        if (locIDEBUG>0) {printf(" r1977 dimids[0] %d  namedim1 %s\n",timeDimid, nameDim1);}
         

            if (numDims>=1){
               if ((retval = nc_def_dim(ncid, nameDim1, ndims[0], &dimids[1]))) ERR(retval);
            }
            if (numDims>=2){
               if ((retval = nc_def_dim(ncid, nameDim2, ndims[1], &dimids[2]))) ERR(retval);
            }
            if (numDims>=3){
               if ((retval = nc_def_dim(ncid, nameDim3, ndims[2], &dimids[3]))) ERR(retval);
            }
            if (numDims>=4){
               if ((retval = nc_def_dim(ncid, nameDim4, ndims[3], &dimids[4]))) ERR(retval);
            }
         }

         /* Reverse all dimensions since NETCDF accesses arrays in the C way */
         dimidsRev[0]=dimids[0];
         for (idim=0;idim<numDims;idim++){
            dimidsRev[idim+1]=dimids[numDims-idim];
         }
        //          for (idim=0;idim<=numDims;idim++){
        //         printf("dimid[%d]=%d  size dim1 %d \n",idim,dimids[idim], ndims[0]);
        // }
        // for (idim=0;idim<=numDims;idim++){
         //        printf("dimidRev[%d]=%d\n",idim,dimidsRev[idim]);
        // }
         if (locIDEBUG>0) {printf("netcdf dimensions have been reversed \n");}

         if (!append){

           if (isdefvar) {

             if (locIDEBUG>0) {printf("ctai_cta_exporttonetcdf: defining new variable %s \n",nameVar);}

             /* Define the array */
               if (datatype == CTA_REAL) {
			     if ((retval = nc_def_var(ncid, nameVar, NC_FLOAT, numDims+1, dimidsRev, &varid))) ERR(retval);
			   } else if (datatype == CTA_INTEGER) {
                 if ((retval = nc_def_var(ncid, nameVar, NC_INT, numDims+1, dimidsRev, &varid))) ERR(retval);

			   } else {  //default: double
                 if ((retval = nc_def_var(ncid, nameVar, NC_DOUBLE, numDims+1, dimidsRev, &varid))) ERR(retval);

			   }
		   }
         }
         free(nameDim1);
         free(nameDim2);
         free(nameDim3);
         free(nameDim4);
         free(nameVar_);
         free(nameVar);

         if (!isdefvar) { // write the data itself

         /* Get the values of the vector */
         /* Note: we will export all values as float for space considerations.
            Output files are mostly meant for visualisation purposes, so floats will be accurate enough. */
         /* NO! for non-visualisation purposes, this gives problems! */

           retval=CTA_SizeOf(datatype,&size_type);

           values=CTA_Malloc(nSize * size_type);

           retval=CTA_TreeVector_GetTag(treeVec, tvtag);
		   if (datatype != CTA_STRING) {
             retval = CTA_Vector_GetVals(v,values,nSize, datatype);
             if (retval !=CTA_OK) {
			   printf("cta_treevector.c: export netcdf: getting values of %s failed %d \n",&tvtag[0],retval);
		     }
		   }

           // At what level must we add our new values???
           count = CTA_Malloc((numDims+1)*sizeof(size_t));
           start = CTA_Malloc((numDims+1)*sizeof(size_t));
           count[0] = 1;

           //reverse dimensions as well
           for (idim=0;idim<numDims;idim++){
             count[idim+1]=ndims[numDims-idim-1];
           }

           start[0] = ntime-1;
           for (i=1; i<=numDims;i++){start[i] = 0;}

	       if (datatype == CTA_REAL) {
              if ((retval = nc_put_vara_float(ncid, varid, start, count, values))) ERR(retval);
    	   } else  if (datatype == CTA_INTEGER) {
           if ((retval = nc_put_vara_int(ncid, varid, start, count, values))) ERR(retval);
    	   } else if (datatype == CTA_DOUBLE){
            if ((retval = nc_put_vara_double(ncid, varid, start, count, values))) ERR(retval);
		   } else { //do nothing
		   }

           free(values);
           free(count);free(start);
         }
      }
   }
   else {

     if (IDEBUG>0) { printf("RECURSIVELY calling ctai_cta_treevectortonetcdf! \n");}

      /* create the path name */
      strcpy(modtag,data1->tag);
      replace_char_of_string(modtag,'_','-');
      extendedPath=CTAI_AddNameToPath(path,modtag,"_");

      /* iterate over all sub-tree-vectors */
      for (istate=0;istate<data1->nsubtrees;istate++){
        CTAI_CTA_TreeVector_ToNetCDF(data1->treevecs[istate], extendedPath,
                   ncid, timeDimid, append, ntime, isdefvar);
      }
      free(extendedPath);
      return CTA_OK;
   }
#else
   printf("CTAI_CTA_TreeVector_ToNetCDF :Version is compiled without NETCDF support. This function should never have been called!!!!\n");
   exit(-1);
#endif
   return CTA_INTERNAL_ERROR;
}



/*
Import a treevector from netcdf. The caller has no idea yet of
the tree-structure, so treevec is not yet filled/structured (it has just been created, nothing more)
and all variables are concatenated in one large CTA_DOUBLE vector.
In the near future, the ctai_treevector_importfromnetcdf will be made smarter
such that the tree-structure will be created from the netcdf-file.
*/
int CTA_TreeVector_VImport(CTA_TreeVector treeVec, CTA_File hfile) {
#if HAVE_LIBNETCDF

   int retval;          //Return value of a call
   int ncid;
   int nvars;
   int varid;
   char var_name[NC_MAX_NAME+1];
   nc_type data_type;
   int numdims;  //total number of dimensions
   int ndimsvar ;// number of dimensions for a certain variable
   int natts, i, ivar;
   int dimids[5];

   double *values_d;
   float *values_f;
   int *values_i;
   double *values_tot;
   int nsize, nsize_tot;
   size_t dimlength;
   int size_type;      //C- sizeof of data items in hvec
   int indx;
   CTA_Datatype datatype; //Data Type of hvec
   CTA_File_GetNetcdf(hfile,&ncid);

   /* Assume that the treevector has just been created. This routine
      simply reads each variable in the netcdf-file and places it in one long vector.
   */

   // first get all dimensions
   retval =  nc_inq_ndims(ncid, &numdims);
   /* the dim-ids are simply in the range 0..numdims-1 */


   retval = nc_inq_nvars(ncid, &nvars);
   /* The var-ids are simply in the range 0..nvars-1
      We skip the first two, since they are ALWAYS (when created here in the native code)
	  time and ntime.  */


   /* first sweep through the netcdf-file to determine the total size */
   nsize_tot = 0;
   for (ivar=2; ivar < nvars ; ivar ++) {
     varid = ivar;

     retval = nc_inq_var(ncid, varid, var_name, &data_type, &ndimsvar, dimids, &natts);
     if (retval != NC_NOERR ) {
      // printf("ctai_tv_importfromnetcdf : error nc_inq_var %d :error nr: %d\n",ncid, retval);
     }
	 nsize = 1;  //0-th dimension
	 for (i=0; i < ndimsvar; i++) {
	     retval = nc_inq_dimlen(ncid, dimids[i], &dimlength);
		 nsize = nsize * dimlength;
	 }
     nsize_tot = nsize_tot + nsize; //incremented length of receiving vector
   }

   /* create receiving vector. Note: this is ALWAYS a double! */
    values_tot = CTA_Malloc(nsize_tot * sizeof(double));

  /* second sweep through the netcdf-file; read everything */

   indx = 0;
   for (ivar=2; ivar < nvars ; ivar ++) {
     varid = ivar;

     retval = nc_inq_var(ncid, varid, var_name, &data_type, &ndimsvar, dimids, &natts);
     nsize = 1;  //0-th dimension
	 for (i=0; i < ndimsvar; i++) {
	     retval = nc_inq_dimlen(ncid, dimids[i], &dimlength);
		 nsize = nsize * dimlength;
	 }

	 if (data_type == NC_DOUBLE) {datatype = CTA_DOUBLE;
	 } else if (data_type == NC_FLOAT) {datatype = CTA_REAL;
	 } else if (data_type == NC_INT) {datatype = CTA_INTEGER;
	 } else { printf("unknown datatype in netcdf file \n");
		 return CTA_ILLEGAL_DATATYPE;
	 }
     retval=CTA_SizeOf(datatype,&size_type);


	 values_d=CTA_Malloc(nsize*sizeof(double));
	 values_f=CTA_Malloc(nsize*sizeof(float));
	 values_i=CTA_Malloc(nsize*sizeof(int));

     if (datatype == CTA_REAL) {
	     retval = nc_get_var_float(ncid, varid, values_f);
	     for (i=0; i< nsize; i++) {
           values_tot[indx + i] = values_f[i];
	    }
	   } else  if (datatype == CTA_INTEGER) {
	     retval = nc_get_var_int(ncid, varid, values_i);
	     for (i=0; i< nsize; i++) {
           values_tot[indx + i] = values_i[i];
	     }

	   } else if (datatype == CTA_DOUBLE){
	     retval = nc_get_var_double(ncid, varid, values_d);
	     for (i=0; i< nsize; i++) {
          values_tot[indx + i] = values_d[i];
	     }
	   }


      indx = indx + nsize;
	  free(values_d);
	  free(values_i);
	  free(values_f);

   }

   // set the treevector
    retval = CTA_TreeVector_SetVals(treeVec, values_tot,nsize_tot, CTA_DOUBLE);
    free(values_tot);

    return retval;

#else
   printf("CTAI_TreeVector_VImport :Version is compiled without NETCDF support. This function should never have been called!!!!\n");
   exit(-1);
#endif
};

/* ------------------------------------------------------- */

int CTAI_TreeVector_ImportFromNetCDF(CTA_TreeVector treeVec, CTA_File hfile){


#if HAVE_LIBNETCDF

   int retval;          //Return value of a call
   int ncid;
   int nvars;
   int varid;
   char var_name[NC_MAX_NAME+1];
   nc_type data_type;
   int numdims;  //total number of dimensions
   int ndimsvar ;// number of dimensions for a certain variable
   int natts, i, ivar;
   int dimids[5];
   void *values;
   CTA_TreeVector hsubstate;
   char *tag, *tag0;
   int nsize, tv_size;
   size_t dimlength;
   int size_type;      //C- sizeof of data items in hvec
   int indx;
   CTA_Datatype datatype; //Data Type of hvec
   CTA_Vector **hvec; //array (of length 1) with pointer to vector of hsubstate

   tag0 = NULL;
   CTA_File_GetNetcdf(hfile,&ncid);

   /* Assume that the treevector already contains the desired sub-treevectors. This routine
      simply reads each variable in the netcdf-file and places it in the proper subtreevector.
   */

   // first get all dimensions
   retval =  nc_inq_ndims(ncid, &numdims);
   /* the dim-ids are simply in the range 0..numdims-1 */


   retval = nc_inq_nvars(ncid, &nvars);
   /* The var-ids are simply in the range 0..nvars-1  */


   for (ivar=0; ivar < nvars ; ivar ++) {
     varid = ivar;

     retval = nc_inq_var(ncid, varid, var_name, &data_type, &ndimsvar, dimids, &natts);
     if (retval != NC_NOERR ) {
       //printf("ctai_tv_importfromnetcdf : error nc_inq_var %d :error nr: %d\n",ncid, retval);
     }
     nsize = 1;  //0-th dimension
     for (i=0; i < ndimsvar; i++) {
       retval = nc_inq_dimlen(ncid, dimids[i], &dimlength);
       nsize = nsize * dimlength;
     }
     /* Now find the subtreevec matching the var-name */
     // printf("variable : name %s  \n",&var_name);

     tag = CTA_Malloc((strlen(var_name)+1)*sizeof(char));
     tag0 = strrchr(&var_name[0],'_');
     if (tag0 != NULL) {
       tag = tag0 + 1 ;
     } else {
       strcpy(tag, "novar");
       if (nvars == 3) { // simple case: no subtrees, only time,ntime,vec
         strcpy(tag , &var_name[0]);
       }
     }

     //printf("last part of variable name = %s, nvars = %d  \n",tag, nvars);

     retval = CTA_TreeVector_GetSubTreeVec(treeVec,  tag, &hsubstate);

     if (retval == CTA_ITEM_NOT_FOUND) {
       //printf(".. no subtreevector found; ignoring the  variable %s \n",&var_name[0]);
     } else {
       retval = CTA_TreeVector_GetSize(hsubstate, &tv_size);
       if (tv_size != nsize) {
         printf("ctai_tv_importfromnetcdf: size subtreevec (%s) of %d does not match netcdf-size %d \n",tag, tv_size,nsize);
         return CTA_DIMENSION_ERROR;
       }
       /* now determine the datatype */

       hvec=CTA_Malloc(sizeof(CTA_Vector*));
       indx=0;
       retval=CTAI_TreeVec_GetVecHandles(hsubstate, hvec, &indx);
       retval=CTA_Vector_GetDatatype(*hvec[0],&datatype);
       retval=CTA_SizeOf(datatype,&size_type);

       values = CTA_Malloc(tv_size * size_type);

       if (datatype == CTA_REAL) {
         retval = nc_get_var_float(ncid, varid, values);
       } else  if (datatype == CTA_INTEGER) {
         retval = nc_get_var_int(ncid, varid, values);
       } else if (datatype == CTA_DOUBLE){
         retval = nc_get_var_double(ncid, varid, values);
       } else {
         printf("no valid datatype to import %s from netcdf \n",&var_name[0]);
         retval  = -1;
       }

       /* now fill the subtreevector */
       if (retval == CTA_OK) {
         retval = CTA_TreeVector_SetVals(hsubstate, values,tv_size, datatype);
       }
       retval = CTA_OK;
       free(values);
       printf("variable %s  done \n",var_name);
     }
   }
   return retval;

#else
   printf("CTAI_TreeVector_ImportFromNetCDF :Version is compiled without NETCDF support. This function should never have been called!!!!\n");
   exit(-1);
#endif



};



int CTAI_TreeVector_ExportToNetCDF(CTA_TreeVector treeVec, CTA_File hfile){


#if HAVE_LIBNETCDF

   int retval;          //Return value of a call
   int timeDimid;
   int oneDimid;
   int ncid;
   BOOL append;
   BOOL isdefvar;
   size_t start[1];
   int one;
   int timeVarid;
   int ntimeVarid;
   int ntime;

   int idebug=0;

   if (idebug>0) {printf("CTAI_TreeVector_ExportToNetCDF: Export treevector to netcdf\n");}

   CTA_File_GetNetcdf(hfile,&ncid);

   /* Check whether the file already contains the time dimension */
   append=TRUE;
   retval = nc_inq_dimid(ncid, "time", &timeDimid);

   if (retval != NC_NOERR){
      if (idebug>0) {printf("CTAI_TreeVector_ExportToNetCDF: First write to this file; ncid: %d\n",ncid);}
      if (idebug>0) {printf("CTAI_TreeVector_ExportToNetCDF: creating time and ntime variables\n");}

      /* Hmm could not find it so let's create it */
      if ((retval = nc_def_dim(ncid, "time", NC_UNLIMITED, &timeDimid))) ERR(retval);

      if ((retval = nc_def_dim(ncid, "one", 1, &oneDimid))) ERR(retval);

      
      append=FALSE;

      /* Define the dimensions and the array*/
      retval = nc_redef(ncid);

      /* Now create time and counter array */
      if ((retval = nc_def_var(ncid, "time",  NC_INT, 1, &timeDimid, &timeVarid))) ERR(retval);
      if ((retval = nc_def_var(ncid, "ntime", NC_INT, 1, &oneDimid,  &ntimeVarid))) ERR(retval);

      retval = nc_enddef(ncid);
      /* Set first value in time array */

      start[0] = 0;
      one      = 1;
      ntime    = 1;
      if ((retval = nc_put_var1_int   (ncid, timeVarid,  start, &one))) {
         ERR(retval);
      }
      if ((retval = nc_put_var1_int   (ncid, ntimeVarid, start, &ntime))) ERR(retval);
   }

   /* Call the actual write routine */
   if ((retval = nc_inq_varid(ncid, "time",  &timeVarid))) ERR(retval);
   if ((retval = nc_inq_varid(ncid, "ntime", &ntimeVarid))) ERR(retval);

   start[0] = 0;
   retval = nc_get_var1_int   (ncid, ntimeVarid, start, &ntime);

   if (append) {
      ntime=ntime+1;
      start[0] = 0;
      if ((retval = nc_put_var1_int   (ncid, ntimeVarid, start, &ntime))) ERR(retval);
      start[0] = ntime-1;
      if ((retval = nc_put_var1_int   (ncid, timeVarid,  start, &ntime))) ERR(retval);
   }
   /* first write only the dimensions, if it is the first time */
   
   isdefvar = TRUE;
   
   
   if (idebug>0){printf("NOW 1st NETCDF sweep: writing dimensions\n");}
   retval=CTAI_CTA_TreeVector_ToNetCDF(treeVec,"",ncid, timeDimid, append, ntime, isdefvar);
   /* now end the definition, if it is the first time */
   
   if (!append){
     if ((retval = nc_enddef(ncid))) ERR(retval);
   }

   /* in the second sweep , actually write the data. */
   isdefvar = FALSE;
   if (IDEBUG>0) {printf("NOW 2nd NETCDF sweep: writing data\n");}
   retval=CTAI_CTA_TreeVector_ToNetCDF(treeVec,"",ncid, timeDimid, append, ntime, isdefvar);

   return CTA_OK;

#else
   printf("CTAI_TreeVector_ExportToNetCDF :Version is compiled without NETCDF support. This function should never have been called!!!!\n");
   exit(-1);
#endif




};



#undef METHOD
#define METHOD "Export"
int CTA_TreeVector_Export(CTA_TreeVector treevec1, CTA_Handle usrdata){
   int retval;          //Return value of a call
   CTAI_TreeVec *data1;   //Tree-vector specific data
   CTAI_TreeVec *data2;   //subTree-vector specific data
   int istate;          // counter over all substates
   FILE *file;          //File pointer
   int n;
   int isnetcdf;
   int locIDEBUG=0;
   CTA_Datatype datatype;
   BOOL excludefv;

   /* check handle and get data object */

   if (IDEBUG>10){printf("\nCTA_TreeVector_Export Start\n");}
   retval=CTA_Handle_Check((CTA_Handle) treevec1,CTA_TREEVECTOR);
   if (locIDEBUG>0){printf("\nCTA_TreeVector_Export1 %d\n",retval);}
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) treevec1,(void*) &data1);
   if (locIDEBUG>0){printf("\nCTA_TreeVector_Export2 %d\n",retval);}
   if (retval!=CTA_OK) return retval;

   CTA_Message_Quiet(CTA_TRUE);
   if (CTA_Handle_Check(usrdata,CTA_FILE)==CTA_OK) {
      CTA_Message_Quiet(CTA_FALSE);
      CTA_File_IsNetcdf(usrdata,&isnetcdf);
      if (isnetcdf==CTA_TRUE){
         if (locIDEBUG>10){printf("\nCTA_TreeVector_Export: NETCDF\n");}
         return CTAI_TreeVector_ExportToNetCDF(treevec1, usrdata);
      }
      else {
         if (locIDEBUG>10){printf("\nCTA_TreeVector_Export: NOT :NETCDF\n");}
         retval=CTA_File_Get(usrdata,&file);
         /* check whether this is a leaf */
         if (data1->nsubtrees==0){
            /* print tag of this state */
            fprintf(file,"%s=",data1->tag);
            /* Print vector */
            if (data1->v) {
               retval=CTA_Vector_Export(*data1->v, usrdata);
               if (retval!=CTA_OK) {
	              CTA_WRITE_ERROR("Error using CTA_Vector_Export");
	              return retval;
               }
            }
            /* print metainfo, if available */
            if (locIDEBUG>0 || 1 ) {
               if (data1->metainfo) {
                 retval=CTA_Metainfo_Export(*data1->metainfo,usrdata);
                 if (retval!=CTA_OK) {
	                CTA_WRITE_ERROR("Error using CTA_Metainfo_Export");
	                return retval;
                 }
               }
            }
         }
         else {

           /* first print metainfo of node, if available */
               if (data1->metainfo) {
                 if (locIDEBUG>1) {printf("METAINFO node: %p  \n ",data1->metainfo);}
                 retval=CTA_Metainfo_Export(*data1->metainfo,usrdata);
                 if (retval!=CTA_OK) {
	                CTA_WRITE_ERROR("Error using CTA_Metainfo_Export");
	                return retval;
                 }
               }

            /* export sub-states */
            for (istate=0;istate<data1->nsubtrees;istate++){
               /* print tag of this state */
               retval=CTA_TreeVector_Export(data1->treevecs[istate], usrdata);
               if (retval!=CTA_OK) {
	              CTA_WRITE_ERROR("Error using CTA_TreeVector_Export");
	              return retval;
               }

               /* Get data object of substate (in order to get its name) */
               retval=CTA_Handle_GetData((CTA_Handle) data1->treevecs[istate],
                                         (void*) &data2);
               if (retval!=CTA_OK) {
	              CTA_WRITE_ERROR("Cannot retrieve handle data");
	              return retval;
               }
               excludefv = data1->nocompute[istate];

               fprintf(file,"%s.%s=%s;",data1->tag,data2->tag,data2->tag);
               if (excludefv == FALSE) {
                 fprintf(file,"part %s\n",data2->tag);
               }
               else {
                 fprintf(file,"part %s, EXCLUDEFROMVECTOR\n",data2->tag);
               }
               fprintf(file,"\n");
            }
         }
      }
   }   /* next section: handle export to pack object  */
     else if (CTA_Handle_Check(usrdata,CTA_PACK)==CTA_OK){
      CTA_Message_Quiet(CTA_FALSE);
      if (locIDEBUG>0){printf("\nCTA_TreeVector_Export packing state\n");}
      retval=CTA_Pack_Add(usrdata,data1,sizeof(CTAI_TreeVec));
      if (retval!=CTA_OK) {
	     CTA_WRITE_ERROR("Error using CTA_Pack_Add");
	     return retval;
      }
      /* add metainfo, if available. This can be the case for both node and leaves. */
      if (data1->metainfo) {
        //Now pack the metainfo
        retval=CTA_Metainfo_Export(*data1->metainfo, usrdata);
        if (retval!=CTA_OK) {
             printf("cta_treevector_export: metainfo could not be added: %d\n",retval);
             return retval;
        }
      }
      if (data1->nsubtrees==0) {
         if (data1->v) {
           if (locIDEBUG>0){printf("\nCTA_TreeVector_Export packing leaf tag %s \n",data1->tag);}
           retval=CTA_Vector_GetSize(*data1->v,&n);
           if (retval!=CTA_OK) {
	          CTA_WRITE_ERROR("Cannot get size of vector");
	          return retval;
           }
           retval=CTA_Pack_Add(usrdata,&n,sizeof(int));
           if (retval!=CTA_OK) {
	          CTA_WRITE_ERROR("Error using CTA_Pack_Add");
	          return retval;
           }
           retval=CTA_Vector_GetDatatype(*data1->v,&datatype);
           if (retval!=CTA_OK) {
	          CTA_WRITE_ERROR("Cannot get datatype of vector");
	          return retval;
           }
           retval=CTA_Pack_Add(usrdata,&datatype,sizeof(CTA_Datatype));
           if (retval!=CTA_OK) {
	          CTA_WRITE_ERROR("Error using CTA_Pack_Add");
	          return retval;
           }

           if (locIDEBUG>0){printf("\nCTA_TreeVector_Export Export vector\n");}
           retval=CTA_Vector_Export(*data1->v, usrdata);
           if (retval!=CTA_OK) {
	          CTA_WRITE_ERROR("Error using CTA_Vector_Export");
	          return retval;
           }
           if (locIDEBUG>0){printf("\nCTA_TreeVector_Export Export done\n");}
         }
      } else {
         if (locIDEBUG>0){printf("\nCTA_TreeVector_Export Packing substates of %s\n",data1->tag);}

           /* add excludevector information  */
         for (istate=0;istate<data1->nsubtrees;istate++){
           retval=CTA_Pack_Add(usrdata,&data1->nocompute[istate],sizeof(int));
           if (retval!=CTA_OK) {
	          CTA_WRITE_ERROR("Error using CTA_Pack_Add");
	          return retval;
           }
		 }

         for (istate=0;istate<data1->nsubtrees;istate++){

           retval=CTA_TreeVector_Export(data1->treevecs[istate], usrdata);
           if (retval!=CTA_OK) {
	          CTA_WRITE_ERROR("Error using CTA_TreeVector_Export");
	          return retval;
           }
         }
      }
   } else {
      CTA_Message_Quiet(CTA_FALSE);
      return CTA_FORMAT_NOT_SUPPORTED;
   }
   if (IDEBUG>0){printf("CTA_TreeVector_Export End\n");}
   return CTA_OK;
};


#undef METHOD
#define METHOD "Import"
int CTA_TreeVector_Import(CTA_TreeVector treevec1, CTA_Handle usrdata){
   int retval;          //Return value of a call
   CTAI_TreeVec *data1;   //Tree-vector specific data
   int istate;          // counter over all substates
   BOOL packout, isfile;

   CTA_Datatype datatype;
   int nsubtrees;
   CTA_Vector *v;
   CTA_TreeVector  *treevecs;
   CTA_Metainfo *minfo;
   BOOL *nocompute;
   int n;
   int isnetcdf;
   char oldtag[CTA_STRLEN_TAG];
   int locIDEBUG = 0;


   if (locIDEBUG>4) {
      printf("\nCTA_TreeVector_Import: Start\n");
   }
   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec1,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) treevec1,(void*) &data1);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Cannot retrieve handle data");
      return retval;
   }

   CTA_Message_Quiet(CTA_TRUE);
   packout = (CTA_Handle_Check(usrdata,CTA_PACK)==CTA_OK);
   CTA_Message_Quiet(CTA_FALSE);
   if (packout){

      if (locIDEBUG>4) {
         printf("CTA_TreeVector_Import: importing from pack array %d \n",usrdata );
      }
      /* Store some data of current state */
      nsubtrees=data1->nsubtrees;
      v=data1->v;
      treevecs=data1->treevecs;
      nocompute=data1->nocompute;
      minfo=data1->metainfo;
      strcpy(oldtag,data1->tag);
      /* unpack the administration */
      retval=CTA_Pack_Get(usrdata,data1,sizeof(CTAI_TreeVec));
      if (retval!=CTA_OK) {
	     CTA_WRITE_ERROR("Cannot retrieve handle data");
	     return retval;
      }

      if (locIDEBUG>9) {
         printf("CTA_TreeVector_Import: data->tag=%s while original tag was %s\n ",data1->tag, oldtag);
         printf("CTA_TreeVector_Import: data->nsubtrees=%d\n",data1->nsubtrees);
         printf("CTA_TreeVector_Import: data->v      =%p\n",data1->v);
         printf("CTA_TreeVector_Import: data->metainfo      =%p\n",data1->metainfo);
      }
      
      if (strlen(oldtag)>0 && strcmp(oldtag,"unknown")!=0) { strcpy(data1->tag, oldtag) ;}
      
      /* unpack metainfo */
      if (minfo && !data1->metainfo) { // remove existing meta-info
        retval = CTA_Metainfo_Free(minfo);
      };

      if (data1->metainfo) {
        /* Check first if a metainfo of the receiving treevector already exists.
             If not, create it first. */

        if (!minfo) {
          if (locIDEBUG>4) {
            printf("receiving state has no metainfo yet \n");
            printf("old minfo: %p,  data1->metainfo: %p\n",minfo,data1->metainfo);
        }
          //CTA_Malloc?
          data1->metainfo = CTA_Malloc(sizeof(CTA_Metainfo));
          retval=CTA_Metainfo_Create(data1->metainfo);

          if (locIDEBUG>4){printf("... therefore it is created. %d \n",retval);}
        } else{ //data1->metainfo and minfo

          data1->metainfo = minfo;
        }

        retval=CTA_Metainfo_Import(*data1->metainfo,usrdata);
        if (retval!=CTA_OK) {
             printf("cta_treevector_import: could not get metainfo : %d\n",retval);
             return retval;
        }
      }


      /* unpack size and datatype of vector */
      if (data1->v) {
         retval=CTA_Pack_Get(usrdata,&n,sizeof(int));
         retval=CTA_Pack_Get(usrdata,&datatype,sizeof(CTA_Datatype));
      }
      /* do both have a vector (leaf) */
      if (!v && data1->v) {
         if (locIDEBUG>4) {
            printf("CTA_TreeVector_Import: this is a leaf\n");
         }
         /* we need to create a leaf-vector */
         data1->v=CTA_Malloc(sizeof(CTA_Vector));
         /* NOTE SIMPLIFICATION.. CREATING CTA_DEFAULT_VECTOR !! */

         retval=CTA_Vector_Create(CTA_DEFAULT_VECTOR, n, datatype, CTA_NULL, data1->v);
         if (retval!=CTA_OK) {
            CTA_WRITE_ERROR("Cannot create Vector");
            return retval;
         }
      } else if (v && !data1->v) {
         retval=CTA_Vector_Free(v);
         if (retval!=CTA_OK) {
            CTA_WRITE_ERROR("Cannot free vector");
            return retval;
         }
      } else {
         data1->v=v;
      }

      /* do we have substates */
      if (nsubtrees==data1->nsubtrees) {
         data1->treevecs=treevecs;
         data1->nocompute=nocompute;  /*CVV*/

      } else {
         /* Deallocate all substates (if there are any) */
         if (treevecs) {
            for (istate=0;istate<nsubtrees;istate++){
               retval=CTA_TreeVector_Free(&treevecs[istate],CTA_TRUE);
               if (retval!=CTA_OK) {
                  CTA_WRITE_ERROR("Cannot free TreeVector");
                  return retval;
               }
            }
            free(treevecs);
            free(nocompute);
         }
         /* create new substates */
         if (data1->nsubtrees>0) {
            data1->treevecs=CTA_Malloc(sizeof(CTA_TreeVector)*data1->nsubtrees);
            data1->nocompute=CTA_Malloc(sizeof(BOOL)*data1->nsubtrees);
            for (istate=0;istate<data1->nsubtrees;istate++){
               retval=CTA_TreeVector_Create("import_state", "unknown",
                                       &data1->treevecs[istate]);
               data1->nocompute[istate] = FALSE;
            }
         }
      }
      /* unpack all data */
      if (data1->nsubtrees>0) {
         if (locIDEBUG>0) {
            printf("CTA_TreeVector_Import: this is a node\n");
         }
        /* Unpack all substates */

        for (istate=0;istate<data1->nsubtrees;istate++){
          retval=CTA_Pack_Get(usrdata,&data1->nocompute[istate],sizeof(int));
          if (retval!=CTA_OK) {
             CTA_WRITE_ERROR("Error using CTA_Pack_Get");
             return retval;
          }
        }

        for (istate=0;istate<data1->nsubtrees;istate++){
          retval=CTA_TreeVector_Import(data1->treevecs[istate], usrdata);
          if (retval!=CTA_OK) {
             CTA_WRITE_ERROR("Error using CTA_TreeVector_Import");
             return retval;
          }
        }
      } else if (data1->v) {
         retval=CTA_Vector_Import(*data1->v,usrdata);
      }
   } else {
     CTA_Message_Quiet(CTA_TRUE);
     isfile =  (CTA_Handle_Check(usrdata,CTA_FILE)==CTA_OK) ;
     CTA_Message_Quiet(CTA_FALSE);
     if (isfile) {
       CTA_File_IsNetcdf(usrdata,&isnetcdf);
       if (isnetcdf==CTA_TRUE){
         return CTAI_TreeVector_ImportFromNetCDF(treevec1, usrdata);
       } else {
         return CTA_FORMAT_NOT_SUPPORTED;
       }
     } else {
         return CTA_FORMAT_NOT_SUPPORTED;
     }
   }
   if (locIDEBUG>4) {
      printf("CTA_TreeVector_Import: End\n");
   }
   return CTA_OK;
};



#undef METHOD
#define METHOD "Free"
int CTA_TreeVector_Free(CTA_TreeVector *treevec, int recursive){

   int retval;         //Return value of a call
   CTAI_TreeVec *data;   //Tree-vector specific data
   int istate;         //Counter over substates
   int refCount;       //Number of referencs to this treevector


   /* Check for quick return */
   if (*treevec==CTA_NULL) return CTA_OK;

   /* Get the reference count */
   refCount=1;
   CTA_Handle_GetRefCount(*treevec,&refCount);
   CTA_Handle_DecrRefCount(*treevec);

   retval=CTA_Handle_GetData((CTA_Handle) *treevec,(void*) &data);
   if (retval) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   // We are in a leaf
   if (data->nsubtrees==0){

      if (refCount<=1){
         if (data->v){
            retval=CTA_Vector_Free(data->v);
            if (retval) {
               CTA_WRITE_ERROR("Cannot free vector");
               return retval;
            }
         }
         if (data->metainfo){
            retval=CTA_Metainfo_Free(data->metainfo);
            if (retval) {
               CTA_WRITE_ERROR("Cannot free metainfo");
               return retval;
            }
         }
      }

   }
   else {

      if (recursive){
         for (istate=0;istate<data->nsubtrees;istate++){

            retval=CTA_TreeVector_Free(&data->treevecs[istate], recursive);
            if (retval) {
	           CTA_WRITE_ERROR("Cannot free treevector");
	           return retval;
            }
         }
      }


   }
   if (refCount<=1){
      if (data->v)         free(data->v);
      if (data->nocompute) free(data->nocompute);
      if (data->treevecs)  free(data->treevecs);
      free(data);
      retval=CTA_Handle_Free((CTA_Handle*) treevec);
      if (retval) {
         CTA_WRITE_ERROR("Cannot free handle");
         return retval;
      }
   }

   return CTA_OK;
};


#undef METHOD
#define METHOD "Info"
int CTA_TreeVector_Info(CTA_TreeVector treevec){

   int retval;         //Return value of a call
   CTAI_TreeVec *data;
   int istate; // counter over all substates
   int n;

   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) treevec,(void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   printf("State-vector information:\n");
   printf("tag     :%s\n",data->tag);
   printf("nsubtrees :%d\n",data->nsubtrees);
   if (data->nsubtrees>0){
      printf("--> nocompute :");
      for (istate=0;istate<data->nsubtrees;istate++){
              printf("%d ",data->nocompute[istate]);
      }
      printf("\n");
   }

   if (data->nsubtrees==0) {
      printf("leaf    :yes\n");
      n=0;
      if (data->v) {
         retval=CTA_Vector_GetSize(*data->v,&n);
         if (retval!=CTA_OK) {
	        CTA_WRITE_ERROR("Cannot get size of vector");
	        return retval;
         }
      }
      printf("dim     :%d\n",n);
   }
   else {
      for (istate=0;istate<data->nsubtrees;istate++){
         retval=CTA_TreeVector_Info(data->treevecs[istate]);
         if (retval!=CTA_OK) {
	        CTA_WRITE_ERROR("Cannot retrieve info from treevector");
	        return retval;
         }
      }
   }
   return CTA_OK;
};


#undef METHOD
#define METHOD "OpOnLeafs"
int CTA_TreeVector_OpOnLeafs(
    CTA_TreeVector treevec1,  // (First) input treevector
    CTA_TreeVector treevec2,  // If (input value == empty treevector):
                              //    output treevector
                              // Else if (input value != CTA_NULL)
                              //    second input treevector
    CTA_Func op,              // Function to be called for every leaf
    CTA_Handle arg)           // Extra arguments
{
   int retval;              //Return value of a call

   CTA_Function * operator;     // the operator given in the input
   CTAI_TreeVec *data1, *data2; // contents of the treevectors
   int istate;                  // counter over all substates
   BOOL empty2=FALSE;           // treevector2 is empty yes/no
   CTA_Vector v2;
   CTA_TreeVector * t2array;
   CTA_TreeVector t2;
   int nsubtrees;
   char tag2[CTA_STRLEN_TAG];

   int jdebug=0;
   if (jdebug)
   {
      printf("CTA_TreeVector_OpOnLeafs: start of function %d %d\n",
             treevec1, treevec2 );
   }

   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec1,CTA_TREEVECTOR);
   if (retval!=CTA_OK)
   {
       char message[1024];
	   sprintf(message,"CTA_TreeVector_OpOnLeafs: handle %d is not a TreeVector",
             treevec1);
	   CTA_WRITE_ERROR(message);
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) treevec1,(void*) &data1);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   if (treevec2 != CTA_NULL)
   {
      retval=CTA_Handle_Check((CTA_Handle) treevec2,CTA_TREEVECTOR);
      if (retval!=CTA_OK)
      {
          char message[1024];
		  sprintf(message,"CTA_TreeVector_OpOnLeafs: handle %d is not a TreeVector",
                treevec2);
		  CTA_WRITE_ERROR(message);
          return retval;
      }
      retval=CTA_Handle_GetData((CTA_Handle) treevec2,(void*) &data2);
      if (retval!=CTA_OK) {
	     CTA_WRITE_ERROR("Cannot retrieve handle data");
	     return retval;
      }
   }

   if (jdebug)
   {
      printf("CTA_TreeVector_OpOnLeafs: input is OK");
   }

   /* consistency check */
   if (treevec2 == CTA_NULL)
   {
      // There is no second treevector
   }
   if (data2->nsubtrees==0 && data2->v == NULL)
   {
      // The second treevector is an empty treevector: fill it
      //     with the output.
      empty2 = TRUE;
   }
   else if (data1->nsubtrees!=data2->nsubtrees)
   {
      return CTA_TREEVECTORS_NOT_COMPATIBLE;
   }

   if (data1->nsubtrees==0)
   {
      /* No subtrees: this treevector is a leaf treevector
         call operator */
      retval = CTA_Func_GetFunc(op, &operator);
      if (retval!=CTA_OK) {
	     CTA_WRITE_ERROR("Error using CTA_Func_GetFunc");
	     return retval;
      }

      if (treevec2 == CTA_NULL)
      {
         operator(data1->tag, *data1->v, CTA_NULL, arg, &retval);
         if (retval!=CTA_OK) {
	        CTA_WRITE_ERROR("Error using operator ...");
	        return retval;
         }
      }
      else if (empty2)
      {
         operator(data1->tag, *data1->v, &v2, arg, &retval);
         if (retval!=CTA_OK)
         {
            char message[1024];
			sprintf(message, "CTA_TreeVector_OpOnLeafs: operator %d kapot",op);
			CTA_WRITE_ERROR(message);
            return retval;
         }
         if (jdebug)
         {
           printf("setting vector %d into treevector %d\n",treevec2,v2);
         }
         retval = CTA_TreeVector_SetVec(treevec2, v2);
         if (retval!=CTA_OK) {
            CTA_WRITE_ERROR("Cannot set vector in treevector");
            return retval;
         }
      }
      else
      {
         operator(data1->tag, *data1->v, *data2->v, arg, &retval);
         if (retval!=CTA_OK) {
            CTA_WRITE_ERROR("Error using operator ...");
            return retval;
         }
      }
   }
   else
   {
      nsubtrees = data1->nsubtrees;
      if (empty2)
      {
         if (jdebug)
         {
            printf("CTA_TreeVector_OpOnLeafs: creating %d subleafs\n",
                   nsubtrees);
         }
         t2array = CTA_Malloc(nsubtrees*sizeof(CTA_TreeVector));
      }

      /* recursion for all sub-states */
      for (istate=0; istate<nsubtrees; istate++)
      {
         if (treevec2==CTA_NULL)
         {
            t2 = CTA_NULL;
         }
         else if (empty2)
         {
            retval=CTA_TreeVector_GetTag(data1->treevecs[istate], tag2);
            if (retval!=CTA_OK) {
               CTA_WRITE_ERROR("Cannot get tag from treevector");
               return retval;
            }

            if (jdebug)
            {
               printf("CTA_TreeVector_OpOnLeafs: creating subleaf %d: '%s'\n",
                      istate, tag2);
            }

            retval=CTA_TreeVector_Create( tag2, tag2, &t2array[istate]);
            if (retval!=CTA_OK) {
               CTA_WRITE_ERROR("Cannot create TreeVector");
               return retval;
            }

            t2 = t2array[istate];
         }
         else
         {
            t2 = data2->treevecs[istate];
         }

         retval=CTA_TreeVector_OpOnLeafs(
                        data1->treevecs[istate], t2, op, arg);
         if (retval!=CTA_OK) {
            CTA_WRITE_ERROR("Error using CTA_TreeVector_OpOnLeafs");
            return retval;
         }
      }

      if (empty2)
      {
         if (jdebug)
         {
            printf("CTA_TreeVector_OpOnLeafs: concatenatng %d leafs\n",
                      nsubtrees);
         }
         retval=CTA_TreeVector_Conc( treevec2, t2array, nsubtrees);
         if (retval!=CTA_OK) {
            CTA_WRITE_ERROR("Error using CTA_TreeVector_COnc");
            return retval;
         }

         free(t2array);
      }
   }
   return CTA_OK;
}


#undef METHOD
#define METHOD "ElmSqrt"
int CTA_TreeVector_ElmSqrt(CTA_TreeVector y){

   int retval;         //Return value of a call
   CTAI_TreeVec *data;
   int istate; // counter over all substates

   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) y ,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) y,(void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   if (data->nsubtrees==0)
   {
      retval=CTA_Vector_ElmSqrt(*(data->v));
      if (retval!=CTA_OK) {
	     CTA_WRITE_ERROR("Error using CTA_Vector_ElmSqrt");
	     return retval;
      }

   } else {
      /* recursion for all sub-states */
      for (istate=0;istate<data->nsubtrees;istate++){
         retval=CTA_TreeVector_ElmSqrt(data->treevecs[istate]);
         if (retval!=CTA_OK) {
	        CTA_WRITE_ERROR("Error using CTA_TreeVector_ElmSqrt");
	        return retval;
         }
      }
   }
   return CTA_OK;
}


void CTAI_Treevector_Operation_ScaledRMS(
     char *tag,
     CTA_Vector v1,
     CTA_Vector vscal,
     CTA_Handle hdum, int *retval)
{

   int jdebug;
   CTA_Vector vwrk;
   double dv2, ds2, drms;

   jdebug=0;

   if (jdebug){
     printf("CTAI_Treevector_Operation_ScaledRMS computing rms ");
     printf("of leaf of treevector\n");
   }

   /* create working copy of the vector:     vwrk = v1; */
   *retval=CTA_Vector_Duplicate(v1,&vwrk);
   if (*retval!=CTA_OK) return;

   /* Scale the vector                       vwrk = v1.*vscal */
   *retval=CTA_Vector_ElmProd(vwrk,vscal);
   if (*retval!=CTA_OK) return;

   /* compute sum of squares  */
       /*  dv2 = | vwrk |^2 = sum( (v1 .* vscal).^2); */
   *retval=CTA_Vector_Nrm2(vwrk,&dv2);
   if (*retval!=CTA_OK) return;
   dv2=dv2*dv2;

   /* Delete work vector */
   *retval=CTA_Vector_Free(&vwrk);
   if (*retval!=CTA_OK) return;

   /* compute scaling vector's sum-of-squares */
       /*  ds2 = | vscal |^2 = sum( vscal.^2); */
   *retval=CTA_Vector_Nrm2(vscal,&ds2);
   if (*retval!=CTA_OK) return;
   ds2=ds2*ds2;

   if (jdebug)
   {
     printf("scaling vector's sum-of-squares is %g\n",ds2);
     printf("rms=sqrt(%g/%g)\n",dv2,ds2);
   }

   /* compute RMS */
   drms=sqrt(dv2/ds2);

   /* output the RMS */
   printf("  %32s: %18.8le\n", tag, drms);
}



void CTAI_Treevector_Operation_ScaledSSQ(
     char *tag,
     CTA_Vector v1,
     CTA_Vector vscal,
     CTA_Handle hdum, int *retval)
{

   int jdebug;
   CTA_Vector vwrk;
   double dv2;

   jdebug=0;

   if (jdebug){
     printf("CTAI_Treevector_Operation_ScaledSSQ computing ");
     printf("ssq of leaf of treevector\n");
   }

   /* create working copy of the vector:     vwrk = v1; */
   *retval=CTA_Vector_Duplicate(v1,&vwrk);
   if (*retval!=CTA_OK) return;

   /* Scale the vector                       vwrk = v1.*vscal */
   *retval=CTA_Vector_ElmProd(vwrk,vscal);
   if (*retval!=CTA_OK) return;

   /* compute sum of squares  */
       /*  dv2 = | vwrk |^2 = sum( (v1 .* vscal).^2); */
   *retval=CTA_Vector_Nrm2(vwrk,&dv2);
   if (*retval!=CTA_OK) return;
   dv2=dv2*dv2;

   /* Delete work vector */
   *retval=CTA_Vector_Free(&vwrk);
   if (*retval!=CTA_OK) return;

   /* output the SSQ */
   printf("  %32s: %18.8le\n", tag, dv2);
}






void CTAI_Treevector_Operation_Amax(
     char *tag,
     CTA_Vector vin,
     CTA_Vector *vout,
     CTA_Handle hdum, int *retval)
{

   int jdebug=0;
   int iloc;

   if (jdebug){
     printf("CTAI_Treevector_Operation_Amax ");
     printf("finding maxabs of leaf of treevector %d\n",vin);
   }

   *retval = CTA_Vector_Amax(vin, &iloc);
   if (*retval!=CTA_OK) return;

   if (jdebug){
     printf("CTAI_Treevector_Operation_Amax ");
     printf("maxabs found at %d\n",iloc);
   }

   *retval = CTA_Vector_Create( CTA_DEFAULT_VECTOR, 1, CTA_INTEGER,
                                CTA_NULL, vout);
   if (*retval!=CTA_OK) return;

   *retval = CTA_Vector_SetVals( *vout, &iloc, 1, CTA_INTEGER);
   if (*retval!=CTA_OK) return;

   if (jdebug){
     printf("CTAI_Treevector_Operation_Amax: succesful ending\n");
     *retval = CTA_Vector_Export( *vout, CTA_FILE_STDOUT);
     if (*retval!=CTA_OK) return;
   }

}

void CTAI_Treevector_Operation_PrintEntry(
     char *tag,
     CTA_Vector vin,
     CTA_Vector viloc,
     CTA_Handle hdum, int *retval)
{

   int jdebug;
   double value;
   int iloc;
   double NaN;

   // Create a NaN
   NaN = 0; NaN = 1.0/NaN; NaN=NaN/NaN;


   jdebug=0;

   if (jdebug){
     printf("CTAI_Treevector_Operation_PrintEntry ");
     printf("print an entry of leaf of treevector\n");
   }

   *retval = CTA_Vector_GetVal(viloc, 1, &iloc, CTA_INTEGER);
   if (*retval!=CTA_OK) return;

   if (iloc==0){
           value=NaN;
   }
   else {
      *retval = CTA_Vector_GetVal(vin, iloc, &value, CTA_DOUBLE);
      if (*retval!=CTA_OK) return;
   }

   /* output the location and value */
   printf("  %32s(%6d): %18.8le\n", tag, iloc, value);

   if (jdebug){
     printf("CTAI_Treevector_Operation_PrintEntry: succesful ending\n");
   }

}









#undef METHOD
#define METHOD "Gemm"
int CTA_TreeVector_Gemm(CTA_TreeVector *hC, int nc, int transa, int transb, double alpha, CTA_TreeVector *hA, int na,
                   CTA_Matrix mB, double beta){
   int ma, mb, mc, nb, m;
   double *B, fac;
   double nul=0.0;
   int ierr, ic, i, j, indx, ia;

   if (IDEBUG>10) {printf("debug: entering cta_treevector_gemm\n");}

   /* check dimensions */
   if (transa) return CTA_NOT_YET_SUPPORTED;

   ierr=CTA_TreeVector_GetSize(hC[0],&mc);
   for (ic=1;ic<nc;ic++){
      ierr=CTA_TreeVector_GetSize(hC[ic],&m); if (ierr!=CTA_OK) return ierr;
      if (m!=mc) {
         printf("Error in CTA_TreeVector_Gemm: Not all treevectors in hC have the same length\n");
         printf("First treevector has length %d and %d-th treevector has length %d\n",mc, ic+1, m);
         return CTA_DIMENSION_ERROR;
      }
   }

   ierr=CTA_TreeVector_GetSize(hA[0],&ma);
   for (ia=1;ia<na;ia++){
      ierr=CTA_TreeVector_GetSize(hA[ia],&m); if (ierr!=CTA_OK) return ierr;
      if (m!=ma){
         printf("Error in CTA_TreeVector_Gemm: Not all treevectors in hA have the same length\n");
         printf("First treevector has length %d and %d-th treevector has length %d\n",ma, ic+1, m);
         return CTA_DIMENSION_ERROR;
      }
   }


   /* Get dimension of B */
   if (transb) {
      ierr=CTA_Matrix_GetSize(mB,&nb,&mb);
   } else {
      ierr=CTA_Matrix_GetSize(mB,&mb,&nb);
   }
   if (ierr!=CTA_OK) {
       CTA_WRITE_ERROR("Cannot get dimension of matrix B");
       return ierr;
   }
   if (mc!=ma || nc!=nb || na!=mb) return CTA_DIMENSION_ERROR;

   /* NOTE A LOT OF OPTIMISATION IS POSSIBLE THIS VERSION WILL JUST MAKE IT WORK */
   B=CTA_Malloc(mb*nb*sizeof(double));
   if (transb) {
      ierr=CTA_Matrix_GetVals(mB,B,nb,mb,CTA_DOUBLE);
   } else {
      ierr=CTA_Matrix_GetVals(mB,B,mb,nb,CTA_DOUBLE);
   }
   if (ierr!=CTA_OK) {
       CTA_WRITE_ERROR("Cannot get values from matrix");
       return ierr;
   }

   if (beta== 0.0) {
      /* Zero C if beta==0.0 (avoiding problems with NaN and Inf in state) */
      for (j=0;j<nc;j++){
         ierr=CTA_TreeVector_SetConstant(hC[j],&nul, CTA_DOUBLE);
      }
   } else {
      /* Scale C with beta  */
      for (j=0;j<nc;j++){
         ierr=CTA_TreeVector_Scal(hC[j],beta);
		 if (ierr!=CTA_OK) {
            CTA_WRITE_ERROR("Cannot scale treevector");
            return ierr;
         }
      }
   }

  for (j=0;j<nc;j++){
     /* Compute one column of C */
     for (i=0;i<mb;i++){
        if (transb) {
           indx=i*nb+j;
        } else {
           indx=j*mb+i;
        }
        fac=alpha*B[indx];
        ierr=CTA_TreeVector_Axpy(hC[j],fac,hA[i]);
		if (ierr!=CTA_OK) {
            CTA_WRITE_ERROR("Error using CTA_TreeVector_Axpy");
            return ierr;
        }
     }
  }

  if (IDEBUG>10) {printf("debug: cta_treevector_gemm  END \n");}

  free(B);
  return CTA_OK;
}


/* -------------------------- */
void XML_newline(int spc, xmlTextWriter *writer){
  int i;
  char spaces[80];
  char str2[5];
  strcpy(str2," ");
  strcpy(spaces,"");

  for (i=0; i<spc; i++){
    strcat(spaces,str2);
  }
  xmlTextWriterWriteString(writer, (xmlChar *) "\n");
  xmlTextWriterWriteString(writer, (xmlChar *) spaces);

}



/** \brief Generate XML from one COSTA state vector
*
*  \param hvec   I  handle of a COSTA state vector
*  \param writer I  the XML text writer
*/
void CTAI_XML_WriteTreeVector(int issubtree, int isxfv,CTA_TreeVector treevec, int level,xmlTextWriter *writer) {
  /* issubtree: flag if this is not the root (so 'subtreevector' should be written instead of 'treevector'  */
  /* isxfv: flag: for excludefromvector. It is always false for the root.       */
   CTAI_TreeVec *data;   //Tree-vector specific data
   CTA_TreeVector subtreevector;
   CTAI_TreeVec *subdata; // subTree-vector specific data
   int isub;
   int subisxfv;
   char *desctext = NULL;

   /* check handle and get data object */
   CTA_Handle_Check((CTA_Handle) treevec,CTA_TREEVECTOR);
   CTA_Handle_GetData((CTA_Handle) treevec,(void*) &data);


   /* Start an element the the name of the tree handle */
   XML_newline(level,writer);
   if (issubtree == 1){
     xmlTextWriterStartElement(writer, (xmlChar *) CTAI_Type2String(CTA_SUBTREEVECTOR));
   } else{
     xmlTextWriterStartElement(writer, (xmlChar *) CTAI_Type2String(CTA_TREEVECTOR));
   }
   /* COSTA-id is not written to file */


   /* Write tag */
   xmlTextWriterWriteAttribute(writer, CTAI_XML_ID,(xmlChar *) data->tag);

   /* Write name */
   xmlTextWriterWriteAttribute(writer, CTAI_XML_CAPTION, (xmlChar *) data->name);

   /* write excludefromvector. this information is available as argument! */
   if (isxfv) {
     xmlTextWriterWriteAttribute(writer, CTAI_XML_EXCLUDEFROMVECTOR, (xmlChar *) "true" );
   }

   level = level + 3; XML_newline(level,writer);
   xmlTextWriterStartElement(writer, (xmlChar *) "description");

   /* Check for meta-info */
   if (data->metainfo) {
     CTA_Metainfo_GetDescription(*data->metainfo,&desctext);
     xmlTextWriterWriteString(writer, (xmlChar *) desctext);
     xmlTextWriterEndElement(writer); XML_newline(level,writer);

   } else {
   /* write (empty) description */
     xmlTextWriterWriteString(writer, (xmlChar *) "no description yet");
     xmlTextWriterEndElement(writer); XML_newline(level,writer);
   }

   /* If we have a vector write the vector */
   if (data->v) {
     CTAI_XML_WriteVector(*data->v, "","",*data->metainfo,level+3, writer);
   } else {
     for (isub=0;isub<data->nsubtrees;isub++){
       subtreevector = data->treevecs[isub];
       /* determine nocompute of substate */
       subisxfv = 0;
       if (data->nocompute[isub] == TRUE) {subisxfv = 1;}

       CTA_Handle_Check((CTA_Handle) subtreevector,CTA_TREEVECTOR);
       CTA_Handle_GetData((CTA_Handle) subtreevector,(void*) &subdata);
       /* If the substate is a leaf, write the vector, i.e. skip the statelevel */
       if (subdata->v) {

         if (subdata->metainfo) {
           //         printf("!!!calling xml_writevector; sub-metainfo: %s %d\n",subdata->tag, *subdata->metainfo);
           CTAI_XML_WriteVector(*subdata->v, subdata->tag, subdata->name,*subdata->metainfo,level+3, writer);
         //  here: possibly add state for coordinates
         }
         if (!subdata->metainfo) {
           //    printf("!!!calling xml_writevector; NO sub-metainfo: %s %d\n",subdata->tag,subdata->metainfo);
           CTAI_XML_WriteVector(*subdata->v, subdata->tag, subdata->name,0,level+3,writer);}

       } else {
         CTAI_XML_WriteTreeVector(1,subisxfv,subtreevector, level, writer);
       }
     }
   }
   level=level-3;XML_newline(level,writer); xmlTextWriterEndElement(writer);
   XML_newline(level,writer);
}


/* -------------------------------------------------------------------- */
/** \OBSOLETE brief Generate XML from one COSTA state vector
*
*  \param hvec   I  handle of a COSTA state vector
*  \param writer I  the XML text writer
*/
void CTAI_XML_WriteState(CTA_TreeVector treevec, xmlTextWriter *writer) {
   CTAI_TreeVec *data;   //Tree-vector specific data
   const char* name;
   int isub;

   /* check handle and get data object */
   CTA_Handle_Check((CTA_Handle) treevec,CTA_TREEVECTOR);
   CTA_Handle_GetData((CTA_Handle) treevec,(void*) &data);

   /* Check for meta-info */
   if (data->metainfo) {
      printf("WARNING in CTAI_XML_WriteState\n");
      printf("State-vector contains META information\n");
      printf("XML-export for META information is not (yet) available \n");
   }

   /* Start an element the the name of the tree handle */
   xmlTextWriterStartElement(writer, (xmlChar *) CTAI_Type2String(CTA_TREEVECTOR));

   /* Write id (if any) */
   name = CTAI_Handle_GetName(treevec);
   if (name && *name) {
      xmlTextWriterWriteAttribute(writer, CTAI_XML_ID, (xmlChar *) name);
   }

   /* Write name */
   xmlTextWriterWriteAttribute(writer, CTAI_XML_NAME, (xmlChar *) data->name);

   /* Write tag */
   xmlTextWriterWriteAttribute(writer, CTAI_XML_TAG, (xmlChar *) data->tag);

   /* If we a vector write the vector */
   if (data->v) {
     CTAI_XML_WriteVector(*data->v,"", "", CTA_NULL, 0,writer);
   } else {
     for (isub=0;isub<data->nsubtrees;isub++){

        CTAI_XML_WriteState(data->treevecs[isub], writer);
     }
   }
   xmlTextWriterEndElement(writer);
}

//------------------------------

/** \brief Create a treeVector from XML.
*
*  \param cur_node  I  Current XML node
*  \return             Handle to create or CTA_NULL in case of an error.
*/
CTA_TreeVector CTAI_XML_CreatetreeVector(xmlNode *cur_node) {

   CTA_TreeVector     treevec;                  /* new state vector */
   CTA_Vector    hsubvec;
   xmlChar       *name = NULL;            /* name of the vector to create */
   xmlChar       *tag;                    /* element/property value */
   xmlNode       *sub_node = NULL;        /* values child node */
   xmlNode       *txt_node = NULL;        /* values child node */
   int           nsubstates;              /* number of sub state vectors */
   int           nsubvectors;              /* number of subvectors */
   CTA_TreeVector     *substates;              /* Array of handles of sub state vectors */
   int           isub;                    /* loop counter in substates */
   xmlChar       *excludefromvector = NULL;            /* name of the vector to create */
   char          *desctext = NULL;
   CTA_Metainfo  minfo;
   CTA_Metainfo  sub_minfo;
   BOOL          *excludefv;
   /* Parse this node's attributes */
   /* Get id */


   /* Get name */
   name = xmlGetProp(cur_node, CTAI_XML_CAPTION);
   if (!name){
     name= (xmlChar*) CTA_Malloc(5*sizeof(xmlChar));
     strcpy((char *) name, (char *) "none");
   }

   /* Get tag */
   tag = xmlGetProp(cur_node, CTAI_XML_ID);

   /* Create the new state-vector */
   CTA_TreeVector_Create((char *) name, (char *) tag, &treevec);

   /* Create metainfo */
   CTA_Metainfo_Create(&minfo);

   /* Set id (=name) of handle */
   CTAI_Handle_SetName(treevec, "tree-vector");

   /* Look for a Vector */
   /* Count number of "TREEVECTOR"-elements */
   nsubstates=0; nsubvectors=0;
   for (sub_node = cur_node->children; sub_node; sub_node = sub_node->next) {
     if (0 == strcmp("description",(char *) sub_node->name)) {

       for (txt_node = sub_node->children; txt_node; txt_node = txt_node->next) {
         if (txt_node->type == XML_TEXT_NODE) {
           if (desctext !=NULL) {
             free(desctext);
           }
           desctext = (char *) CTA_Malloc((1+strlen((char *) txt_node->content))*sizeof(char));
           strcpy( desctext, (char *) txt_node->content);
           CTA_Metainfo_SetDescription(minfo, (char *) txt_node->content);

           // controle
           //     retval = CTA_Metainfo_GetDescription(minfo,&desctext);
           // printf("controle: desctext: %s\n",desctext);
           // free(desctext);
           // free(minfo);
         }
       }
       /* attach the metainfo (with only the description) to this state */
       /* note: grid and unit information are  in a metainfo on a lower level: */
       /* in the state where the actual vector is */
       CTA_TreeVector_SetMetainfo(treevec, minfo);

     }
     else if (CTA_VECTOR == CTAI_String2Type((char *) sub_node->name)) {
       /* if a treeVectorLeaf is found */
       // now COSTA needs to build an extra state-level at this point.
       nsubvectors++;
     }
     else if (CTA_TREEVECTOR == CTAI_String2Type((char *) sub_node->name)) {
       nsubstates++;
     } else {
        /* continue */
     }
   }


   isub = 0;
   if (nsubvectors+nsubstates>0) {
     substates=CTA_Malloc((nsubvectors+nsubstates)*sizeof(CTA_TreeVector));
     excludefv=CTA_Malloc((nsubvectors+nsubstates)*sizeof(BOOL));

     /* first, loop along all  subvectors (COSTA-states with one vector)
      We need therefore to make an extra state  */
     if (nsubvectors>0) {
       for (sub_node = cur_node->children; sub_node; sub_node = sub_node->next) {

         if (CTA_VECTOR == CTAI_String2Type((char *) sub_node->name)) {

           /* Get name */
           name = xmlGetProp(sub_node, CTAI_XML_CAPTION);
           if (!name){
             name= (xmlChar *) CTA_Malloc(5*sizeof(xmlChar)); strcpy((char *) name,"none");
           }

           /* Get tag */
           tag = xmlGetProp(sub_node, CTAI_XML_ID);

           /* get excludefromVector */
           excludefromvector = xmlGetProp(sub_node, CTAI_XML_EXCLUDEFROMVECTOR);
           excludefv[isub] = FALSE;

           if (excludefromvector) {    //it exists
             if (0 == strcmp("true", (char *) excludefromvector)) {
               excludefv[isub] = TRUE;
             }
           }


           CTA_Metainfo_Create(&sub_minfo);
           hsubvec = CTAI_XML_CreateVector_New(sub_node,sub_minfo);

           /* now make the  extra state to  attach the vector and the metainfo */
           /* essentially, we create an extra layer that the xml-description does not need */
           CTA_TreeVector_Create((char *) name, (char *) tag, &substates[isub]);
           CTA_TreeVector_SetVec(substates[isub],hsubvec);

           /* here the metainfo with grid should be set */
           CTA_TreeVector_SetMetainfo(substates[isub], sub_minfo);

           isub++;

        }
       }

     }

     /* now, add the treevectorsleafs to substatepositions isub+1..(nsubvectors+nsubstates)
        (these are COSTA substates)  */
     if (nsubstates>0) {
       for (sub_node = cur_node->children; sub_node; sub_node = sub_node->next) {
         if (CTA_TREEVECTOR == CTAI_String2Type((char *) sub_node->name)) {

           /* get excludefromVector */
           excludefromvector = xmlGetProp(sub_node, CTAI_XML_EXCLUDEFROMVECTOR);
           excludefv[isub] = FALSE;
           if (excludefromvector) {    //it exists
             if (0 == strcmp("true", (char *) excludefromvector)) {
               excludefv[isub] = TRUE;
             }
           }

           substates[isub]= CTAI_XML_CreatetreeVector(sub_node);
           isub++;
         }
       }

     }
     CTA_TreeVector_Conc(treevec, substates, nsubvectors+nsubstates);
     /* fill the excludefromvector list */
     CTAI_TreeVector_ExcludeFromVector(treevec,excludefv);
     // CTA_TreeVector_Export(treevec,CTA_FILE_STDOUT);
     free(substates);
   }

   xmlFree(name);
   xmlFree(tag);
   xmlFree(excludefromvector);

   return treevec;
}
//------------------------
/* sweep through the treevector and fill the regular grids using the references to its own
   (nocompute)-coordinate-vectors */
int CTAI_XML_TreeVector_regular_grid(CTA_TreeVector treevec, CTA_TreeVector root ){
  int retval, istate;
  CTA_TreeVector coords;
  CTAI_TreeVec *data;   //Tree-vector specific data
  CTAI_Gridm thisgrid;
  char *tag0,*tag;
  int ndims_phys, j,ncoords, found;
  double *vals = NULL;

  /* Parse the whole tree and look for grids */

   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec,CTA_TREEVECTOR);
   if (retval!=CTA_OK) return retval;
   retval=CTA_Handle_GetData((CTA_Handle) treevec,(void*) &data);
   if (retval!=CTA_OK) return retval;

   if (data->metainfo) {
     retval = CTA_Metainfo_GetGrid(*data->metainfo,&thisgrid);
     if (thisgrid.type > -4 && thisgrid.type <0) {  // reference to coords; not yet filled in
       ndims_phys = -thisgrid.type;

       /* the first dimension (x) */
       tag0 =  strrchr(thisgrid.refdimp[1],'/');
       if (tag0 == NULL) {
         tag = thisgrid.refdimp[1];}
       else {
         tag = tag0 + 1 ;
       }
       //       printf("ctai-xml-tv_reg_grid: searching for tag |%s| \n",tag);
       retval = CTA_TreeVector_GetSubTreeVec(root, tag, &coords);
       if (retval !=  CTA_OK) {
         printf("ctai_xml_treevector_regular_grid: tag %s not found! \n",tag);exit(-1);
       }
       retval = CTA_TreeVector_GetSize(coords,&ncoords);
       if (ncoords != thisgrid.nsize) {
         printf("ctai_xml_treevector_regular_grid: number of coordinates %d is wrong! \n",ncoords);exit(-1);
       }
       vals = CTA_Malloc(ncoords*sizeof(double));  // allocate for first time
       retval = CTA_TreeVector_GetVals(coords, vals, ncoords, CTA_DOUBLE);
       thisgrid.x_origin = vals[0];
       for (j=2; j<= ncoords; j++) {
         if (abs(vals[j-1] - vals[0]) < 1.0E-5) {break;}
       }
       thisgrid.nx = j-1;
       thisgrid.dx =  vals[1]-vals[0];
       if (j >= ncoords) {
         if (ndims_phys > 1) {printf("for regular grids a repeating x-sequence is required!\n"); exit(-1);}
         thisgrid.nx=ncoords;
       }

       /* now, the second dimension (y) */
       if (ndims_phys > 1) {

         tag0 =  strrchr(thisgrid.refdimp[2],'/') ;
         if (tag0 == NULL) {
           tag = thisgrid.refdimp[1];}
         else {
           tag = tag0 + 1 ;
         }
         // printf("ctai-xml-tv_reg_grid: searching for tag |%s| \n",tag);
         retval = CTA_TreeVector_GetSubTreeVec(root, tag, &coords);
         if (retval !=  CTA_OK) {
           printf("ctai_xml_treevector_regular_grid: tag %s not found! \n",tag);
           exit(-1);
         }
         retval = CTA_TreeVector_GetSize(coords,&ncoords);
         if (ncoords != thisgrid.nsize) {
           printf("ctai_xml_treevector_regular_grid: number of coordinates %d is wrong! \n",ncoords);exit(-1);
         }
         retval = CTA_TreeVector_GetVals(coords, vals, ncoords, CTA_DOUBLE);
         thisgrid.y_origin = vals[0];
         found = 0;
         for (j=1; j <  ncoords/thisgrid.nx; j++) {
           if (abs(vals[0+j*thisgrid.nx] - vals[0]) < 1.0E-5) {found=1; break;}
         }
         if (found == 1){
           thisgrid.ny = j;
         } else {
           thisgrid.ny = ncoords/thisgrid.nx;
           if (ndims_phys > 2) {printf("for regular grids a repeating y-sequence is required!\n"); exit(-1);}
         }
         thisgrid.dy =  vals[0+thisgrid.nx]-vals[0];
       }
       /* now, the third and last dimension (z) */
       if (ndims_phys > 2) {
         tag0 =  strrchr(thisgrid.refdimp[3],'/') ;
         if (tag0 == NULL) {
           tag = thisgrid.refdimp[1];}
         else {
           tag = tag0 + 1 ;
         }
         //printf("ctai-xml-tv_reg_grid: searching for tag |%s| \n",tag);
         retval = CTA_TreeVector_GetSubTreeVec(root, tag, &coords);
         if (retval !=  CTA_OK) {
           printf("ctai_xml_treevector_regular_grid: tag %s not found! \n",tag);
           exit(-1);
         }
         retval = CTA_TreeVector_GetSize(coords,&ncoords);
         if (ncoords != thisgrid.nsize) {
           printf("ctai_xml_treevector_regular_grid: number of coordinates %d is wrong! \n",ncoords);exit(-1);
         }
         retval = CTA_TreeVector_GetVals(coords, vals, ncoords, CTA_DOUBLE);
         thisgrid.z_origin = vals[0];
         found = 0;
         for (j=2; j <= thisgrid.nx*thisgrid.ny; j++) {
           if (abs(vals[j-1] - vals[0]) > 1.0E-5) {found=1;}
         }
         if (found == 0){
           thisgrid.nz =  ncoords / (thisgrid.nx*thisgrid.ny);
         } else {
              printf("for regular grids a repeating y-sequence is required!\n"); exit(-1);
         }
         thisgrid.dz =  vals[0+thisgrid.nx*thisgrid.ny]-vals[0];

       }
       thisgrid.type = ndims_phys;
       retval = CTA_Metainfo_SetGrid(*data->metainfo,&thisgrid);
       //       retval = CTA_Metainfo_Copy(minfo,*data->metainfo);
     }
   }




   /* parse subtrees */
   for (istate=0;istate<data->nsubtrees;istate++){
     retval = CTAI_XML_TreeVector_regular_grid(data->treevecs[istate], root);
   }
   return CTA_OK;
}


//---------------------------
int CTAI_TreeVector_ExcludeFromVector(CTA_TreeVector treevec, BOOL *excludefv){

   int retval;         //Return value of a call
   CTAI_TreeVec *data;
   int istate; // counter over all substates


   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec,CTA_TREEVECTOR);
   if (retval!=CTA_OK) return retval;
   retval=CTA_Handle_GetData((CTA_Handle) treevec,(void*) &data);
   if (retval!=CTA_OK) return retval;

   if (data->nsubtrees==0) {
      printf("CTAI:TreeVector_ExcludeFromVector: ERROR: no substates available.\n");
      return CTA_CONCAT_NOT_POSSIBLE;
   }
   else {
      for (istate=0;istate<data->nsubtrees;istate++){
        data->nocompute[istate] = excludefv[istate];
      }
      //  printf("ctai_treevec_XFV: state:  %s %d %d %d %d \n",data->tag,data->nsubtrees,data->nocompute[0],data->nocompute[1],data->nocompute[2]);
   }
   return CTA_OK;
};



#undef METHOD
#define METHOD "IncRefCount"
int CTA_TreeVector_IncRefCount(CTA_TreeVector treevec){

   int retval;         //Return value of a call
   CTAI_TreeVec *data1;  //Tree-vector specific data
   int istate;         // counter over all substates

   /* check handle and get data object */
   retval=CTA_Handle_Check((CTA_Handle) treevec,CTA_TREEVECTOR);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
      return retval;
   }

   /* Increase reference count */
   CTA_Handle_IncRefCount((CTA_Handle) treevec);


   retval=CTA_Handle_GetData((CTA_Handle) treevec,(void*) &data1);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* Increase reference count of all sub treevectors as well */
   for (istate=0;istate<data1->nsubtrees;istate++){
      retval=CTA_TreeVector_IncRefCount(data1->treevecs[istate]);
      if (retval!=CTA_OK) {
         CTA_WRITE_ERROR("Error setting reference count");
         return retval;
      }

   }
   return CTA_OK;
};
















//----------------------------

/* Interfacing with Fortran */
CTAEXPORT void CTA_TREEVECTOR_CREATE_F77(char *name, char *tag, int *treevec, int *ierr,
                       int len_name, int len_tag){

  char  *c_name;
  char  *c_tag;
  // create a c-string equivalent to name
  c_name=CTA_Malloc((len_name+1)*sizeof(char));
  CTA_fstr2cstr(name,c_name,len_name);
  // create a c-string equivalent to tag
  c_tag=CTA_Malloc((len_tag+1)*sizeof(char));
  CTA_fstr2cstr(tag,c_tag,len_tag);

   *ierr=CTA_TreeVector_Create(c_name, c_tag, (CTA_TreeVector*) treevec);

   free(c_name);
   free(c_tag);

};

CTAEXPORT void CTA_TREEVECTOR_DUPLICATE_F77(int *treevec1, int *treevec2, int *ierr ){
   *ierr=CTA_TreeVector_Duplicate((CTA_TreeVector) *treevec1, (CTA_TreeVector*) treevec2 );
};


CTAEXPORT void CTA_TREEVECTOR_CONC_F77(int *treevec1, int *treevecs, int *nsubtrees, int *ierr){

   *ierr=CTA_TreeVector_Conc((CTA_TreeVector) *treevec1, (CTA_TreeVector*) treevecs, *nsubtrees);
};


CTAEXPORT void CTA_TREEVECTOR_GETSUBTREEVEC_F77(int *treevec, char *tag, int *hsubstate, int *ierr,
                          int len_tag){
   char  *c_tag;
   // create a c-string equivalent to tag
   c_tag=CTA_Malloc((len_tag+1)*sizeof(char));
   CTA_fstr2cstr(tag,c_tag,len_tag);

   *ierr=CTA_TreeVector_GetSubTreeVec((CTA_TreeVector) *treevec, c_tag,
                               (CTA_TreeVector*) hsubstate) ;
   free(c_tag);
};

CTAEXPORT void CTA_TREEVECTOR_SETSUBTREENOCOMPUTE_F77(int *treevec, char *tag, int *ierr,
                          int len_tag){
   char  *c_tag;
   // create a c-string equivalent to tag
   c_tag=CTA_Malloc((len_tag+1)*sizeof(char));
   CTA_fstr2cstr(tag,c_tag,len_tag);

   *ierr=CTA_TreeVector_SetSubTreeNocompute((CTA_TreeVector) *treevec, c_tag) ;
   free(c_tag);
};


CTAEXPORT void CTA_TREEVECTOR_GETSUBTREEVECINDEX_F77(int *treevec, int *index, int *hsubstate, int *ierr){

   *ierr=CTA_TreeVector_GetSubTreeVecIndex((CTA_TreeVector) *treevec, *index,
                               (CTA_TreeVector*) hsubstate) ;
};

CTAEXPORT void CTA_TREEVECTOR_GETSUBTREEVECID_F77(int *treevec, int *index, char *tag, int *ierr, int len_tag){
   char  c_tag[CTA_STRLEN_TAG];
   *ierr=CTA_TreeVector_GetSubTreeVecId((CTA_TreeVector) *treevec, *index,
                               c_tag) ;
   CTA_cstr2fstr(c_tag,tag,len_tag);
};

CTAEXPORT void CTA_TREEVECTOR_GETTAG_F77(int *treevec, char *tag, int *ierr, int len_tag){
   char  c_tag[CTA_STRLEN_TAG];

   *ierr=CTA_TreeVector_GetTag((CTA_TreeVector) *treevec, c_tag) ;
   CTA_cstr2fstr(c_tag,tag,len_tag);
};

CTAEXPORT void CTA_TREEVECTOR_SETVEC_F77(int *treevec, int *hvec, int *ierr){
   *ierr=CTA_TreeVector_SetVec((CTA_TreeVector) *treevec, (CTA_Vector) *hvec);
};

CTAEXPORT void CTA_TREEVECTOR_GETVEC_F77(int *treevec, int *hvec, int *ierr){
   *ierr=CTA_TreeVector_GetVec((CTA_TreeVector)* treevec, (CTA_Vector) *hvec);
};

CTAEXPORT void CTA_TREEVECTOR_SETVALS_F77(int *treevec, void *val,int *nval, int *datatype, int *ierr){
   *ierr=CTA_TreeVector_SetVals((CTA_TreeVector) *treevec, val, *nval, (CTA_Datatype) *datatype);
};

CTAEXPORT void CTA_TREEVECTOR_GETVALS_F77(int *treevec, void *val,int *nval, int *datatype, int *ierr){
   *ierr=CTA_TreeVector_GetVals((CTA_TreeVector) *treevec, val, *nval, (CTA_Datatype) *datatype);
};


CTAEXPORT void CTA_TREEVECTOR_GETVAL_F77(int *treevec, int *i, void *val, int *datatype, int *ierr){
   *ierr=CTA_TreeVector_GetVal((CTA_TreeVector) *treevec, *i, val, (CTA_Datatype) *datatype);
};

CTAEXPORT void CTA_TREEVECTOR_SETVAL_F77(int *treevec, int *i, void *val, int *datatype, int *ierr){
   *ierr=CTA_TreeVector_SetVal((CTA_TreeVector) *treevec, *i, val, (CTA_Datatype) *datatype);
};



CTAEXPORT void CTA_TREEVECTOR_GETSIZE_F77(int *treevec, int *n, int *ierr){
   *ierr=CTA_TreeVector_GetSize((CTA_TreeVector) *treevec, n);
};

CTAEXPORT void CTA_TREEVECTOR_SETMETAINFO_F77(int *treevec, int *minfo, int *ierr){
  *ierr= CTA_TreeVector_SetMetainfo((CTA_TreeVector) *treevec, (CTA_Metainfo) *minfo);
}

CTAEXPORT void CTA_TREEVECTOR_GETMETAINFO_F77(int *treevec, int *minfo, int *ierr){
  *ierr= CTA_TreeVector_GetMetainfo((CTA_TreeVector) *treevec, (CTA_Metainfo) *minfo);
}


CTAEXPORT void CTA_TREEVECTOR_COPY_F77(int *treevec1, int *treevec2, int *ierr){
   *ierr=CTA_TreeVector_Copy((CTA_TreeVector) *treevec1, (CTA_TreeVector) *treevec2);
}

CTAEXPORT void CTA_TREEVECTOR_AXPY_F77(int *treevecy, double *alpha, int *treevecx, int *ierr){
   *ierr=CTA_TreeVector_Axpy((CTA_TreeVector) *treevecy, *alpha, (CTA_TreeVector) *treevecx);
}

CTAEXPORT void CTA_TREEVECTOR_DOT_F77(int *treevec1, int *treevec2, double *dotprod, int *ierr){
   *ierr=CTA_TreeVector_Dot((CTA_TreeVector) *treevec1, (CTA_TreeVector) *treevec2,
                       dotprod);
}

CTAEXPORT void CTA_TREEVECTOR_NRM2_F77(int *treevec1, double *nrm2, int *ierr){
   *ierr=CTA_TreeVector_Nrm2((CTA_TreeVector) *treevec1, nrm2);
}

CTAEXPORT void CTA_TREEVECTOR_SETCONSTANT_F77(int *treevec, void *val, int *datatype, int *ierr){
   *ierr=CTA_TreeVector_SetConstant((CTA_TreeVector) *treevec, val, (CTA_Datatype) *datatype);
}

CTAEXPORT void CTA_TREEVECTOR_SCAL_F77(int *treevec, double *val, int *ierr){
   *ierr=CTA_TreeVector_Scal((CTA_TreeVector) *treevec, *val);
}


CTAEXPORT void CTA_TREEVECTOR_EXPORT_F77(int *hvec_x, int *usrdata, int *ierr){
   *ierr=CTA_TreeVector_Export((CTA_TreeVector) *hvec_x, (CTA_Handle) *usrdata);
}

CTAEXPORT void CTA_TREEVECTOR_IMPORT_F77(int *hvec_x, int *usrdata, int *ierr){
   *ierr=CTA_TreeVector_Import((CTA_TreeVector) *hvec_x, (CTA_Handle) *usrdata);
}

CTAEXPORT void CTA_TREEVECTOR_FREE_F77(int *hvec_x, int *recursive, int *ierr){
   *ierr=CTA_TreeVector_Free((CTA_TreeVector *) hvec_x, *recursive);
}


CTAEXPORT void CTA_TREEVECTOR_INFO_F77(int *hvec_x, int *ierr){
   *ierr=CTA_TreeVector_Info((CTA_TreeVector) *hvec_x);

}

CTAEXPORT void CTA_TREEVECTOR_LIST_F77(int *hvec_x, int *hvec, int *ierr){
   *ierr=CTA_TreeVector_List((CTA_TreeVector) *hvec_x, (CTA_Vector) *hvec);
}

CTAEXPORT void CTA_TREEVECTOR_GEMM_F77(int *sC, int *nc, int *transa, int *transb, double *alpha, int *sA, int *na,
                        int *mB, double *beta, int *ierr){
   *ierr=CTA_TreeVector_Gemm((CTA_TreeVector*) sC, *nc, *transa, *transb, *alpha, (CTA_TreeVector*) sA, *na,
                         (CTA_Matrix) *mB, *beta);
}


CTAEXPORT void CTA_TREEVECTOR_OPONLEAFS_F77( int *treevec1, int* treevec2,
               int *op, int *arg, int *ierr)
{
   *ierr = CTA_TreeVector_OpOnLeafs( (CTA_TreeVector) *treevec1,
               (CTA_TreeVector) *treevec2, (CTA_Func) *op,
              (CTA_Handle) *arg);
}



CTAEXPORT void CTA_TREEVECTOR_ELMSQRT_F77(int *y, int *ierr){
   *ierr=CTA_TreeVector_ElmSqrt((CTA_TreeVector) *y);
}

CTAEXPORT void CTA_TREEVECTOR_ELMPROD_F77(int *y, int *x, int *ierr){
   *ierr=CTA_TreeVector_ElmProd((CTA_TreeVector) *y, (CTA_TreeVector) *x);
}

CTAEXPORT void CTA_TREEVECTOR_GETNUMSUBTREE_F77(int *treevec, int* numSubTrees, int *ierr){
   *ierr=CTA_TreeVector_GetNumSubTree((CTA_TreeVector) *treevec, numSubTrees);
}

