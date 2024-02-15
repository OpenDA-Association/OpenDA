/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_reltable.c $
$Revision: 3567 $, $Date: 2012-10-08 16:53:30 +0200 (Mon, 08 Oct 2012) $

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

#include "cta_mem.h"
#include "f_cta_utils.h"
#include "ctai.h"
#include "cta_reltable.h"
#include "cta_errors.h"
#include "cta_util_sort.h"
#include "cta_string.h"
#include "cta_defaults.h"
#include "cta_message.h"

#define CTA_RELTABLE_CREATE_F77    F77_CALL(cta_reltable_create,CTA_RELTABLE_CREATE)
#define CTA_RELTABLE_FREE_F77      F77_CALL(cta_reltable_free,CTA_RELTABLE_FREE)
#define CTA_RELTABLE_APPLY_F77     F77_CALL(cta_reltable_appy,CTA_RELTABLE_APPY)
#define CTA_RELTABLE_APPLYINV_F77  F77_CALL(cta_reltable_appyinv,CTA_RELTABLE_APPYINV)
#define CTA_RELTABLE_SETSELECT_F77 F77_CALL(cta_reltable_setselect,CTA_RELTABLE_SETSELECT)
#define CTA_RELTABLE_APPLY_VAL_F77     F77_CALL(cta_reltable_appyval,CTA_RELTABLE_APPYVAL)
#define CTA_RELTABLE_APPLYINV_VAL_F77  F77_CALL(cta_reltable_appyinvval,CTA_RELTABLE_APPYINVVAL)
#define CTA_RELTABLE_SETSELECT_VAL_F77 F77_CALL(cta_reltable_setselectval,CTA_RELTABLE_SETSELECTVAL)
#define CTA_SETTABLECOMBINE_F77    F77_CALL(cta_settablecombine,CTA_SETTABLECOMBINE)
#define CTA_RELTABLE_COUNT_F77     F77_CALL(cta_reltable_count,CTA_RELTABLE_COUNT)

#define IDEBUG (0)

#define CLASSNAME "CTA_Reltable"
/* Struct holding all data associated to an COSTA time span */

typedef struct {
int nsel1;  /* size of array sel1 if 0 then use identity for set1 */
int nsel2;  /* size of array sel2 if 0 then use identity for set2 */
int *sel1;  /* points in origin set to be copied                  */
int *sel2;  /* points in target set to copy elements from sel1 to */
} CTAI_RelTable;

/** \brief Get data of relation table
 *
 *  \param hrel     (I) Handle of relation table
 *  \param reltable (O) Data of relation table
 *
 * \return error status: CTA_OK if successful
 */
int CTAI_RelTable_GetData(CTA_RelTable hrel, CTAI_RelTable **reltable ){
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) hrel,CTA_RELTABLE);
   if (retval!=CTA_OK) return retval;
   retval=CTA_Handle_GetData((CTA_Handle) hrel,(void**) reltable);
   if (retval!=CTA_OK) return retval;

   return CTA_OK;
}

/** \brief Get (pointers to) information in relation table
 *         taking into account for inverses.
 *
 *  \param reltable (I) Data of relation table
 *  \param inverse  (I) Select CTA_TRUE/CTA_FALSE to return info on inverse
 *                      of relation table table
 *  \param nsel1    (O) size of sel1
 *  \param sel1     (O) selected points of origin set
 *  \param nsel1    (O) size of sel2
 *  \param sel1     (O) selected points of target set
 */
void CTAI_GetRelTableInfo(CTAI_RelTable *reltable, int inverse,
                         int *nsel1, int **sel1,
                         int *nsel2, int **sel2){

    if (inverse) {
       *nsel1=reltable->nsel2;
       *nsel2=reltable->nsel1;
       *sel1=reltable->sel2;
       *sel2=reltable->sel1;
   }
   else {
       *nsel1=reltable->nsel1;
       *nsel2=reltable->nsel2;
       *sel1=reltable->sel1;
       *sel2=reltable->sel2;
  }
}


/** \brief Apply relation table to two components
 *
 * \param reltable (I)  Data of relation table
 * \param hrom     (I)  Source set of elements
 * \param hto      (IO) Target set (some will be overwitten)
 * \param iverse   (I)  CTA_TRUE/CTA_FALSE apply inverse table
 *
 * \return error status: CTA_OK if successful
 */
int CTAI_RelTable_Apply(CTA_RelTable hreltable,
                       CTA_Handle hfrom, CTA_Handle hto, int inverse){

   int retval;
   CTAI_RelTable *reltable;
   int size_of_datatype;
   void *val;
   int isel, isel1, isel2;
   int nsel, nsel1, nsel2;
   int *sel1, *sel2;
   CTA_Datatype type_from, type_to, datatype;

   /* Get data object of relation table */
   retval=CTAI_RelTable_GetData(hreltable, &reltable);
   if (retval!=CTA_OK) return retval;

   /* Set relation table administration depending on inverse option */
   CTAI_GetRelTableInfo(reltable, inverse, &nsel1, &sel1, &nsel2, &sel2);

   /* check kind of object to copy elements from: */
   retval=CTA_Handle_GetDatatype(hfrom, &type_from);
   if (retval!=CTA_OK) return retval;
   retval=CTA_Handle_GetDatatype(hto, &type_to);
   if (retval!=CTA_OK) return retval;

   /* Are they both of the same datatype? */
   if (type_to!=type_from) return CTA_INPUT_OBJECTS_ARE_INCOMPATIBLE;

   /* At this time we use an expensive way of applying the relation tables
      we need to make changes (extentions to the interface) of the
      supported objects in order to improve the performance */
   switch(type_from){
      case CTA_VECTOR:
         /* get datatype of vector object */
         retval=CTA_Vector_GetDatatype(hfrom,&datatype);
         /* strings are not yet supported */


         /* allocate memory block for storing values */
         retval=CTA_SizeOf(datatype, &size_of_datatype);
         if (retval!=CTA_OK) return retval;
         val=CTA_Malloc(size_of_datatype);

         /* Create string instance for holding copied values
            when vector contains strings                    */
         if (datatype==CTA_STRING){
            retval=CTA_String_Create(val);
            if (retval!=CTA_OK) return retval;
         };

         /* Loop over all elements that need to be copied */
         nsel=MAX(nsel1,nsel2);
         for (isel=1;isel<=nsel;isel++){
            if (nsel1==0) {isel1=isel;} else {isel1=sel1[isel-1];}
            if (nsel2==0) {isel2=isel;} else {isel2=sel2[isel-1];}

            retval=CTA_Vector_GetVal(hfrom,isel1,val,datatype);
            if (retval!=CTA_OK) {return retval;}
            retval=CTA_Vector_SetVal(hto,isel2,val,datatype);
            if (retval!=CTA_OK)  {return retval;}
         }

         /* Free string instance (when strings have been copied) */
         if (datatype==CTA_STRING){
            retval=CTA_String_Free(val);
            if (retval!=CTA_OK) return retval;
         };
         free(val);

         break;
      case CTA_TREEVECTOR:
         return CTA_NOT_YET_SUPPORTED;
         break;
      default:
         return CTA_INPUT_OBJECT_NOT_SUPPORTED;
         break;
   }
   return CTA_OK;
}

void CTAI_RelTable_SetWorkingCopy(int nsel1, int *sel1, int nsel2, int *sel2,
                                 int *nelm,  int **from, int **to){

   int i;


   *nelm=MAX(nsel1,nsel2);
   *from=CTA_Malloc((*nelm)*sizeof(int));
   *to  =CTA_Malloc((*nelm)*sizeof(int));

   /* fill the from table */
   if (nsel1==0) {
      for (i=0;i<*nelm;i++){(*from)[i]=i+1;}
   } else {
      for (i=0;i<*nelm;i++){(*from)[i]=sel1[i];}
   }

   /* fill the to table */
   if (nsel2==0) {
      for (i=0;i<*nelm;i++){(*to)[i]=i+1;}
   } else {
      for (i=0;i<*nelm;i++){(*to)[i]=sel2[i];}
   }

   /* now we have a table; shall we sort the first or second column? */



}





#undef METHOD
#define METHOD "Create" 
int CTA_RelTable_Create(CTA_RelTable *hreltable){

   CTAI_RelTable *reltable;
   int retval;

   /* allocate memory for new time object */
   reltable=CTA_Malloc(sizeof(CTAI_RelTable));

   /* initialise administration */
   reltable->nsel1=0;
   reltable->nsel2=0;
   reltable->sel1=NULL;
   reltable->sel2=NULL;

   /* Allocate new handle and return error when unsuccesfull */
   retval=CTA_Handle_Create("reltable",CTA_RELTABLE,reltable,hreltable);
   if (retval) {
       CTA_WRITE_ERROR("Cannot create handle");
       return retval;
   }

   return CTA_OK;
}

#undef METHOD
#define METHOD "Create" 
int CTA_RelTable_Free(CTA_RelTable *hreltable){

   CTAI_RelTable *reltable;
   int retval;

   /* Get data object of relation table */
   retval=CTAI_RelTable_GetData(*hreltable, &reltable);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* free selection arrays */
   if (reltable->sel1){free(reltable->sel1);}
   if (reltable->sel2){free(reltable->sel2);}

   /* free data block and handle */
   free(reltable);
   retval=CTA_Handle_Free(hreltable);
   return retval;
}

#undef METHOD
#define METHOD "ApplyVal" 
int CTA_RelTable_ApplyVal(CTA_RelTable hreltable,
                          void *vfrom, int nfrom,
                          void *vto, int nto, CTA_Datatype datatype){
  int retval;
  CTA_Vector hfrom, hto;

  /* create and fill the 'from' vector */
   retval=CTA_Vector_Create(CTA_DEFAULT_VECTOR, nfrom, datatype, CTA_NULL, &hfrom);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Cannot create vector");
       return retval;
   }

   retval=CTA_Vector_SetVals(hfrom, vfrom, nfrom, datatype);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Cannot set values");
       return retval;
   }

  /* create the 'to' vector */
   retval=CTA_Vector_Create(CTA_DEFAULT_VECTOR, nto, datatype, CTA_NULL, &hto);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Cannot create vector");
       return retval;
   }

   retval = CTA_RelTable_Apply(hreltable, hfrom, hto);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Cannot apply RelTabkle");
       return retval;
   }

   /* fill the 'vto' array and clean up */
   retval=CTA_Vector_GetVals(hto, vto, nto, datatype);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Cannot get values");
       return retval;
   }

   retval=CTA_Vector_Free(&hfrom);
   retval=CTA_Vector_Free(&hto);
   return retval;

}

int CTA_RelTable_ApplyInvVal(CTA_RelTable hreltable,
                          void *vfrom, int nfrom,
                          void *vto, int nto, CTA_Datatype datatype){

  return CTA_RelTable_ApplyVal(hreltable, vto,nto, vfrom,nfrom, datatype);

}

int CTA_RelTable_Apply(CTA_RelTable hreltable,
                       CTA_Handle hfrom, CTA_Handle hto){

   return CTAI_RelTable_Apply(hreltable, hfrom, hto, FALSE);

}

int CTA_RelTable_ApplyInv(CTA_RelTable hreltable,
                       CTA_Handle hfrom, CTA_Handle hto){

   return CTAI_RelTable_Apply(hreltable, hfrom, hto, TRUE);
}


#undef METHOD
#define METHOD "SetSelectVal" 
int CTA_RelTable_SetSelectVal(CTA_RelTable hreltable, void *val, int nval, CTA_Datatype datatype){
   CTA_Vector vwork;
   int retval;

   retval=CTA_Vector_Create(CTA_DEFAULT_VECTOR, nval, datatype, CTA_NULL, &vwork);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Cannot create verctor handle");
       return retval;
   }
   retval=CTA_Vector_SetVals(vwork, val, nval, datatype);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Cannot set values in vector");
       return retval;
   }
   retval=CTA_RelTable_SetSelect(hreltable,vwork);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Error in CTA_RelTable_SetSelect");
       return retval;
   }
   retval=CTA_Vector_Free(&vwork);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Cannot free vector");
       return retval;
   }

   return CTA_OK;
}



#undef METHOD
#define METHOD "SetSelect" 
int CTA_RelTable_SetSelect(CTA_RelTable hreltable, CTA_Vector vselect){
   int retval;
   int i, n;
   CTAI_RelTable *reltable;

   /* Get data object of relation table */
   retval=CTAI_RelTable_GetData(hreltable, &reltable);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* free memory if relation table is used in past */
   if (reltable->sel1){free(reltable->sel1);}
   if (reltable->sel2){free(reltable->sel2);}
   reltable->nsel2=0;

   /* Set cardinality */
   retval=CTA_Vector_GetSize(vselect, &n);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get size of vector");
	   return retval;
   }
   reltable->nsel1=n;

   /* copy values of vselect into local administration */
   reltable->sel1=CTA_Malloc(n*sizeof(int));
   retval=CTA_Vector_GetVals(vselect,  reltable->sel1, n, CTA_INTEGER);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get values from vector");
	   return retval;
   }

   if (IDEBUG) {
      printf("CTA_RelTable_SetSelect: set a new relation table\n");
      printf("reltable->nsel1=%d\n",reltable->nsel1);
      printf("reltable->nsel2=%d\n",reltable->nsel2);
      printf("reltable->sel1=");
      for (i=0;i<reltable->nsel1;i++){
         printf("%d ",reltable->sel1[i]);
      }
      printf("\n");
      printf("reltable->sel2=");
      for (i=0;i<reltable->nsel2;i++){
         printf("%d ",reltable->sel1[i]);
      }
      printf("\n");
   }

   return CTA_OK;
}


#undef METHOD
#define METHOD "SetTableCombine" 
int CTA_RelTable_SetTableCombine(CTA_RelTable hreltable,
                        CTA_RelTable hrel1, int inverse1,
                        CTA_RelTable hrel2, int inverse2 ){

int retval, i,j;
BOOL found;
int nsel1_rel1, nsel2_rel1, nsel1_rel2, nsel2_rel2;
int *sel1_rel1, *sel2_rel1, *sel1_rel2, *sel2_rel2;
int nelm1, nelm2, *from1, *to1, *from2, *to2;
int *from;

CTAI_RelTable *reltable, *reltable1, *reltable2;

   /* Get data object of relation tables */
   retval=CTAI_RelTable_GetData(hreltable, &reltable);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   retval=CTAI_RelTable_GetData(hrel1, &reltable1);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   retval=CTAI_RelTable_GetData(hrel2, &reltable2);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

/* first check for empty tables */
   if (MAX(reltable1->nsel1,reltable1->nsel2) == 0) {
     reltable = reltable1; return CTA_OK; //mag deze toekenning?
   }
   if (MAX(reltable2->nsel1,reltable2->nsel2) == 0) {
     reltable = reltable2; return CTA_OK; //mag deze toekenning?
   }



   /* Set local variables and pointers in order to handle the inverse
    * options
    */
   CTAI_GetRelTableInfo(reltable1, inverse1,
                        &nsel1_rel1, &sel1_rel1,
                        &nsel2_rel1, &sel2_rel1);

   CTAI_GetRelTableInfo(reltable2, inverse2,
                        &nsel1_rel2, &sel1_rel2,
                        &nsel2_rel2, &sel2_rel2);


   /* Create a local copy of administration of relation tables */
   CTAI_RelTable_SetWorkingCopy(nsel1_rel1, sel1_rel1,
                                nsel2_rel1, sel2_rel1,
                                &nelm1, &from1, &to1);

   CTAI_RelTable_SetWorkingCopy(nsel1_rel2, sel1_rel2,
                                nsel2_rel2, sel2_rel2,
                                &nelm2, &from2, &to2);

   if (IDEBUG > 0) {printf("reltable_combine1 %d %d %d %d \n",nsel1_rel1,nsel2_rel1,nsel1_rel2,nsel2_rel2);}
   //   printf("from2 01: %d %d \n",from2[0], from2[1]);

   /* permutate towards target of first relation table */
   CTA_Util_IQSort2(to1, from1, nelm1);

   if (IDEBUG > 0) {printf("reltable_combine2-1 %d %d\n",nelm1,from2[0]);}

   /* permutate towards origin of second relation table */
   CTA_Util_IQSort2(to2, from2, nelm2);

   if (IDEBUG > 0) {printf("reltable_combine2-2; nelm1,2:%d %d   \n", nelm1, from2[0]);}

   /* find all target elements of first relation table  that corresponds
      that are an origin element of second relation table. */
   from=CTA_Malloc(sizeof(int)*nelm2);
   j=0;
   for (i=0;i<nelm2;i++){
      for (;j<nelm1;j++){
         found=FALSE;
         //    printf("checking : i j %d %d: to1[j] %d from2[01]%d %d \n",i,j,to1[j],from2[0],from2[1]);
         if (to1[j]==from2[i]){
            from[i]=from1[j];
            found=TRUE;
            break;
         }
      }
      /* Check whether element is found */
      if (!found) {
         printf("STOP: error: reltables cannot be combined ");
         free(to1); free(from1); free(to2); free(from2); free(from);

         return CTA_RELTABLES_CANNOT_BE_COMBINED;
      }
   }
   if (IDEBUG > 0) {printf("reltable_combine4 %d %d\n",nelm1,nelm2);}


   /* free memory if relation table is used in past */
   if (reltable->sel1){free(reltable->sel1);}
   if (reltable->sel2){free(reltable->sel2);}
   reltable->nsel2=0;
   reltable->sel1=CTA_Malloc(nelm2*sizeof(int));
   reltable->sel2=CTA_Malloc(nelm2*sizeof(int));
   reltable->nsel1=nelm2;
   reltable->nsel2=nelm2;

   if (IDEBUG > 0) {printf("reltable_combine6\n");}

   /* Copy the relation table */
   for (i=0;i<nelm2;i++){
     reltable->sel1[i]=from[i];
     reltable->sel2[i]=to2[i];
   }


  /* free all work variables */
  free(to1); free(from1); free(to2); free(from2); free(from);

  return CTA_OK;

}


#undef METHOD
#define METHOD "Count"
int CTA_RelTable_Count(CTA_RelTable hreltable, int *nelt){

   CTAI_RelTable *reltable;
   int retval;

   /* Get data object of relation table */
   retval=CTAI_RelTable_GetData(hreltable, &reltable);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   *nelt=MAX(reltable->nsel1,reltable->nsel2);

   return CTA_OK;
}





/* Interfacing with Fortran */

CTAEXPORT void CTA_RELTABLE_COUNT_F77(int *hreltable, int *nelt, int *ierr){
   *ierr=CTA_RelTable_Count((CTA_RelTable) *hreltable, nelt);
}

CTAEXPORT void CTA_RELTABLE_CREATE_F77(int *hreltable, int *ierr){
   *ierr=CTA_RelTable_Create((CTA_RelTable*) hreltable);
}

CTAEXPORT void CTA_RELTABLE_FREE_F77(int *hreltable, int *ierr){
   *ierr=CTA_RelTable_Free((CTA_RelTable*) hreltable);
}

CTAEXPORT void CTA_RELTABLE_APPLY_F77(int *hreltable, int *hfrom, int *hto, int *ierr){
   *ierr=CTA_RelTable_Apply((CTA_RelTable) *hreltable,
                             (CTA_Handle) *hfrom, (CTA_Handle) *hto);
}

CTAEXPORT void CTA_RELTABLE_APPLY_VAL_F77(int *hreltable, void *hfrom,  int *nfrom,
                                              void *hto, int *nto, int *datatype, int *ierr){
   *ierr=CTA_RelTable_ApplyVal((CTA_RelTable) *hreltable,
                               hfrom, *nfrom, hto, *nto, (CTA_Datatype) *datatype);
}


CTAEXPORT void CTA_RELTABLE_APPLYINV_F77(int *hreltable, int *hfrom, int *hto,
                               int *ierr){
   *ierr=CTA_RelTable_ApplyInv((CTA_RelTable) *hreltable,
                       (CTA_Handle) *hfrom, (CTA_Handle) *hto);
}

CTAEXPORT void CTA_RELTABLE_APPLYINV_VAL_F77(int *hreltable, void *hfrom,  int *nfrom,
                                              void *hto, int *nto, int *datatype, int *ierr){
   *ierr=CTA_RelTable_ApplyInvVal((CTA_RelTable) *hreltable,
                               hfrom, *nfrom, hto, *nto, (CTA_Datatype) *datatype);

}


CTAEXPORT void CTA_RELTABLE_SETSELECT_F77(int *hreltable, int *selorig, int *ierr){
   *ierr=CTA_RelTable_SetSelect((CTA_RelTable) *hreltable,
                                (CTA_Vector) *selorig);
}

CTAEXPORT void CTA_RELTABLE_SETSELECT_VAL_F77(int *hreltable, void *val, int *nval, int*datatype, int *ierr){
  *ierr=CTA_RelTable_SetSelectVal((CTA_RelTable) *hreltable, val, *nval,
                                (CTA_Datatype) *datatype);
}


CTAEXPORT void CTA_SETTABLECOMBINE_F77(int *hreltable,
                             int *hrel1, int *inverse1,
                             int *hrel2, int *inverse2,
                             int *ierr ){
   *ierr=CTA_RelTable_SetTableCombine((CTA_RelTable) *hreltable,
                             (CTA_RelTable) *hrel1, *inverse1,
                             (CTA_RelTable) *hrel2, *inverse2);
}

