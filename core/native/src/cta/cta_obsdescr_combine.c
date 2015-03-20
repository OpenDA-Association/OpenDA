/*
$URL: https://repos.deltares.nl/repos/openda/trunk/costa/src/cta/cta_obsdescr_combine.c $
$Revision: 671 $, $Date: 2008-10-07 14:49:42 +0200 (Tue, 07 Oct 2008) $

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

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <memory.h>
#include "cta_mem.h"
#include "cta_file.h"
#include "cta_errors.h"
#include "cta_string.h"
//#include "cta_obsdescr_combine.h"
#include "cta_defaults.h"

#include "cta_sobs_combine.h"

#define IDEBUG (0)

typedef struct {
CTA_Handle myhandle;
char* condition;
int n_keys;
int nmeasr;
CTA_String *Keys;
int nofsubdescr;      // number of descriptions
CTA_Vector subdescr; // list of handles of descriptions
} CTAI_ObsDescr_combine;


void CTAI_ObsDescr_combine_Create_Size(
   // OUTPUTS:
      int *memsize,           // The number of bytes which are necessary to 
                              //     store one CTAI_SObs_combine, with a 
                              //     pointer to the contents (data), 
                              //     but without the contents themselves.
      int *retval             // error code (see cta_datatypes.h for possible 
                              //     error codes)
   ){
//   *memsize=(int) sizeof(CTAI_SObs_combine);
   *memsize=(int) sizeof(CTAI_ObsDescr_combine);
   *retval=CTA_OK;
};

void CTAI_ObsDescr_combine_Create_Init(
/*
   Allocate the memory which is necessary to store the data necessary for a
   combine-observer
*/
     // INPUT:
      CTA_ObsDescr *myhandle,  /* Handle assigned by COSTA */
     // IN-OUTPUTS
        CTAI_ObsDescr_combine *descr,// The combine-observation description
                                 //     for which the memory must be
                                 //     allocated
     // INPUTS:
        CTA_Handle *usrdat, // User data: de data from the stochobs. Here it is the CTAI_Sobs_combine
     // OUTPUTS
        int *retval)             // Error code.
{
  CTAI_SObs_combine *x;
  int nofsubdescr, idescr, ierr;
  CTA_Handle subsob;
  CTA_Vector hvec1;
  CTA_ObsDescr hsubdescr;  // description of  sub-stochobs subsob

  // get the CTAI_Sobs_Combine 
   *retval= CTA_Handle_GetData(*usrdat, (void **) &x);
   if (*retval!=CTA_OK) return;

   /* Copy the information about the number of subsobs */
  /* Note: the number of subdescr is, by definition, equal to the number of subsobs. */
   descr->nofsubdescr =  x->nofsubsobs ; 

   nofsubdescr = descr->nofsubdescr; 
   /* create a vector for the handles of ghe observation descriptions */
   ierr=CTA_Vector_Create(CTA_DEFAULT_VECTOR, nofsubdescr, CTA_HANDLE, CTA_NULL, &hvec1);

   for (idescr=1; idescr<=nofsubdescr; idescr++) {
     //get the sub-stochastic observer
       ierr=CTA_Vector_GetVal(x->subsobs,idescr,&subsob,CTA_HANDLE);
       /* check if handle is a stochastic observation? (superfluous check) */
       ierr=CTA_Handle_Check((CTA_Handle) subsob,CTA_SOBS);
       if (IDEBUG > 0) 
         printf("sobs_obsdescr_combine_create_init: TEST if subsobs %d is real (sub-)sobs : retval %d \n",idescr,ierr);
       /* Each subsobs already  has its own  description created. */ 
       ierr = CTA_SObs_GetDescription(subsob,&hsubdescr);

     /* put handle in cta-vector of handles */    
       ierr=CTA_Vector_SetVal(hvec1,idescr,&hsubdescr,CTA_HANDLE);

   }


   /* Finally, set the vector with sub descriptions */
     descr->subdescr = hvec1;

};

void CTAI_ObsDescr_combine_CreateSel(CTAI_ObsDescr_combine *descr,
          CTA_String *selection, CTA_RelTable *reltab, 
          CTA_ObsDescr *myhandle_out, 
          CTAI_ObsDescr_combine *descrout, int *retval){

  int nofsubdescr,idescr;
  CTA_Handle subdescr_in ;
  int ierr;
  CTA_Vector hvec1;
  CTA_ObsDescr subdescr_out;

  nofsubdescr = descr->nofsubdescr;

  /* Warning: only the memory of descrout has been created already,
     the nofsubdescr and vector have to be defined now. */
  descrout->nofsubdescr = nofsubdescr;
  ierr=CTA_Vector_Create(CTA_DEFAULT_VECTOR, nofsubdescr, CTA_HANDLE, CTA_NULL, &hvec1);      

  for (idescr=1; idescr<=nofsubdescr; idescr++) {
       ierr=CTA_Vector_GetVal(descr->subdescr,idescr,&subdescr_in,CTA_HANDLE);
       /* call the createsel function for each individual sub-sobdescr */
       ierr = CTA_ObsDescr_CreateSel(subdescr_in, *selection,   
                            *reltab, &subdescr_out);

       /* put the selected subdescr in the vector descrout->subdescr */
       ierr=CTA_Vector_SetVal(hvec1,idescr,&subdescr_out,CTA_HANDLE);    
       if (ierr != CTA_OK) {
         printf("ctai_obsdescr_createsel: ERROR %d \n",ierr);}

  }
  /* set the cta_vector of the descrout */
  descrout->subdescr = hvec1;

  *retval = ierr ;
  /* TODO: think about reltab and myhandle. */


}
         


void CTAI_ObsDescr_combine_Get_Keys(
         CTAI_ObsDescr_combine *descr,
         CTA_Vector *Keys,
         int *retval)
{

  int nofsubdescr,idescr;
  CTA_Handle subdescr;
  int nsubkeys,ierr, offset, j;
  CTA_Vector hsubvec;
  CTA_String hstr1;

  nofsubdescr = descr->nofsubdescr;
  offset=0; 
  for (idescr=1; idescr<=nofsubdescr; idescr++) {
       ierr=CTA_Vector_GetVal(descr->subdescr,idescr,&subdescr,CTA_HANDLE);
       /* count the number of keys in sub-obsdescr */
        ierr=CTA_ObsDescr_Property_Count(subdescr, &nsubkeys);

       /* create a vector holding the keys of the sub-obsdescr */
       ierr=CTA_Vector_Create(CTA_DEFAULT_VECTOR, nsubkeys, CTA_STRING, CTA_NULL, &hsubvec);
       /* call the get_propertykeys function for each individual sub-sobdescr */
       ierr = CTA_ObsDescr_Get_PropertyKeys(subdescr, hsubvec);

       /* add the new keys to the existing keys */
       ierr =  CTA_String_Create(&hstr1);
       for (j=1; j<= nsubkeys; j++){
         ierr = CTA_Vector_GetVal(hsubvec,j, &hstr1,CTA_STRING);
         ierr = CTA_Vector_SetVal(*Keys,offset+j, &hstr1,CTA_STRING);
       }
       offset = offset + nsubkeys;
       ierr = CTA_Vector_Free(&hsubvec);
  }
  *retval = ierr;

  // TODO: hier moeten wel de dubbele uit!
};

void CTAI_ObsDescr_combine_Property_Count(
         CTAI_ObsDescr_combine *descr, 
         int *nkeys, 
         int *retval)
{
  int nofsubdescr,idescr;
  CTA_Handle subdescr;
  int nsubkeys,ierr;

  *nkeys = 0;
  nofsubdescr = descr->nofsubdescr; 
  for (idescr=1; idescr<=nofsubdescr; idescr++) {
       ierr=CTA_Vector_GetVal(descr->subdescr,idescr,&subdescr,CTA_HANDLE);
       /* call the propertycount function for each individual sub-obsdescr */
       ierr = CTA_ObsDescr_Property_Count(subdescr, &nsubkeys);
       *nkeys = *nkeys + nsubkeys;
  }
  *retval = ierr;
}
;

void CTAI_ObsDescr_combine_Observation_Count(
         CTAI_ObsDescr_combine *descr, 
         int *nobs, 
         int *retval)
{

  int nofsubdescr,idescr;
  CTA_Handle subdescr;
  int nsubobs,ierr;

  *nobs = 0;
  nofsubdescr = descr->nofsubdescr; 
  for (idescr=1; idescr<=nofsubdescr; idescr++) {
       ierr=CTA_Vector_GetVal(descr->subdescr,idescr,&subdescr,CTA_HANDLE);
       /* check if handle is a observation description? */
       ierr=CTA_Handle_Check((CTA_Handle) subdescr,CTA_OBSDESCR);

       if (IDEBUG > 0) {printf("ctai_obsdescr_combine_observation_count: "); 
         printf("TEST if subdescr %d is a real (sub-)descr : retval %d \n",idescr,ierr);}

       /* call the count function for each individual sub-sobdescr */
       ierr = CTA_ObsDescr_Observation_Count(subdescr, &nsubobs);
       *nobs = *nobs + nsubobs;
  }
  *retval = ierr;
};


void CTAI_ObsDescr_combine_Get_Properties(
         CTAI_ObsDescr_combine *descr, 
         const char *Key,
         CTA_Vector *Properties,
         CTA_Datatype *datatype,
         int *retval)
{
  int nofsubdescr,idescr,j;
  CTA_Handle subdescr;
  int ierr, nsubobs, ntotobs,offset;
  CTA_Vector hsubvec; 
  CTA_String *strval, *substrval;
  double *val, *subval;

  //  CTA_String *strval, *substrval;  

  /* first ask the total number of values of the vector to be filled*/
  ierr = CTA_Vector_GetSize(*Properties,&ntotobs);
  
/* allocate the main array containing all the values */
  if(*datatype == CTA_STRING)  {strval = CTA_Malloc(sizeof(CTA_String)*ntotobs); 
  if (IDEBUG > 0)  printf("obsdescr_combine_get_properties: Warning: string as propertydatatype!\n");}
  else { val = CTA_Malloc(sizeof(double)*ntotobs);}


  offset = 0;
  nofsubdescr = descr->nofsubdescr; 
  for (idescr=1; idescr<=nofsubdescr; idescr++) {
       ierr=CTA_Vector_GetVal(descr->subdescr,idescr,&subdescr,CTA_HANDLE);
       /* First count the number of observations in sub descr */
       ierr = CTA_ObsDescr_Observation_Count(subdescr, &nsubobs);

       if (nsubobs > 0) {

         /* create the subvector to receive the values of the subdescr */
         ierr=CTA_Vector_Create(CTA_DEFAULT_VECTOR, nsubobs, *datatype, CTA_NULL, &hsubvec);

         /* call the get_valueproperties function for each individual sub-sobdescr */
         ierr = CTA_ObsDescr_Get_ValueProperties(subdescr, Key, hsubvec, *datatype);
       
         /* put the values in the sub-array. In case of String-output, create the strings */
         if (*datatype == CTA_STRING) {
           substrval = CTA_Malloc(sizeof(CTA_String)*nsubobs); 
           for (j=0; j<nsubobs; j++)
             { ierr = CTA_String_Create(&substrval[j]);}
           ierr=CTA_Vector_GetVals(hsubvec,substrval, nsubobs, *datatype);
         }
         else {
           subval = CTA_Malloc(sizeof(double)*nsubobs);
           ierr=CTA_Vector_GetVals(hsubvec,subval, nsubobs, *datatype);
         }
       

         /* fill the main vector  */
         if (*datatype == CTA_STRING) { 
           for (j=0; j < nsubobs; j++) {
             //the duplicate function already creates the cta_string for us.
             ierr = CTA_String_Duplicate(substrval[j], &strval[offset+j]); 
           
           }
         } else {
           for (j=0; j < nsubobs; j++) {
             val[offset+j] = subval[j];
           }
         }

         // In case of String-output, erase the strings
         if (*datatype == CTA_STRING)
           {
             for (j=0; j<nsubobs; j++) {
               ierr = CTA_String_Free(&substrval[j]);}
             free(substrval);
           } else {
           free(subval);}

         ierr = CTA_Vector_Free(&hsubvec);
         offset = offset + nsubobs;
       }
  }
  /* fill *Properties with the main array */
  if (*datatype == CTA_STRING) {
    ierr=CTA_Vector_SetVals(*Properties,strval, ntotobs, *datatype);
    for (j=0; j<ntotobs; j++)
      ierr = CTA_String_Free(&strval[j]);
    free(strval);
  } else {
    ierr=CTA_Vector_SetVals(*Properties,val, ntotobs, *datatype);
    free(val);
  }


  // for (j=0; j < ntotobs; j++) {printf("%d %f\n",j, val[j]);    }

  *retval = ierr;


}


void CTAI_ObsDescr_combine_Free(
         CTAI_ObsDescr_combine *descr, 
         int *retval)
{
  int nofsubdescr, idescr, ierr;
  CTA_Handle subdescr;

  nofsubdescr = descr->nofsubdescr; 
  for (idescr=1; idescr<=nofsubdescr; idescr++) {
       ierr=CTA_Vector_GetVal(descr->subdescr,idescr,&subdescr,CTA_HANDLE);
       ierr = CTA_ObsDescr_Free(&subdescr);
  } 
  *retval = ierr;

}


void CTA_ObsDescr_combine_initialise(CTA_ObsDescrClass *hobsdescrcl)
{
   CTA_Intf hintf=0;
   CTA_Func h_func[I_CTA_OBSDESCR_NUMFUNC];

   // The vector h_func is filled with COSTA-function handles of the 
   // implementations in this file.
   CTA_Func_Create(" ",&CTAI_ObsDescr_combine_Create_Size,    hintf,
                       &h_func[I_CTA_OBSDESCR_CREATE_SIZE]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_combine_Create_Init,    hintf,
                       &h_func[I_CTA_OBSDESCR_CREATE_INIT]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_combine_Property_Count,    hintf,
                       &h_func[I_CTA_OBSDESCR_COUNT_PROPERTIES]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_combine_Get_Properties,    hintf,
                       &h_func[I_CTA_OBSDESCR_GET_PROPERTIES]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_combine_Observation_Count, hintf,
                       &h_func[I_CTA_OBSDESCR_COUNT_OBSERVATIONS]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_combine_Get_Keys,    hintf,
                       &h_func[I_CTA_OBSDESCR_GET_KEYS]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_combine_Free,    hintf,
                       &h_func[I_CTA_OBSDESCR_FREE]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_combine_CreateSel,    hintf,
                       &h_func[I_CTA_OBSDESCR_SELECTION]);
   CTA_ObsDescr_DefineClass("cta_obsdescr_combine",h_func,hobsdescrcl);

}




