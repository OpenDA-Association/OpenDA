/*
$URL: https://repos.deltares.nl/repos/openda/trunk/costa/src/cta/cta_stoch_observer_combine.c $
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
#include <memory.h>
#include "cta_mem.h"
#include "cta.h"
#include "ctai.h"
#include "cta_flush.h"
#include "cta_util_statistics.h"
//#include "cta_util_combine.h"
#include "cta_file.h"
#include "cta_string.h"
//#include "cta_obsdescr_combine.h"
//#include "cta_sobs_combine.h"
#include "cta_sobs.h"
#include "cta_errors.h"
#include "cta_reltable.h"

#include "cta_sobs_combine.h"

#define IDEBUG (0)



void CTAI_SObs_combine_Create_Size(
   // OUTPUTS:
      int *memsize,           // The number of bytes which are necessary to store one 
                              //     CTAI_SObs_combine, with a pointer to the contents (data), 
                              //     but without the contents themselves.
      int *retval             // error code (see cta_datatypes.h for possible error codes)
   ){
   *memsize=(int) sizeof(CTAI_SObs_combine);
   *retval=CTA_OK;
};





void CTAI_SObs_combine_Create_Init(
/*
   Allocate the memory which is necessary to store the data necessary for a
   combined observer
*/
     // IN-OUTPUTS
        CTAI_SObs_combine *x,   // The combine-observer for which the memory must 
                                //     be allocated
     // INPUTS:
        CTA_Handle userdata,   // User data: vector of size 2 containing tree with SObs and timeoffset
     // OUTPUTS
        int *retval)            // Error code. Possible error: Illegal data type
{
  //CTA_Tree hsubsobs_tr;
  CTA_Handle *hsubsobsstr;
  int ierr;
  int nofsubsobs,isob;
  CTA_String h_ss_name;
  char ss_name[100];
  CTA_Vector hvec1; 
  double timeoffset;
  CTA_Handle  userdata1, userdata2;

   ierr=CTA_Handle_Check((CTA_Handle) userdata,CTA_VECTOR);
   if (ierr!=CTA_OK) {
     printf("CTAI_SObs_combine_Create_Init: userdata is not a vector! \n");exit(-1);
   }
   ierr=CTA_Vector_GetVal(userdata,1,&userdata1,CTA_HANDLE);
   ierr=CTA_Vector_GetVal(userdata,2,&userdata2,CTA_HANDLE);

   ierr=CTA_Handle_Check((CTA_Handle) userdata1,CTA_TREE);
   if (ierr!=CTA_OK) {
     printf("CTAI_SObs_combine_Create_Init: userdata1 is not a tree! \n");
   }
    printf("CTAI_SObs_combine_Create_Init: -----------boom:  \n");
    ierr = CTA_Tree_Print(userdata1);
    printf("------ \n");


     /* determine number of sub-SObs (nofsubsobs) */
     ierr = CTA_Tree_CountItems(userdata1, &nofsubsobs );
     printf("combine-SObs: count subsobs %d %d\n",ierr, nofsubsobs);
     x->nofsubsobs = nofsubsobs;

     /* Create array with input configuration of all sobs */
     hsubsobsstr = CTA_Malloc((nofsubsobs+1)*sizeof(CTA_Tree));



     /* Create and store handles to all subsobs */
     for (isob=1; isob<=nofsubsobs; isob++) {
       ierr = CTA_Tree_GetItem (userdata1, isob, &hsubsobsstr[isob]);
       printf("get substochobs: number :%d  ,retval %d \n",isob,ierr);
       ierr=CTA_Handle_Check((CTA_Handle) hsubsobsstr[isob],CTA_SOBS);
       printf("get substochobs TEST if it is a real sobs : retval %d \n",ierr);

       ierr = CTA_String_Create(&h_ss_name);
       ierr=CTA_Handle_GetName(hsubsobsstr[isob],h_ss_name);
       //   printf("getname: %d \n",ierr);
       ierr=CTA_String_Get(h_ss_name, ss_name);
       //printf("string get: %d \n",ierr);
       //printf("naam is %s \n",ss_name);
     }

     /* put array in cta-vector of handles */    
     ierr=CTA_Vector_Create(CTA_DEFAULT_VECTOR, nofsubsobs, CTA_HANDLE, CTA_NULL, &hvec1);

     for (isob=1; isob<=nofsubsobs; isob++) {
       ierr=CTA_Vector_SetVal(hvec1,isob,&hsubsobsstr[isob],CTA_HANDLE);
     }
     x->subsobs = hvec1;

     ierr=CTA_Vector_GetVal(userdata2,1,&timeoffset,CTA_DOUBLE);
   if (ierr!=CTA_OK) {
     printf("CTAI_SObs_combine_Create_Init: userdata2 is not a vector of doubles! \n");
   }
     x->timeoffset = timeoffset;

     *retval = ierr;
};



void CTAI_SObs_combine_CreateSel(
/*
   Allocate the memory which is neccesary to store the data necessary for a
   combine-observer
*/
     // INPUTS
        CTAI_SObs_combine *obsin,// The combine-observer of which a selection 
                                 // is to be made
        CTA_Handle *userdata_in,   // User data: condition
     // OUTPUTS
        CTAI_SObs_combine *obsout, // The combine-observer which is a selection 
                                // of observer obsin
        int *retval)            // Error code
{
  CTA_Handle userdata=*userdata_in;
  int nofsubsobs,isob;
  CTA_Handle subsob;
  CTA_StochObs hobsout;
  int ierr, totcount, subcount, selected_subcount, sel2count, i;
  CTA_Vector hvec1;
  CTA_RelTable reltab2;
  int * shadow_vals;
  int * selected_shadow_vals;
  int * sel2_shadow_vals;

  CTA_Datatype datatype;

  CTA_Handle_GetDatatype(userdata, &datatype);
  //  if (datatype==CTA_STRING){printf("CTAI_SOBS_Combine_Createsel(0): userdata STRING\n"); }
  // if (datatype==CTA_RELTABLE){printf("CTAI_SOBS_Combine_Createsel(0): userdata RELTABLE\n"); }


  /* Warning: if userdata is a relation table, then calling the subsobs with this userdata
   will not work. Solution: make a, adjusted relation table
  */

  nofsubsobs = obsin->nofsubsobs;

  /* Warning: only the memory of obsout has been created already,
     the nofsubsobs and vector have to be defined now. */
  obsout->nofsubsobs = nofsubsobs;
  ierr=CTA_Vector_Create(CTA_DEFAULT_VECTOR, nofsubsobs, CTA_HANDLE, CTA_NULL, &hvec1);      
  
  //  printf("CTAI_SOBS_Combine_Createsel(1): nofsubsobs: %d ierr: %d \n",nofsubsobs,ierr);
  //printf("CTAI_SOBS_Combine_Createsel(2): userdata: %d   \n",userdata);


  totcount = 0;
  for (isob=1; isob<=nofsubsobs; isob++) {
    //   printf("-------------------sobs_combine_createsel: subsob %d of %d \n",isob, nofsubsobs);
       ierr=CTA_Vector_GetVal(obsin->subsobs,isob,&subsob,CTA_HANDLE);
       /* check if handle is a stochobs? */
       ierr=CTA_Handle_Check((CTA_Handle) subsob,CTA_SOBS);
       if (IDEBUG > 5) {
         printf("sobs_combine_createsel: TEST if subsobs %d is a real (sub-)sobs : retval %d \n",isob,ierr);}
       /* call the createsel function for each individual sub-sobserver. Adjust userdata in case of reltable! */

       ierr = CTA_SObs_Count(subsob, &subcount);


       if (datatype!=CTA_RELTABLE || subcount == 0) {
         ierr = CTA_SObs_CreateSel(subsob,userdata, &hobsout);
       }
       else {  /* relation tables has to be redefined for each subsob !           */
        // use the relation table
         CTA_RelTable_Count(userdata,&selected_subcount);

         shadow_vals = CTA_Malloc(subcount * sizeof(int));
 
         selected_shadow_vals = CTA_Malloc(selected_subcount* sizeof(int));
         sel2_shadow_vals = CTA_Malloc(selected_subcount* sizeof(int));
         for (i=0; i < subcount; i++) { shadow_vals[i] = i+1 ;}

         CTA_RelTable_ApplyVal(userdata, shadow_vals, subcount, 
                                           selected_shadow_vals, selected_subcount , CTA_INTEGER);
         sel2count = 0;
         for (i=0; i < selected_subcount; i++) { 
           if (selected_shadow_vals[i] - totcount > 0) {
                 sel2_shadow_vals[sel2count] = selected_shadow_vals[i] - totcount ;
                 sel2count = sel2count + 1;
           }
         }
         CTA_RelTable_Create(&reltab2);
         CTA_RelTable_SetSelectVal(reltab2, sel2_shadow_vals, sel2count, CTA_INTEGER);

         //         printf("sobcombine_createsel: nieuwe reltab: van: %d naar %d; %d \n",selected_subcount, sel2count, sel2_shadow_vals[0]);

         ierr = CTA_SObs_CreateSel(subsob,reltab2, &hobsout);
         free(shadow_vals); free(selected_shadow_vals); free(sel2_shadow_vals);
       }

       /* put the selected subsobs in the vector subsout->subsobs */
       ierr=CTA_Vector_SetVal(hvec1,isob,&hobsout,CTA_HANDLE);    
       if (ierr != CTA_OK) {
         printf("ctai_sobs_createsel: ERROR %d \n",ierr);


       totcount = totcount + subcount;
       }
  }
  /* set the cta_vector of the obsout */
  obsout->subsobs = hvec1;

  *retval = ierr;

};


void CTAI_SObs_combine_Count(
// Return the number of measurements in the observer
   // INPUTS
      CTAI_SObs_combine *x,  // The StochObserver of which the number of measurements is
                                  //     returned 
   // OUTPUTS 
      int *nmeasr,
      int *retval
   )
{
  int nofsubsobs, isob, ierr;
  CTA_Handle subsob;
  int nsubmeasr;
  
  *nmeasr = 0;
  nofsubsobs = x->nofsubsobs;

  for (isob=1; isob<=nofsubsobs; isob++) {
       ierr=CTA_Vector_GetVal(x->subsobs,isob,&subsob,CTA_HANDLE);
       ierr = CTA_SObs_Count(subsob, &nsubmeasr);
       *nmeasr = *nmeasr + nsubmeasr;
  } 
  if (IDEBUG>10) printf("ctai_sobs_combine_count; total: %d\n",*nmeasr);

  *retval = ierr;
};




void CTAI_SObs_combine_GetVals(
// Get all the values from a combine-StochObserver
   // INPUTS
   CTAI_SObs_combine *x, // StochObserver from which the measurements are 
                         //    returned
   // OUTPUTS
   CTA_Vector *hvec,      // COSTA-vector containing the values
   int *retval
   )
{
  int nsubmeasr, totmeasr,j;
  int nofsubsobs, offset, isob, ierr;
  CTA_Handle subsob;
  CTA_Vector hsubvec;
  double *val, *subval;  
  CTA_String *strval, *substrval;

  CTA_Datatype dt;  

  nofsubsobs = x->nofsubsobs;
  /* first ask the total number of values  and the datatype*/
  ierr = CTA_Vector_GetSize(*hvec,&totmeasr);
  ierr = CTA_Vector_GetDatatype(*hvec,&dt);
  if (IDEBUG > 5) printf("ctai_sobs_combine_getvals: subsobs;ask total length %d %d \n",nofsubsobs,totmeasr);

  /* allocate the main array containing all the values */
  if(dt == CTA_STRING)  {strval = CTA_Malloc(sizeof(CTA_String)*totmeasr); 
            printf("sobs_combine_getvals: PAS OP: string als propertydatatype!\n");}
  else { val = CTA_Malloc(sizeof(double)*totmeasr);}

  offset = 0;
  for (isob=1; isob<=nofsubsobs; isob++) {
       ierr=CTA_Vector_GetVal(x->subsobs,isob,&subsob,CTA_HANDLE);
       /* first count the the measurements in the subsobs */
       ierr = CTA_SObs_Count(subsob, &nsubmeasr);

       if (nsubmeasr > 0) {

         /* create the vector which receives the values */
         ierr=CTA_Vector_Create(CTA_DEFAULT_VECTOR, nsubmeasr, dt, CTA_NULL, &hsubvec);
         /* receive the values */
         ierr = CTA_SObs_GetVal(subsob, hsubvec);

         /* put the values in the sub-array. In case of String-output, create the strings */
         if (dt == CTA_STRING) {
           substrval = CTA_Malloc(sizeof(CTA_String)*nsubmeasr); 
           for (j=0; j<nsubmeasr; j++)
             { ierr = CTA_String_Create(&substrval[j]);}
           ierr=CTA_Vector_GetVals(hsubvec,substrval, nsubmeasr, dt);
         }
         else {
           subval = CTA_Malloc(sizeof(double)*nsubmeasr);
           ierr=CTA_Vector_GetVals(hsubvec,subval, nsubmeasr, dt);
         }
         
         /* fill the main vector  */
         if (dt == CTA_STRING) { 
           for (j=0; j < nsubmeasr; j++) {
             //the duplicate function already creates the cta_string for us.
             ierr = CTA_String_Duplicate(substrval[j], &strval[offset+j]); 
           }
         } else {
           for (j=0; j < nsubmeasr; j++) {
             val[offset+j] = subval[j];
           }
         }
         
         offset = offset + nsubmeasr;
         ierr = CTA_Vector_Free(&hsubvec);

         // In case of String-output, erase the strings
         if (dt == CTA_STRING)
           {
             for (j=0; j<nsubmeasr; j++) {
               ierr = CTA_String_Free(&substrval[j]);}
             free(substrval);
           } else {
           free(subval);}
         
       }
  }
  /* fill hvec with the main array */
  if (dt == CTA_STRING) {
    ierr=CTA_Vector_SetVals(*hvec,strval, totmeasr, dt);
    for (j=0; j<totmeasr; j++)
      ierr = CTA_String_Free(&strval[j]);
    free(strval);
  } else {
    ierr=CTA_Vector_SetVals(*hvec,val, totmeasr, dt);
    free(val);
  }

  // for (j=0; j < totmeasr; j++) {printf("%d %f\n",j, val[j]);    }

  *retval = ierr;

};

void CTAI_SObs_combine_GetTimes(
// Get all the times from a combine-StochObserver
   // INPUTS
   CTAI_SObs_combine *x, // StochObserver from which the measurements are 
                         //    returned
   // OUTPUTS
   CTA_Vector *hvec,      // COSTA-vector containing the values
   int *retval
   )
{ int nsubmeasr, offset,j, totmeasr; 
  int nofsubsobs, isob, ierr;
  CTA_Handle subsob;
  CTA_Vector hsubvec;  
  double *val, *subval;  
  CTA_Datatype dt;  

  nofsubsobs = x->nofsubsobs;
  /* first ask the total number of values  and the datatype*/
  ierr = CTA_Vector_GetSize(*hvec,&totmeasr);
  ierr = CTA_Vector_GetDatatype(*hvec,&dt);
  if (IDEBUG > 0) printf("ctai_sobs_combine_gettimes: subsobs;ask total length %d %d \n",nofsubsobs,totmeasr);

  /* allocate the main array containing all the values */
  val = CTA_Malloc(sizeof(double)*totmeasr);

  offset = 0;
  for (isob=1; isob<=nofsubsobs; isob++) {
       ierr=CTA_Vector_GetVal(x->subsobs,isob,&subsob,CTA_HANDLE);
       /* first count the the measurements in the subsobs */
       ierr = CTA_SObs_Count(subsob, &nsubmeasr);
       /* create the vector which receives the values */
       ierr=CTA_Vector_Create(CTA_DEFAULT_VECTOR, nsubmeasr, dt, CTA_NULL, &hsubvec);
       /* receive the values */
       ierr = CTA_SObs_GetTimes(subsob, hsubvec);
       
       /* put the values in the sub-array */
       subval = CTA_Malloc(sizeof(double)*nsubmeasr);
       ierr=CTA_Vector_GetVals(hsubvec,subval, nsubmeasr, dt);

       for (j=0; j < nsubmeasr; j++) { val[offset+j] = subval[j] ; }
       offset = offset + nsubmeasr;
       free(subval);
       ierr = CTA_Vector_Free(&hsubvec);
  }
  /* fill hvec with the main array */
  ierr=CTA_Vector_SetVals(*hvec,val, totmeasr, dt);
  free(val);
  if (IDEBUG>0) {printf("sobs_combine_gettimes: total %d, first,last:%f %f \n",totmeasr,val[0],val[totmeasr-1]);}

  // for (j=0; j < totmeasr; j++) {printf("%d %f\n",j, val[j]);    }

  *retval = ierr;

};

void CTAI_SObs_combine_GetVariances(
// Get all the variances of the measurements in a combine-StochObserver
   // INPUTS
   CTAI_SObs_combine *x, // StochObserver from which the measurements are 
                         //    returned
   int *returnvar,        //return variance (CTA_TRUE) or std (CTA_FALSE)
   // OUTPUTS
   CTA_Vector *hvec,      // COSTA-vector containing the values
   int *retval
   )
{
  int nsubmeasr, offset, totmeasr; 
  int nofsubsobs, isob, j,ierr;
  CTA_Handle subsob;
  CTA_Vector hsubvec;  
  double *val, *subval;  
  CTA_Datatype dt;  


  nofsubsobs = x->nofsubsobs;
  /* first ask the total number of values  and the datatype*/
  ierr = CTA_Vector_GetSize(*hvec,&totmeasr);
  ierr = CTA_Vector_GetDatatype(*hvec,&dt);
  /* allocate the main array containing all the values */
  val = CTA_Malloc(sizeof(double)*totmeasr);
  offset = 0;

  for (isob=1; isob<=nofsubsobs; isob++) {
       ierr=CTA_Vector_GetVal(x->subsobs,isob,&subsob,CTA_HANDLE);
       /* first count the the measurements in the subsobs */
       ierr = CTA_SObs_Count(subsob, &nsubmeasr);
       /* create the vector which receives the values */
       ierr=CTA_Vector_Create(CTA_DEFAULT_VECTOR, nsubmeasr, dt, CTA_NULL, &hsubvec);
       /* receive the values */
       if (*returnvar == CTA_TRUE) {
         ierr = CTA_SObs_GetVar(subsob,hsubvec);
       }
       else {
         ierr = CTA_SObs_GetStd(subsob,hsubvec);
       }
       /* put the values in the sub-array */
       subval = CTA_Malloc(sizeof(double)*nsubmeasr);
       ierr=CTA_Vector_GetVals(hsubvec,subval, nsubmeasr, dt);
       /* fill the main vector  */
       for (j=0; j < nsubmeasr; j++) { val[offset+j] = subval[j]; }
       offset = offset + nsubmeasr;
       free(subval);
       ierr = CTA_Vector_Free(&hsubvec);
  }
  /* fill hvec with the main array */
  ierr=CTA_Vector_SetVals(*hvec,val, totmeasr, dt);
  free(val);

  *retval = ierr;

};


void CTAI_SObs_combine_GetRealisation(
// Calculate stochastic realizations for all the measurements in a combine-StochObserver
   // INPUTS
   CTAI_SObs_combine *x,  // StochObserver from which the measurements are 
                         //    returned
   // OUTPUTS
   CTA_Vector *hvec,      // COSTA-vector containing the realizations
   int *retval
   )
{
  int nsubmeasr, offset, totmeasr; 
  int nofsubsobs, isob, j,ierr;
  CTA_Handle subsob;
  CTA_Vector hsubvec;  
  double *val, *subval;  
  CTA_Datatype dt;  



  nofsubsobs = x->nofsubsobs;
  /* first ask the total number of values  and the datatype*/
  ierr = CTA_Vector_GetSize(*hvec,&totmeasr);
  ierr = CTA_Vector_GetDatatype(*hvec,&dt);

  /* allocate the main array containing all the values */
  val = CTA_Malloc(sizeof(double)*totmeasr);

  offset = 0;
  for (isob=1; isob<=nofsubsobs; isob++) {
       ierr=CTA_Vector_GetVal(x->subsobs,isob,&subsob,CTA_HANDLE);
       /* first count the the measurements in the subsobs */
       ierr = CTA_SObs_Count(subsob, &nsubmeasr);
       /* create the vector which receives the values */
       ierr=CTA_Vector_Create(CTA_DEFAULT_VECTOR, nsubmeasr, dt, CTA_NULL, &hsubvec);
       /* Call the getRealisation of the subsob */
       ierr = CTA_SObs_GetRealisation(subsob,hsubvec);
       
       /* put the values in the sub-array */
       subval = CTA_Malloc(sizeof(double)*nsubmeasr);
       ierr=CTA_Vector_GetVals(hsubvec,subval, nsubmeasr, dt);
       /* fill the main vector  */
       for (j=0; j < nsubmeasr; j++) { val[offset+j] = subval[j]; }
       offset = offset + nsubmeasr;
       free(subval);
       ierr = CTA_Vector_Free(&hsubvec);
  }
  /* fill hvec with the main array */
  ierr=CTA_Vector_SetVals(*hvec,val, totmeasr, dt);
  free(val);
  // for (j=0; j < totmeasr; j++) {printf("%d %f\n",j, val[j]);    }

  *retval = ierr;

};

void CTAI_SObs_combine_GetCovMat(
// Get all the variances of the measurements in a combine-StochObserver
   // INPUTS
   CTAI_SObs_combine *x,  // StochObserver from which the measurements are 
                         //    returned
   // OUTPUTS
   CTA_Matrix *hmat,      /* Covariance matrix */
   int *retval
   )
{
  int nofsubsobs, isob, offset, j,k,ierr;
  int nsubmeasr, nmeasr_tot,n,m;
  CTA_Handle subsob;
  CTA_Matrix hsubmat;
  double *val, *subval, dzero;
  CTA_Datatype dt;  

  nofsubsobs = x->nofsubsobs;
  /* first ask the total number of values  and the datatype*/
  ierr = CTA_Matrix_GetSize(*hmat,&m, &n);
  if (n != m ) printf("ctai_sobs_combine_getcovmat: ERROR: matrix is not square \n");
  nmeasr_tot = m;
  ierr = CTA_Matrix_GetDatatype(*hmat,&dt);

  /* allocate the main array containing all the values */
  val = CTA_Malloc(sizeof(double)*nmeasr_tot*nmeasr_tot);

  dzero = 0.0;
  offset = 0;

  /* initialize the combined matrix */
  ierr = CTA_Matrix_SetConstant(*hmat, &dzero, dt);

  for (isob=1; isob<=nofsubsobs; isob++) {
       ierr=CTA_Vector_GetVal(x->subsobs,isob,&subsob,CTA_HANDLE);
       /* first count the the measurements in the subsobs */
       ierr = CTA_SObs_Count(subsob, &nsubmeasr);
       //       printf("sobs_combine_getcovmat: sobs_count %d %d \n",nsubmeasr,ierr);CTA_Matrix_Export(*hmat,CTA_FILE_STDOUT);

       if (nsubmeasr > 0) {
         /* create the matrix which receives the values */
         ierr=CTA_Matrix_Create(CTA_DEFAULT_MATRIX, nsubmeasr, nsubmeasr, dt, CTA_NULL, &hsubmat);
         ierr = CTA_Matrix_SetConstant(hsubmat, &dzero, dt);

         /* Call the getcovmat of the subsob */
         ierr = CTA_SObs_GetCovMat(subsob,hsubmat);

         /* Fill the combined matrix. It is a block matrix, so the first and third quadrant
            should contain zeros. This is the case since we initialized the matrix in the beginning. 
         */
         subval = CTA_Malloc(sizeof(double)*nsubmeasr*nsubmeasr);
         ierr = CTA_Matrix_GetVals(hsubmat, subval, nsubmeasr, nsubmeasr, dt);

         /* fill the main vector  */

         for (j=0; j < nsubmeasr; j++) {
           for (k=0; k < nsubmeasr; k++) {
             val[(offset+j)*nmeasr_tot + offset + k] = subval[j*nsubmeasr + k]; }
         }
         
         offset = offset + nsubmeasr;
         free(subval);
         ierr = CTA_Matrix_Free(&hsubmat);
       }
  }
  /* fill hmat with the main array */
  ierr=CTA_Matrix_SetVals(*hmat,val, nmeasr_tot, nmeasr_tot, dt);
  free(val);

 
  *retval = ierr;
};


void CTAI_SObs_combine_export(
   CTAI_SObs_combine *x,
   CTA_Handle *userdata,
   int *retval
  )
{
  int nofsubsobs, isob, ierr;
  CTA_Handle subsob;
  nofsubsobs = x->nofsubsobs;
  for (isob=1; isob<=nofsubsobs; isob++) {
       ierr=CTA_Vector_GetVal(x->subsobs,isob,&subsob,CTA_HANDLE);
       ierr = CTA_SObs_Export(subsob, *userdata);
  } 
  *retval = ierr;
};


void CTAI_SObs_combine_Free(
   CTAI_SObs_combine *x,
   int *retval
  )
{  
  int nofsubsobs, isob, ierr;
  CTA_Handle subsob;
  nofsubsobs = x->nofsubsobs;
  for (isob=1; isob<=nofsubsobs; isob++) {
       ierr=CTA_Vector_GetVal(x->subsobs,isob,&subsob,CTA_HANDLE);
       ierr = CTA_SObs_Free(&subsob);
  } 
  *retval = ierr;
}


/*

*/
void CTA_SObs_combine_initialise(CTA_SObsClass *hsobscl)
{
   CTA_Intf hintf=0;
   CTA_Func h_func[CTA_SOBS_NUMFUNC];
   CTA_ObsDescrClass descrcl;
   int i;

   // The vector h_func is filled with COSTA-function handles of the 
   // implementations in this file.
   for (i=0;i<CTA_SOBS_NUMFUNC;i++){
      h_func[i]=CTA_NULL;
   }
   CTA_Func_Create(" ",&CTAI_SObs_combine_Create_Size,    hintf,
                       &h_func[CTA_SOBS_CREATE_SIZE]);
   CTA_Func_Create(" ",&CTAI_SObs_combine_Create_Init,    hintf,
                       &h_func[CTA_SOBS_CREATE_INIT]);
   CTA_Func_Create(" ",&CTAI_SObs_combine_CreateSel  ,    hintf,
                       &h_func[CTA_SOBS_CREATE_SELECTION]);
   CTA_Func_Create(" ",&CTAI_SObs_combine_Count,          hintf,
                       &h_func[I_CTA_SOBS_COUNT]      );
   CTA_Func_Create(" ",&CTAI_SObs_combine_GetVals,        hintf,
                       &h_func[CTA_SOBS_GET_VALUES] );
   CTA_Func_Create(" ",&CTAI_SObs_combine_GetTimes,       hintf,
                       &h_func[CTA_SOBS_GET_TIMES] );
   CTA_Func_Create(" ",&CTAI_SObs_combine_GetVals,        hintf,
                       &h_func[CTA_SOBS_GET_EXPECTATION]);
   CTA_Func_Create(" ",&CTAI_SObs_combine_GetRealisation, hintf,
                       &h_func[CTA_SOBS_GET_REALISATION]);
   CTA_Func_Create(" ",&CTAI_SObs_combine_GetVariances,   hintf,
                       &h_func[CTA_SOBS_GET_VARIANCE]);
   CTA_Func_Create(" ",&CTAI_SObs_combine_GetCovMat,      hintf,
                       &h_func[CTA_SOBS_GET_COV_MATRIX]);
   CTA_Func_Create(" ",&CTAI_SObs_combine_export,     hintf,
                       &h_func[I_CTA_SOBS_EXPORT]     );
   CTA_Func_Create(" ",&CTAI_SObs_combine_Free,     hintf,
                            &h_func[I_CTA_SOBS_FREE]     );
   
   CTA_ObsDescr_combine_initialise(&descrcl);

   CTA_SObs_DefineClass("cta_sobs_combine",h_func,descrcl,hsobscl);


}





