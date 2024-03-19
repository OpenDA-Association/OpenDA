/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_modbuild_sp.c $
$Revision: 3407 $, $Date: 2012-08-17 13:50:50 +0200 (Fri, 17 Aug 2012) $

COSTA: Problem solving environment for data assimilation
Copyright (C) 2006  Nils van Velzen

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
//#include<dlfcn.h>
#include "cta_mem.h"
#include "cta.h"
#include "ctai.h"
#include "f_cta_utils.h"
#include "cta_model_utilities.h"
#include "cta_util_statistics.h"
#include "cta_metainfo.h"
#include "cta_message.h"

#define INDX_THIS              ( 0) /* Handle of instance               */
#define INDX_TIME              ( 1) /* Time instance of model (state)   */
#define INDX_TIMEHORIZON       ( 2) /* Time instance of model (state)   */
#define INDX_STATE             ( 3) /* State vector of model            */
#define INDX_ADD_NOISE_SPAN    ( 4)
#define INDX_AXPY_FORC_SPAN    ( 5) /* Timespan to add given offset to forcings */
#define INDX_AXPY_FORC         ( 6) /* offset to be added to model forcings     */
#define INDX_PARAM             ( 7) /* State vector of model parameters */
#define INDX_NAME_NOISE        ( 8)
#define INDX_NNOISE            ( 9)
#define INDX_ZERO_FORC         (10) /* zero noise vector */
#define INDX_USRDATA           (11)
#define INDX_USRFUNC_CREATE    (12)
#define INDX_USRFUNC_COVAR     (13)
#define INDX_USRFUNC_OBS       (14)
#define INDX_USRFUNC_OBSSEL    (15)
#define INDX_USRFUNC_COMPUTE   (16)
#define INDX_USRFUNC_FREE      (17)
#define INDX_FORC              (18) /* State vector of forcings */
#define INDX_ADJ_SPAN          (19) /* Time span to prepaire adjoint for */
#define INDX_ADJ_DESCR_FORC    (20) /* Observation description describing the forcings of the adjoint */
#define INDX_ADJ_VFORC         (21) /* Vector The forcings of the adjoint run that have been set */
#define SIZE_DATABLK           (22)

#define IDEBUG (0)
#define CLASSNAME "CTA_Modbuild_sp"

void modbuild_sp_create_size(CTA_Handle userdata, int *memsize, int *ierr){
   /* Create size for datablock holding:
      0 CTA_Model:  mthis     Handle of this model
      1 CTA_Time :  time      Time instance of model
      2 CTA_TreeVector:  state     Model state
      3 CTA_TreeVector:  snoise    Noise offset state-vector
      4 CTA_TreeVector:  sparam    Parameter state-vector
      5 CTA_Handle: tusrmod   User data for model
      6... CTA_Func: func     User functions
   */
   *memsize=sizeof(CTA_Handle)*(SIZE_DATABLK);
}

/** \brief Get a user function as specified in the input-tree
 *  \note internal function
 *
 *  \param tinput  I  input-configuration tree
 *  \param path    I  path in input tree to user function
 *  \param func    O  handle to user function
 *  \return CTA_OK if successful
 */
#undef METHOD
#define METHOD "get_user_function" 
int modbuild_sp_getusrfunction(CTA_Tree tinput, char *path, CTA_Func *func){
   CTA_Handle hfunc;
   CTA_Func func_wrk;
   CTA_Datatype datatype;
   int retval;

   retval=CTA_Tree_GetHandleStr(tinput,path, &hfunc);
   if (retval==CTA_OK) {
      CTA_Handle_GetDatatype(hfunc, &datatype);
      if (datatype==CTA_STRING) {
         retval=CTA_Handle_Find(hfunc, CTA_FUNCTION, &func_wrk);
         if (retval!=CTA_OK){
			 char message[1024];
             sprintf(message,"Cannot find the function that is specified in %s \n",path);
             CTA_WRITE_INFO(message);		
             return retval;
         }
         retval=CTA_Func_Duplicate(func_wrk,func);
         if (retval!=CTA_OK) return retval;
      } else if (datatype==CTA_FUNCTION) {
         retval=CTA_Func_Duplicate(hfunc,func);
         if (retval!=CTA_OK) return retval;
      } else {
	      char message[1024];
		  sprintf(message,"Specification in \"%s\" is neither CTA_STRING nor CTA_FUNC \n",path);
          CTA_WRITE_ERROR(message);
          return CTA_INCOMPATIBLE_HANDLE;
      }
   } else {
	  char message[1024];
      sprintf(message,"Path \"%s\" is not specified in input\n",path);
      CTA_WRITE_INFO(message);
   }
   return CTA_OK;
}

#undef METHOD
#define METHOD "Create" 
void modbuild_sp_create_init(CTA_Handle *this, CTA_Handle *data, CTA_Handle *hinput, int *ierr){
   int cleanup;               /*Flag indicating whether tinput must be freed */
   CTA_Func fusrcreate;       /* Function handle of user implementation      */
   CTA_Function *usrcreate;   /* Function pointer of user implementation     */
   CTA_Tree tinput;           /* Input configuration tree                    */
   int retval;                /* return status of COSTA method               */
   CTA_String funcar1;        /* Name of ar(1)-function                      */
   CTA_Handle hmodelinput;    /* Handle to model specific input              */
   CTA_Handle hspecialinput;  /* Handle to special model input               */
   double dzero;              /* double with value 0.0                       */
   double tstart, tend;       /* start and end time of time horizon of model */

   if (IDEBUG) printf("Start of modbuild_sp_create_init\n");

   dzero=0.0;


   /* Convert input into a COSTA-tree if it is not done already */
    *ierr=CTA_Model_Util_InputTree(*hinput, &tinput, &cleanup);
    if (*ierr!=CTA_OK) return;

   /* Initialise DATA */

     data[INDX_THIS]            =*this;
     data[INDX_STATE]           =CTA_NULL;
     data[INDX_AXPY_FORC]       =CTA_NULL;
     data[INDX_PARAM]           =CTA_NULL;
     data[INDX_NNOISE]          =0;
     data[INDX_ZERO_FORC]       =CTA_NULL;
     data[INDX_USRDATA]         =CTA_NULL;
     data[INDX_USRFUNC_CREATE]  =CTA_NULL;
     data[INDX_USRFUNC_COVAR]   =CTA_NULL;
     data[INDX_USRFUNC_OBS]     =CTA_NULL;
     data[INDX_USRFUNC_OBSSEL]  =CTA_NULL;
     data[INDX_USRFUNC_COMPUTE] =CTA_NULL;
     data[INDX_USRFUNC_FREE]    =CTA_NULL;


     *ierr=CTA_Time_Create(&data[INDX_TIME]);
     *ierr=CTA_Time_Create(&data[INDX_TIMEHORIZON]);
     *ierr=CTA_Time_Create(&data[INDX_ADD_NOISE_SPAN]);
     *ierr=CTA_Time_Create(&data[INDX_AXPY_FORC_SPAN]);
     *ierr=CTA_String_Create(&data[INDX_NAME_NOISE]);
     *ierr=CTA_Time_Create(&data[INDX_ADJ_SPAN]);

     /* First, check for a special 'model builder'-model: the AR(1) process model */

     retval=CTA_Tree_GetHandleStr(tinput,"/modelbuild_sp/special_ar1",
                                  &hspecialinput);

     if (retval ==CTA_OK) {
       /* skip the remainder and provide the special ar(1) model */
       /*  define the following functions directly:
             modbuild_sp_ar1_compute
             modbuild_sp_ar1_covar

       */
       *ierr = CTA_String_Create(&funcar1);

       *ierr = CTA_String_Set(funcar1, "modbuild_sp_ar1_create");
       *ierr=CTA_Handle_Find(funcar1, CTA_FUNCTION, &data[INDX_USRFUNC_CREATE]);
       *ierr = CTA_String_Set(funcar1, "modbuild_sp_ar1_compute");
       *ierr=CTA_Handle_Find(funcar1, CTA_FUNCTION, &data[INDX_USRFUNC_COMPUTE]);
       *ierr = CTA_String_Set(funcar1, "modbuild_sp_ar1_covar");
       *ierr=CTA_Handle_Find(funcar1, CTA_FUNCTION, &data[INDX_USRFUNC_COVAR]);
       *ierr = CTA_String_Set(funcar1, "modbuild_sp_ar1_getobsval");
       *ierr=CTA_Handle_Find(funcar1, CTA_FUNCTION, &data[INDX_USRFUNC_OBS]);

        retval=CTA_Tree_GetHandleStr(tinput,"/modelbuild_sp/special_ar1", &hmodelinput);


     }
     else {
       /* Get the provided function names from the input and store the
          corresponding function handles in data-array */
        retval=modbuild_sp_getusrfunction(tinput, "/modelbuild_sp/functions/create",     &data[INDX_USRFUNC_CREATE]);
        retval=modbuild_sp_getusrfunction(tinput,"/modelbuild_sp/functions/covariance",  &data[INDX_USRFUNC_COVAR]);
        retval=modbuild_sp_getusrfunction(tinput, "/modelbuild_sp/functions/getobsvals", &data[INDX_USRFUNC_OBS]);
        retval=modbuild_sp_getusrfunction(tinput, "/modelbuild_sp/functions/getobssel",  &data[INDX_USRFUNC_OBSSEL]);
        retval=modbuild_sp_getusrfunction(tinput, "/modelbuild_sp/functions/compute",    &data[INDX_USRFUNC_COMPUTE]);
        retval=modbuild_sp_getusrfunction(tinput, "/modelbuild_sp/functions/free",       &data[INDX_USRFUNC_FREE]);


        if (IDEBUG>0) {
          printf("Modbuild_sp: the following function handles have been created:\n");
          printf("create: %d \n",data[INDX_USRFUNC_CREATE]);
          printf("covariance: %d \n",data[INDX_USRFUNC_COVAR]);
          printf("getobsvals: %d \n",data[INDX_USRFUNC_OBS]);
          printf("getobssel: %d \n",data[INDX_USRFUNC_OBSSEL]);
          printf("compute: %d \n",data[INDX_USRFUNC_COMPUTE]);
          printf("free: %d \n",data[INDX_USRFUNC_FREE]);
          printf("end message.\n");
        }

        /* Get model input from the input tree */
        retval=CTA_Tree_GetHandleStr(tinput,"/modelbuild_sp/model", &hmodelinput);

     } // end if: if ar1 then use standard function else read from xml-file

     /* Call the user supplied create function */
   fusrcreate=data[INDX_USRFUNC_CREATE];
   //  printf("indx_usrfunc_create %d\n",INDX_USRFUNC_CREATE);
   if (fusrcreate==CTA_NULL) {
       CTA_WRITE_ERROR("Usrfunc_create function does not exist! \n");
      *ierr=CTA_NOT_IMPLEMENTED;
      return;
   }

   /* Get function pointer */
   *ierr=CTA_Func_GetFunc(fusrcreate,&usrcreate);

   if (*ierr!=CTA_OK) return;

   /* CALL USR_CREATE(CTA_Handle *hmodelinput, CTA_TreeVector *state,
                      CTA_TreeVector *sbound, CTA_TreeVector *sparam, int *nnoise,
                      CTA_Time  *time0, CTA_String *snamnoise,
                      CTA_Handle *husrdata, int* ierr)             */

   /* Get input of the model itself */

   /* call user function */
   if (IDEBUG) printf("Calling user function 'usrcreate'\n");
   usrcreate(&hmodelinput, &data[INDX_STATE],  &data[INDX_AXPY_FORC],
             &data[INDX_PARAM], &data[INDX_NNOISE], &data[INDX_TIMEHORIZON],
             &data[INDX_NAME_NOISE], &data[INDX_USRDATA], ierr);
   if (IDEBUG) printf("End of user function 'usrcreate'\n");
   if (*ierr!=CTA_OK) return;

   /* Set time (Start of the time horizon) */
   *ierr=CTA_Time_GetSpan(data[INDX_TIMEHORIZON],&tstart, &tend);
   if (*ierr!=CTA_OK) return;
   *ierr=CTA_Time_SetSpan(data[INDX_TIME], tstart, tend);
   if (*ierr!=CTA_OK) return;

   /* Some additional initialisation: */
       //printf(" additional initialisation \n");

   /* Set forcings axpy state vectors */
   if (data[INDX_AXPY_FORC]!=CTA_NULL) {
      *ierr=CTA_TreeVector_SetConstant(data[INDX_AXPY_FORC], &dzero,CTA_DOUBLE);
      if (*ierr!=CTA_OK) return;
      *ierr=CTA_TreeVector_Duplicate(data[INDX_AXPY_FORC],&data[INDX_ZERO_FORC]);
      if (*ierr!=CTA_OK) return;
   }

   *ierr=CTA_Time_SetSpan(data[INDX_ADD_NOISE_SPAN],0.0,-1.0); /* empty span */
   if (*ierr!=CTA_OK) return;
   *ierr=CTA_Time_SetSpan(data[INDX_AXPY_FORC_SPAN],0.0,-1.0); /* empty span */
   if (*ierr!=CTA_OK) return;
   *ierr=CTA_Time_SetSpan(data[INDX_ADJ_SPAN],0.0,-1.0);      /* empty span */
   if (*ierr!=CTA_OK) return;

   if (IDEBUG) printf("before cleaning up the input tree\n");


   /* Clean-up input tree */
   if (cleanup==CTA_TRUE){
      *ierr=CTA_Tree_Free(&tinput);
   }
   *ierr=CTA_OK;
   if (IDEBUG) printf(" end of create_init \n");
}

void modbuild_sp_export(CTA_Handle *data, CTA_Handle *hexport, int *ierr){

   CTA_Datatype datatype;

   /* Get type of handle of hexport */
   *ierr=CTA_Handle_GetDatatype(*hexport, &datatype);
   if (*ierr != CTA_OK) return;

   /* Export header */
   if (datatype==CTA_PACK){
      CTA_Pack_Add(*hexport,data,sizeof(CTA_HANDLE)*SIZE_DATABLK);
   }
   else if (datatype==CTA_FILE){
      *ierr=CTA_FORMAT_NOT_SUPPORTED;
      return;
   }
   else {
      *ierr=CTA_FORMAT_NOT_SUPPORTED;
      return;
   }

   /* Pack current time */
   if (data[INDX_TIME]!=CTA_NULL){
      CTA_Time_Export(data[INDX_TIME], *hexport);
   }

   /* Pack time horizon */
   if (data[INDX_TIMEHORIZON]!=CTA_NULL){
      CTA_Time_Export(data[INDX_TIMEHORIZON], *hexport);
   }


   /* Pack state-vector */
   if (data[INDX_STATE]!=CTA_NULL){
      CTA_TreeVector_Export(data[INDX_STATE], *hexport);
   }
   /* Pack noise span */
   if (data[INDX_ADD_NOISE_SPAN]!=CTA_NULL){
      CTA_Time_Export(data[INDX_ADD_NOISE_SPAN], *hexport);
   }
   /* Pack forcings span */
   if (data[INDX_AXPY_FORC_SPAN]!=CTA_NULL){
      CTA_Time_Export(data[INDX_AXPY_FORC_SPAN], *hexport);
   }

   /* Pack adjoint span */
   if (data[INDX_ADJ_SPAN]!=CTA_NULL){
      CTA_Time_Export(data[INDX_ADJ_SPAN], *hexport);
   }

   /* Pack forcings addition */
   if (data[INDX_AXPY_FORC]!=CTA_NULL){
      CTA_TreeVector_Export(data[INDX_AXPY_FORC], *hexport);
   }

   /* Pack model parameters */
   if (data[INDX_PARAM]!=CTA_NULL){
      CTA_TreeVector_Export(data[INDX_PARAM], *hexport);
   }
}


void modbuild_sp_import(CTA_Handle *data, CTA_Handle *himport, int *ierr){

   CTA_Handle header[SIZE_DATABLK];
   CTA_Datatype datatype;

   /* Get type of handle of hexport */
   *ierr=CTA_Handle_GetDatatype(*himport, &datatype);
   if (*ierr != CTA_OK) return;

   /* Import header */
   if (datatype==CTA_PACK){
      CTA_Pack_Get(*himport,header,sizeof(CTA_HANDLE)*SIZE_DATABLK);
   }
   else if (datatype==CTA_FILE){
      *ierr=CTA_FORMAT_NOT_SUPPORTED;
      return;
   }
   else {
      *ierr=CTA_FORMAT_NOT_SUPPORTED;
      return;
   }

   /* Unpack current time */
   if (header[INDX_TIME]!=CTA_NULL){
      CTA_Time_Import(data[INDX_TIME], *himport);
   }

   /* Unpack time horizon */
   if (header[INDX_TIMEHORIZON]!=CTA_NULL){
      CTA_Time_Import(data[INDX_TIMEHORIZON], *himport);
   }

   /* Unpack state-vector */
   if (header[INDX_STATE]!=CTA_NULL){
      CTA_TreeVector_Import(data[INDX_STATE], *himport);
   }

   /* Unpack noise span */
   if (header[INDX_ADD_NOISE_SPAN]!=CTA_NULL){
      CTA_Time_Import(data[INDX_ADD_NOISE_SPAN], *himport);
   }

   /* Unpack forcings span */
   if (header[INDX_AXPY_FORC_SPAN]!=CTA_NULL){
      CTA_Time_Import(data[INDX_AXPY_FORC_SPAN], *himport);
   }

   /* Unpack adjoint span */
   if (header[INDX_ADJ_SPAN]!=CTA_NULL){
      CTA_Time_Import(data[INDX_ADJ_SPAN], *himport);
   }

   /* Unpack forcings addition */
   if (header[INDX_AXPY_FORC]!=CTA_NULL){
      CTA_TreeVector_Import(data[INDX_AXPY_FORC], *himport);
   }

   /* Unpack model parameters */
   if (header[INDX_PARAM]!=CTA_NULL){
      CTA_TreeVector_Import(data[INDX_PARAM], *himport);
   }
}


void modbuild_sp_adjcompute(CTA_Handle *data ,int* time, int *ierr){
    *ierr=CTA_NOT_IMPLEMENTED;
}

void modbuild_sp_adjprepare(CTA_Handle *data ,int* time, int *ierr){
   /* do we already have a add noise timespan? */
   if (data[INDX_ADJ_SPAN]==CTA_NULL) {
      *ierr=CTA_Time_Create(&data[INDX_ADJ_SPAN]);
      if (*ierr!=CTA_OK) return;
   }
   /* Just copy time span */
   *ierr=CTA_Time_Copy(*time,data[INDX_ADJ_SPAN]);
}


void modbuild_sp_adjsetforc(CTA_Handle *data ,int* hdescr, int *vforc, int *ierr){
    int nmeasr;
    CTA_Vector vforc_temp;

    *ierr=CTA_ObsDescr_Observation_Count(*hdescr, &nmeasr);
    if (*ierr!=CTA_OK) return;
    if (nmeasr<0) return;

    /* Create work variables */
    *ierr=CTA_Vector_Duplicate(*vforc, &vforc_temp);
    if (*ierr!=CTA_OK) return;

    /* TODO WE HAVE TO COPY THE OBJECT HOMEWORK FOR NILS FOR TIME BEEING HOPE THAT OBJECT IS NOT DELETED AND ONLY SAVE HANDLE!!!!!! */
    if (data[INDX_ADJ_DESCR_FORC]!=CTA_NULL){
        *ierr=CTA_Vector_Create(CTA_DEFAULT_VECTOR, 1, CTA_HANDLE, CTA_NULL, &data[INDX_ADJ_DESCR_FORC]);
        *ierr=CTA_Vector_SetVal(data[INDX_ADJ_DESCR_FORC], 1, hdescr, CTA_HANDLE);

        *ierr=CTA_Vector_Duplicate(*vforc, &data[INDX_ADJ_VFORC]);
    }
    *ierr=CTA_Vector_AppendVal(data[INDX_ADJ_DESCR_FORC], hdescr, CTA_HANDLE);

    *ierr=CTA_Vector_AppendVal(data[INDX_ADJ_VFORC], &vforc_temp, CTA_HANDLE);
}


void modbuild_sp_free(CTA_Handle *data ,int *ierr){

   CTA_Func fusrfree;       /* Function handle of user implementation       */
   CTA_Function *usrfree;   /* Function pointer of user implementation      */
   CTA_Handle husrdata;     /* Handle to user data                          */

   fusrfree  = data[INDX_USRFUNC_FREE];
   husrdata  = data[INDX_USRDATA];

   if (fusrfree!=CTA_NULL){
      /* Get function pointer */
      *ierr=CTA_Func_GetFunc(fusrfree,&usrfree);
      if (*ierr!=CTA_OK) return;
      usrfree(&husrdata, ierr);
   }
    *ierr=CTA_OK;
}

#undef METHOD
#define METHOD "Compute"
void modbuild_sp_compute(
          CTA_Handle *data,
          CTA_Time *timSim, /* current time, "now" */
          int *ierr)
{
   CTA_TreeVector state;         /* state vector of model                        */
   CTA_TreeVector sparam;        /* state vector of model parameters             */
   CTA_TreeVector saxpyforc;     /* state vector of added forcings               */
   CTA_Func fusrcompute;         /* Function handle of user implementation       */
   CTA_Function *usrcompute;     /* Function pointer of user implementation      */
   CTA_Handle husrdata;          /* Handle to model-spefic data                  */
   CTA_Time timemod;             /* Current time instance of model               */
   CTA_Time timeaddnoise;        /* Time instance for model to add noise         */
   CTA_Time timeaxpy;            /* Timespan to apply given noise axpy           */
   double t1, t2, t1Sim, tDum;   /* start and end of simulation timespan         */
   BOOL inspan;                  /* simulation timespan in inside given span     */
   BOOL baddnoise;               /* Model must add its own noise or not          */
   CTA_Time timespan;            /* Timespan of computations                     */
   int isSpan;                   /* Flag indicating whether timSim is a timespan */
   double eps;                   /* Tollerence for comparing time instances      */


   fusrcompute = data[INDX_USRFUNC_COMPUTE];

   /* check whether we have an implementation and number of noise
      parameters is >0                                           */
   if (fusrcompute==CTA_NULL) {
      CTA_WRITE_ERROR("user compute function does not exist \n");
      *ierr=CTA_NOT_IMPLEMENTED;
      return;
   }

   state        = data[INDX_STATE];
   timemod      = data[INDX_TIME];
   timeaddnoise = data[INDX_ADD_NOISE_SPAN];
   timeaxpy     = data[INDX_AXPY_FORC_SPAN];
   sparam       = data[INDX_PARAM];
   husrdata     = data[INDX_USRDATA];


   /* time management */
   *ierr=CTA_Time_GetSpan(*timSim,&t1Sim, &t2);
   *ierr=CTA_Time_GetSpan(timemod,&t1, &tDum);

   *ierr=CTA_Time_IsSpan(*timSim, &isSpan);
   if (isSpan == CTA_TRUE){
      eps=M_EPS*fabs(t1Sim)+M_EPS;
      if (fabs(t1Sim-t1)>eps) {
         printf("Error in modbuild_sp_compute: current time of model is %f; start of simulation span is %f\n",
            t1Sim,t1);
         exit(-1);
      }
   }

   /* Set the simulation span of this compute */
   *ierr=CTA_Time_Create(&timespan);
   *ierr=CTA_Time_SetSpan(timespan, t1, t2);

   /* Addition axpy of noise */
   *ierr=CTA_Time_InSpan(timespan, timeaxpy, &inspan); /* is "now" inside timeasxpy? */
   if (*ierr!=CTA_OK) {
     CTA_WRITE_ERROR("Timespan is not in interval");
     return;
   }
   if (inspan) {
      saxpyforc=data[INDX_AXPY_FORC];
   } else {
      saxpyforc=data[INDX_ZERO_FORC];
   }

   /* Addition of noise by model itself */

   *ierr=CTA_Time_InSpan(timespan, timeaddnoise, &baddnoise);
   if (*ierr!=CTA_OK) {
     printf("modbuild_sp_compute:cta_time_inspan noise: ierr: %d \n",*ierr);
     return;
      }

   /* Get function pointer */
   *ierr=CTA_Func_GetFunc(fusrcompute,&usrcompute);
   if (*ierr!=CTA_OK) return;

   *ierr=CTA_Time_GetSpan(timespan, &t1, &t2);
//     printf("cta_modbuild_compute:before call my_compute t1, t2:   %f %f \n", t1,t2);
	 fflush(stdout);


   /* CALL USR_COMPUTE(CTA_Time *timesspan,CTA_TreeVector *state,
                      CTA_TreeVector *saxpyforc, BOOL *baddnoise,
                      CTA_TreeVector *sparam, CTA_Handle husrdata,
                      int* ierr) */
   usrcompute(&timespan, &state, &saxpyforc, &baddnoise, &sparam,
               &husrdata, ierr);

   if (*ierr!=CTA_OK) {
     printf("modbuild_sp_compute:usr_compute: WRONG: %d  \n",*ierr);
	 fflush(stdout);
     return;
   }

   // printf("Back in modbuild_sp_compute; adjust timespan  \n");

   *ierr=CTA_Time_SetSpan(timemod, t2, t2);
   if (*ierr!=CTA_OK) {
     printf("modbuild_sp_compute:CTA_Time_SetSpan:  %d   ",*ierr);
	 fflush(stdout);
     return;
   }

   *ierr=CTA_Time_Free(&timespan);

}


void modbuild_sp_setstate(CTA_Handle *data, CTA_TreeVector *state, int *ierr){

     *ierr=CTA_TreeVector_Copy(*state,data[INDX_STATE]);
}

void modbuild_sp_getstate(CTA_Handle *data, CTA_TreeVector *state, int *ierr){

  if (IDEBUG>9) {printf("DEBUG: modbuild_sp_getstate:state :  %d \n",*state);}

     /* Duplicate the state if necessary */
     if (*state==CTA_NULL){

        *ierr=CTA_TreeVector_Duplicate(data[INDX_STATE], state);
     }
     else {
     /* Just copy the state */

        *ierr=CTA_TreeVector_Copy(data[INDX_STATE], *state);
     }
}


void modbuild_sp_axpymodel(CTA_Handle *datay, double *alpha, CTA_Handle *datax,
                           int *ierr){

     *ierr=CTA_TreeVector_Axpy(datay[INDX_STATE], *alpha, datax[INDX_STATE]);
}


void modbuild_sp_axpystate(CTA_Handle *data, double *alpha, CTA_TreeVector *statex,
                           int *ierr){

     *ierr=CTA_TreeVector_Axpy(data[INDX_STATE], *alpha, *statex);
}

void modbuild_sp_setforc(CTA_Handle *data, CTA_TreeVector *state, int *ierr){

     *ierr=CTA_TreeVector_Copy(*state,data[INDX_FORC]);
}

void modbuild_sp_getforc(CTA_Handle *data, CTA_TreeVector *state, int *ierr){

     /* Duplicate the state if necessary */
     if (*state==CTA_NULL){
        *ierr=CTA_TreeVector_Duplicate(data[INDX_FORC], state);
     }
     else {
     /* Just copy the state */
        *ierr=CTA_TreeVector_Copy(data[INDX_FORC], *state);
     }

}

void modbuild_sp_axpyforc(CTA_Handle *data, CTA_Time *tspan, double *alpha,
                          CTA_TreeVector *statex, int *ierr){


   /* Do we have a time object ? */
   if (data[INDX_AXPY_FORC_SPAN]==CTA_NULL) {
      *ierr=CTA_Time_Create(&data[INDX_AXPY_FORC_SPAN]);
      if (*ierr!=CTA_OK) return;
   }
   /* Copy time */
   *ierr=CTA_Time_Copy(data[INDX_AXPY_FORC_SPAN],*tspan);
   if (*ierr!=CTA_OK) return;

   /* Copy state */
   *ierr=CTA_TreeVector_Copy(*statex,data[INDX_AXPY_FORC]);
   if (*ierr!=CTA_OK) return;

   /* apply scaling factor alpha */
   *ierr=CTA_TreeVector_Scal(data[INDX_AXPY_FORC], *alpha);
   if (*ierr!=CTA_OK) return;
}

void modbuild_sp_setparam(CTA_Handle *data, CTA_TreeVector *state, int *ierr){
     *ierr=CTA_TreeVector_Copy(*state,data[INDX_PARAM]);
   /* *ierr=CTA_NOT_IMPLEMENTED; */ /*wednesday, 27-09-06 jhs*/
}

void modbuild_sp_getparam(CTA_Handle *data, CTA_TreeVector *state, int *ierr){

     /* Duplicate the state if necessary */
     if (*state==CTA_NULL){
        *ierr=CTA_TreeVector_Duplicate(data[INDX_PARAM], state);
     }
     else {
     /* Just copy the state */
        *ierr=CTA_TreeVector_Copy(data[INDX_PARAM], *state);
     }

    /* *ierr=CTA_NOT_IMPLEMENTED;*/  /*wednesday, 27-09-06 jhs*/
}

void modbuild_sp_axpyparam(CTA_Handle *data, double *alpha, CTA_TreeVector *statex,
                           int *ierr){
     *ierr=CTA_TreeVector_Axpy(data[INDX_PARAM], *alpha, *statex);
   /* *ierr=CTA_NOT_IMPLEMENTED; */ /*wednesday, 27-09-06 jhs*/
}

void modbuild_sp_getnoisecount(CTA_Handle *data, int* nnoise, int* ierr) {
    *nnoise=data[INDX_NNOISE];
    *ierr=CTA_OK;
}

void modbuild_sp_getcurrenttime(CTA_Handle *data, int* tCurrent, int* ierr) {

   *ierr=CTA_Time_Copy(data[INDX_TIME],*tCurrent);
}

void modbuild_sp_gettimehorizon(CTA_Handle *data, int* tHorizon, int* ierr) {

   *ierr=CTA_Time_Copy(data[INDX_TIMEHORIZON],*tHorizon);
}


void modbuild_sp_getnoisecovar(CTA_Handle *data, CTA_TreeVector *colsvar, int* ierr){
   int nnoise;            /* number of noise parameters              */
   CTA_Handle husrdata;   /* Handle to model-spefic data             */
   CTA_TreeVector state;       /* state vector of model                   */
   CTA_TreeVector snoise;      /* noise state vector                      */
   CTA_TreeVector snoisesub;   /* name of noise state vector              */
   int inoise;            /* loop counter over noise parameters      */
   CTA_Func fusrcovar;    /* Function handle of user implementation  */
   CTA_Function *usrcovar;/* Function pointer of user implementation */
   char *tag;             /* name/tag of substate with noise         */
   int  len;              /* length of string tag (excluding char(0))*/

   nnoise    = data[INDX_NNOISE];
   state     = data[INDX_STATE];
   snoisesub = data[INDX_NAME_NOISE];
   snoise    = CTA_NULL;
   fusrcovar = data[INDX_USRFUNC_COVAR];
   husrdata  = data[INDX_USRDATA];

   /* check whether we have an implementation and number of noise
      parameters is >0                                           */


   if (nnoise==0 || fusrcovar==CTA_NULL) {
      *ierr=CTA_NOT_IMPLEMENTED;
      return;
   }


      /* create state-vectors when colsvar contains CTA_NULL */
   for (inoise=0; inoise<nnoise; inoise++) {
      if (colsvar[inoise]==CTA_NULL){
         /* we need to create states of noise parameters */
         /* Check whether we already have handle to noise */
         /* if not get it */
         if (snoise==CTA_NULL){
             /* Get name of substate */
             *ierr=CTA_String_GetLength(snoisesub, &len);
             if (*ierr!=CTA_OK) return;
             tag=CTA_Malloc(sizeof(char)*(len+1));
             *ierr=CTA_String_Get(snoisesub, tag);
             if (*ierr!=CTA_OK) return;
             /* get handle to noise substate */

             *ierr=CTA_TreeVector_GetSubTreeVec(state, tag, &snoise);

             if (*ierr!=CTA_OK) return;
             free(tag);
         }
         /* duplicate noise state */
         *ierr=CTA_TreeVector_Duplicate(snoise, &colsvar[inoise]);
         if (*ierr!=CTA_OK) return;
      }
   }
   /* Get function pointer */

   *ierr=CTA_Func_GetFunc(fusrcovar,&usrcovar);
   if (*ierr!=CTA_OK) return;


  /* CALL USR_COVAR(CTA_TreeVector *colsvar,int* nnoise, CTA_Handle husrdata,
                    int* ierr) */
  usrcovar(colsvar, &nnoise, &husrdata, ierr);
}


void modbuild_sp_getobsvalues(CTA_Handle *data, CTA_Time *ttime,
                               CTA_ObsDescr *hdescr, CTA_Vector *vval,
                               int* ierr){

   CTA_TreeVector state;      /* state vector of model                   */
   CTA_Handle husrdata;  /* Handle to model-spefic data             */
   CTA_Func fusrobs;     /* Function handle of user implementation  */
   CTA_Function *usrobs; /* Function pointer of user implementation */

   state     = data[INDX_STATE];
   fusrobs = data[INDX_USRFUNC_OBS];
   husrdata  = data[INDX_USRDATA];

   /* check whether we have an implementation and number of noise
      parameters is >0                                           */
   if (fusrobs==CTA_NULL) {
      *ierr=CTA_NOT_IMPLEMENTED;
      return;
   }

   /* Get function pointer */
   *ierr=CTA_Func_GetFunc(fusrobs,&usrobs);
   if (*ierr!=CTA_OK) return;

   /* CALL USR_OBS(CTA_TreeVector *state ,CTA_ObsDescr *hdescr CTA_Vector *vval,
                    CTA_Handle husrdata, int* ierr) */
   usrobs(&state, hdescr, vval, &husrdata, ierr);

}

void modbuild_sp_getobsselect(CTA_Handle *data, CTA_Time *ttime, CTA_ObsDescr *hdescr,
                                CTA_String *sselect, int* ierr){

   CTA_TreeVector state;         /* state vector of model                   */
   CTA_Handle husrdata;     /* Handle to model-spefic data             */
   CTA_Func fusrobssel;     /* Function handle of user implementation  */
   CTA_Function *usrobssel; /* Function pointer of user implementation */

   if (IDEBUG>10){printf("modbuild_sp_getobsselect START \n");}

   state     = data[INDX_STATE];
   fusrobssel = data[INDX_USRFUNC_OBSSEL];
   husrdata  = data[INDX_USRDATA];

   /* check whether we have an implementation and number of noise
      parameters is >0                                           */
   if (fusrobssel==CTA_NULL) {
   if (IDEBUG>10){printf("modbuild_sp_getobsselect ; returning 1>0 \n");}
      *ierr=CTA_String_Set(*sselect," 1 >= 0 ");
      return;
   }

   /* Get function pointer */
   *ierr=CTA_Func_GetFunc(fusrobssel,&usrobssel);
   if (*ierr!=CTA_OK) return;

   /* CALL USR_OBSSEL(CTA_TreeVector *state, CTA_Time *ttime, CTA_ObsDescr *hdescr,
                                CTA_String *sselect, CTA_Handle husrdata,
                                int* ierr)             */
   usrobssel(&state, ttime, hdescr, sselect, &husrdata, ierr);

}

void modbuild_sp_addnoise(CTA_Handle *data, CTA_Time *ttime, int* ierr){

   /* do we already have a add noise timespan? */
   if (data[INDX_ADD_NOISE_SPAN]==CTA_NULL) {
      *ierr=CTA_Time_Create(&data[INDX_ADD_NOISE_SPAN]);
      if (*ierr!=CTA_OK) return;
   }
   /* Just copy time span */
   *ierr=CTA_Time_Copy(*ttime,data[INDX_ADD_NOISE_SPAN]);
}






   /* CALL USR_CREATE(CTA_Handle *hmodelinput, CTA_TreeVector *state,
                      CTA_TreeVector *sbound, CTA_TreeVector *sparam, int *nnoise,
                      CTA_Time  *time0, CTA_String *snamnoise,
                      CTA_Handle *husrdata, int* ierr)             */


/*
typedef struct {
char name[20];
  BOOL is2D;
  int nx, ny, nz,nsize;
double x_origin, y_origin, z_origin;
double dx,dy,dz;
} CTAI_Grid;

typedef struct {
char name[20];
  int npoints;
double x_origin, y_origin;
CTA_Vector h_x_coords,h_y_coords;
} CTAI_curve;
*/


void modbuild_sp_ar1_create(CTA_Handle *hinput, CTA_TreeVector *state,
                      CTA_TreeVector *sbound, CTA_TreeVector *sparam, int *nnoise,
                      CTA_Time  *tHorizon, CTA_String *snamnoise,
                                    CTA_Handle *husrdata, int *ierr){
   double p0[3];
   double zeroes = 0.0;
   CTA_Vector hvec1,hvec2, h_x_coords,h_y_coords;
   CTA_String htag, hgridtype;
   int len, retval;
   char *str_grid, *str_tag;
   CTAI_Gridm thisgrid, hgrid2;
   CTA_Metainfo hdescr_state;
   CTA_Handle hhandle;
   int rest0=12;
   CTA_Time inputTimeHorizon;

   *ierr=CTA_Tree_GetValueStr(*hinput,"parameters/std_dev",&p0[0],CTA_DOUBLE);
   *ierr=CTA_Tree_GetValueStr(*hinput,"parameters/kar_t",&p0[1],CTA_DOUBLE);
   *ierr=CTA_Tree_GetValueStr(*hinput,"parameters/kar_l",&p0[2],CTA_DOUBLE);
   if (*ierr != CTA_OK) {
     printf("Error in ar1_create. Initial parameters not specified in input\n");
     return;
   }

   *ierr=CTA_Metainfo_Create(&hdescr_state);
   //   printf("metainfo_create: retval %d \n", *ierr);

   /* get the tag of the metainfo of the noise model */
   *ierr = CTA_Tree_GetHandleStr(*hinput,"tag",&htag);
   *ierr = CTA_String_GetLength (htag, &len);
   str_tag=CTA_Malloc((len+1)*sizeof(char));
   *ierr = CTA_String_Get(htag, str_tag);

   *ierr=CTA_Metainfo_SetRest(hdescr_state,&rest0);

   *ierr=CTA_Metainfo_SetTag(hdescr_state,str_tag);

    //    *ierr=CTA_Metainfo_SetTag(hdescr_state,"ar1-noise");

   *ierr = CTA_Tree_GetHandleStr(*hinput,"grid/type_id",&hgridtype);
   if (*ierr != CTA_OK) {
     printf("Error in ar1_create. Grid type not given. \n");
     return; }

   *ierr = CTA_String_GetLength (hgridtype, &len);
   str_grid=CTA_Malloc((len+1)*sizeof(char));
   *ierr = CTA_String_Get(hgridtype, str_grid);
   if (!strncmp("2D",str_grid,len) || !strncmp("3D",str_grid,len )) {
     //  printf("grid type is 2D/3D, now reading more parameters \n");

     strcpy(thisgrid.name,"AR(1)-process");
     thisgrid.type = 2;
     thisgrid.nz = 1; thisgrid.dz = 1.0; thisgrid.z_origin = 0.0;
     *ierr=CTA_Tree_GetValueStr(*hinput,"grid/gridsize/nx", &thisgrid.nx, CTA_INTEGER);

     *ierr=CTA_Tree_GetValueStr(*hinput,"grid/gridsize/ny", &thisgrid.ny, CTA_INTEGER);

     *ierr=CTA_Tree_GetValueStr(*hinput,"grid/gridparams/x_origin",
                             &thisgrid.x_origin, CTA_DOUBLE);
     *ierr=CTA_Tree_GetValueStr(*hinput,"grid/gridparams/y_origin",
                             &thisgrid.y_origin, CTA_DOUBLE);
     *ierr=CTA_Tree_GetValueStr(*hinput,"grid/gridparams/dx",&thisgrid.dx, CTA_DOUBLE);
     *ierr=CTA_Tree_GetValueStr(*hinput,"grid/gridparams/dy",&thisgrid.dy, CTA_DOUBLE);

     if (!strncmp("3D",str_grid,len )) {
       thisgrid.type = 3;
       *ierr=CTA_Tree_GetValueStr(*hinput,"grid/gridsize/nz", &thisgrid.nz, CTA_INTEGER);
       *ierr=CTA_Tree_GetValueStr(*hinput,"grid/gridparams/z_origin",
                             &thisgrid.z_origin, CTA_DOUBLE);
       *ierr=CTA_Tree_GetValueStr(*hinput,"grid/gridparams/dz",&thisgrid.dz,CTA_DOUBLE);
     }
     thisgrid.nsize = thisgrid.nx * thisgrid.ny * thisgrid.nz;


   } // end of reading 2D/3D grid info


   if (!strncmp("curve",str_grid,len)) {
     //  printf("grid type is curve, now reading more parameters \n");

     strcpy(thisgrid.name,"AR(1)-process");
     thisgrid.type = 10;
     *ierr=CTA_Tree_GetValueStr(*hinput,"grid/gridsize/nsize", &thisgrid.nsize, CTA_INTEGER);
     *ierr=CTA_Tree_GetValueStr(*hinput,"grid/gridparams/x_origin",
                             &thisgrid.x_origin, CTA_DOUBLE);
     *ierr=CTA_Tree_GetValueStr(*hinput,"grid/gridparams/y_origin",
                             &thisgrid.y_origin, CTA_DOUBLE);
     // read vector with x-coords and y-coords
     *ierr=CTA_Tree_GetHandleStr(*hinput,"grid/x-coords",&h_x_coords);
     *ierr=CTA_Tree_GetHandleStr(*hinput,"grid/y-coords",&h_y_coords);
   } // end of reading curve info
   // printf("end of reading grid info \n");

   *ierr = CTA_TreeVector_Create("ar1 model","ar1_model",state);
   if (*ierr != CTA_OK) {printf("Error in ar1_create. no state \n");return; }
   *ierr=CTA_Vector_Create(CTA_DEFAULT_VECTOR,thisgrid.nsize, CTA_DOUBLE, CTA_NULL, &hvec1);

   CTA_Vector_SetConstant(hvec1,&zeroes,CTA_DOUBLE);
   CTA_TreeVector_SetVec(*state,hvec1);


   if (*ierr != CTA_OK) { printf("Error in ar1_create. State not initialised \n");
     return;
   }


   /* attach grid information to state meta-info */
   //     printf("Now grid will be attached to metainfo \n");
   *ierr=CTA_Metainfo_SetGrid(hdescr_state, &thisgrid);
   // printf("QQ metainfo_setgrid: nsize(=8*12)   %d |%d| \n", *ierr ,thisgrid.nsize);
   if (*ierr != CTA_OK) { printf("Error in ar1_create.grid not attached \n");
     return;
   }

   // attach meta-info to state

   // printf("Now meta_info will be attached to state \n");
   *ierr = CTA_TreeVector_SetMetainfo(*state, hdescr_state);
   if (*ierr != CTA_OK) { printf("Error in ar1_create.meta-info not attached \n");
     return;
   }

   *sbound = CTA_NULL;

   /* fill husrdata with grid and parameters; necessary for ar1_covar */
   /* do this by constructing a tree */
   *ierr=CTA_Vector_Create(CTA_DEFAULT_VECTOR, 3, CTA_DOUBLE, CTA_NULL, &hvec2);
   *ierr=CTA_Vector_SetVals(hvec2,p0,3,CTA_DOUBLE);

   CTA_Tree_Create(husrdata);

   CTA_Tree_AddHandle(*husrdata, "p0", hvec2);

   *ierr=CTA_Handle_Create("grid",CTA_DATABLOCK,&hgrid2,&hhandle);
   if (*ierr!=CTA_OK) {printf("ar1_create: error husrdata tree \n");}
   *ierr=CTA_Tree_AddHandle(*husrdata, "hgrid", hhandle);
   if (*ierr!=CTA_OK) {printf("ar1_create: error husrdata tree \n");}



   /* fill sparam with parameters */
   *ierr = CTA_TreeVector_Create(" ","ar1_params",sparam);
   *ierr=CTA_TreeVector_SetVec(*sparam,hvec2);
   if (*ierr!=CTA_OK) {printf("error vector params create %d\n",*ierr);}
   //   *sparam = CTA_NULL;


   *nnoise = thisgrid.nsize;
   //printf("ar1_create: nnoise %d \n",*nnoise);

   /* Read time from the input configuration */
   retval=CTA_Tree_GetHandleStr (*hinput, "timehorizon",  &inputTimeHorizon);
   if (retval==CTA_OK){
      *ierr=CTA_Time_Copy(inputTimeHorizon,*tHorizon);
   } else {
      printf("WARNING: No time horizon has been set for AR(1) model\n");
      printf("WARNING: Using default horizon [0, 1.0e6]\n");
      *ierr=CTA_Time_SetSpan(*tHorizon,0.0,1.0e6); /* empty span */
   }
   if (*ierr!=CTA_OK) {printf("error handling the time horizon of the model %d\n",*ierr);}


   *ierr=CTA_String_Set(*snamnoise,"ar1_model");

   //   printf("End of modbuild_sp_ar1_create  \n");
   printf("--------------------------  \n");
}

/*------------------------------------------------------------------------------------*/

  /* CALL USR_COVAR(CTA_TreeVector *colsvar,int* nnoise, CTA_Handle husrdata,
                    int* ierr) */
void  modbuild_sp_ar1_covar(CTA_TreeVector *colsvar,int* nnoise, CTA_Handle husrdata,int* ierr){
  int i,j,ier2;
  double p0[3];
  int nmodel;
  CTA_Handle hhandle;
  CTAI_Gridm *hgrid;
  CTA_Vector hvec1, hvec2;
  double *Lar1;
  double *col1;

 /* NB gridinfo moet uit de husrdata gehaald! */
 /* en ook de parameters in p0! */
 /* note: nnoise equals nmodel */

  nmodel = *nnoise;
  Lar1=CTA_Malloc(nmodel*nmodel*sizeof(double));
  col1=CTA_Malloc(nmodel*sizeof(double));
  hgrid=CTA_Malloc(sizeof(CTAI_Gridm));

  CTA_Vector_Create(CTA_DEFAULT_VECTOR, 3, CTA_DOUBLE, CTA_NULL, &hvec1);
  ier2 = CTA_Tree_GetHandleStr(husrdata,"/p0",&hvec1);
  if (IDEBUG) printf ("-----------ar1_covar:tree: ier2=%d   \n",ier2);
  ier2=CTA_Vector_GetVals(hvec1,p0,3,CTA_DOUBLE);
  if (IDEBUG) printf("ar1_covar: getvals ier p0 %d %f \n",ier2, p0[1]);

  ier2 = CTA_Tree_GetHandleStr(husrdata,"/hgrid",&hhandle);

  ier2=CTA_Handle_GetData((CTA_Handle) hhandle,(void**) &hgrid);

  if (IDEBUG) printf("ar1_covar: ier2 grid.nx %d %d \n",ier2,hgrid->nx);


  ier2 = modbuild_sp_compute_covars(nmodel,Lar1,p0, *hgrid);

  for (i=0;i<nmodel; i++){
    for (j=0;j<nmodel;j++){
      col1[j] = Lar1[i*nmodel + j];
    }
    CTA_Vector_Create(CTA_DEFAULT_VECTOR, nmodel, CTA_DOUBLE, CTA_NULL, &hvec2);
    CTA_Vector_SetVals(hvec2,col1,nmodel,CTA_DOUBLE);
    CTA_TreeVector_SetVec(colsvar[i],hvec2);
    if (IDEBUG) printf("ar1_covar: state-setvec ier %d %d\n",i,ier2);
  }

  CTA_Vector_Free(&hvec1);
  CTA_Vector_Free(&hvec2);

}


/*------------------------------------------------------------------------------------*/
void modbuild_sp_ar1_compute(CTA_Time *timesspan,CTA_TreeVector *state,
                      CTA_TreeVector *saxpyforc, BOOL *baddnoise,
                      CTA_TreeVector *sparam, CTA_Handle husrdata,
                             int* ierr) {
  int i;
  int nmodel;
  double t1,t2;
  CTA_Metainfo minfo;
  CTAI_Gridm hgrid;
  double *x_in;
  double *Lar1;
  double *x_out, *rndh;
  double p0[3];
  double kar_t, alfa;
  double *AR_Noise;
  double eend;
  int    eeni;
  char   dumch;

  //  printf("modbuild_sp_ar1_compute: start  \n");

    *ierr = CTA_Time_GetSpan(*timesspan,&t1,&t2);

    //  printf("modbuild_sp_ar1_compute: getspan %f %f \n",t1,t2 );

   *ierr=CTA_Metainfo_Create(&minfo);
   *ierr = CTA_TreeVector_GetMetainfo(*state,minfo);
   *ierr = CTA_Metainfo_GetGrid(minfo,&hgrid);
   nmodel = hgrid.nsize;

   x_in=CTA_Malloc(nmodel*sizeof(double));
   x_out=CTA_Malloc(nmodel*sizeof(double));
   rndh=CTA_Malloc(nmodel*sizeof(double));
   AR_Noise=CTA_Malloc(nmodel*sizeof(double));
   for (i=0;i<nmodel;i++) {AR_Noise[i] = 0.0;}
   Lar1=CTA_Malloc(nmodel*nmodel*sizeof(double));

//   double x_in[nmodel], x_out[nmodel];
//   double Lar1[nmodel][nmodel];

   *ierr = CTA_TreeVector_GetVals(*state,x_in,nmodel,CTA_DOUBLE);
   //   printf("x-in: %f %f \n ",x_in[0],x_in[1]);

  if (*ierr!=CTA_OK) {return;}
  // Get 'sparam' and set the value of the local variable 'param'
   *ierr = CTA_TreeVector_GetVals(*sparam,p0,3,CTA_DOUBLE);
   //printf("modbuild_sp_ar1_compute: p0 %f %f\n ",p0[1],p0[2]);


   modbuild_sp_compute_covars(nmodel,Lar1,p0, hgrid);


   /* The ar1 model */

   /* Propagate the values by multiplying with the time correspondence */
   kar_t = p0[1];
   alfa = exp(-(t2-t1) / kar_t);

   /* Doe we need to add noise? */
   if (*baddnoise){
     //printf("adding noise\n");
     /* Collect random numbers for white noise */
     for (i=0; i < nmodel; i++){
       CTA_rand_n(&rndh[i]);
     }

     /* Add the white noise to the model state */

     eend  = 1.0;
     eeni  = 1;
     dumch = 'N';
     DGEMV_F77(&dumch,&nmodel,&nmodel,&eend, Lar1,&nmodel, rndh,&eeni,&eend,AR_Noise,&eeni,1);


     /* Scale the space-correlated, time-uncorrelated noise so that
        adding this to the colored noise results in the correct
        stationary covariance matrix */
     for (i=0; i < nmodel; i++){
       AR_Noise[i] = AR_Noise[i] * sqrt(1.0-alfa*alfa);
//       printf("AR_Noise[%i]=%f\n",i,AR_Noise[i]);
       x_out[i] = alfa * x_in[i]  + AR_Noise[i];
     }
   }
   else {
     for (i=0; i < nmodel; i++){
       x_out[i] = alfa * x_in[i];
     }
   }
   *ierr = CTA_TreeVector_SetVals(*state,x_out,nmodel,CTA_DOUBLE);

   // free variabelen
   free(x_in); free(x_out); free(Lar1); free(rndh); free(AR_Noise);

}
/*------------------------------------------------------------------------------------*/
void modbuild_sp_ar1_getobsval(CTA_TreeVector *state, CTA_ObsDescr *hdescr, CTA_Vector *vval,
                CTA_Handle *husrdata, int *ierr){

int imeasr, nmeasr, indx;
CTA_Vector vindex;
double dval;

   *ierr=CTA_ObsDescr_Observation_Count(*hdescr, &nmeasr);
   if (*ierr!=CTA_OK) return;
   if (nmeasr<0) return;

   /* Create work variables */
   *ierr=CTA_Vector_Create(CTA_DEFAULT_VECTOR, nmeasr, CTA_INTEGER,
                          CTA_NULL, &vindex);
   if (*ierr!=CTA_OK) return;

   /* Use the INDX-property to find the position in the state */
   *ierr=CTA_ObsDescr_Get_ValueProperties(*hdescr, "INDX", vindex, CTA_INTEGER);
    if (*ierr!=CTA_OK) {
      printf ("Cannot get property ""INDX"" from observation description\n");
      printf ("Did you use the wrong observation database?\n");
      printf ("Fatal error in oscill_obs\n");
      exit(1);
    }

    for (imeasr=1;imeasr<=nmeasr;imeasr++){
      CTA_Vector_GetVal(vindex,imeasr,&indx,CTA_INTEGER);
      CTA_TreeVector_GetVal(*state,imeasr,&dval,CTA_DOUBLE);
      CTA_Vector_SetVal(*vval,imeasr,&dval,CTA_DOUBLE);
    }

   CTA_Vector_Free(&vindex);
   *ierr=CTA_OK;
}
/*------------------------------------------------------------------------------------*/


int modbuild_sp_compute_covars(int nmodel, double *Qar1, double *p0, CTAI_Gridm hgrid){
  int i,j,qi;
  double std_dev, kar_l;
  double x1,y1,x2,y2,distance2;
  double rdum;
  int info;
  int nul;
  char dumch;

  std_dev = p0[0];
  //kar_t = p0[1];
  kar_l = p0[2];

 
      if (hgrid.type ==2) {

        for (i=1;i<=nmodel;i++){
          x1 = ((i-1) % hgrid.nx)*hgrid.dx + hgrid.x_origin;
          y1 = ((i-1)/hgrid.nx)*hgrid.dy + hgrid.y_origin;

            for (j = 1;j <= nmodel; j++){
              x2 = ((j-1) % hgrid.nx)*hgrid.dx + hgrid.x_origin;
              y2 = ((j-1)/hgrid.nx)*hgrid.dy  + hgrid.y_origin;

              distance2 = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2);
              qi = (i-1)*nmodel + j - 1;
              Qar1[qi] = std_dev*std_dev * exp(-distance2/(kar_l*kar_l));
          
            }

        }
        //printf("end of matrix \n");

      }
      else { printf("compute_covar: 3D grid and curve not yet implemented \n");
        exit(-1);
      }

     
  /*   Compute Cholesky decomposition of this matrix */
          nul = 0;
          dumch='L';
          DPOSV_F77( &dumch, &nmodel, &nul, Qar1, &nmodel, &rdum, &nmodel, &info);
       if (info !=0 ) {printf("compute_covar: error in dposv %d \n",info) ;}
       // if (info ==0 ) {printf("compute_covar: dposv succeeded %d \n",info) ;}


  return CTA_OK;

}





/*-------------------------------------------------------------------------------------*/

void CTA_Modbuild_sp_CreateClass(CTA_ModelClass *modelcls){
   int ierr; //COSTA return value
   int aierr[CTA_MODEL_NUMFUNC];
   int i;
   CTA_Func hfunc[CTA_MODEL_NUMFUNC];
   CTA_Func hfuncar1[4];
   CTA_Intf intf;

   // Create a COSTA model component from my own implementation
   intf=CTA_NULL;
   for (i=0; i<CTA_MODEL_NUMFUNC; i++){
      hfunc[i]=CTA_NULL;
      aierr[i]=CTA_OK;
   }
   aierr[ 0]=CTA_Func_Create("modbuild_sp_create_size",     &modbuild_sp_create_size,    intf, &hfunc[I_CTA_MODEL_CREATE_SIZE  ]);
   aierr[ 1]=CTA_Func_Create("modbuild_sp_create_init",     &modbuild_sp_create_init,    intf, &hfunc[I_CTA_MODEL_CREATE_INIT  ]);
   aierr[ 2]=CTA_Func_Create("modbuild_sp_free",            &modbuild_sp_free,           intf, &hfunc[I_CTA_MODEL_FREE         ]);
   aierr[ 3]=CTA_Func_Create("modbuild_sp_compute",         &modbuild_sp_compute,        intf, &hfunc[I_CTA_MODEL_COMPUTE      ]);
   aierr[ 4]=CTA_Func_Create("modbuild_sp_setstate",        &modbuild_sp_setstate,       intf, &hfunc[I_CTA_MODEL_SET_STATE      ]);
   aierr[ 5]=CTA_Func_Create("modbuild_sp_getstate",        &modbuild_sp_getstate,       intf, &hfunc[I_CTA_MODEL_GET_STATE      ]);
   aierr[ 6]=CTA_Func_Create("modbuild_sp_axpymodel",       &modbuild_sp_axpymodel,      intf, &hfunc[CTA_MODEL_AXPY_MODEL     ]);
   aierr[ 7]=CTA_Func_Create("modbuild_sp_axpystate",       &modbuild_sp_axpystate,      intf, &hfunc[CTA_MODEL_AXPY_STATE     ]);
   aierr[ 8]=CTA_Func_Create("modbuild_sp_setforc",         &modbuild_sp_setforc,        intf, &hfunc[CTA_MODEL_SET_FORC       ]);
   aierr[ 9]=CTA_Func_Create("modbuild_sp_getforc",         &modbuild_sp_getforc,        intf, &hfunc[CTA_MODEL_GET_FORC       ]);
   aierr[10]=CTA_Func_Create("modbuild_sp_axpyforc",        &modbuild_sp_axpyforc,       intf, &hfunc[CTA_MODEL_AXPY_FORC      ]);
   aierr[11]=CTA_Func_Create("modbuild_sp_setparam",        &modbuild_sp_setparam,       intf, &hfunc[CTA_MODEL_SET_PARAM      ]);
   aierr[12]=CTA_Func_Create("modbuild_sp_getparam",        &modbuild_sp_getparam,       intf, &hfunc[CTA_MODEL_GET_PARAM      ]);
   aierr[13]=CTA_Func_Create("modbuild_sp_axpyparam",       &modbuild_sp_axpyparam,      intf, &hfunc[CTA_MODEL_AXPY_PARAM     ]);
   aierr[14]=CTA_Func_Create("modbuild_sp_getnoisecount",   &modbuild_sp_getnoisecount,  intf, &hfunc[CTA_MODEL_GET_NOISE_COUNT]);
   aierr[15]=CTA_Func_Create("modbuild_sp_getnoisecovar",   &modbuild_sp_getnoisecovar,  intf, &hfunc[CTA_MODEL_GET_NOISE_COVAR]);
   aierr[16]=CTA_Func_Create("modbuild_sp_getobsvalues",    &modbuild_sp_getobsvalues,   intf, &hfunc[CTA_MODEL_GET_OBSVALUES  ]);
   aierr[17]=CTA_Func_Create("modbuild_sp_getobsselect",    &modbuild_sp_getobsselect,   intf, &hfunc[CTA_MODEL_GET_OBSSELECT  ]);
   aierr[18]=CTA_Func_Create("modbuild_sp_addnoise",        &modbuild_sp_addnoise,       intf, &hfunc[CTA_MODEL_ADD_NOISE      ]);
   aierr[19]=CTA_Func_Create("modbuild_sp_export",          &modbuild_sp_export,         intf, &hfunc[I_CTA_MODEL_EXPORT         ]);
   aierr[20]=CTA_Func_Create("modbuild_sp_import",          &modbuild_sp_import,         intf, &hfunc[I_CTA_MODEL_IMPORT         ]);
   aierr[21]=CTA_Func_Create("modbuild_sp_adjcompute",      &modbuild_sp_adjcompute,     intf, &hfunc[CTA_MODEL_ADJ_COMPUTE    ]);
   aierr[22]=CTA_Func_Create("modbuild_sp_adjprepare",      &modbuild_sp_adjprepare,     intf, &hfunc[CTA_MODEL_ADJ_PREPARE    ]);
   aierr[23]=CTA_Func_Create("modbuild_sp_adjsetforc",      &modbuild_sp_adjsetforc,     intf, &hfunc[CTA_MODEL_ADJ_SET_FORC   ]);

   aierr[23]=CTA_Func_Create("modbuild_sp_gettimehorizon",  &modbuild_sp_gettimehorizon, intf, &hfunc[CTA_MODEL_GET_TIMEHORIZON   ]);
   aierr[24]=CTA_Func_Create("modbuild_sp_getcurrenttime",  &modbuild_sp_getcurrenttime, intf, &hfunc[CTA_MODEL_GET_CURRENTTIME   ]);



   ierr=CTA_Func_Create("modbuild_sp_ar1_create",&modbuild_sp_ar1_create,intf, &hfuncar1[0]);
   ierr=CTA_Func_Create("modbuild_sp_ar1_covar",&modbuild_sp_ar1_covar,intf, &hfuncar1[1]);
   ierr=CTA_Func_Create("modbuild_sp_ar1_compute",&modbuild_sp_ar1_compute,intf, &hfuncar1[2]);
   ierr=CTA_Func_Create("modbuild_sp_ar1_getobsval",&modbuild_sp_ar1_getobsval,intf, &hfuncar1[3]);


   // If something went wrong we are really fatal!
   for (i=0; i<CTA_MODEL_NUMFUNC ;i++){
      if (aierr[i]!=CTA_OK) {
      printf("Internal error in CTA_Modbuild_sp_createclass\n");
      printf("Cannot create function handles for own implementation\n");
      exit(1);
      }
   }
   /* Create new model class */
   ierr=CTA_Model_DefineClass("CTA_MODBUILD_SP", hfunc, modelcls);
   if (ierr!=CTA_NULL) {
      printf("Internal error in CTA_Modbuild_sp_createclass\n");
      printf("Cannot create new class\n");
      exit(1);
   }
}

#define CTA_MODELBUILD_SP_CREATECLASS_F77  F77_CALL(cta_modbuild_sp_createclass,CTA_MODBUILD_SP_CREATECLASS_F)
void CTA_MODELBUILD_SP_CREATECLASS_F77(CTA_ModelClass *modelcls){
   CTA_Modbuild_sp_CreateClass(modelcls);
}

