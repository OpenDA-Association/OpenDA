/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_model.c $
$Revision: 4055 $, $Date: 2013-07-03 10:16:57 +0200 (Wed, 03 Jul 2013) $

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
#include "cta.h"

#include "cta_mem.h"
#include "cta_datatypes.h"
#include "ctai_xml.h"
#include "cta_handles.h"
#include "ctai_handles.h"
#include "f_cta_utils.h"
#include "cta_model.h"
#include "cta_errors.h"
#include "cta_f77lapack.h"
#include "cta_reltable.h"
#include "cta_par.h"
#include "cta_message.h"
#include "cta_model_factory.h"

#define MAX(a,b) (a>b ? a: b)
#define IDEBUG (0)


#define CTA_MODEL_DEFINECLASS_F77      F77_CALL(cta_model_defineclass,CTA_MODEL_DEFINECLASS)
#define CTA_MODEL_CREATE_F77           F77_CALL(cta_model_create,CTA_MODEL_CREATE)
#define CTA_MODEL_COMPUTE_F77          F77_CALL(cta_model_compute,CTA_MODEL_COMPUTE)
#define CTA_MODEL_SETSTATE_F77         F77_CALL(cta_model_setstate,CTA_MODEL_SETSTATE)
#define CTA_MODEL_GETSTATE_F77         F77_CALL(cta_model_getstate,CTA_MODEL_GETSTATE)
#define CTA_MODEL_AXPYSTATE_F77        F77_CALL(cta_model_axpystate,CTA_MODEL_AXPYSTATE)
#define CTA_MODEL_GETSTATESCALING_F77  F77_CALL(cta_model_getstatescaling,CTA_MODEL_GETSTATESCALING)
#define CTA_MODEL_SETFORC_F77          F77_CALL(cta_model_setforc,CTA_MODEL_SETFORC)
#define CTA_MODEL_GETFORC_F77          F77_CALL(cta_model_getforc,CTA_MODEL_GETFORC)
#define CTA_MODEL_AXPYFORC_F77         F77_CALL(cta_model_axpyforc,CTA_MODEL_AXPYFORC)
#define CTA_MODEL_SETPARAM_F77         F77_CALL(cta_model_setparam,CTA_MODEL_SETPARAM)
#define CTA_MODEL_GETPARAM_F77         F77_CALL(cta_model_getparam,CTA_MODEL_GETPARAM)
#define CTA_MODEL_AXPYPARAM_F77        F77_CALL(cta_model_axpyparam,CTA_MODEL_AXPYPARAM)
#define CTA_MODEL_GETTIMEHORIZON_F77   F77_CALL(cta_model_gettimehorizon,CTA_MODEL_GETTIMEHORIZON)
#define CTA_MODEL_GETCURRENTTIME_F77   F77_CALL(cta_model_getcurrenttime,CTA_MODEL_GETCURRENTTIME)
#define CTA_MODEL_GETNOISECOVAR_F77    F77_CALL(cta_model_getnoisecovar,CTA_MODEL_GETNOISECOVAR)
#define CTA_MODEL_GETNOISECOUNT_F77    F77_CALL(cta_model_getnoisecount,CTA_MODEL_GETNOISECOUNT)
#define CTA_MODEL_FREE_F77             F77_CALL(cta_model_free,CTA_MODEL_FREE)
#define CTA_MODEL_ANNOUNCEOBSVALUES_F77 F77_CALL(cta_model_announceobsvalues,CTA_MODEL_ANNOUNCEOBSVALUES)
#define CTA_MODEL_GETOBSVALUES_F77     F77_CALL(cta_model_getobsvalues,CTA_MODEL_GETOBSVALUES)
#define CTA_MODEL_GETOBSSELECT_F77     F77_CALL(cta_model_getobsselect,CTA_MODEL_GETOBSSELECT)
#define CTA_MODEL_ADDNOISE_F77         F77_CALL(cta_model_addnoise,CTA_MODEL_ADDNOISE)
#define CTA_MODEL_IMPORT_F77           F77_CALL(cta_model_import,CTA_MODEL_IMPORT)
#define CTA_MODEL_EXPORT_F77           F77_CALL(cta_model_export,CTA_MODEL_EXPORT)
#define CTA_MODEL_GETOBSLOCALIZATION_F77   F77_CALL(cta_model_getobslocalization,CTA_MODEL_GETOBSLOCALIZATION)
#define CTA_MODEL_ADJSETFORC_F77       F77_CALL(cta_model_adjsetforc,CTA_MODEL_ADJSETFORC)
#define CTA_MODEL_ADJCOMPUTE_F77       F77_CALL(cta_model_adjcompute,CTA_MODEL_ADJCOMPUTE)
#define CTA_MODEL_ADJPREPARE_F77       F77_CALL(cta_model_adjprepare,CTA_MODEL_ADJPREPARE)
#define CTA_MODEL_LOADPERSISTENTSTATE_F77     F77_CALL(cta_model_loadpersistentstate,CTA_MODEL_LOADPERSISTENTSTATE)
#define CTA_MODEL_SAVEINTERNALSTATE_F77       F77_CALL(cta_model_saveinternalstate,CTA_MODEL_SAVEINTERNALSTATE)
#define CTA_MODEL_RESTOREINTERNALSTATE_F77    F77_CALL(cta_model_restoreinternalstate,CTA_MODEL_RESTOREINTERNALSTATE)
#define CTA_MODEL_RELEASEINTERNALSTATE_F77    F77_CALL(cta_model_releaseinternalstate,CTA_MODEL_RELEASEINTERNALSTATE)
#define CTA_MODEL_SAVEPERSISTENTSTATE_F77     F77_CALL(cta_model_savepersistentstate,CTA_MODEL_SAVEPERSISTENTSTATE)

#define CLASSNAME "CTA_Model"


/* Struct holding all data associated to an COSTA Model */
typedef struct{
   CTA_ObsDescr ObsDescr;
   double *times;
   int ntimes;
   int isset;
   CTA_Vector values;
} CTAI_AnnoinceObs;

typedef struct {
  int flag;
  double t_step;
} CTAI_Barrier;

typedef struct {
CTA_Func functions[CTA_MODEL_NUMFUNC];
CTA_ModelClass hmodcl;
void *data;      /*implementation specific data */
CTAI_AnnoinceObs announced;
CTAI_Barrier barrier;
} CTAI_Model;

typedef struct {
CTA_Func functions[CTA_MODEL_NUMFUNC];
} CTAI_ModelClass;


void CTAI_Clear_Announced(CTAI_AnnoinceObs *announced, BOOL init){

   /* Free existing data */
   if (!init){
      if (announced->times) {
         free(announced->times);
         announced->times=NULL;
      }
      if (announced->values!=CTA_NULL){
         CTA_Vector_Free(&(announced->values));
      }
      if (announced->ObsDescr !=CTA_NULL){
        CTA_ObsDescr_Free(&(announced->ObsDescr));
      }
   }

   announced->ObsDescr=CTA_NULL;
   announced->values=CTA_NULL;
   announced->times=NULL;
   announced->ntimes=0;
   announced->isset=FALSE;
}



/** \brief Check model handle and return data of a model instance
 *
 * \param hmodel     I Model instance handle
 * \param findex     I Index/ID of user function
 * \param data       O Data block of model
 * \param function   O function pointer to user function
 * \return error status: CTA_OK if successful
 */
int CTAI_Model_GetDataAndFunc(CTA_Model hmodel, int findex, CTAI_Model **data, CTA_Function **function){
   int retval; /* Return status of COSTA method */

   /* Check type of handle */
   retval=CTA_Handle_Check((CTA_Handle) hmodel,CTA_MODEL);
   if (retval!=CTA_OK) return retval;

   /* Get Modeldata object */
   retval=CTA_Handle_GetData((CTA_Handle) hmodel, (void**) data);
   if (retval!=CTA_OK) return retval;

   /* Check whether user function is implemented */
   if ((*data)->functions[findex]==CTA_NULL) return CTA_NOT_IMPLEMENTED;

   /* Get function */
   retval=CTA_Func_GetFunc((*data)->functions[findex],function);
   if (retval!=CTA_OK) return retval;

   return CTA_OK;
}

#undef METHOD
#define METHOD "GetSet"
/* function used by the different get and set functions
   (all user implementations have the same interface) */
int CTAI_Model_GetSet(
   CTA_Model hmodel,     /* handle of model                 */
   CTA_Time  tspan,
   CTA_Handle *hthing,   /* set or returned item            */
   int Func_ID,          /* ID (parameter) of user function */
   int idomain
){

   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTA_Function *function;   /* Function that must be called */
   CTAI_Model *data;         /* Data associated to model     */
   double one;               /* value 1.0 in double          */
   CTA_ModelClass hmodcl;    /* handle to ModelClass         */
   int isBlocked;            /* flag indicating tha model is currently blocked */

   /* Check type of handle */
   retval=CTA_Handle_Check((CTA_Handle) hmodel,CTA_MODEL);
   if (retval!=CTA_OK) return retval;

   /* Get Modeldata object */
   retval=CTA_Handle_GetData((CTA_Handle) hmodel, (void*) &data);
   if (retval!=CTA_OK) return retval;

   /* Check if model is currently blocked */
   hmodcl = data->hmodcl;
   isBlocked = CTAI_ModelFac_GetBlock(hmodcl, hmodel);
   if (isBlocked==CTA_TRUE) {
      if (Func_ID==I_CTA_MODEL_GET_STATE || Func_ID==CTA_MODEL_GET_STATEDOMAIN ){
         CTA_WRITE_WARNING("CTA_MODEL_GETSTATE: model is currently blocked.\n");
      }
      else {
         CTA_WRITE_ERROR("This operation is not permitted while model is blocked\n");
         return -1;
      }
   }

   /* Clear the announced administration in case of I_CTA_MODEL_SET_STATE */
   if (Func_ID==I_CTA_MODEL_SET_STATE){
      CTAI_Clear_Announced(&(data->announced), FALSE);
   }

   /* Check existence of Function pointer :*/

   /* -special treatment for CTA_MODEL_GET_STATESCALING      */
   /* if function does not exist return state containing 1.0 */
   if (Func_ID==CTA_MODEL_GET_STATESCALING){
      if (data->functions[Func_ID]==CTA_NULL){
         /* create a state-vector when necessary by calling getstate */
         if (*hthing==CTA_NULL) {
            if (Func_ID==CTA_MODEL_GET_STATESCALING){
                retval=CTAI_Model_GetSet(hmodel, CTA_NULL, hthing,
                                    I_CTA_MODEL_GET_STATE, idomain);
            }
            if (retval!=CTA_OK) return retval;
         }
         /* set identity */
         one=1.0;
         return CTA_TreeVector_SetConstant((CTA_TreeVector) *hthing,&one,CTA_DOUBLE);
      }
   }

   if (data->functions[Func_ID]==CTA_NULL) return CTA_NOT_IMPLEMENTED;

   retval=CTA_Func_GetFunc(data->functions[Func_ID],&function);
   if (retval!=CTA_OK) return retval;

   /* Call function */
   if (Func_ID== CTA_MODEL_SET_FORC || Func_ID== CTA_MODEL_GET_FORC ){
     function(data->data, &tspan, hthing,&retval);
   }
   else if (Func_ID == CTA_MODEL_GET_STATEDOMAIN){
     function(data->data, &idomain, hthing, &retval);
   } else {
     function(data->data, hthing,&retval);
   }
   return retval;
}

#undef METHOD
#define METHOD "SetBarrier"
/* Set Barrier information for this model */
int CTAI_Model_SetBarrier(CTA_ModelClass hmodcl, CTAI_Barrier *barrier ) {

   int retval;               /* Return value of COSTA call              */
   char *flag_barrier;       /* flag                                    */
   double t_step;            /* maximum interval that model may run     */

   if (IDEBUG>0) {printf("CTA_Model_SetBarrier: Start of function \n");}

   /* get barrier information from the model class */
   retval = CTAI_ModelFac_GetBarrierData(hmodcl, &flag_barrier, &t_step);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Error: cannot get barrier information.");
      return retval;
   }

   barrier->flag   = CTA_FALSE;
   barrier->t_step = 0.0;

   if (flag_barrier != NULL) {
      if ( (0 == strcmp("true", flag_barrier)) && t_step ) {
      /* Set flag_barrier */
      barrier->flag = CTA_TRUE;
      /* Set t_step */
      barrier->t_step = t_step;
      }
   }
   if (IDEBUG>0) {printf("CTA_Model_SetBarrier: End of function \n");}
   return CTA_OK;
}

int CTAI_Model_Axpy(
   CTA_Model  hmodel,  /* y element in axpy (handle of model)            */
   CTA_Time  tspan,
   double     alpha,   /* scalar multyplication factor for x             */
   CTA_TreeVector  h_x,     /* x element in axpy state handle of model handle */
   int axpy_id,        /* ID (parameter) of axpy user function           */
   int get_id,         /* ID (parameter) of get user function            */
   int set_id          /* ID (parameter) of set user function            */
   ){

   /* Local variables */
   int retval;               /* Return value of COSTA call           */
   CTA_Function *function;   /* Function that must be called         */
   CTAI_Model *data;         /* Data associated to model hmodel      */
   CTA_TreeVector y;              /* internal y in model state            */


   /* Check type of model handle */
   retval=CTA_Handle_Check((CTA_Handle) hmodel,CTA_MODEL);
   if (retval!=CTA_OK) return retval;

   /* Get data object from model */
   retval=CTA_Handle_GetData((CTA_Handle) hmodel, (void*) &data);
   if (retval!=CTA_OK) return retval;

   /* Check whether user implementation is available */
   if (axpy_id==CTA_NULL){
      /* Try default implementation :*/
      if (get_id==CTA_NULL || set_id==CTA_NULL) return CTA_NOT_IMPLEMENTED;

      /* get y-vector from model */
      y=CTA_NULL;
      retval=CTAI_Model_GetSet(hmodel,tspan, &y,get_id, -1);
      if (retval!=CTA_OK) return retval;

      /* do a state axpy */
      retval=CTA_TreeVector_Axpy(y, alpha,h_x);
      if (retval!=CTA_OK) return retval;

      /* set new y-vector in model */
      retval=CTAI_Model_GetSet(hmodel,tspan, &y,set_id, -1);
      if (retval!=CTA_OK) return retval;

      /* free work variable */
      retval=CTA_TreeVector_Free(&y,CTA_TRUE);
      if (retval!=CTA_OK) return retval;

   }
   else{
      /* Use user implementation */
      retval=CTA_Func_GetFunc(data->functions[axpy_id],&function);
      if (retval!=CTA_OK) return retval;
      if (axpy_id==CTA_MODEL_AXPY_FORC){
        function(data->data, &tspan, &alpha, &h_x, &retval);
      }
      else {
        function(data->data, &alpha, &h_x, &retval);
      }
      if (retval!=CTA_OK) return retval;
   }
   return CTA_OK;
}


//int CTA_Model_DefineClass(
//   const char *name,
//   const CTA_Func h_func[CTA_MODEL_NUMFUNC],
//   CTA_ModelClass     *hmodcl
//   ){
//
//   CTAI_ModelClass *data;
//   int retval;
//   int i;
//
//   /* Allocate new Vector object */
//   data=CTA_Malloc(sizeof(CTAI_ModelClass));
//
//   for (i=0;i<CTA_MODEL_NUMFUNC;i++){
//      data->functions[i]=h_func[i];
//   }
//
//   // Allocate new handle and return eror when unsuccesfull
//   retval=CTA_Handle_Create(name,CTA_MODELCLASS,data,hmodcl);
//   return retval;
//}

#undef METHOD
#define METHOD "Create" 
int CTA_Model_Create(CTA_ModelClass hmodcl, CTA_Handle userdata, CTA_Model *hmodel){

   CTAI_Model *model;
   int memsize;
   int retval;
   CTAI_ModelClass *clsdata;
   CTA_Function *function;
   CTA_Tree tConfig;

   if (IDEBUG>0) {
      printf("Start of CTA_Model_Create hmodcl=%d userdata=%d\n",hmodcl,userdata);
   }

   /* Check in what context we are */
   if (!(hmodcl==CTA_MODBUILD_PAR) &&
       CTA_IS_PARALLEL==CTA_TRUE &&
       CTA_MY_PROC_TYPE==CTA_ParMaster) {

      /* We are not creating an instance of the parallel model builder */
      /* We are running parallel and this is the master process        */
      /* We create the model on one of the worker processes            */
      CTA_Tree_Create(&tConfig);
      CTA_Tree_AddHandle(tConfig, "model", userdata);
      CTA_Tree_AddHandle(tConfig, "modelclass", hmodcl);
      /* Create model (using parallel modelbuild model */
      //printf("CREATE A MODEL USING PARALLE MODEL BUILDER\n");
      retval=CTA_Model_Create(CTA_MODBUILD_PAR, tConfig, hmodel);
      if (retval!=CTA_OK){
		  char message[1024];
		  sprintf(message,"#%d Error in model create %d\n",CTA_PAR_MY_RANK, retval);
	  }

      // DON't free since we will lose the input then!!!
      // Have to fix this in Tree-object //
      // retval=CTA_Tree_Free(&tConfig);

      //printf("WE ZIJN NA DE PARALLEL MODEL CREATE!!!\n");
   } else {
      //printf("CREATING A MODEL LOCALLY\n");

      /* We are running sequential or we are a worker process */
      /* We need to create the model instance locally         */

      //printf("WE ZIJN BIJ DE SEQUENTIAL MODEL CREATE!!!\n");
      //exit(-1);


      if (IDEBUG>0) printf("#%d Start of cta_model_create\n", CTA_PAR_MY_RANK);
      //if (IDEBUG>0) printf("#%d hmodcls=%d type=%d",CTA_PAR_MY_RANK, hmodcl,CTAI_Handle_GetDatatype(hmodcl));
      if (IDEBUG>0) printf("#%d hmodcls=%d type=%d \n",CTA_PAR_MY_RANK, hmodcl,999);

      retval=CTA_Handle_Check((CTA_Handle) hmodcl,CTA_MODELCLASS);
      if (retval!=CTA_OK) {
          CTA_WRITE_ERROR("Handle is not a cta_modelclass handle");
          return retval;
      }

      /* Get class data containing all function pointers */
      retval=CTA_Handle_GetData((CTA_Handle) hmodcl,(void*) &clsdata);
      if (retval!=CTA_OK) {
         CTA_WRITE_ERROR("Cannot retrieve handle data");
         return retval;
      }

      if (IDEBUG>0) printf("#%d cta_model_create: Initialise model (size)\n", CTA_PAR_MY_RANK);
      /* determine size of data object (CTA_VECTOR_CREATE_SIZE)*/
      if (clsdata->functions[I_CTA_MODEL_CREATE_SIZE]==CTA_NULL)
      {
        printf("#%d ERROR in CTA_Model_Create\n", CTA_PAR_MY_RANK);
        printf("#%d There is no handle for the function 'Create_size'\n", CTA_PAR_MY_RANK);
        return CTA_NOT_IMPLEMENTED;
      }
      retval=CTA_Func_GetFunc(clsdata->functions[I_CTA_MODEL_CREATE_SIZE],&function);
      if (retval!=CTA_OK) {
         char message[1024];
         sprintf(message,"#%d ERROR in CTA_Model_Create;\nCannot get handle for function 'Create_size'\n", CTA_PAR_MY_RANK);
         CTA_WRITE_ERROR(message);
         return retval;
      }
      (void) function(&userdata,&memsize,&retval);
      if (retval) {
         CTA_WRITE_ERROR("Error in function CTA_MODEL_CREATE_SIZE");
         return retval;
      }

      /* allocate memory for new model object */
      model=CTA_Malloc(sizeof(CTAI_Model));
      model->data=CTA_Malloc(memsize);

      /* copy function pointers */
      for (int i=0;i<CTA_MODEL_NUMFUNC;i++){
         model->functions[i]=clsdata->functions[i];
      }
      /* set other general information */
      model->hmodcl=hmodcl;

      // set barrier information for current model
      retval = CTAI_Model_SetBarrier( hmodcl, &model->barrier);
      if (retval) {
         CTA_WRITE_ERROR("Error in CTA_Model_SetBarrierData");
         return retval;
      }

      /* Initialise announced observation admin */
      CTAI_Clear_Announced(&(model->announced), TRUE);

      /* Copy user's function handles */
      if (clsdata->functions[I_CTA_MODEL_CREATE_INIT]==CTA_NULL) return CTA_NOT_IMPLEMENTED;

      /* Allocate new handle and return error when unsuccesfull */
      retval=CTA_Handle_Create("model",CTA_MODEL,model,hmodel);
      if (retval) {
         CTA_WRITE_ERROR("Handle CTA_MODEL cannot be created");
         return retval;
      }

      if (IDEBUG>0) printf("#%d cta_model_create: Initialise model (init)\n", CTA_PAR_MY_RANK);
      /* Initialise a new model */
      retval=CTA_Func_GetFunc(clsdata->functions[I_CTA_MODEL_CREATE_INIT],&function);

      if (retval!=CTA_OK) {
         CTA_WRITE_ERROR("Cannot get function CTA_MODEL_CREATE_INIT");
         return retval;
      }

      (void) function(hmodel, model->data, &userdata, &retval);
      if (IDEBUG>0) printf("#%d cta_model_create: Initialise model (done) retval=%d\n", CTA_PAR_MY_RANK,retval);
      if (retval) {
         CTA_WRITE_ERROR("Error in function CTA_MODEL_CREATE_INIT");
         return retval;
      }
   }
   /* The model has been created. Administrate this model in the modelclass object */
   CTAI_ModelFac_AddModelInstance(hmodcl, *hmodel);

   if (IDEBUG>0) printf("#%d cta_model_create: end of function\n", CTA_PAR_MY_RANK);
   return CTA_OK;
}


#undef METHOD
#define METHOD "AddNoise" 
int CTA_Model_AddNoise(
   CTA_Model hmodel, /* handle of model */
   CTA_Time htime    /* timespan of simulation */
){

   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTA_Function *function;   /* Function that must be called */
   CTAI_Model *data;         /* Data assosiated to model     */

   /* Check type of handle */
   retval=CTA_Handle_Check((CTA_Handle) hmodel,CTA_MODEL);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_model handle");
       return retval;
   } 

   /* Get Modeldata object */
   retval=CTA_Handle_GetData((CTA_Handle) hmodel, (void*) &data);
   if (retval!=CTA_OK)  {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* Get Function pointer */
   if (data->functions[CTA_MODEL_ADD_NOISE]==CTA_NULL) return CTA_NOT_IMPLEMENTED;
   retval=CTA_Func_GetFunc(data->functions[CTA_MODEL_ADD_NOISE],&function);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function CTA_MODEL_ADD_NOISE");
	   return retval;
   }

   /* Call function */
   function(data->data,&htime,&retval);
   return retval;
}

#undef METHOD
#define METHOD "PerformTimesteps"
/* handle barrier information and advance the model in time */
int CTAI_Model_PerformTimesteps(
   CTA_Model hmodel,         /* handle of model */
   CTA_Function *function,   /* Function that must be called */
   CTA_Time htime,           /* timespan to compute */
   int mindBarrier           /* Flag to activate barrier check fixed timestepping */
){

   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTAI_Model *data;         /* Data associated to model     */
   double tstart, tstop;     /* Begin and end time of htime  */
   double tstep;             /* Maximum number of steps that model can proceed */
   int isspan;               /* flag indicating whether input is timespan */
   int isBlocked;            /* flag indicating that maximum number of model steps is exceeded */

   if (IDEBUG>0) {
     printf("CTA_MODEL DEBUG: Calling performTimeStep function \n");
   }

   /* Check type of handle */
   retval=CTA_Handle_Check((CTA_Handle) hmodel,CTA_MODEL);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_model handle");
       return retval;
   }

   /* Get Modeldata object */
   retval=CTA_Handle_GetData((CTA_Handle) hmodel, (void*) &data);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Cannot retrieve handle data");
      return retval;
   }

   isBlocked = CTA_FALSE;
   if (mindBarrier == CTA_TRUE){
     /* Time management */
     retval=CTA_Time_GetSpan(htime,&tstart,&tstop);
     retval=CTA_Time_IsSpan(htime, &isspan);
     /* Is there a possible barrier? */
     if (data->barrier.flag==CTA_TRUE) {
       tstep = data->barrier.t_step;
       int nstep = (int) ((tstop-tstart)/tstep+0.5);
       if (IDEBUG>0) {
          printf("CTA_performTimesteps: Time information: \n");
          printf("Tstart= %f, Tstep=%f, Tstop=%f, nStep=%d\n ", tstart, tstep, tstop, nstep);
       }
       if (nstep>1) {
          if (IDEBUG>0) printf("CTA_performTimesteps: set block; I have to wait\n");
          isBlocked = CTA_TRUE;
       }
       else {
          if (IDEBUG>0) printf("CTA_performTimesteps: no barrier in this time interval\n");
       }
     }
   }

   if (isBlocked == CTA_TRUE) {
     if (IDEBUG>0) printf("CTA_performTimesteps: TimeStepAllModels\n");
     /* set block for this model in modelclass administration */
     retval=CTAI_ModelFac_SetBlock(data->hmodcl,hmodel);
     /* advance all childmodels */
     retval=CTAI_ModelFac_TimeStepAllModels(data->hmodcl,function, tstart, tstop);
     if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("problem in TimeStepAllModels \n");
       return retval;
     }
   } else {
     if (IDEBUG>0) printf("CTA_performTimesteps: compute for model %d \n", hmodel);
     function(data->data,&htime,&retval);
     if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("problem in compute");
       return retval;
     }
   }
   return CTA_OK;
}




#undef METHOD
#define METHOD "Compute"
/* handle announced observations if necessary and advance the model in
   time by calling method PerformTimeSteps */
int CTA_Model_Compute(
   CTA_Model hmodel, /* handle of model */
   CTA_Time htime    /* timespan of simulation */
){

   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTA_Function *function;   /* Function that must be called */
   CTAI_Model *data;         /* Data associated to model     */
   double tstart, tstop;     /* Begin and end time of htime  */
   double t1, t2;            /* begin and end time of sub-step */
   CTA_Time hstep;           /* Singe step in compute        */
   int ntimes;               /* number of times where observations are
                              * available */
   int ntimes_loop;          /* number time intervals for compute */
   CTA_ObsDescr obsdescr_sub;/* Observation description of subset of
                                observations */
   int nobs;                 /* number of observations in obsdescr_sub */
   CTA_RelTable reltab;      /* Relation table for selection of
                                observations within a give timespan */
   CTA_Vector vals;          /* predicted values in a sub-step */
   double eps;               /* small value for comparing doubles */
   CTA_Time currentTime;     /* time instance of the model */
   int isspan;               /* flag indicating whether input is timespan */

   /* Time management */
   retval=CTA_Time_GetSpan(htime,&tstart,&tstop);
   retval=CTA_Time_IsSpan(htime, &isspan);
   if (isspan!=CTA_TRUE) {
      // Get the current time of the model
      CTA_Time_Create(&currentTime);
      CTA_Model_GetCurrentTime(hmodel,currentTime);
      retval=CTA_Time_GetSpan(currentTime,&t1,&t2);
      tstart=t1;
      CTA_Time_SetSpan(htime,tstart,tstop);
      CTA_Time_Free(&currentTime);
   }

   if (IDEBUG) printf("entering cta_model_compute: tstart,tstop: %d %f %f \n",htime,tstart,tstop);

   if (IDEBUG) printf("CTA_MODEL DEBUG: Calling compute function \n");

   /* Check type of handle */
   retval=CTA_Handle_Check((CTA_Handle) hmodel,CTA_MODEL);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_model handle");
       return retval;
   }

   /* Get Modeldata object */
   retval=CTA_Handle_GetData((CTA_Handle) hmodel, (void*) &data);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Cannot retrieve handle data");
      return retval;
   }

   /* Get Function pointer */
   if (data->functions[I_CTA_MODEL_COMPUTE]==CTA_NULL) return CTA_NOT_IMPLEMENTED;
   retval=CTA_Func_GetFunc(data->functions[I_CTA_MODEL_COMPUTE],&function);
   if (retval!=CTA_OK)  {
	   CTA_WRITE_ERROR("Cannot get function I_CTA_MODEL_COMPUTE");
	   return retval;
   }

   /* Do we need to handle announced observations? */
   ntimes=data->announced.ntimes;
   if (IDEBUG) printf("We have %d times with announced observations\n",ntimes);
   if (ntimes>0){
      /* Do we need to interrupt the computations? */

      if (IDEBUG>0) {
         for (int itime=0;itime<ntimes;itime++){
            printf("%f ",data->announced.times[itime]);
         }
         printf("First announced time %f\n",data->announced.times[0]);
         printf("Last announced time  %f\n",data->announced.times[ntimes-1]);
         printf("compute interval [%f,%f]\n",tstart,tstop);
      }
      /* check whether announce is within this compute interval */
      if (data->announced.times[0]<=tstart ||
         tstop <data->announced.times[ntimes-1]){
         fprintf(stderr,"Error: Interval of announced observations lies before current time of the model\n");
         fprintf(stderr,"Tstart= %g, first announced=%g\n",
            tstart,data->announced.times[0]);
         fprintf(stderr,"Tstop= %g, last announced=%g\n",
            tstop,data->announced.times[ntimes-1]);
         return CTA_ANNOUNCED_OBS_INTERVAL_ERROR;
      }

      /* Create relation table for observations */
      retval=CTA_RelTable_Create(&reltab);
      if (retval!=CTA_OK)  {
         CTA_WRITE_ERROR("Cannot create RelTable");
         return retval;
      }

      /* determine number of steps in the time-loop by checking whether
         the last observation is at time tstop */
      eps=M_EPS*MAX(fabs(tstart),fabs(tstop))+M_EPS;
      if (fabs(tstop-data->announced.times[ntimes-1])<eps){
         ntimes_loop=ntimes;
      } else {
         ntimes_loop=ntimes+1;
      }
       if (IDEBUG>0) {printf("Number of steps in compute is %d\n",ntimes_loop);}

       /* loop over all intervals of observations */
      CTA_Time_Create(&hstep);
      CTA_Time_SetSpan(hstep,tstart,data->announced.times[0]);

      for (int itime=0;itime<ntimes_loop;itime++){
         if (IDEBUG>0){
            CTA_Time_GetSpan(hstep,&t1,&t2);
            printf("running from %f to %f \n",t1,t2);
         }

         /* create an obs-selection for this timestep */

         retval=CTA_ObsDescr_CreateTimSel(data->announced.ObsDescr, hstep,
            reltab, &obsdescr_sub);
         if (retval!=CTA_OK)  {
            char message[1024];
            sprintf(message,"Cannot create obs-selection for timestep %d",hstep);
            CTA_WRITE_ERROR(message);
            return retval;
        }

         /* Determine the number of observations */
         retval=CTA_ObsDescr_Observation_Count(obsdescr_sub, &nobs);
         if (retval!=CTA_OK) {
            CTA_WRITE_ERROR("Cannot determine the number of observations");
            return retval;
         }

         if (IDEBUG) {printf("Number of obs in this step are %d\n",nobs);}

         /* perform timestep(s) */
         retval = CTAI_Model_PerformTimesteps(hmodel,function,hstep,CTA_TRUE);
         if (retval!=CTA_OK) {
            char message[1024];
            sprintf(message,"Error in performing timestep %d",hstep);
            CTA_WRITE_ERROR(message);
            return retval;
         }

         /* create vector for holding the observations */
         retval=CTA_Vector_Create(CTA_DEFAULT_VECTOR,nobs,CTA_DOUBLE,CTA_NULL,
            &vals);
         if (retval!=CTA_OK) {
            CTA_WRITE_ERROR("Cannot create vector");
            return retval;
         }

         /* get observed values */
         retval=CTA_Model_GetObsValues(hmodel,hstep,obsdescr_sub,vals);
         if (retval!=CTA_OK) {
            char message[1024];
            sprintf(message,"Cannot get observated values of timestep %d",hstep);
            CTA_WRITE_ERROR(message);
            return retval;
         }

         /* copy observed values in global vector */
         retval=CTA_RelTable_ApplyInv(reltab, vals, data->announced.values);
         if (retval!=CTA_OK) {
            CTA_WRITE_ERROR("Cannot copy observed values in global vector");
            return retval;
         }

         if (IDEBUG) {
            printf("predicted values:\n");
            CTA_Vector_Export(data->announced.values,CTA_FILE_STDOUT);
         }
         /* Free variables */
         retval=CTA_Vector_Free(&vals);
         if (retval!=CTA_OK) {
            CTA_WRITE_ERROR("Cannot free vector");
            return retval;
         }

         retval=CTA_ObsDescr_Free(&obsdescr_sub);
         if (retval!=CTA_OK) {
            CTA_WRITE_ERROR("Cannot free ObsDescr??");
            return retval;
         }


         /* set timespan for next loop */
         if (itime<ntimes-1){
            CTA_Time_SetSpan(hstep,data->announced.times[itime],
                                   data->announced.times[itime+1]);
         } else {
            CTA_Time_SetSpan(hstep,data->announced.times[ntimes-1],tstop);
         }
      }

      /* Mark announced observations as being set */
      data->announced.isset=TRUE;

      /* Free variables */
      retval=CTA_Time_Free(&hstep);
      if (retval!=CTA_OK) {
         CTA_WRITE_ERROR("Cannot free time");
         return retval;
      }

      retval=CTA_RelTable_Free(&reltab);
      if (retval!=CTA_OK) {
         CTA_WRITE_ERROR("Cannot free RelTable");
         return retval;
      }

      if (IDEBUG) printf("CTA_MODEL DEBUG: (1) Return code export function %d \n", retval);

  } else {
     /* No model handles, we only need to call the compute method */
      retval=CTA_Time_GetSpan(htime,&tstart,&tstop);
      retval = CTAI_Model_PerformTimesteps(hmodel,function,htime,CTA_TRUE);
      if (IDEBUG) printf("CTA_MODEL DEBUG: (2) Return code compute function %d \n",
         retval);
      return retval;
  }
  return CTA_OK;
}



int CTA_Model_SetState(
   CTA_Model hmodel,  /* handle of model              */
   CTA_TreeVector hstate    /* model state that must be set */
){

   return CTAI_Model_GetSet(hmodel, CTA_NULL, &hstate,I_CTA_MODEL_SET_STATE, -1);
}

int CTA_Model_GetStateScaling(
   CTA_Model hmodel,         /* handle of model       */
   CTA_TreeVector *hstate    /* returned scaling vector for model state  */
){
   return CTAI_Model_GetSet(hmodel, CTA_NULL, hstate,
                            CTA_MODEL_GET_STATESCALING, -1);
}


int CTAI_Model_GetObsLocalization(
   CTA_Model hmodel,       /* handle of model       */
   CTA_ObsDescr hdescr,    /* observation description */
   double distance,        /* characteristic distance */
   int iDomain,            /* sub domain number (<0 whole model) */
   CTA_Vector locVecs
){

   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTA_Function *function;   /* Function that must be called */
   CTAI_Model *data;         /* Data associated to model     */
   int iObs, nObs;
   double one=1.0;
   CTA_TreeVector hState;

   int domain_version;


   domain_version=iDomain>=0;

   if (domain_version){
        retval=CTAI_Model_GetDataAndFunc(hmodel, CTA_MODEL_GET_OBSLOCALIZATIONDOMAIN, &data, &function);
   }
   else {
        retval=CTAI_Model_GetDataAndFunc(hmodel, I_CTA_MODEL_GETOBSLOCALIZATION, &data, &function);
   }

   if (retval == CTA_NOT_IMPLEMENTED){
      printf("Debug: CTA_Model_GetObsLocalization: No localization function is implemented for this model\n");
      printf("Debug: Consider running method without localization (if possible), this will improve performance \n");
      /* Return identity localization */

      retval = CTA_Vector_GetSize(locVecs, &nObs);
      printf("Lengte van de obs-vector is %d\n",nObs);

      for (iObs=1; iObs<=nObs; iObs++){
         CTA_Vector_GetVal(locVecs, iObs, &hState, CTA_HANDLE);
         if (domain_version){
            CTA_Model_GetStateDomain(hmodel,iDomain, &hState);
         }
         else {
            CTA_Model_GetState(hmodel,&hState);
         }
         CTA_TreeVector_SetConstant(hState, &one, CTA_DOUBLE );
         CTA_Vector_SetVal(locVecs, iObs, &hState, CTA_HANDLE);
      }
      return retval;
   }
   else if (retval == CTA_OK) {
      /* Call user supplied implementation */
      if (IDEBUG) printf("CTA_MODEL DEBUG: Calling Localization function \n");
      if (domain_version){
         function(data->data, &hdescr, &distance, &locVecs, &retval);
      }
      else {
         function(data->data, &hdescr, &distance, iDomain, &locVecs, &retval);
      }
      if (IDEBUG) printf("CTA_MODEL DEBUG: Return code Localization function %d \n",
         retval);
      return retval;
   }
   else {
      /* Error in retrieving function handle */
      CTA_WRITE_ERROR("Error in CTAI_Model_GetDataAndFunc");
      return retval;
   }
}


int CTA_Model_GetObsLocalization(
   CTA_Model hmodel,       /* handle of model       */
   CTA_ObsDescr hdescr,    /* observation description */
   double distance,        /* characteristic distance */
   CTA_Vector locVecs
){
 
   return CTAI_Model_GetObsLocalization(hmodel,hdescr, distance, -1, locVecs);

}





int CTA_Model_GetState(
   CTA_Model hmodel,   /* handle of model       */
   CTA_TreeVector *hstate    /* returned model state  */
){
  //  printf("cta_model_getstate : model %d state %d\n  ",hmodel,*hstate);
   return CTAI_Model_GetSet(hmodel, CTA_NULL, hstate, I_CTA_MODEL_GET_STATE, -1);
}

int CTA_Model_SetForc(
   CTA_Model hmodel,  /* handle of model                        */
   CTA_Time tspan,
   CTA_TreeVector hforc    /* model forcings values that must be set */
){
   return CTAI_Model_GetSet(hmodel,tspan, &hforc,CTA_MODEL_SET_FORC, -1);
}

int CTA_Model_GetForc(
   CTA_Model hmodel,   /* handle of model                  */
   CTA_Time tspan,
   CTA_TreeVector *hforc    /* returned model forcings values  */
){
   return CTAI_Model_GetSet(hmodel,tspan, hforc,CTA_MODEL_GET_FORC, -1);

}

int CTA_Model_AxpyForc(
   CTA_Model hmodel,    /* handle of model  */
   CTA_Time tspan,
   double alpha,        /* alpha of axpy    */
   CTA_TreeVector h_x        /* x-vector of axpy */
){
   return CTAI_Model_Axpy(hmodel, tspan, alpha, h_x, CTA_MODEL_AXPY_FORC,
       CTA_MODEL_GET_FORC,CTA_MODEL_SET_FORC);
}

int CTA_Model_SetParam(
   CTA_Model hmodel,  /* handle of model                   */
   CTA_TreeVector hparam   /* model parameters that must be set */
){
   return CTAI_Model_GetSet(hmodel,CTA_NULL, &hparam,CTA_MODEL_SET_PARAM, -1);
}

int CTA_Model_GetParam(
   CTA_Model hmodel,   /* handle of model             */
   CTA_TreeVector *hparam   /* returned model parameters  */
){
   return CTAI_Model_GetSet(hmodel,CTA_NULL, hparam,CTA_MODEL_GET_PARAM, -1);
}

int CTA_Model_AxpyParam(
   CTA_Model hmodel,    /* handle of model  */
   double alpha,        /* alpha of axpy    */
   CTA_TreeVector h_x        /* x-vector of axpy */
){
   return CTAI_Model_Axpy(hmodel, CTA_NULL, alpha, h_x, CTA_MODEL_AXPY_PARAM,
       CTA_MODEL_GET_PARAM,CTA_MODEL_SET_PARAM);
}

/* AXPY operation for models: add alpha times a vector to a model
   Importante notice: the usual order of the arguments differs from
   the order we use in this function. In BLAS we have:
   y=alpha*x+y, yielding in a call like axpy(alpha,x,y)

   For the model axpy, the model state is y.! Because the model handle
   is the first argument in all COSTA model calls. axpy for models
   is defined by;
   axpy(y,alpha,x)

   The "vector" x can have two different types
   1) CTA_TreeVector (COSTA state vector)
   2) CTA_MODEL (COSTA model)
*/
#undef METHOD
#define METHOD "AxpyState"
int CTA_Model_AxpyState(
   CTA_Model  hmodel,   /* y element in axpy (handle of model)            */
   double     alpha,    /* scalar multyplication factor for x             */
   CTA_Handle h_x       /* x element in axpy state handle of model handle */
){

   /* Local variables */
   int retval;               /* Return value of COSTA call      */
   CTA_Function *function;   /* Function that must be called    */
   CTAI_Model *data_y;       /* Data assosiated to model hmodel */
   CTAI_Model *data_x;       /* Data assosiated to model h_x    */
   CTA_TreeVector hstate_x;  /* State vector   (x)              */
   CTA_TreeVector hstate_y;  /* State vector   (y)              */
   BOOL same_models;         /* flag: model class of hmode and h_x are the same */
   CTA_Handle dt;

   /* Check type of model handle */
   retval=CTA_Handle_Check((CTA_Handle) hmodel,CTA_MODEL);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_model handle");
       return retval;
   }

   /* Get Modeldata object */
   retval=CTA_Handle_GetData((CTA_Handle) hmodel, (void*) &data_y);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Cannot retrieve handle data");
      return retval;
   }

   /* Clear the announced administration */
   CTAI_Clear_Announced(&(data_y->announced), FALSE);
   retval = CTA_Handle_GetDatatype(h_x, &dt);
   if (retval != CTA_OK) {return retval; }

   if (dt == CTA_MODEL) {

   /* This is a MODEL+alpha*MODEL axpy */
      /* Get Modeldata object of x */
      retval=CTA_Handle_GetData((CTA_Handle) h_x, (void*) &data_x);
      if (retval!=CTA_OK) {
         CTA_WRITE_ERROR("Cannot retrieve handle data");
         return retval;
      }

      /* check whether models are of the same model class */
      same_models=(data_y->hmodcl==data_x->hmodcl);

      /* If models are not the same or no user implementation is available: */
      if (same_models && data_y->functions[CTA_MODEL_AXPY_MODEL]==CTA_NULL)
      {
         /* Use default implementation :      */
         /* -> get state from model h_x       */
         /* -> call CTA_Model_Axpy with state */

         hstate_x=CTA_NULL;
         retval=CTA_Model_GetState(h_x,&hstate_x);
         if (retval!=CTA_OK) {
            CTA_WRITE_ERROR("Cannot get state");
            return retval;
         }

         retval=CTA_Model_AxpyState(hmodel,alpha,hstate_x);
         if (retval!=CTA_OK){
     	    CTA_WRITE_ERROR("Cannot get AxpyState");
	        return retval;
         } 

         retval=CTA_TreeVector_Free(&hstate_x, CTA_TRUE);
         if (retval!=CTA_OK) {
     	    CTA_WRITE_ERROR("Cannot free tree vector");
	        return retval;
         } 
      }
      else
      {
         /* Use user implementation */
         retval=CTA_Func_GetFunc(data_y->functions[CTA_MODEL_AXPY_MODEL],&function);
         if (retval!=CTA_OK) {
     	    CTA_WRITE_ERROR("Cannot get function CTA_MODEL_AXPY_MODEL");
	        return retval;
         } 
         function(data_y->data, &alpha, data_x->data, &retval);
         if (retval!=CTA_OK) {
     	    CTA_WRITE_ERROR("Error in function CTA_MODEL_AXPY_MODEL");
	        return retval;
         } 
      }
   }
   else{
   /* This is a MODEL+alpha*STATE axpy */


      if (dt != CTA_TREEVECTOR) {

         CTA_WRITE_ERROR("Handle is not a cta_treevector");
         return CTA_ILLEGAL_HANDLE;
      }

      /* Check whether user implementation is available */
      if (data_y->functions[CTA_MODEL_AXPY_STATE]==CTA_NULL)
      {
         /* Use default implementation */
         hstate_y=CTA_NULL;
         retval=CTA_Model_GetState(hmodel,&hstate_y);
         if (retval!=CTA_OK) {
            CTA_WRITE_ERROR("Cannot get state");
            return retval;
         }

         retval=CTA_TreeVector_Axpy(hstate_y,alpha, h_x);
         if (retval!=CTA_OK) {
            CTA_WRITE_ERROR("Error in CTA_TreeVector_Axpy");
            return retval;
         }
         retval=CTA_Model_SetState(hmodel,hstate_y);
         if (retval!=CTA_OK) {
            CTA_WRITE_ERROR("Error in CTA_Model_SetState");
            return retval;
         }
         retval=CTA_TreeVector_Free(&hstate_y, CTA_TRUE);
         if (retval!=CTA_OK)  {
            CTA_WRITE_ERROR("Cannot free TreeVector");
            return retval;
         }
      }
      else
      {
         /* Use user implementation */
         retval=CTA_Func_GetFunc(data_y->functions[CTA_MODEL_AXPY_STATE],&function);
         if (retval!=CTA_OK)  {
            CTA_WRITE_ERROR("Cannot get function CTA_MODEL_AXPY_STATE");
            return retval;
         }
         function(data_y->data, &alpha, &h_x, &retval);
//         function(&alpha, &h_x, data_y->data,&retval);
         if (retval!=CTA_OK)  {
            CTA_WRITE_ERROR("Error in function CTA_MODEL_AXPY_STATE");
            return retval;
         }
      }
   }
   return CTA_OK;
}




#undef METHOD
#define METHOD "GetNoiseCovar" 
int CTA_Model_GetNoiseCovar(
   CTA_Model hmodel,   /* handle of model       */
   CTA_TreeVector *hstmat   /* returned state-matrix */
){

   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTA_Function *function;   /* Function that must be called */
   CTAI_Model *data;         /* Data assosiated to model     */


   /* Check type of handle */
   retval=CTA_Handle_Check((CTA_Handle) hmodel,CTA_MODEL);
   if (retval!=CTA_OK)  {
       CTA_WRITE_ERROR("Handle is not a cta_model handle");
       return retval;
   }

   /* Get Modeldata object */
   retval=CTA_Handle_GetData((CTA_Handle) hmodel, (void*) &data);
   if (retval!=CTA_OK)  {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* Get Function pointer */
   if (data->functions[CTA_MODEL_GET_NOISE_COVAR]==CTA_NULL){
	  CTA_WRITE_ERROR("Error in function ...");
	  return CTA_NOT_IMPLEMENTED;
   }

   retval=CTA_Func_GetFunc(data->functions[CTA_MODEL_GET_NOISE_COVAR],&function);
   if (retval!=CTA_OK) {
	  CTA_WRITE_ERROR("Cannot get function CTA_MODEL_GET_NOISE_COVAR");
	  return CTA_NOT_IMPLEMENTED;
   }

   /* Call function */
   function(data->data,hstmat,&retval);

   return retval;
}

#undef METHOD
#define METHOD "GetNoiseCount" 
int CTA_Model_GetNoiseCount(
   CTA_Model hmodel,   /* handle of model       */
   int *nnoise         /* returned number of noise parameters */
){

   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTA_Function *function;   /* Function that must be called */
   CTAI_Model *data;         /* Data assosiated to model     */

   /* Check type of handle */
   retval=CTA_Handle_Check((CTA_Handle) hmodel,CTA_MODEL);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_model handle");
       return retval;
   }

   /* Get Modeldata object */
   retval=CTA_Handle_GetData((CTA_Handle) hmodel, (void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* Get Function pointer */
   if (data->functions[CTA_MODEL_GET_NOISE_COUNT]==CTA_NULL) return CTA_NOT_IMPLEMENTED;
   retval=CTA_Func_GetFunc(data->functions[CTA_MODEL_GET_NOISE_COUNT],&function);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function CTA_MODEL_GET_COISE_COUNT");
	   return retval;
   }

   /* Call function */
   function(data->data,nnoise,&retval);
   return retval;
}

#undef METHOD
#define METHOD "Free" 
int CTA_Model_Free(
   CTA_Model *hmodel  /* Handle of model  */
   ){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Model *data;       /* All data of hmodel  */
   CTA_Function *function; /* Function that must be called */

   /* Get pointer to implementation of this function */

   if (*hmodel==CTA_NULL) return CTA_OK;

   retval=CTA_Handle_Check((CTA_Handle) *hmodel,CTA_MODEL);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_model handle");
       return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) *hmodel, (void*) &data);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Cannot retrieve handle data");
       return retval;
   }

   /* call user implementation if available */
   if (data->functions[I_CTA_MODEL_FREE]!=CTA_NULL){
      retval=CTA_Func_GetFunc(data->functions[I_CTA_MODEL_FREE],&function);
      if (retval!=CTA_OK) return retval;
      function(data->data,&retval);
   }
   free(data->data);
   free(data);
   retval=CTA_Handle_Free(hmodel);

   return retval;
};

/** \brief Fill the struct announced with info from the given
 *         observation description.
 *
 * \param hdescr    I Observation description with annonced observations
 * \param announced O handle of model instance
 * \return error status: CTA_OK if successful
 */
#undef METHOD
#define METHOD "AnnounceObsValues_Set" 
int CTA_Model_AnnounceObsValues_Set(
  CTA_ObsDescr hdescr,          /* observation description */
  CTAI_AnnoinceObs *announced   /* data-block of model     */
){
   /* Local variables */
   int nobs;                 /* Number of observations       */
   double *allTimes;         /* Array with all available times */
   CTA_Vector vtimes;        /* Vector with all available times */
   int info;
   int nunique;              /* Number of unique times of observations */
   double lastTime;          /* Previous time in list                  */
   int i,itime;              /* Loop counters */
   char direc;               /* Sorting direction */

   /* free all administration that might be available */
   CTAI_Clear_Announced(announced, FALSE);

   if (hdescr!=CTA_NULL) {
      int retval;               /* Return value of COSTA call   */

      /* If there are any observations in the description */
      retval=CTA_ObsDescr_Observation_Count(hdescr, &nobs);
      if (retval!=CTA_OK) {
         CTA_WRITE_ERROR("There are not any observations in the description");
         return retval;
      }

      if (IDEBUG) {printf("CTA_Model_AnnounceObsValues_Set: nobs=%d\n",nobs);}

      if (nobs>0){
         /* Allocate an vector for holding all predicted values */
         retval=CTA_Vector_Create(CTA_DEFAULT_VECTOR,nobs,
                                  CTA_DOUBLE,CTA_NULL,&(announced->values));
         if (retval!=CTA_OK) {
            CTA_WRITE_ERROR("Cannot create vector");
            return retval;
         }

         /* Allocate vector for holding all times of the observations */
         retval=CTA_Vector_Create(CTA_DEFAULT_VECTOR,nobs,
                                  CTA_DOUBLE,CTA_NULL,&vtimes);
         if (retval!=CTA_OK) {
            CTA_WRITE_ERROR("Cannot create vector");
            return retval;
         }

         /* Get times associated to all observations */
         retval=CTA_ObsDescr_Get_ValueProperties(hdescr, "TIME", vtimes, CTA_DOUBLE);
         if (retval!=CTA_OK) {
            CTA_WRITE_ERROR("Cannot get times associated to all observations");
            return retval;
         }

         /* Get all values in a C-array */
         allTimes=CTA_Malloc(nobs*sizeof(double));
         retval=CTA_Vector_GetVals(vtimes,allTimes,nobs,CTA_DOUBLE);
         if (retval!=CTA_OK) {
            CTA_WRITE_ERROR("Cannot get values in array");
            return retval;
         }

         /* Sort times */
         direc='I';
         DLASRT_F77(&direc, &nobs, allTimes, &info ,1);

         /* count the number of different times in allTimes */
         nunique=1;
         lastTime=allTimes[0];
         for (i=1;i<nobs;i++){
            if (lastTime!=allTimes[i]){
               nunique++;
               lastTime=allTimes[i];
            }
         }
         if (IDEBUG>0) {
            printf("CTA_Model_AnnounceObsValues_Set: observations at %d times\n",
               nunique);
            printf("CTA_Model_AnnounceObsValues_Set: times are: \n");

            for (itime=0;itime<nunique; itime++){
               printf("%f ",allTimes[itime]);
            }
            printf("\n");
         }

         /* allocate array times (only containing the unique times */
         announced->times=CTA_Malloc(nunique*sizeof(double));
         announced->ntimes=nunique;
         announced->isset=FALSE;
         announced->times[0]=allTimes[0];
         nunique=0;
         for (i=1;i<nobs;i++){
            if (announced->times[nunique]!=allTimes[i]){
               nunique++;
               announced->times[nunique]=allTimes[i];
                       }
         }
         free(allTimes);
         CTA_Vector_Free(&vtimes);
         CTA_ObsDescr_Create(CTA_OBSDESCR_TABLE, hdescr,
                             &(announced->ObsDescr));
      }
   }
   return CTA_OK;
}


#undef METHOD
#define METHOD "AnnounceObsValues"
int CTA_Model_AnnounceObsValues(
  CTA_Model hmodel,    /* handle of model               */
  CTA_ObsDescr hdescr  /* observation description       */
){
   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTA_Function *function;   /* Function that must be called */
   CTAI_Model *data;         /* Data assosiated to model     */

   if (IDEBUG) {printf("CTA_Model_AnnounceObsValues: start\n");}
   if (IDEBUG) {printf("CTA_Model_AnnounceObsValues: hmodel=%d, hdescr=%d\n",
      hmodel,hdescr);}

   /* Check type of handle */
   retval=CTA_Handle_Check((CTA_Handle) hmodel,CTA_MODEL);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Handle is not a cta_model handle");
      return retval;
   }

   /* Get Modeldata object */
   retval=CTA_Handle_GetData((CTA_Handle) hmodel, (void*) &data);
   if (retval!=CTA_OK)  {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* Get Function pointer */
   if (data->functions[CTA_MODEL_ANNOUNCE_OBSVALUES]!=CTA_NULL) {
      if (IDEBUG) {printf("CTA_Model_AnnounceObsValues: using user-implementation\n");}

      /* User implementation is available of this method */
      retval=CTA_Func_GetFunc(data->functions[CTA_MODEL_ANNOUNCE_OBSVALUES],&function);
      if (retval!=CTA_OK)  {
	     CTA_WRITE_ERROR("Cannot get function CTA_Model_AnnoundeObsValues");
	     return retval;
      }

      /* Call function */
      function(data->data, &hdescr, &retval);
   } else {
      if (IDEBUG) {printf("CTA_Model_AnnounceObsValues: using costa-implementation\n");}

      /* Not available, then COSTA will try to do it for you :*/
      /* COSTA will fill the announced administration: */
      retval=CTA_Model_AnnounceObsValues_Set(hdescr, &(data->announced));
      if (retval!=CTA_OK)  {
	     CTA_WRITE_ERROR("Cannot fill announced administation");
	     return retval;
      }
   }
   return CTA_OK;
}


#undef METHOD
#define METHOD "GetObsValues" 
int CTA_Model_GetObsValues(
  CTA_Model hmodel,    /* handle of model               */
  CTA_Time htime,      /* timespan of simulation        */
  CTA_ObsDescr hdescr, /* observation description       */
  CTA_Vector values   /* returned (interpolated) values */
){
   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTA_Function *function;   /* Function that must be called */
   CTAI_Model *data;         /* Data assosiated to model     */

   /* Check type of handle */
   retval=CTA_Handle_Check((CTA_Handle) hmodel,CTA_MODEL);
   if (retval!=CTA_OK)  {
       CTA_WRITE_ERROR("Handle is not a cta_model handle");
       return retval;
   }

   /* Get Modeldata object */
   retval=CTA_Handle_GetData((CTA_Handle) hmodel, (void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* Do we have announced obs */
   if (data->announced.isset){

     /* We shoud check whether obsdescr is same as announced
        But for now we will not check just return the values
     */

     CTA_Vector_Copy(data->announced.values,values);

     /* We will now free the internal administration */
     CTAI_Clear_Announced(&(data->announced), FALSE);

      return CTA_OK;
   } else {
      /* We do not have announced observations just call user function */

      /* Get Function pointer */
      if (data->functions[CTA_MODEL_GET_OBSVALUES]==CTA_NULL){
         return CTA_NOT_IMPLEMENTED;
      }
      retval=CTA_Func_GetFunc(data->functions[CTA_MODEL_GET_OBSVALUES],
         &function);
      if (retval!=CTA_OK) {
	     CTA_WRITE_ERROR("Cannot get function CTA_MODEL_GET_OBSVALUES");
	     return retval;
      }

      /* Call function */
      function(data->data,&htime, &hdescr, &values, &retval);

      return retval;
   }
}

int CTA_Model_GetObsSelect(
  CTA_Model hmodel,    /* handle of model               */
  CTA_Time htime,      /* timespan of simulation        */
  CTA_ObsDescr hdescr, /* observation description       */
  CTA_String sselect   /* query to filter out the observations */
){
   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTA_Function *function;   /* Function that must be called */
   CTAI_Model *data;         /* Data associated to model     */

   if (IDEBUG>10) {printf("cta_model_getobsselect START \n");}

   /* Check type of handle */
   retval=CTA_Handle_Check((CTA_Handle) hmodel,CTA_MODEL);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_model handle");
	   return retval;
   }
   /* Get Modeldata object */
   retval=CTA_Handle_GetData((CTA_Handle) hmodel, (void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* Get Function pointer */
   if (data->functions[CTA_MODEL_GET_OBSSELECT]==CTA_NULL) {
      /* Obsselect is not implemented : return "*" */
      retval=CTA_String_Set(sselect," 1 >= 0 ");
      return retval;

   } else {
      retval=CTA_Func_GetFunc(data->functions[CTA_MODEL_GET_OBSSELECT],&function);

      if (retval!=CTA_OK) {
	     char message[1024];
         sprintf(message,"cta_model_getobsselect: user function not found %d \n",retval);
		 CTA_WRITE_ERROR(message);
         return retval;
      }

      if (IDEBUG>10) {printf("cta_model_getobsselect call user function \n");}

      /* Call function */
      function(data->data,&htime, &hdescr, &sselect, &retval);
      return retval;
   }}


#undef METHOD
#define METHOD "Export" 
int CTA_Model_Export(
  CTA_Model hmodel,     /* handle of model                                      */
  CTA_Handle hexport    /* export object (e.q. handle to file or pack instance) */
){
   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTA_Function *function;   /* Function that must be called */
   CTAI_Model *data;         /* Data associated to model     */

   retval=CTAI_Model_GetDataAndFunc(hmodel, I_CTA_MODEL_EXPORT, &data, &function);
   if (retval!=CTA_OK)  {
      CTA_WRITE_ERROR("Error in CTAI_Model_Get_DataAndFunk");
      return retval;
   }

   if (IDEBUG) printf("CTA_MODEL DEBUG: Calling export function \n");
   function(data->data, &hexport, &retval);
   if (IDEBUG) printf("CTA_MODEL DEBUG: Return code export function %d \n",
      retval);
   return retval;
}

#undef METHOD
#define METHOD "Import" 
int CTA_Model_Import(
  CTA_Model hmodel,     /* handle of model                                      */
  CTA_Handle himport     /* import object (e.q. handle to file or pack instance) */
){
   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTA_Function *function;   /* Function that must be called */
   CTAI_Model *data;         /* Data associated to model     */

   retval=CTAI_Model_GetDataAndFunc(hmodel, I_CTA_MODEL_IMPORT, &data, &function);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Error in GetDataAndFunc");
      return retval;
   }

   if (IDEBUG) printf("CTA_MODEL DEBUG: Calling import function \n");
   function(data->data, &himport, &retval);
   if (IDEBUG) printf("CTA_MODEL DEBUG: Return code import function %d \n",
      retval);
   return retval;
}


#undef METHOD
#define METHOD "GetTimeHorizon" 
int CTA_Model_GetTimeHorizon(
  CTA_Model hmodel,
  CTA_Time tHorizon
){
   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTA_Function *function;   /* Function that must be called */
   CTAI_Model *data;         /* Data associated to model     */

   retval=CTAI_Model_GetDataAndFunc(hmodel, CTA_MODEL_GET_TIMEHORIZON, &data, &function);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Error in CTAI_Model_GetDataAndFunc");
      return retval;
   }

   if (IDEBUG) printf("CTA_MODEL DEBUG: Calling TimeHorizon function \n");
   function(data->data, &tHorizon, &retval);
   if (IDEBUG) printf("CTA_MODEL DEBUG: Return code TimeHorizon function %d \n",
      retval);
   return retval;
}

#undef METHOD
#define METHOD "GetCurrentTime" 
int CTA_Model_GetCurrentTime(
  CTA_Model hmodel,
  CTA_Time tCurrent
){
   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTA_Function *function;   /* Function that must be called */
   CTAI_Model *data;         /* Data associated to model     */

   retval=CTAI_Model_GetDataAndFunc(hmodel, CTA_MODEL_GET_CURRENTTIME, &data, &function);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Error in CTAI_Model_GetDataAndFunc");
      return retval;
   }

   if (IDEBUG) printf("CTA_MODEL DEBUG: Calling CurrentTime function \n");
   function(data->data, &tCurrent, &retval);
   if (IDEBUG) printf("CTA_MODEL DEBUG: Return code CurrentTime function %d \n",
      retval);
   return retval;
}


#undef METHOD
#define METHOD "AdjCompute" 
int CTA_Model_AdjCompute(CTA_Model hmodel, CTA_Time time){
   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTA_Function *function;   /* Function that must be called */
   CTAI_Model *data;         /* Data associated to model     */

   retval=CTAI_Model_GetDataAndFunc(hmodel, CTA_MODEL_ADJ_COMPUTE, &data, &function);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Error in CTAI_Model_GetDataAndFunc");
      return retval;
   }

   function(data->data, &time, &retval);
   return retval;
}


#undef METHOD
#define METHOD "AdjPrepare" 
int CTA_Model_AdjPrepare(CTA_Model hmodel, CTA_Time time){
   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTA_Function *function;   /* Function that must be called */
   CTAI_Model *data;         /* Data associated to model     */

   retval=CTAI_Model_GetDataAndFunc(hmodel, CTA_MODEL_ADJ_PREPARE, &data, &function);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Error in CTAI_Model_GetDataAndFunc");
      return retval;
   }

   function(data->data, &time, &retval);
   return retval;
}


#undef METHOD
#define METHOD "AdjSetForc"
int CTA_Model_AdjSetForc(CTA_Model hmodel, CTA_ObsDescr hdescr, CTA_Vector vforc){

   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTA_Function *function;   /* Function that must be called */
   CTAI_Model *data;         /* Data associated to model     */

   retval=CTAI_Model_GetDataAndFunc(hmodel, CTA_MODEL_ADJ_SET_FORC, &data, &function);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Error in CTAI_Model_GetDataAndFunc");
      return retval;
   }
   function(data->data, &hdescr, &vforc, &retval);
   return retval;

}

#undef METHOD
#define METHOD "LoadPersistentState"
int CTA_Model_LoadPersistentState(CTA_Model hmodel, CTA_String filename, CTA_String *instanceID){

   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTA_Function *function;   /* Function that must be called */
   CTAI_Model *data;         /* Data associated to model     */

   retval=CTAI_Model_GetDataAndFunc(hmodel, CTA_MODEL_LOAD_PERSISTENTSTATE, &data, &function);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Error in CTA_Model_LoadPersistentState");
      return retval;
   }
   function(data->data, &filename, instanceID, &retval);
   return retval;
}

#undef METHOD
#define METHOD "SaveInternalState"
int CTA_Model_SaveInternalState(CTA_Model hmodel, CTA_String *instanceID){

   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTA_Function *function;   /* Function that must be called */
   CTAI_Model *data;         /* Data associated to model     */

   if (IDEBUG>0) {
      printf("Start of CTA_Model_SaveInternalState \n");
      printf("Model handle = %d\n",hmodel);
   }

   retval=CTAI_Model_GetDataAndFunc(hmodel, CTA_MODEL_SAVE_INTERNALSTATE, &data, &function);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Error in CTA_Model_SaveInternalState");
      return retval;
   }
   function(data->data, instanceID, &retval);
   if (IDEBUG>0) {
      printf("End of CTA_Model_SaveInternalState \n");
   }
   return retval;
}

#undef METHOD
#define METHOD "RestoreInternalState"
int CTA_Model_RestoreInternalState(CTA_Model hmodel, CTA_String instanceID){

   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTA_Function *function;   /* Function that must be called */
   CTAI_Model *data;         /* Data associated to model     */

   retval=CTAI_Model_GetDataAndFunc(hmodel, CTA_MODEL_RESTORE_INTERNALSTATE, &data, &function);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Error in CTA_Model_RestoreState");
      return retval;
   }
   function(data->data, &instanceID, &retval);
   return retval;
}

#undef METHOD
#define METHOD "ReleaseInternalState"
int CTA_Model_ReleaseInternalState(CTA_Model hmodel, CTA_String instanceID){

   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTA_Function *function;   /* Function that must be called */
   CTAI_Model *data;         /* Data associated to model     */

   if (IDEBUG>0) {
      printf("Start of CTA_Model_ReleaseInternalState \n");
   }
   retval=CTAI_Model_GetDataAndFunc(hmodel, CTA_MODEL_RELEASE_INTERNALSTATE, &data, &function);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Error in CTA_Model_ReleaseInternalState");
      return retval;
   }
   function(data->data, &instanceID, &retval);
   return retval;
}

#undef METHOD
#define METHOD "SavePersistentState"
// DE fop methode die eigenlijk bij een ImodelState zou moeten horen
int CTA_Model_SavePersistentState(CTA_Model hmodel, CTA_String filename, CTA_String instanceID){

   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTA_Function *function;   /* Function that must be called */
   CTAI_Model *data;         /* Data associated to model     */

   if (IDEBUG>0) {
      printf("Start of CTA_Model_SavePersistentState \n");
   }

   retval=CTAI_Model_GetDataAndFunc(hmodel, CTA_MODEL_SAVE_PERSISTENTSTATE, &data, &function);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Error in CTA_Model_SavePersistentState");
      return retval;
   }
   /* Create new handle with usrdata */
   //retval=CTA_Handle_Create("file",CTA_FILE,filename,hfile);

   function(data->data, &filename, &instanceID, &retval);
   return retval;
}

/** \brief Get the number of domains for local analysis
 *  
 * \param hmodel   I  handle of model instance
 * \param distance I  characteristic distance
 * \param ndomain  O  number of domains
 * \param locVecs  O  costa vector of handles to treevectors (scaling vectors). The treevectors
 *                    are created when the indices are CTA_NULL on entry
 *
 * \return error status: CTA_OK if successful
 */
#undef METHOD
#define METHOD "GetNumDomains"
int CTA_Model_GetNumDomains(CTA_Model hmodel, double distance, int *nDomains){

   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTA_Function *function;   /* Function that must be called */
   CTAI_Model *data;         /* Data associated to model     */

   retval=CTAI_Model_GetDataAndFunc(hmodel, CTA_MODEL_GET_NUMDOMAINS, &data, &function);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Error in retreving function pointer of CTA_MODEL_GET_NUMDOMAINS");
      return retval;
   }

   if (IDEBUG) printf("CTA_MODEL DEBUG: Callling CTA_MODEL_GET_NUMDOMAINS function \n");
   function(data->data, &distance, nDomains, &retval);
   if (IDEBUG) printf("CTA_MODEL DEBUG: Return code CTA_MODEL_GET_NUMDOMAINS function %d\n",
      retval);
   return retval;
}


/** \brief Get selection of observations that are relevnet for assimilation in the given domain
 *  
 * \param hmodel   I  handle of model instance
 * \param hdescr   I  observation description of all observations
 * \param distance I  characteristic distance
 * \param idomain  I  domain number
 * \param locVecs  O  costa vector of handles to treevectors (scaling vectors). The treevectors
 *                    are created when the indices are CTA_NULL on entry
 *
 * \return error status: CTA_OK if successful
 */
#undef METHOD
#define METHOD "GetObsSelector"
int CTA_Model_GetObsSelector( CTA_Model hmodel, 
                   CTA_ObsDescr hdescr, double distance, int iDomain, CTA_Vector *selection){

   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTA_Function *function;   /* Function that must be called */
   CTAI_Model *data;         /* Data associated to model     */

   retval=CTAI_Model_GetDataAndFunc(hmodel, CTA_MODEL_GET_OBSSELECTOR, &data, &function);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Error in retreving function pointer of CTA_MODEL_GET_OBSSELECTOR");
      return retval;
   }

   if (IDEBUG) printf("CTA_MODEL DEBUG: Callling CTA_MODEL_GET_OBSSELECTOR function \n");
   function(data->data, hdescr, &distance, &iDomain, selection, &retval);
   if (IDEBUG) printf("CTA_MODEL DEBUG: Return code CTA_MODEL_GET_OBSSELECTOR function %d\n",
      retval);
   return retval;
}



/** \brief Get for each observation a localization scaling vector for single domain
 *  
 * \param hmodel   I  handle of model instance
 * \param hdescr   I  observation description for which we want localization scaling vectors
 * \param distance I  characteristic distance
 * \param idomain  I  domain number
 * \param locVecs  O  costa vector of handles to treevectors (scaling vectors). The treevectors
 *                    are created when the indices are CTA_NULL on entry
 *
 * \return error status: CTA_OK if successful
 */
#undef METHOD
#define METHOD "GetObsLocalizationDomain"
int CTA_Model_GetObsLocalizationDomain( CTA_Model hmodel, 
                   CTA_ObsDescr hdescr, double distance, int iDomain, CTA_Vector locVecs){

   return CTAI_Model_GetObsLocalization(hmodel, hdescr, distance, iDomain, locVecs);

}


/** \brief Get a copy of the internal state.
 *
 * \note Optionally a tree-vector is created. In that case the caller of this
 * method is responsible for freeing that tree-vector. The input state must be compatible
 * (same size and or composition) as the models internal state.
 * \note If *hstate == CTA_NULL a new object is created, user is responsible for freeing this object.
 *
 * \param hmodel   I  handle of model instance
 * \param iDomain  I  domain number
 * \param hstate   IO receives state of the model, *hstate can be CTA_NULL on calling (see note)
 * \return error status: CTA_OK if successful
 */
#undef METHOD
#define METHOD "GetStateDomain"
int CTA_Model_GetStateDomain(CTA_Model hmodel, int iDomain, CTA_TreeVector *hstate){
   return CTAI_Model_GetSet(hmodel, CTA_NULL, hstate, I_CTA_MODEL_GET_STATE, iDomain); 
}

/** \brief Perform axpy operation on the internal state for a single domain 
 *
 * \note AXPY: y=alpha*x+y. y corresponds to the models
 *       internal state and x can be a state vector or a model
 
 * \param hmodel   IO handle of model instance (y)
 * \param alpha    I  alpha
 * \param hx       I  handle of x (state vector )
 * \param iDomain  I  domain number
 * \return error status: CTA_OK if successful
 */
#undef METHOD
#define METHOD "AxpyStateDomain"
int CTA_Model_AxpyStateDomain(CTA_Model hmodel, double alpha, int iDomain, CTA_Handle hx){

   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTA_Function *function;   /* Function that must be called */
   CTAI_Model *data;         /* Data associated to model     */


   retval=CTAI_Model_GetDataAndFunc(hmodel, CTA_MODEL_AXPY_STATEDOMAIN, &data, &function);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Error in retreving function pointer of CTA_MODEL_GET_OBSSELECTOR");
      return retval;
   }
   function(data->data, &alpha, &hx, &iDomain, &retval);
   if (retval!=CTA_OK)  {
      CTA_WRITE_ERROR("Error in function CTA_MODEL_AXPY_STATEDOMAIN");
      return retval;
    }
    return CTA_OK;
}


CTAEXPORT void CTA_MODEL_CREATE_F77(int *hmodcl, int *userdata, int *hmodel, int *ierr){
   *ierr=CTA_Model_Create((CTA_ModelClass) *hmodcl, (CTA_Handle) *userdata, (CTA_Model*) hmodel);
}

CTAEXPORT void CTA_MODEL_COMPUTE_F77(int *hmodel, int *htime, int *ierr){
   *ierr=CTA_Model_Compute((CTA_Model) *hmodel, (CTA_Time) *htime);
}

CTAEXPORT void CTA_MODEL_ADDNOISE_F77(int *hmodel, int *htime, int *ierr){
   *ierr=CTA_Model_AddNoise((CTA_Model) *hmodel, (CTA_Time) *htime);
}
CTAEXPORT void CTA_MODEL_SETSTATE_F77(int *hmodel, int *hstate, int *ierr){
   *ierr=CTA_Model_SetState((CTA_Model) *hmodel, (CTA_TreeVector) *hstate);
}

CTAEXPORT void CTA_MODEL_GETSTATE_F77(int *hmodel, int *hstate, int *ierr){
   *ierr=CTA_Model_GetState((CTA_Model) *hmodel, (CTA_TreeVector*) hstate);
}

CTAEXPORT void CTA_MODEL_AXPYSTATE_F77(int *hmodel, double *alpha, int *hstate_x, int *ierr){
   *ierr=CTA_Model_AxpyState((CTA_Model) *hmodel, (double) *alpha, (CTA_Handle) *hstate_x);
}

CTAEXPORT void CTA_MODEL_GETSTATESCALING_F77(int *hmodel, int *hscal, int *ierr){
   *ierr=CTA_Model_GetStateScaling((CTA_Model) *hmodel, (CTA_TreeVector*) hscal);
}

CTAEXPORT void CTA_MODEL_SETFORC_F77(int *hmodel, int *tspan, int *hstate, int *ierr){
   *ierr=CTA_Model_SetForc((CTA_Model) *hmodel, (CTA_Time) *tspan, (CTA_TreeVector) *hstate);
}

CTAEXPORT void CTA_MODEL_GETFORC_F77(int *hmodel, int *tspan, int *hstate, int *ierr){
   *ierr=CTA_Model_GetForc((CTA_Model) *hmodel, (CTA_Time) *tspan, (CTA_TreeVector*) hstate);
}

CTAEXPORT void CTA_MODEL_AXPYFORC_F77(int *hmodel, int *tspan, double *alpha, int *hstate_x, int *ierr){
   *ierr=CTA_Model_AxpyForc((CTA_Model) *hmodel, (CTA_Time) *tspan, (double) *alpha, (CTA_TreeVector) *hstate_x);
}

CTAEXPORT void CTA_MODEL_SETPARAM_F77(int *hmodel, int *hstate, int *ierr){
   *ierr=CTA_Model_SetParam((CTA_Model) *hmodel, (CTA_TreeVector) *hstate);
}

CTAEXPORT void CTA_MODEL_GETPARAM_F77(int *hmodel, int *hstate, int *ierr){
   *ierr=CTA_Model_GetParam((CTA_Model) *hmodel, (CTA_TreeVector*) hstate);
}

CTAEXPORT void CTA_MODEL_AXPYPARAM_F77(int *hmodel, double *alpha, int *hstate_x, int *ierr){
   *ierr=CTA_Model_AxpyParam((CTA_Model) *hmodel, (double) *alpha, (CTA_TreeVector) *hstate_x);
}

CTAEXPORT void CTA_MODEL_GETNOISECOVAR_F77(int *hmodel, int *hstmat, int *ierr){
   *ierr=CTA_Model_GetNoiseCovar((CTA_Model) *hmodel, (CTA_TreeVector*) hstmat);
}

CTAEXPORT void CTA_MODEL_GETNOISECOUNT_F77(int *hmodel,int *nnoise, int *ierr){
   *ierr=CTA_Model_GetNoiseCount((CTA_Model) *hmodel,(CTA_Time *) nnoise);
};

CTAEXPORT void CTA_MODEL_FREE_F77(int *hmodel, int *ierr){
   *ierr=CTA_Model_Free((CTA_Model*) hmodel);
};

CTAEXPORT void CTA_MODEL_EXPORT_F77(int *hmodel, int *hexport, int *ierr){
   *ierr=CTA_Model_Export((CTA_Model) *hmodel, (CTA_Handle) *hexport);
};

CTAEXPORT void CTA_MODEL_IMPORT_F77(int *hmodel, int *himport, int *ierr){
   *ierr=CTA_Model_Import((CTA_Model) *hmodel, (CTA_Handle) *himport);
};

CTAEXPORT void CTA_MODEL_GETTIMEHORIZON_F77(int *hmodel, int *tHorizon,
                                            int *ierr){
   *ierr=CTA_Model_GetTimeHorizon((CTA_Model) *hmodel, (CTA_Time) *tHorizon);
};

CTAEXPORT void CTA_MODEL_GETCURRENTTIME_F77(int *hmodel, int *tHorizon,
                                            int *ierr){
   *ierr=CTA_Model_GetCurrentTime((CTA_Model) *hmodel, (CTA_Time) *tHorizon);
};

CTAEXPORT void CTA_MODEL_ANNOUNCEOBSVALUES_F77(int *hmodel, int *hdescr, int *ierr){
   *ierr=CTA_Model_AnnounceObsValues((CTA_Model) *hmodel,
                                      (CTA_ObsDescr) *hdescr);
};

CTAEXPORT void CTA_MODEL_GETOBSVALUES_F77(int *hmodel, int *htime, int *hdescr, int *values, int *ierr){

   *ierr=CTA_Model_GetObsValues((CTA_Model) *hmodel, (CTA_Time) *htime,
                                (CTA_ObsDescr) *hdescr, (CTA_Vector) *values);
}

CTAEXPORT void CTA_MODEL_GETOBSLOCALIZATION_F77 (int *hmodel, int *hdescr, double *distance, int *locVecs, int*ierr){

   *ierr=CTA_Model_GetObsLocalization((CTA_Model) *hmodel,(CTA_ObsDescr) *hdescr,  (double) *distance, (CTA_Vector) *locVecs);
}

CTAEXPORT void CTA_MODEL_GETOBSSELECT_F77(int *hmodel, int *htime, int *hdescr, int *sselect, int *ierr){

   *ierr=CTA_Model_GetObsSelect((CTA_Model) *hmodel, (CTA_Time) *htime,
                                (CTA_ObsDescr) *hdescr, (CTA_String) *sselect);
};

CTAEXPORT void CTA_MODEL_SAVEINTERNALSTATE_F77(int *hmodel, int *instanceID, int *ierr){
  *ierr=CTA_Model_SaveInternalState((CTA_Model) *hmodel, (CTA_String *) instanceID);
};

CTAEXPORT void CTA_MODEL_RESTOREINTERNALSTATE_F77(int *hmodel, int *instanceID, int *ierr){
  *ierr=CTA_Model_RestoreInternalState((CTA_Model) *hmodel, (CTA_String) *instanceID);
};

CTAEXPORT void CTA_MODEL_RELEASEINTERNALSTATE_F77(int *hmodel, int *instanceID, int *ierr){
  *ierr=CTA_Model_ReleaseInternalState((CTA_Model) *hmodel, (CTA_String) *instanceID);
};

CTAEXPORT void CTA_MODEL_SAVEPERSISTENTSTATE_F77(int *hmodel, int *filename, int *instanceID, int *ierr){
  *ierr=CTA_Model_SavePersistentState((CTA_Model) *hmodel, (CTA_String) *filename, (CTA_String) *instanceID);
};

CTAEXPORT void CTA_MODEL_LOADPERSISTENTSTATE_F77(int *hmodel, int *filename, int *instanceID, int *ierr){
  *ierr=CTA_Model_LoadPersistentState((CTA_Model) *hmodel, (CTA_String) *filename, (CTA_String *) instanceID);
};
