/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_model_factory.c $
$Revision: 4029 $, $Date: 2013-06-13 16:30:19 +0200 (Thu, 13 Jun 2013) $

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

#include "cta_string.h"
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
#include "ctai.h"

#if defined(_WIN32) || defined(_WIN64) 
  #define snprintf _snprintf 
  #define vsnprintf _vsnprintf 
  #define strcasecmp _stricmp 
  #define strncasecmp _strnicmp 
#endif


#define MAX(a,b) (a>b ? a: b)
#define IDEBUG (1)

#define CTA_MODEL_DEFINECLASS_F77      F77_CALL(cta_model_defineclass,CTA_MODEL_DEFINECLASS)

#define CLASSNAME "CTA_Model_Factory"

typedef struct {
CTA_Func functions[CTA_MODEL_NUMFUNC];
char *implements;
char *parallel_type;
char *spawn_workers;
char *nproc;
char *ntimes;
char *dumProcs;
char *flag_barrier;
double t_step;
int nChildModels;            /* Number of child models that are created (and still alive) from this class.              */
CTA_Model *ChildModels;      /* Handles of all child models                                                             */
int *ChildComputeIsBlocked;  /* Flags indication which child models are blocked due to a compute over interval > t_step */
} CTAI_ModelClass;


int CTAI_ModelFac_SetImplements(CTA_ModelClass hmodcl, char *implements){

   CTAI_ModelClass *clsdata;
   int retval;

   /* Get class data containing all function pointers */
   retval=CTA_Handle_Check((CTA_Handle) hmodcl,CTA_MODELCLASS);
   if (retval!=CTA_OK) return retval;
   retval=CTA_Handle_GetData((CTA_Handle) hmodcl,(void*) &clsdata);
   if (retval!=CTA_OK) return retval;

   if (clsdata->implements) free(clsdata->implements);
   if (implements) {
      clsdata->implements=CTA_Malloc(sizeof(char)*(strlen(implements)+1));
      strcpy(clsdata->implements,implements);
   } else {
      clsdata->implements=CTA_Malloc(sizeof(char)*(strlen("unknown")+1));
      strcpy(clsdata->implements,"unknown");
   }
   return CTA_OK;
}

int CTAI_Model_DuplicateClass(CTA_ModelClass hmodcl,
                              CTA_ModelClass *hmodcl_dup){

   CTAI_ModelClass *clsdata;
   CTAI_ModelClass *clsdata_dup;
   CTA_Func hfunc;
   CTA_String sname;
   char *implements, *parallel_type, *nproc, *ntimes, *dumProcs, *spawn_workers;
   char *flag_barrier;
   double t_step;
   int nChildModels;
   CTA_Model *ChildModels;
   int *ChildComputeIsBlocked;
   int retval;
   int i;

   /* Get class data containing all function pointers */
   retval=CTA_Handle_Check((CTA_Handle) hmodcl,CTA_MODELCLASS);
   if (retval!=CTA_OK) return retval;
   retval=CTA_Handle_GetData((CTA_Handle) hmodcl,(void*) &clsdata);
   if (retval!=CTA_OK) return retval;

   /* Allocate new ModelClass object */
   clsdata_dup=CTA_Malloc(sizeof(CTAI_ModelClass));
   clsdata_dup->implements            = NULL;
   clsdata_dup->parallel_type         = NULL;
   clsdata_dup->spawn_workers         = NULL;
   clsdata_dup->nproc                 = NULL;
   clsdata_dup->ntimes                = NULL;
   clsdata_dup->dumProcs              = NULL;
   clsdata_dup->flag_barrier          = NULL;
   clsdata_dup->t_step                = 0.0;
   clsdata_dup->nChildModels          = 0;
   clsdata_dup->ChildModels           = NULL;
   clsdata_dup->ChildComputeIsBlocked = NULL;

   /* Duplicate all function pointers */
   for (i=0;i<CTA_MODEL_NUMFUNC;i++){
      CTA_Func_Duplicate(clsdata->functions[i], &hfunc);
      clsdata_dup->functions[i]=hfunc;
   }

   /* Duplicate implements */
   implements=clsdata->implements;
   if (implements) {
      clsdata_dup->implements=CTA_Malloc(sizeof(char)*(strlen(implements)+1));
      strcpy(clsdata_dup->implements,implements);
   }

   /* Duplicate parallel_type */
   parallel_type=clsdata->parallel_type;
   if(parallel_type) {
      clsdata_dup->parallel_type=CTA_Malloc(sizeof(char)*(strlen(parallel_type)+1));
      strcpy(clsdata_dup->parallel_type,parallel_type);
   }

   /* Duplicate spawn_workers */
   spawn_workers=clsdata->spawn_workers;
   if(spawn_workers) {
      clsdata_dup->spawn_workers=CTA_Malloc(sizeof(char)*(strlen(spawn_workers)+1));
      strcpy(clsdata_dup->spawn_workers,spawn_workers);
   }

   /* Duplicate nproc */
   nproc=clsdata->parallel_type;
   if (nproc) {
      clsdata_dup->nproc=CTA_Malloc(sizeof(char)*(strlen(nproc)+1));
      strcpy(clsdata_dup->nproc,nproc);
   }

   /* Duplicate ntimes */
   ntimes=clsdata->ntimes;
   if (ntimes) {
      clsdata_dup->ntimes=CTA_Malloc(sizeof(char)*(strlen(ntimes)+1));
      strcpy(clsdata_dup->ntimes,ntimes);
   }

   /* Duplicate dumProcs */
   dumProcs=clsdata->dumProcs;
   if (dumProcs) {
      clsdata_dup->dumProcs=CTA_Malloc(sizeof(char)*(strlen(dumProcs)+1));
      strcpy(clsdata_dup->dumProcs,dumProcs);
   }

   /* Duplicate flag_barrier */
   flag_barrier=clsdata->flag_barrier;
   if (flag_barrier) {
      clsdata_dup->flag_barrier=CTA_Malloc(sizeof(char)*(strlen(flag_barrier)+1));
      strcpy(clsdata_dup->flag_barrier,flag_barrier);
   }

   /* Duplicate t_step */
   t_step=clsdata->t_step;
   clsdata_dup->t_step=t_step;

   /* Duplicate administration of child models */
   nChildModels = clsdata->nChildModels;
   clsdata_dup->nChildModels=nChildModels;

   ChildModels = clsdata->ChildModels;
   if (ChildModels) {
      clsdata_dup->ChildModels = CTA_Malloc(nChildModels*sizeof(CTA_Model));
      memcpy(clsdata_dup->ChildModels, ChildModels, sizeof(clsdata_dup->ChildModels));
   }

   ChildComputeIsBlocked = clsdata->ChildComputeIsBlocked;
   if (ChildComputeIsBlocked) {
      clsdata_dup->ChildComputeIsBlocked = CTA_Malloc(nChildModels*sizeof(int));
      memcpy(clsdata_dup->ChildComputeIsBlocked, ChildComputeIsBlocked, sizeof(clsdata_dup->ChildComputeIsBlocked));
   }

   // Allocate new handle and return error when unsuccesfull
   CTA_String_Create(&sname);
   CTA_Handle_GetName(hmodcl, sname);
   if (IDEBUG>0) printf("CTAI_Model_DuplicateClass: NAME OF MODEL CLASS IS %s\n", CTAI_String_GetPtr(sname));


   retval=CTA_Handle_Create(CTAI_String_GetPtr(sname), CTA_MODELCLASS,
                            clsdata_dup, hmodcl_dup);
   CTA_String_Free(&sname);
   return retval;
}

/* Add new model instance to list of all models */
int CTAI_ModelFac_AddModelInstance(CTA_ModelClass hmodcl, CTA_Model hmodel){

   /* Local variables */
   CTAI_ModelClass *clsdata; /* Data associated to modelclass     */
   int retval;               /* Return value of COSTA call        */
   int imodel;               /* loop variable                     */

   if (IDEBUG>0) printf("CTA_ModelFac_AddModelInstance: Start of function \n");

   /* Get class data */
   retval=CTA_Handle_Check((CTA_Handle) hmodcl,CTA_MODELCLASS);
   if (retval!=CTA_OK) return retval;
   retval=CTA_Handle_GetData((CTA_Handle) hmodcl,(void*) &clsdata);
   if (retval!=CTA_OK) return retval;

   // reallocate 2 arrays + initialisation
   clsdata->nChildModels++;

   if (clsdata->ChildModels==NULL) {
      clsdata->ChildModels = CTA_Malloc(clsdata->nChildModels*sizeof(CTA_Model));
   } else {
      CTA_Model *tmp1 = CTA_Realloc(clsdata->ChildModels, clsdata->nChildModels*sizeof(CTA_Model));
      if (tmp1!=NULL) {
        clsdata->ChildModels = tmp1;
      }else{
        free(clsdata->ChildModels);
        printf("error allocating");
        return 1;
      }
    }

   if (clsdata->ChildComputeIsBlocked==NULL) {
      clsdata->ChildComputeIsBlocked = CTA_Malloc(clsdata->nChildModels*sizeof(int));
   } else {
      CTA_Model *tmp2 = CTA_Realloc(clsdata->ChildComputeIsBlocked, clsdata->nChildModels*sizeof(int));
      if (tmp2!=NULL) {
         clsdata->ChildComputeIsBlocked = tmp2;
      } else{
         free(clsdata->ChildComputeIsBlocked);
         printf("error allocating");
         return 1;
      }
   }

   clsdata->ChildModels[clsdata->nChildModels-1] = hmodel;
   clsdata->ChildComputeIsBlocked[clsdata->nChildModels-1] = CTA_FALSE;

   if (IDEBUG>0) {
     printf("number of Child models: %d \n", clsdata->nChildModels);
     for (imodel=0;imodel<clsdata->nChildModels;imodel++){
     printf("i, hmodel,flag: %d, %d, %d \n",imodel,clsdata->ChildModels[imodel],
         clsdata->ChildComputeIsBlocked[imodel]);
     }
   }
   if (IDEBUG>0) printf("CTA_ModelFac_AddModelInstance: End of function \n");
   return CTA_OK;
}

/* Remove model instance from list of all models */
int CTAI_ModelFac_DelModelInstance(CTA_ModelClass hmodcl, CTA_Model hmodel){

   /* Local variables */
   CTAI_ModelClass *clsdata; /* Data associated to modelclass           */
   int retval;               /* Return value of COSTA call              */
   int imodel;               /* loop variable                           */
   int ifound;               /* flag indicating model was found in list */

   if (IDEBUG>0) printf("CTA_ModelFac_DelModelInstance: Start of function \n");

   /* Get class data */
   retval=CTA_Handle_Check((CTA_Handle) hmodcl,CTA_MODELCLASS);
   if (retval!=CTA_OK) return retval;
   retval=CTA_Handle_GetData((CTA_Handle) hmodcl,(void*) &clsdata);
   if (retval!=CTA_OK) return retval;

   ifound = -1;
   /* find index of model to be removed from list*/
   for (imodel=0;imodel<clsdata->nChildModels;imodel++){
     if (clsdata->ChildModels[imodel] == hmodel) {
       ifound = imodel;
     }
   }

   /* overwrite information of this model */
   if (ifound>0) {
     clsdata->nChildModels--;
     for (imodel=ifound;imodel<clsdata->nChildModels;imodel++) {
       clsdata->ChildModels[imodel] = clsdata->ChildModels[imodel+1];
       clsdata->ChildComputeIsBlocked[imodel] = clsdata->ChildComputeIsBlocked[imodel+1];
     }
   }
   if (IDEBUG>0) printf("CTA_ModelFac_DelModelInstance: End of function \n");
   return CTA_OK;
}

#undef METHOD
#define METHOD "SetBlock"
/* Set block for own model if barrier falls within current compute interval */
int CTAI_ModelFac_SetBlock(CTA_ModelClass hmodcl, CTA_Model hmodel){

   /* Local variables */
   CTAI_ModelClass *clsdata; /* Data associated to modelclass     */
   int retval;               /* Return value of COSTA call        */
   int imodel;               /* loop variable                     */

   if (IDEBUG>0) printf("CTA_ModelFac_SetBlock: Start of function \n");

   /* Get class data */
   retval=CTA_Handle_Check((CTA_Handle) hmodcl,CTA_MODELCLASS);
   if (retval!=CTA_OK) return retval;
   retval=CTA_Handle_GetData((CTA_Handle) hmodcl,(void*) &clsdata);
   if (retval!=CTA_OK) return retval;

   for (imodel=0;imodel<clsdata->nChildModels;imodel++){
      if (clsdata->ChildModels[imodel]==hmodel){
         clsdata->ChildComputeIsBlocked[imodel]=CTA_TRUE;
      }
   }
   if (IDEBUG>0) printf("CTA_ModelFac_SetBlock: End of function \n");
   return CTA_OK;
}

#undef METHOD
#define METHOD "GetBlock"
/* Return value of clsdata->ChildComputeIsBlocked[hmodel] */
int CTAI_ModelFac_GetBlock(CTA_ModelClass hmodcl, CTA_Model hmodel){

   /* Local variables */
   CTAI_ModelClass *clsdata; /* Data associated to modelclass     */
   int retval;               /* Return value of COSTA call        */
   int imodel;               /* loop variable                     */

   if (IDEBUG>0) printf("CTA_ModelFac_GetBlock: Start of function \n");

   /* Get class data */
   retval=CTA_Handle_Check((CTA_Handle) hmodcl,CTA_MODELCLASS);
   if (retval!=CTA_OK) return retval;
   retval=CTA_Handle_GetData((CTA_Handle) hmodcl,(void*) &clsdata);
   if (retval!=CTA_OK) return retval;

   for (imodel=0;imodel<clsdata->nChildModels;imodel++){
      if (clsdata->ChildModels[imodel]==hmodel){
         return clsdata->ChildComputeIsBlocked[imodel];
      }
   }
   CTA_WRITE_ERROR("Model was not found in list of Child Models\n");
   return -1;
}

#undef METHOD
#define METHOD "TimeStepAllModels"
int CTAI_ModelFac_TimeStepAllModels(CTA_ModelClass hmodcl, CTA_Function *function, double tstart, double tstop){

   /* Local variables */
   CTAI_ModelClass *clsdata; /* Data associated to modelclass           */
   CTA_Model hmodel;         /* Handle of model                         */
   CTA_Time hstep;           /* timespan to compute                     */
   double t1,t2;             /* Begin and end time of hstep             */
   int retval;               /* Return value of COSTA call              */
   int imodel,istep;         /* loop counters                           */
   int nStep;                /* number of sub-intervals to step         */
   int nblock;               /* number of models currently blocked      */
   double tstep;             /* maximum interval that model may run     */

   if (IDEBUG>0) printf("CTA_ModelFac_TimeStepAllModels: Start of function \n");

   /* Get class data */
   retval=CTA_Handle_Check((CTA_Handle) hmodcl,CTA_MODELCLASS);
   if (retval!=CTA_OK) return retval;
   retval=CTA_Handle_GetData((CTA_Handle) hmodcl,(void*) &clsdata);
   if (retval!=CTA_OK) return retval;

   /* Find number of blocked models */
   nblock = 0;
   for (imodel = 0;imodel < clsdata->nChildModels;imodel++){
       nblock = nblock + clsdata->ChildComputeIsBlocked[imodel];
   }
   if (nblock==clsdata->nChildModels) {
      /* all child models are blocked*/
      if (IDEBUG>0) printf("CTA_ModelFac_TimeStepAllModels: All model instances are waiting\n");

      /* determine number of barrier steps in compute interval */
      tstep = clsdata->t_step;
      nStep=(int) ((tstop-tstart)/tstep + 0.5);
      if (IDEBUG>0) printf("CTA_ModelFac_TimeStepAllModels: number of timesteps: nStep = %d\n",nStep);

      CTA_Time_Create(&hstep);

      /* loop over all barrier intervals */
      t1 = tstart;
      for (istep=0; istep < nStep; istep++){
         t2 = MIN(t1 + tstep,tstop);
         retval=CTA_Time_SetSpan(hstep,t1,t2);
         if (IDEBUG>0) printf("CTA_ModelFac_TimeStepAllModels: iStep=%d of nStep=%d tstep=%f\n",istep, nStep,tstep);
         if (IDEBUG>0) printf("CTA_ModelFac_TimeStepAllModels: tstart=%f tstop=%f\n",t1,t2);

         /* loop over all models */
         for (imodel = 0;imodel < clsdata->nChildModels;imodel++){
            if (IDEBUG>0) printf("model %d: from t=%f to t=%f\n",imodel,t1,t2);

               /* Check type of handle */
               hmodel = clsdata->ChildModels[imodel];
               retval=CTA_Handle_Check((CTA_Handle) hmodel,CTA_MODEL);
               if (retval!=CTA_OK) {
                  CTA_WRITE_ERROR("Handle is not a cta_model handle");
                  return retval;
               }
            retval = CTAI_Model_PerformTimesteps(hmodel,function,hstep,CTA_FALSE);
         }
         t1 = t2;
      }

      //reset barrier information for all child models
      if (IDEBUG>0) {printf("CTA_ModelFac_TimeStepAllModels: barrier information is reset. \n");}
      for (imodel = 0;imodel < clsdata->nChildModels;imodel++){
         clsdata->ChildComputeIsBlocked[imodel] = CTA_FALSE;
      }
      CTA_Time_Free(&hstep);
   }
   if (IDEBUG>0) {printf("CTA_ModelFac_TimeStepAllModels: End of function \n");}
   return CTA_OK;
}

int CTAI_ModelFac_SetBarrierData(CTA_ModelClass hmodcl, char *flag_barrier, char *t_step ) {

   CTAI_ModelClass *clsdata; /* Data associated to modelclass           */
   int retval;               /* Return value of COSTA call              */
   double fac;               /* multiply factor                         */
   double tval;              /* help variable                           */

   if (IDEBUG>0) {
      printf("CTA_ModelFac_SetBarrierData: Start of function \n");
      printf("flag = %s, t_step = %s \n", flag_barrier, t_step);
   }

   /* Get class data containing all function pointers */
   retval=CTA_Handle_Check((CTA_Handle) hmodcl,CTA_MODELCLASS);
   if (retval!=CTA_OK) return retval;
   retval=CTA_Handle_GetData((CTA_Handle) hmodcl,(void*) &clsdata);
   if (retval!=CTA_OK) return retval;

   if (clsdata->flag_barrier) free(clsdata->flag_barrier);

   clsdata->flag_barrier = CTA_FALSE;
   clsdata->t_step       = 0.0;

   if (flag_barrier != NULL) {
      if ( (0 == strcmp("true", flag_barrier)) && t_step ) {
         /* Set flag_barrier */
         clsdata->flag_barrier=CTA_Malloc(sizeof(char)*(strlen(flag_barrier)+1));
         strcpy(clsdata->flag_barrier,flag_barrier);

         /* Set t_step.                             */
         /* Possibly remove units MJD/HOUR/MIN/SEC */
         fac = 1.0;
         if (strlen(t_step)>4) {
             if (0==strcasecmp( "HOUR",(t_step+strlen(t_step)-4))) {
               t_step[strlen(t_step)-4]='\0';
               fac = 1.0/24;
             }
         }
         if (strlen(t_step)>3) {
            if (0==strcasecmp( "MJD",(t_step+strlen(t_step)-3))) {
              t_step[strlen(t_step)-3]='\0';
            }
            if (0==strcasecmp( "SEC",(t_step+strlen(t_step)-3))) {
              t_step[strlen(t_step)-3]='\0';
              fac = 1.0/(24*60*60);
            }
            if (0==strcasecmp( "MIN",(t_step+strlen(t_step)-3))) {
              t_step[strlen(t_step)-3]='\0';
              fac = 1.0/(24*60);
            }
         }
         tval = atof(t_step);
         clsdata->t_step=fac*tval;
      } else {
         if (0 == strcmp("false", flag_barrier)) {
            /* all ok no barrier info is specified */
         } else {
           CTA_WRITE_ERROR("Error: Incomplete specification of barrier information.\n");
           exit(-1);
        }
      }
   }
   if (IDEBUG>0) {
     printf("CTA_ModelFac_SetBarrierData: End of function \n");
     printf("clsdata->flag_barrier = %s, clsdata->t_step = %f \n", clsdata->flag_barrier, clsdata->t_step);
   }
   return CTA_OK;
}

int CTAI_ModelFac_SetParallelData(CTA_ModelClass hmodcl, char *implements, char *parallel_type, char *spawn_workers, char *nproc, char *ntimes, char *dumProcs ){

   CTAI_ModelClass *clsdata;
   int retval;

   /* Get class data containing all function pointers */
   retval=CTA_Handle_Check((CTA_Handle) hmodcl,CTA_MODELCLASS);
   if (retval!=CTA_OK) return retval;
   retval=CTA_Handle_GetData((CTA_Handle) hmodcl,(void*) &clsdata);
   if (retval!=CTA_OK) return retval;

   if (clsdata->parallel_type) free(clsdata->parallel_type);
   if (clsdata->spawn_workers) free(clsdata->spawn_workers);
   if (clsdata->nproc)         free(clsdata->nproc);
   if (clsdata->ntimes)        free(clsdata->ntimes);
   if (clsdata->implements)    free(clsdata->implements);
   if (clsdata->dumProcs)      free(clsdata->dumProcs);

   clsdata->parallel_type =NULL;
   clsdata->spawn_workers =NULL;
   clsdata->nproc         =NULL;
   clsdata->ntimes        =NULL;
   clsdata->implements    =NULL;
   clsdata->dumProcs      =NULL;

   /* Set implements */
   retval=CTAI_ModelFac_SetImplements(hmodcl, implements);

//   printf("%p, %p, %p, %p\n",parallel_type, nproc, ntimes, implements);
   if (parallel_type && nproc && ntimes && implements) {

      /* Set parallel_type */
      clsdata->parallel_type=CTA_Malloc(sizeof(char)*(strlen(parallel_type)+1));
      strcpy(clsdata->parallel_type,parallel_type);

      /* Set spawn_workers */
      clsdata->spawn_workers=CTA_Malloc(sizeof(char)*(strlen(spawn_workers)+1));
      strcpy(clsdata->spawn_workers,spawn_workers);

      /* Set nproc */
      clsdata->nproc=CTA_Malloc(sizeof(char)*(strlen(nproc)+1));
      strcpy(clsdata->nproc,nproc);

      /* Set ntimes */
      clsdata->ntimes=CTA_Malloc(sizeof(char)*(strlen(ntimes)+1));
      strcpy(clsdata->ntimes,ntimes);

      if (dumProcs){
         clsdata->dumProcs=CTA_Malloc(sizeof(char)*(strlen(dumProcs)+1));
         strcpy(clsdata->dumProcs,dumProcs);
      }
   } else if (!parallel_type && !nproc && !ntimes) {
      /* all ok no parallel info is specified */
   } else {
      printf("Error: Incomplete specification of parallel information of model_factory\n");
      printf("All or none of the fields  'parallel_type', 'nproc', 'ntimes' must be specified in the input\n");
      printf("specified are:\n");
      if (parallel_type) printf("parallel_type='%s'\n",parallel_type);
      if (spawn_workers) printf("spawn_workers='%s'\n",spawn_workers);
      if (nproc)         printf("nproc        ='%s'\n",nproc);
      if (ntimes)        printf("ntimes       ='%s'\n",ntimes);
      if (implements)    printf("implements   ='%s'\n",implements);
      exit(-1);
   }

   return CTA_OK;

}

int CTAI_ModelFac_GetParallelData(CTA_ModelClass hmodcl, char **implements, char **parallel_type, char **spawn_workers, char **nproc, char **ntimes, char **dumProcs ){

   CTAI_ModelClass *clsdata;
   int retval;

   /* Get class data containing all function pointers */
   retval=CTA_Handle_Check((CTA_Handle) hmodcl,CTA_MODELCLASS);
   if (retval!=CTA_OK) return retval;
   retval=CTA_Handle_GetData((CTA_Handle) hmodcl,(void*) &clsdata);
   if (retval!=CTA_OK) return retval;

   *parallel_type = clsdata->parallel_type;
   *spawn_workers = clsdata->spawn_workers;
   *nproc         = clsdata->nproc;
   *ntimes        = clsdata->ntimes;
   *implements    = clsdata->implements;
   *dumProcs      = clsdata->dumProcs;

   return CTA_OK;

}

int CTAI_ModelFac_GetBarrierData(CTA_ModelClass hmodcl, char **flag_barrier, double *t_step ) {

   CTAI_ModelClass *clsdata;
   int retval;

   /* Get class data containing all function pointers */
   retval=CTA_Handle_Check((CTA_Handle) hmodcl,CTA_MODELCLASS);
   if (retval!=CTA_OK) return retval;

   retval=CTA_Handle_GetData((CTA_Handle) hmodcl,(void*) &clsdata);
   if (retval!=CTA_OK) return retval;

   *flag_barrier = clsdata->flag_barrier;
   *t_step       = clsdata->t_step;

   if (IDEBUG>0) {
      printf("CTA_ModelFac_GetBarrierData: flag_barrier = %s, t_step = %f \n", *flag_barrier, *t_step);
   }
   return CTA_OK;

}

const char *CTAI_ModelFac_GetImplements(CTA_ModelClass hmodcl){

   CTAI_ModelClass *clsdata;
   int retval;

   /* Get class data containing all function pointers */
   retval=CTA_Handle_Check((CTA_Handle) hmodcl,CTA_MODELCLASS);
   if (retval!=CTA_OK) return NULL;
   retval=CTA_Handle_GetData((CTA_Handle) hmodcl,(void*) &clsdata);
   if (retval!=CTA_OK) return NULL;

   return clsdata->implements;
}

#undef METHOD
#define METHOD "New"
int CTA_ModelFactory_New(const char *fName, CTA_ModelClass *modelClass){

   int retval;
   CTA_String sFile;

   CTA_Tree   hTree;
   CTA_ModelClass modelClassFromTree;

   /* Read the model configuration file */
   if (IDEBUG>0) {printf("CTA_ModelFactory_New: Start of function \n");}
   if (IDEBUG>0) {printf("CTA_ModelFactory_New: name of configuration file: %s\n",fName);}
   retval=CTA_String_Create(&sFile);
   retval=CTA_String_Set(sFile,fName);
   retval=CTA_XML_Read(sFile, &hTree);
   retval=CTA_String_Free(&sFile);

   /* Get model class from tree */
   if (IDEBUG>0) {printf("CTA_ModelFactory_New: Get the modelclass from the tree \n");}
   retval=CTA_Tree_GetItem(hTree,1,&modelClassFromTree);
   if (retval!=CTA_OK){
	  char message[1024];
      sprintf(message,"CTA_ModelFactory_New cannot get first item from input-tree \n");
	  CTA_WRITE_ERROR(message);
      return retval;
   }

   /* Duplicate the modelclass since it will be deleted
      when the tree is deleted*/
   if (IDEBUG>0) {printf("CTA_ModelFactory_New: Duplicate this model class \n");}
   retval=CTAI_Model_DuplicateClass(modelClassFromTree, modelClass);
   if (retval!=CTA_OK){
      char message[1024];
	  sprintf(message,"CTA_ModelFactory_New cannot duplicate class from input-file\n");
      CTA_WRITE_ERROR(message);
	  return retval;
   }
   /* Delete the input-tree */
   if (IDEBUG>0) {printf("CTA_ModelFactory_New: Delete input tree \n");}
   retval=CTA_Tree_Free(&hTree);
   if (retval!=CTA_OK){
      char message[1024];
	  sprintf(message,"CTA_ModelFactory_New cannot free input tree\n");
      CTA_WRITE_ERROR(message);
	  return retval;
   }

   if (IDEBUG>0) {printf("CTA_ModelFactory_New: end of function class handle is %d\n",*modelClass);}
   return CTA_OK;
}

int CTA_Model_DefineClass(
   const char *name,
   const CTA_Func h_func[CTA_MODEL_NUMFUNC],
   CTA_ModelClass     *hmodcl
   ){

   CTAI_ModelClass *data;
   int retval;
   int i;

   /* Allocate new Vector object */
   data=CTA_Malloc(sizeof(CTAI_ModelClass));
   data->implements            = NULL;
   data->parallel_type         = NULL;
   data->spawn_workers         = NULL;
   data->nproc                 = NULL;
   data->ntimes                = NULL;
   data->dumProcs              = NULL;
   data->flag_barrier          = NULL;
   data->t_step                = 0.0;
   data->nChildModels          = 0;
   data->ChildModels           = NULL;
   data->ChildComputeIsBlocked = NULL;

   for (i=0;i<CTA_MODEL_NUMFUNC;i++){
      data->functions[i]=h_func[i];
   }
   data->implements=CTA_Malloc(sizeof(char)*(strlen("unknown")+1));
   strcpy(data->implements,"unknown");

   // Allocate new handle and return eror when unsuccesfull
   retval=CTA_Handle_Create(name,CTA_MODELCLASS,data,hmodcl);
   return retval;
}


int CTA_Model_DefineClass2(
   const char *name,
   const char *implements,
   const CTA_Func h_func[CTA_MODEL_NUMFUNC],
   CTA_ModelClass     *hmodcl
   ){

   CTAI_ModelClass *data;
   int retval;
   int i;

   if (IDEBUG>0) printf("CTA_Model_DefineClass2\n");

   /* Allocate new Vector object */
   data=CTA_Malloc(sizeof(CTAI_ModelClass));
   data->implements              = NULL;
   data->parallel_type           = NULL;
   data->spawn_workers           = NULL;
   data->nproc                   = NULL;
   data->ntimes                  = NULL;
   data->dumProcs                = NULL;
   data->flag_barrier            = NULL;
   data->t_step                  = 0.0;
   data->nChildModels            = 0;
   data->ChildModels             = NULL;
   data->ChildComputeIsBlocked   = NULL;

   for (i=0;i<CTA_MODEL_NUMFUNC;i++){
      data->functions[i]=h_func[i];
   }

   /* Implements model */
   if (implements) {
      data->implements=CTA_Malloc(sizeof(char)*(strlen(implements)+1));
      strcpy(data->implements,implements);
   } else {
      data->implements=CTA_Malloc(sizeof(char)*(strlen("unknown")+1));
      strcpy(data->implements,"unknown");
   }

   // Allocate new handle and return error when unsuccesfull
   retval=CTA_Handle_Create(name,CTA_MODELCLASS,data,hmodcl);
   return retval;
}


/* Interfacing with Fortran */
CTAEXPORT void CTA_MODEL_DEFINECLASS_F77(char *name, int *h_func, int *hmodcl, int *ierr, int len_name){
   char  *c_name;

   /* create a c-string equivalent to name */
   c_name=CTA_Malloc((len_name+1)*sizeof(char));
   CTA_fstr2cstr(name,c_name,len_name);

   *ierr=CTA_Model_DefineClass(c_name, (CTA_Func*) h_func, (CTA_ModelClass*) hmodcl);
   free(c_name);
}


/** \brief Create a COSTA modell class from XML
*          (load from methods from dynamic load library).
*
*  \param cur_node  I  Current XML node
*  \return             Handle to create or CTA_NULL in case of an error.
*/
CTA_ModelClass CTAI_XML_CreateModelClass(xmlNode *cur_node) {

   CTA_Func      hfunc;                  /* the new function */
   xmlChar       *id = NULL;             /* id of function in XML-tree */
   xmlChar       *clsname = NULL;        /* (lookup) name of the model class */
   xmlChar       *flag_barrier = NULL;   /* flag indicating that model cannot be advanced unlimited*/
   xmlChar       *t_step = NULL;         /* maximum number of time-steps that model may perform */
   xmlChar       *implements = NULL;     /* Name of model that is implemented */
   xmlChar       *parallel_type = NULL;  /* Type of parallelism that is used */
   xmlChar       *spawn_workers = NULL;  /* Spawn worker processes (1) yes/ (0) no */
   xmlChar       *nproc         = NULL;  /* number of processes for a single instance */
   xmlChar       *ntimes        = NULL;  /* number of times the workers will be assinged to processors */
   xmlChar       *dumProcs      = NULL;  /* ranks of the dummy processes in the worker-worker model */
   int           retval;                 /* return status of creation       */
   CTA_Func      h_func[CTA_MODEL_NUMFUNC]; /* List of functions */
   xmlNode       *func_node = NULL;      /* values of children nodes */
   int i;
   CTA_ModelClass hmodcl;                /* function class */
   CTA_ModelClass hmodcl_old;
   const char *name;
   CTA_String   sclsname;

   if (IDEBUG>0) printf("CTAI_XML_CreateModelClass: Start of function\n");
   /* Parse this node's attributes */
   /* Get id */
   id = xmlGetProp(cur_node, CTAI_XML_ID);

   /* Get class name */
   clsname = xmlGetProp(cur_node, CTAI_XML_NAME);

   /* Get implements_model */
   implements = xmlGetProp(cur_node, CTAI_XML_IMPLEMENTS);

   /* parallel options: */
   /*-number of processes */
   nproc = xmlGetProp(cur_node, CTAI_XML_NPROC);

   /*-type of parallel run */
   parallel_type = xmlGetProp(cur_node, CTAI_XML_PARALLEL_TYPE);

   /*-type of parallel run */
   spawn_workers = xmlGetProp(cur_node, CTAI_XML_SPAWN_WORKERS);

   /*-Number of times the parallel group must be created */
   ntimes = xmlGetProp(cur_node, CTAI_XML_NTIMES);

   /*-Process numbers (ranks) of dummy processes */
   dumProcs = xmlGetProp(cur_node, CTAI_XML_DUMPROCS);

   /* Flag for barrier moment */
   flag_barrier = xmlGetProp(cur_node, CTAI_XML_FLAG_BARRIER);

   /* Get T_step */
   t_step = xmlGetProp(cur_node, CTAI_XML_T_STEP);

   /* Check whether this is a known model-class */
   CTA_String_Create(&sclsname);

   CTA_String_Set(sclsname,(char *) clsname);

   retval=CTA_Handle_Find(sclsname, CTA_MODELCLASS, &hmodcl_old);

   CTA_String_Free(&sclsname);

   if (retval==CTA_OK) {
      /* duplicate model class */
      CTAI_Model_DuplicateClass(hmodcl_old, &hmodcl);

      /* Set implements and any parallel information set for this model? */
      CTAI_ModelFac_SetParallelData(hmodcl, (char *) implements, (char *) parallel_type, (char *) spawn_workers, (char *) nproc, (char *) ntimes, (char *) dumProcs);

      /* model-class is known */
      if (IDEBUG>0) printf("CTAI_XML_CreateModelClass: found modelclass '%s'\n",clsname);
      if (IDEBUG>0) printf("CTAI_XML_CreateModelClass: duplication is created \n");

   } else {
      /* Set all function handles to CTA_NULL */
      for (i=0;i<CTA_MODEL_NUMFUNC;i++){
         h_func[i]=CTA_NULL;
      }

      /* Load all functions that are specified in input */
      for (func_node = cur_node->children; func_node;
              func_node = func_node->next) {
         if (0 == strcmp("CTA_FUNCTION", (char *) func_node->name)){
            hfunc=CTAI_XML_CreateFunc(func_node);
            if (hfunc!=CTA_NULL){
               name = CTAI_Handle_GetName(hfunc);
               if (0 == strcmp("CTA_MODEL_CREATE_SIZE", name)){
                  h_func[I_CTA_MODEL_CREATE_SIZE    ]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_CREATE_INIT", name)){
                  h_func[I_CTA_MODEL_CREATE_INIT    ]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_FREE", name)){
                  h_func[I_CTA_MODEL_FREE           ]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_COMPUTE", name)){
                  h_func[I_CTA_MODEL_COMPUTE        ]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_SET_STATE", name)){
                  h_func[I_CTA_MODEL_SET_STATE      ]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_GET_STATE", name)){
                  h_func[I_CTA_MODEL_GET_STATE      ]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_AXPY_STATE", name)){
                  h_func[CTA_MODEL_AXPY_STATE     ]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_AXPY_MODEL", name)){
                  h_func[CTA_MODEL_AXPY_MODEL     ]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_SET_FORC", name)){
                  h_func[CTA_MODEL_SET_FORC       ]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_GET_FORC", name)){
                  h_func[CTA_MODEL_GET_FORC       ]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_AXPY_FORC", name)){
                  h_func[CTA_MODEL_AXPY_FORC      ]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_SET_PARAM", name)){
                  h_func[CTA_MODEL_SET_PARAM      ]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_GET_PARAM", name)){
                 h_func[CTA_MODEL_GET_PARAM      ]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_AXPY_PARAM", name)){
                 h_func[CTA_MODEL_AXPY_PARAM     ]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_GET_NOISE_COUNT", name)){
                 h_func[CTA_MODEL_GET_NOISE_COUNT]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_GET_NOISE_COVAR", name)){
                 h_func[CTA_MODEL_GET_NOISE_COVAR]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_GET_OBSVALUES", name)){
                 h_func[CTA_MODEL_GET_OBSVALUES  ]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_ANNOUNCE_OBSVALUES", name)){
                 h_func[CTA_MODEL_ANNOUNCE_OBSVALUES  ]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_GET_OBSSELECT", name)){
                 h_func[CTA_MODEL_GET_OBSSELECT  ]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_ADD_NOISE", name)){
                 h_func[CTA_MODEL_ADD_NOISE      ]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_IMPORT", name)){
                 h_func[I_CTA_MODEL_IMPORT      ]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_EXPORT", name)){
                 h_func[I_CTA_MODEL_EXPORT      ]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_GET_STATESCALING", name)){
                 h_func[CTA_MODEL_GET_STATESCALING]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_GETTIMEHORIZON", name)){
                 h_func[CTA_MODEL_GET_TIMEHORIZON]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_GETCURRENTTIME", name)){
                 h_func[CTA_MODEL_GET_CURRENTTIME]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_GETOBSLOCALIZATION", name)){
                 h_func[I_CTA_MODEL_GETOBSLOCALIZATION]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_SAVEINTERNALSTATE", name)){
                 h_func[CTA_MODEL_SAVE_INTERNALSTATE]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_RESTOREINTERNALSTATE", name)){
                 h_func[CTA_MODEL_RESTORE_INTERNALSTATE]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_RELEASEINTERNALSTATE", name)){
                 h_func[CTA_MODEL_RELEASE_INTERNALSTATE]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_SAVEPERSISTENTSTATE", name)){
                 h_func[CTA_MODEL_SAVE_PERSISTENTSTATE]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_LOADPERSISTENTSTATE", name)){
                 h_func[CTA_MODEL_LOAD_PERSISTENTSTATE]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_GET_NUMDOMAINS", name)){
                 h_func[CTA_MODEL_GET_NUMDOMAINS]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_GET_OBSSELECTOR", name)){
                 h_func[CTA_MODEL_GET_OBSSELECTOR]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_GET_OBSLOCALIZATIONDOMAIN", name)){
                 h_func[CTA_MODEL_GET_OBSLOCALIZATIONDOMAIN]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_GET_STATEDOMAIN", name)){
                 h_func[CTA_MODEL_GET_STATEDOMAIN]=hfunc;
               } else if (0 == strcmp("CTA_MODEL_AXPY_STATEDOMAIN", name)){
                 h_func[CTA_MODEL_AXPY_STATEDOMAIN]=hfunc;
               } else {
                  printf("CTAI_XML_CreateModelClass :Warning found unknown node %s\n", name);
               }
            }
         }
      }
      /* Create new function class */
      retval=CTA_Model_DefineClass2((char *) clsname, (char *) implements, h_func, &hmodcl);
      /* Set barrier information */
      CTAI_ModelFac_SetBarrierData(hmodcl, (char *) flag_barrier, (char *) t_step);
      /* Set parallel info */
      CTAI_ModelFac_SetParallelData(hmodcl, (char *) implements, (char *) parallel_type, (char *) spawn_workers, (char *) nproc, (char *) ntimes, (char *) dumProcs);

      /* Set id (=name) of handle */
      CTAI_Handle_SetName(hmodcl, (char *) id);

   }
   CTAI_Handle_SetName(hmodcl, (char *) id);
   xmlFree(id);
   xmlFree(clsname);
   xmlFree(implements);
   xmlFree(parallel_type);
   xmlFree(spawn_workers);
   xmlFree(nproc);
   xmlFree(ntimes);
   xmlFree(dumProcs);
   xmlFree(flag_barrier);
   xmlFree(t_step);

   if (IDEBUG>0) printf("CTAI_XML_CreateModelClass: End of function\n");
   return hmodcl;
}



