/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_sobs.c $
$Revision: 3732 $, $Date: 2012-12-19 12:08:58 +0100 (Wed, 19 Dec 2012) $

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

#include <stdio.h>
#include <memory.h>
#include "cta_mem.h"
#include "f_cta_utils.h"
#include "cta_sobs.h"
#include "cta_errors.h"
#include "cta_handles.h"
#include "ctai.h"
#include "ctai_handles.h"
#include "cta_message.h"

#define CTA_SOBS_CREATE_F77          F77_CALL(cta_sobs_create,CTA_SOBS_CREATE)
#define CTA_SOBS_CREATESEL_F77       F77_CALL(cta_sobs_createsel,CTA_SOBS_CREATESEL)
#define CTA_SOBS_CREATETIMSEL_F77    F77_CALL(cta_sobs_createtimsel,CTA_SOBS_CREATETIMSEL)
#define CTA_SOBS_COUNT_F77           F77_CALL(cta_sobs_count,CTA_SOBS_COUNT)
#define CTA_SOBS_GETVAL_F77          F77_CALL(cta_sobs_getval,CTA_SOBS_GETVAL)
#define CTA_SOBS_GETTIMES_F77        F77_CALL(cta_sobs_gettimes,CTA_SOBS_GETTIMES)
#define CTA_SOBS_GETREALISATION_F77  F77_CALL(cta_sobs_getrealisation,CTA_SOBS_GETREALISATION)
#define CTA_SOBS_GETEXPECTATION_F77  F77_CALL(cta_sobs_getexpectation,CTA_SOBS_GETEXPECTATION)
#define CTA_SOBS_EVALPDF_F77         F77_CALL(cta_sobs_evalpdf,CTA_SOBS_EVALPDF)
#define CTA_SOBS_GETCOVMAT_F77       F77_CALL(cta_sobs_getcovmat,CTA_SOBS_GETCOVMAT)
#define CTA_SOBS_GETVAR_F77          F77_CALL(cta_sobs_getvar,CTA_SOBS_GETVAR)
#define CTA_SOBS_GETSTD_F77          F77_CALL(cta_sobs_getstd,CTA_SOBS_GETSTD)
#define CTA_SOBS_GETDESCRIPTION_F77  F77_CALL(cta_sobs_getdescription,CTA_SOBS_GETDESCRIPTION)
#define CTA_SOBS_EXPORT_F77          F77_CALL(cta_sobs_export,CTA_SOBS_EXPORT)
#define CTA_SOBS_FREE_F77            F77_CALL(cta_sobs_free,CTA_SOBS_FREE)

#define IDEBUG (0)

#define CLASSNAME "CTA_Sobs"
/* Struct holding all data associated to an COSTA Vector */
typedef struct {
CTA_Func functions[CTA_SOBS_NUMFUNC];
CTA_ObsDescrClass descrcl;
} CTAI_SObsClass;                    // A SObsClass contains a list of the member-functions


typedef struct {
CTA_Func functions[CTA_SOBS_NUMFUNC]; // See cta_sobs.h for a list of
                        //               available stochobs-functions
CTA_SObsClass hsobscl;  // StochObs-class
void *data;             // pointer to the implementation-specific data.
CTA_ObsDescr hdescr;    // The observation-descriptor of this observer
CTA_Handle *userdata;   // Copy of the userdata handles
} CTAI_SObs;





int CTAI_SObs_member_function(
        // INPUTS
        CTA_StochObs hsobs,    /* Handle of the stochastic observer of
                                  which a member function is wanted */
        int member,            /* Code of the member function */
        // OUTPUT
        CTAI_SObs **sobs,      /* All data of observer hsobs  */
        CTA_Function **memfun  /* Member-Function pointer */
)
{

   int retval;
   /* Check that the given handle is indeed an observer */
   retval=CTA_Handle_Check((CTA_Handle) hsobs, CTA_SOBS);
   if (retval!=CTA_OK) return retval;

   /* Get pointer to struct with observer data */
   retval=CTA_Handle_GetData((CTA_Handle) hsobs,(void**) sobs);
   if (retval!=CTA_OK) return retval;

   /* Get pointer to implementation of this function */
   retval=CTA_Func_GetFunc((*sobs)->functions[member],memfun);
   return retval;
}


#undef METHOD
#define METHOD "Create"
int CTA_SObs_Create(
    // INPUTS:
        CTA_SObsClass hsobscl, // stochastic observer class
        CTA_Handle userdata,  // e.g. the name of the database
    // OUTPUTS:
        CTA_StochObs *hsobs)   // The new COSTA-stochastic observer
                                   // (handle)
{
   CTAI_SObs *sobs;
   int memsize;
   int retval;
   CTAI_SObsClass *clsdata;
   CTA_Function *my_Create_Size, *my_Create_Init;
   int i;
   char message[256];

   /* Get class data containing all function pointers */
   retval=CTA_Handle_Check((CTA_Handle) hsobscl,CTA_SOBSCLASS);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_sobsclass handle");
       return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) hsobscl,(void**) &clsdata);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Cannot retrieve handle data");
      return retval;
   }

   /* determine size of data object (CTA_SOBS_CREATE_SIZE)*/
   printf("CTA_SOBS: get CREATE_SIZE \n");
   retval=CTA_Func_GetFunc(clsdata->functions[CTA_SOBS_CREATE_SIZE],
                           &my_Create_Size);
   printf("CTA_SOBS: done calling CREATE_SIZE %p\n",my_Create_Size);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Cannot get function CTA_SOB_CREATE_SIZE");
      return retval;
   }
   printf("Calling my_create_size\n");
   my_Create_Size(&memsize,&retval);
   printf("DOne Calling my_create_size\n");
   if (retval) {
      CTA_WRITE_ERROR("Error in my_Create_Size");
      return retval;
   }

   /* allocate memory for new stochobs object */
   sobs=CTA_Malloc(sizeof(CTAI_SObs));
   sobs->data=CTA_Malloc(memsize);
   sobs->userdata = NULL;


   /* copy function pointers */
   for (i=0;i<CTA_SOBS_NUMFUNC;i++){
      printf("Member function[%d]=%d\n",i,clsdata->functions[i]);
      sobs->functions[i]=clsdata->functions[i];
   }

   /* set other general information */
   sobs->hsobscl = hsobscl;
   sobs->hdescr  = CTA_NULL;

   /* Initialise and fill new stochobs */
   retval=CTA_Func_GetFunc(clsdata->functions[CTA_SOBS_CREATE_INIT],
                           &my_Create_Init);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Cannot get function CTA_SOBS_CREATE_INIT");
      return retval;
   }
   my_Create_Init(sobs->data, userdata, &retval);
   if (retval!=CTA_OK) {
      sprintf(message,"my_Create_Init returned with (COSTA) error code %d",retval);
      CTA_WRITE_ERROR(message);
      return retval;
   }

   /* Allocate new handle and return eror when unsuccesfull */
   retval=CTA_Handle_Create("stochobs",CTA_SOBS,sobs,hsobs);
   if (retval) {
      CTA_WRITE_ERROR("Cannot create handle");
      return retval;
   }

   return CTA_OK;
}

#undef METHOD
#define METHOD "CreateTimSel"
int CTA_SObs_CreateTimSel(
   CTA_StochObs hsobsin,   /* Handle of the stochastic observer of
                                    which  a selection is to be made*/
   CTA_Time timespan,      /* Timespan over which selection has te be made*/
   CTA_StochObs *hsobsout) /* The new COSTA-stochastic observer
                                    (handle) */
{
   int ierr;
   char str[80];
   double t1,t2;
   double eps;

   CTA_String sselect;

   if (IDEBUG>0) { printf("DEBUG: CTA_SObs_CreateTimSel: START \n");}

   *hsobsout=CTA_NULL;
   if (hsobsin!=CTA_NULL){
      // Get interval of timespan (t1,t2)
      ierr=CTA_Time_GetSpan(timespan,&t1,&t2);
     if (ierr!=CTA_OK) {
        char message[1024];
        sprintf(message,"Cannot retrieve interval of timespan (%g,%g)",t1,t2);
       CTA_WRITE_ERROR(message);
        return ierr;
      }
      // Create the Selection string (SQL-statement)
      ierr=CTA_String_Create(&sselect);
     if (ierr!=CTA_OK) {
        CTA_WRITE_ERROR("Cannot create string");
       return ierr;
      }

      eps=(t2-t1)*1.0e-4+1.0e-16;
      t1=t1+eps;
      t2=t2+eps;
      sprintf(str, "time BETWEEN %f AND %f",t1, t2 );
      ierr=CTA_String_Set(sselect,str);
     if (ierr!=CTA_OK) {
        CTA_WRITE_ERROR("Cannot set string");
       return ierr;
      }

     // Create the selcection
      ierr=CTA_SObs_CreateSel(hsobsin, sselect, hsobsout);
      if (ierr!=CTA_OK) {
        CTA_WRITE_ERROR("Error in CreateSel");
       return ierr;
      }

      // Free work variables
      ierr=CTA_String_Free(&sselect);
     if (ierr!=CTA_OK)  {
        CTA_WRITE_ERROR("Cannot free string");
       return ierr;
      }
   }
   return CTA_OK;
};

#undef METHOD
#define METHOD "CreateSel"
int CTA_SObs_CreateSel(
   // INPUTS:
   CTA_StochObs hsobsin,   /* Handle of the stochastic observer of
                                    which  a selection is to be made*/
   CTA_Handle userdata,   /* Inputs necessary for making a selection */
   // OUTPUTS:
   CTA_StochObs *hsobsout) /* The new COSTA-stochastic observer
                                    (handle) */
{
   /* Local variables */
   int retval;                 /* Return value of COSTA call    */
   CTAI_SObs* sobsin;          /* All data of observer hsobsin  */
   CTAI_SObs* sobsout;         /* All data of observer hsobsout */
   CTA_Function *memfun;       /* Function that must be called  */
   int memsize,i;

   if (hsobsin==CTA_NULL){
      *hsobsout=CTA_NULL;
      return CTA_OK;
   }
   /* Look up the Create-size function and the data of the input observer*/
   retval = CTAI_SObs_member_function(hsobsin, CTA_SOBS_CREATE_SIZE,
                                      &sobsin, &memfun);
   if (retval) {
       CTA_WRITE_ERROR("Error while looking up the Create-size function and the data of the input observer");
       return retval;
   }

   /* Determine the necessary memory size and allocate memory */
   memfun(&memsize,&retval);
   if (retval) {
       CTA_WRITE_ERROR("Error while determining the necessary memory size and allocate memory");
       return retval;
   }

   sobsout=CTA_Malloc(sizeof(CTAI_SObs));
   sobsout->data=CTA_Malloc(memsize);
   sobsout->userdata=NULL;


   /* copy function pointers and observation class handle */
   sobsout->hsobscl=sobsin->hsobscl;
   for (i=0;i<CTA_SOBS_NUMFUNC;i++)
      { sobsout->functions[i]=sobsin->functions[i]; }

   /* Look up member function and observer data */
   retval = CTA_Func_GetFunc(sobsout->functions[CTA_SOBS_CREATE_SELECTION],
                             &memfun);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Error while loking up member functions and observer data");
       return retval;
   }

   /* Call (user) implementation */
   //   printf("memfun, sobsout->functions[i],  %d %d\n",&memfun,sobsout->functions[CTA_SOBS_CREATE_SELECTION]);
   //   printf("cta_sobs_createsel: calling member function with userdata %d \n",userdata);
   memfun(sobsin->data,&userdata,sobsout->data,&retval);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Error while calling (user) implementation");
       return retval;
   }

   /* Allocate new handle and return error when unsuccesfull */
   retval=CTA_Handle_Create("stochobs",CTA_SOBS,sobsout,hsobsout);
   if (IDEBUG>0) printf("end of cta_sobs_createsel: retval %d  handle %d \n",retval,*hsobsout);

   return retval;
};



#undef METHOD
#define METHOD "Count"
int CTA_SObs_Count(
   // INPUTS:
   CTA_StochObs hsobs,   /* Handle of the stochastic observer */
   // OUTPUTS:
   int *nmeasr           /* number of measurements in this observer */
   )
{
   /* Local variables */
   int retval;                 /* Return value of COSTA call   */
   CTAI_SObs *sobs;            /* All data of observer hsobs   */
   CTA_Function *memfun;       /* Function that must be called */

   /* trivial return we accept CTA_NULL -> nmeasr=0 */
   if (hsobs==CTA_NULL){
      *nmeasr=0;
      return CTA_OK;
   }
   /* Look up member function and sobserver data */
   retval = CTAI_SObs_member_function(hsobs, I_CTA_SOBS_COUNT,
                                      &sobs, &memfun);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Error while looking up member function and sobserver data");
       return retval;
   }

   /* Call (user) implementation */
   memfun(sobs->data,nmeasr,&retval);
   return retval;
};

#undef METHOD
#define METHOD "GetvVl"
int CTA_SObs_GetVal(
   // INPUTS:
   CTA_StochObs hsobs,   /* Handle of the stochastic observer */
   // OUTPUTS:
   CTA_Vector hvec       /* measurements */
   )
{
   /* Local variables */
   int retval;                 /* Return value of COSTA call   */
   CTAI_SObs *sobs;            /* All data of observer hsobs   */
   CTA_Function *memfun;       /* Function that must be called */

   /* Look up member function and sobserver data */
   retval = CTAI_SObs_member_function(hsobs, CTA_SOBS_GET_VALUES,
                                      &sobs, &memfun);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Error while looking up member function and sobserver data");
       return retval;
   }

   /* Call (user) implementation */
   memfun(sobs->data,&hvec,&retval);
   return retval;
};

#undef METHOD
#define METHOD "GetTimes"
int CTA_SObs_GetTimes(
   // INPUTS:
   CTA_StochObs hsobs,   /* Handle of the stochastic observer */
   // OUTPUTS:
   CTA_Vector hvec       /* time instances associated to measurements */
   )
{
   /* Local variables */
   int retval;                 /* Return value of COSTA call   */
   CTAI_SObs *sobs;            /* All data of observer hsobs   */
   CTA_Function *memfun;       /* Function that must be called */

   /* Look up member function and sobserver data */
   retval = CTAI_SObs_member_function(hsobs, CTA_SOBS_GET_TIMES,
                                      &sobs, &memfun);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Error while looking up member function and sobserver data");
       return retval;
   }

   /* Call (user) implementation */
   memfun(sobs->data,&hvec,&retval);
   return retval;
};


#undef METHOD
#define METHOD "GetRealisation"
int CTA_SObs_GetRealisation(
   // INPUTS:
   CTA_StochObs hsobs,   /* Handle of the stochastic observer */
   // OUTPUTS:
   CTA_Vector hvec       /* measurements */
   )
{
   /* Local variables */
   int retval;                 /* Return value of COSTA call   */
   CTAI_SObs *sobs;            /* All data of observer hsobs   */
   CTA_Function *memfun;       /* Function that must be called */

   /* Look up member function and sobserver data */
   retval = CTAI_SObs_member_function(hsobs, CTA_SOBS_GET_REALISATION,
                                      &sobs, &memfun);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Error while looking up member function and sobserver data");
       return retval;
   }

   /* Call (user) implementation */
   memfun(sobs->data,&hvec,&retval);
   return retval;
};




#undef METHOD
#define METHOD "GetExpectation"
int CTA_SObs_GetExpectation(
   // INPUTS:
   CTA_StochObs hsobs,   /* Handle of the stochastic observer */
   // OUTPUTS:
   CTA_Vector hvec       /* measurements */
   )
{
   /* Local variables */
   int retval;                 /* Return value of COSTA call   */
   CTAI_SObs *sobs;            /* All data of observer hsobs   */
   CTA_Function *memfun;       /* Function that must be called */

   /* Look up member function and sobserver data */
   retval = CTAI_SObs_member_function( hsobs, CTA_SOBS_GET_EXPECTATION,
                                       &sobs, &memfun);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Error while looking up member function and sobserver data");
       return retval;
   }

   /* Call (user) implementation */
   memfun(sobs->data,&hvec,&retval);
   return retval;
};



#undef METHOD
#define METHOD "EvalPDF"
int CTA_SObs_EvalPDF(
/* Ik vind dit nogal een vage functie. Ik weet niet wat een mens
   hiermee moet gaan doen. In sommige gevallen kan ook geen eerlijk
   antwoord worden gegeven (Dirac-achtige dingen en zo) */

   // INPUTS:
   CTA_StochObs hsobs,   /* Handle of the stochastic observer */
   CTA_Vector hvecx,     /* location for evaluating PDF */
   // OUTPUTS:
   CTA_Vector hvecy      /* PDF-value */
   )
{
   /* Local variables */
   int retval;                 /* Return value of COSTA call   */
   CTAI_SObs *sobs;            /* All data of observer hsobs   */
   CTA_Function *memfun;       /* Function that must be called */

   /* Look up member function and sobserver data */
   retval = CTAI_SObs_member_function(hsobs, CTA_SOBS_EVALUATE_PDF,
                                      &sobs, &memfun);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Error while looking up member function and sobserver data");
       return retval;
   }

   /* Call (user) implementation */
   memfun(sobs->data,&hvecx,&hvecy,&retval);
   return retval;
};




#undef METHOD
#define METHOD "GetCovMat"
int CTA_SObs_GetCovMat(
   // INPUTS:
   CTA_StochObs hsobs,   /* Handle of the stochastic observer */
   // OUTPUTS:
   CTA_Matrix hmat       /* Covariance matrix */
   )
{
   /* Local variables */
   int retval;                 /* Return value of COSTA call   */
   CTAI_SObs *sobs;            /* All data of observer hsobs   */
   CTA_Function *memfun;       /* Function that must be called */

   /* Look up member function and sobserver data */
   retval = CTAI_SObs_member_function( hsobs, CTA_SOBS_GET_COV_MATRIX,
                                       &sobs, &memfun);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Error while looking up member function and sobserver data");
       return retval;
   }

   /* Call (user) implementation */
   memfun(sobs->data,&hmat,&retval);
   return retval;
};




#undef METHOD
#define METHOD "GetVar"
int CTA_SObs_GetVar(
   // INPUTS:
   CTA_StochObs hsobs,   /* Handle of the stochastic observer */
   // OUTPUTS:
   CTA_Vector hvec       /* Variances of measurements */
   )
{
   /* Local variables */
   int retval;                 /* Return value of COSTA call   */
   CTAI_SObs *sobs;            /* All data of observer hsobs   */
   CTA_Function *memfun;       /* Function that must be called */
   int varflag;

   /* Look up member function and sobserver data */
   retval = CTAI_SObs_member_function( hsobs, CTA_SOBS_GET_VARIANCE,
                                       &sobs, &memfun);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Error while looking up member function and sobserver data");
       return retval;
   }

   /* Call (user) implementation */
   varflag=CTA_TRUE;
   memfun(sobs->data,&varflag,&hvec,&retval);
   return retval;
};

#undef METHOD
#define METHOD "GetStd"
int CTA_SObs_GetStd(
   // INPUTS:
   CTA_StochObs hsobs,   /* Handle of the stochastic observer */
   // OUTPUTS:
   CTA_Vector hvec       /* Variances of measurements */
   )
{
   /* Local variables */
   int retval;                 /* Return value of COSTA call   */
   CTAI_SObs *sobs;            /* All data of observer hsobs   */
   CTA_Function *memfun;       /* Function that must be called */
   int varflag;

   /* Look up member function and sobserver data */
   retval = CTAI_SObs_member_function( hsobs, CTA_SOBS_GET_VARIANCE,
                                       &sobs, &memfun);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Error while looking up member function and sobserver data");
       return retval;
   }

   /* Call (user) implementation */
   varflag=CTA_FALSE;
   memfun(sobs->data, &varflag, &hvec, &retval);
   return retval;
};




#undef METHOD
#define METHOD "GetDescription"
int CTA_SObs_GetDescription(
    CTA_StochObs hsobs,     /* Handle of the stochastic observer */
    CTA_ObsDescr *hobsdescr  /* descriptor of the stochastic observer*/
    )
{
   int retval;
   int nmeasr;           /* number of mesurements in hsobs */
   CTAI_SObs *sobs;      /* All data of observer hsobs  */
   CTAI_SObsClass *clsdata;
   CTA_Handle hsobs_data;

   /* Check that the given handle is indeed an observer */
   retval=CTA_Handle_Check((CTA_Handle) hsobs, CTA_SOBS);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_sobs handle");
       return retval;
   }
   /* Get pointer to struct with observer data */
   retval=CTA_Handle_GetData((CTA_Handle) hsobs,(void**) &sobs);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Cannot retrieve handle data");
      return retval;
   }

   /* If stochastic observer does not contain any observations
      Return CTA_NULL                                          */
      retval=CTA_SObs_Count(hsobs, &nmeasr);
      if (nmeasr==0) {
        if (IDEBUG > 0) printf("cta_stoch_observer:CTA_SObs_GetDescription: Warning: you asked observations description of empty stochastic observer %d !\n",hsobs);
        *hobsdescr=CTA_NULL;
        return CTA_OK;
      }
      retval=CTA_Handle_GetData((CTA_Handle) sobs->hsobscl,
                                (void**) &clsdata);
      if (retval!=CTA_OK) {
        CTA_WRITE_ERROR("Cannot retrieve handle data");
        return retval;
      }

      /* Create a handle for holding pointer to data-object "sobs" */
      retval=CTA_Handle_Create("", CTA_DATABLOCK, sobs->data, &hsobs_data);
      if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Cannot create handle");
      return retval;
   }

      retval = CTA_ObsDescr_Create(clsdata->descrcl, hsobs_data,  hobsdescr);
      if (retval!=CTA_OK){
         CTA_WRITE_ERROR("Cannot create observation description");
      }

      /* Free handle (note data is not deallocated!) */
      retval=CTA_Handle_Free(&hsobs_data);
      if (retval!=CTA_OK) {
        CTA_WRITE_ERROR("Cannot free handle");
        return retval;
      }

return CTA_OK;
}

#undef METHOD
#define METHOD "Export"
int CTA_SObs_Export(
   CTA_StochObs hsobs,  /* Handle of the stochastic observer */
   CTA_Handle userdata
   )
{
   /* Local variables */
   int retval;                 /* Return value of COSTA call   */
   CTAI_SObs *sobs;            /* All data of observer hsobs   */
   CTA_Function *memfun;       /* Function that must be called */

   /* Look up member function and sobserver data */
   retval = CTAI_SObs_member_function( hsobs, I_CTA_SOBS_EXPORT,
                                       &sobs, &memfun);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Error while looking up member function and sobserver data");
      return retval;
   }

   /* Call (user) implementation */
   memfun(sobs->data,&userdata,&retval);
   return retval;
};




#undef METHOD
#define METHOD "Free"
int CTA_SObs_Free(
   CTA_StochObs *hsobs  /* Handle of stochastic observer  */
   )
{
   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_SObs *sobs;        /* All data of stochastic observer */
   CTA_Function *my_free; /* Function that must be called */
   CTA_Handle *userdata;   /* User data of function call   */


   /* Check for quick return */
   if (*hsobs==CTA_NULL) return CTA_OK;

   /* Look up member function and sobserver data */
   retval = CTAI_SObs_member_function(*hsobs, I_CTA_SOBS_FREE,
                                      &sobs, &my_free);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Error while looking up member function and sobserver data");
      return retval;
   }

   /* Call (user) implementation */
   my_free(sobs->data,&userdata,&retval);
   free(sobs->data);
   if (sobs->userdata) free(sobs->userdata);
   free(sobs);
   retval=CTA_Handle_Free(hsobs);

   return retval;
};


CTA_Handle CTAI_SObs_GetUserData(CTA_StochObs hsobs, int index) {
//   CTAI_SObs *sobs;        /* All data of stochastic observer */

//   sobs = (CTAI_SObs *)CTAI_Handle_GetData(hsobs);
   return CTA_NULL;
}

/* Interfacing with Fortran */

CTAEXPORT void CTA_SOBS_CREATE_F77(int *hstochobscl, int *userdata,
                         int *hstochobs, int *ierr){

  *ierr=CTA_SObs_Create((CTA_SObsClass) *hstochobscl,
     (CTA_Handle) *userdata, (CTA_StochObs *) hstochobs);
}



CTAEXPORT void CTA_SOBS_CREATESEL_F77(int *hsobsin, int *userdata,
                            int *hsobsout, int *ierr){

   *ierr=CTA_SObs_CreateSel((CTA_StochObs) *hsobsin,
                            (CTA_Handle) *userdata,
                            (CTA_StochObs*) hsobsout);
}

CTAEXPORT void CTA_SOBS_CREATETIMSEL_F77(int *hsobsin, int *timespan,
                            int *hsobsout, int *ierr){

   *ierr=CTA_SObs_CreateTimSel((CTA_StochObs) *hsobsin,
                            (CTA_Time) *timespan,
                            (CTA_StochObs*) hsobsout);
}



CTAEXPORT void CTA_SOBS_COUNT_F77(int *hsobs, int *nmeasr, int *ierr){

   *ierr=CTA_SObs_Count((CTA_StochObs) *hsobs, nmeasr);
}


CTAEXPORT void CTA_SOBS_GETVAL_F77(int *hsobs, int *hvec, int *ierr){
   *ierr=CTA_SObs_GetVal((CTA_StochObs) *hsobs,  (CTA_Vector) *hvec);
}

CTAEXPORT void CTA_SOBS_GETTIMES_F77(int *hsobs, int *hvec, int *ierr){
   *ierr=CTA_SObs_GetTimes((CTA_StochObs) *hsobs,  (CTA_Vector) *hvec);
}

CTAEXPORT void CTA_SOBS_GETREALISATION_F77(int *hsobs, int *hvec, int *ierr){
    *ierr=CTA_SObs_GetRealisation((CTA_StochObs) *hsobs,
                                   (CTA_Vector) *hvec);
}


CTAEXPORT void CTA_SOBS_GETEXPECTATION_F77(int *hsobs, int *hvec, int *ierr){
   *ierr=CTA_SObs_GetExpectation((CTA_StochObs) *hsobs,
                                 (CTA_Vector) *hvec);
}

CTAEXPORT void CTA_SOBS_EVALPDF_F77(int *hsobs, int *hvecx,  int *hvecy, int *ierr){

   *ierr=CTA_SObs_EvalPDF((CTA_StochObs) *hsobs, (CTA_Vector) *hvecx,
                           (CTA_Vector) *hvecy);
}

CTAEXPORT void CTA_SOBS_GETCOVMAT_F77(int *hsobs, int *hmat, int *ierr){
   *ierr=CTA_SObs_GetCovMat((CTA_StochObs) *hsobs, (CTA_Matrix) *hmat);
}


CTAEXPORT void CTA_SOBS_GETVAR_F77(int *hsobs, int *hvec, int *ierr){
   *ierr=CTA_SObs_GetVar((CTA_StochObs) *hsobs, (CTA_Matrix) *hvec);
}

CTAEXPORT void CTA_SOBS_GETSTD_F77(int *hsobs, int *hvec, int *ierr){
   *ierr=CTA_SObs_GetStd((CTA_StochObs) *hsobs, (CTA_Matrix) *hvec);
}

CTAEXPORT void CTA_SOBS_GETDESCRIPTION_F77(int *hsobs, int *hobsdescr, int *ierr){
   *ierr=CTA_SObs_GetDescription((CTA_StochObs) *hsobs,
                                 (CTA_ObsDescr*) hobsdescr);
}


CTAEXPORT void CTA_SOBS_EXPORT_F77(int  *hsobs, int *userdata, int *ierr){
   *ierr=CTA_SObs_Export((CTA_StochObs) *hsobs, (CTA_Handle) *userdata);
}

CTAEXPORT void  CTA_SOBS_FREE_F77( int *hsobs, int *ierr ){
   *ierr=CTA_SObs_Free( (CTA_StochObs*) hsobs);
}


