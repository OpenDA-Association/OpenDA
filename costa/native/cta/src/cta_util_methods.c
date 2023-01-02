/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_util_methods.c $
$Revision: 2751 $, $Date: 2011-09-09 08:58:46 +0200 (Fri, 09 Sep 2011) $

COSTA: Problem solving environment for data assimilation
Copyright (C) 2007  Nils van Velzen

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
#include "cta_util_methods.h"
#include "cta_message.h"
#include "f_cta_utils.h"

#define CTA_UTIL_METHODSPRINTOBSERVATIONS_F77  F77_CALL(cta_util_methodsprintobservations,CTA_UTIL_METHODSPRINTOBSERVATIONS)
#define CTA_UTIL_METHODSSELECTOBSERVATIONS_F77 F77_CALL(cta_util_methodsselectobservations,CTA_UTIL_METHODSSELECTOBSERVATIONS)
#define CTA_UTIL_METHODSOPENRESULTFILE_F77 F77_CALL(cta_util_methodsopenresultfile,CTA_UTIL_METHODSOPENRESULTFILE)

#define IDEBUG (0)

#define CLASSNAME "CTA_Util_Methods"

/** \brief Internal routine prints the predicted values and the observed values
 *
 * \param fgModel   :predicted values of foreground run
 * \param bgModel   :predicted values of background run
 * \param
 *                   if set to CTA_NULL NaN will be printed as result
 * \param vObs        :Observed values
 * \param time        :Corresponding time
 * \param file        :Output file
 *
 * \return error status: CTA_OK if successful
 */
int CTAI_Util_MethodsPrintObservations(CTA_Vector fgModel, CTA_Vector bgModel,
      CTA_Vector vObs, CTA_Vector stationNames, CTA_Time time,
      CTA_File file){

   int iObs, nObs;
   int ierr;
   CTA_String vStationNames;
   double vFgModel, vBgModel, vObservations, vTime;
   double t1, t2;
   char outStr[1024];

   ierr=CTA_String_Create(&vStationNames);

   /* Get the number of observations */
   ierr=CTA_Vector_GetSize(fgModel, &nObs);
   if (ierr!=CTA_OK) return ierr;

   /* Get time Note we will print end-time of span */
   ierr=CTA_Time_GetSpan(time, &t1, &t2);
   if (ierr!=CTA_OK) return ierr;
   vTime=t2;

   for (iObs=1;iObs<=nObs;iObs++){
      /* Get values */
      ierr=CTA_Vector_GetVal(fgModel,      iObs, &vFgModel,      CTA_DOUBLE);
      if (ierr!=CTA_OK) return ierr;
      ierr=CTA_Vector_GetVal(bgModel,      iObs, &vBgModel,      CTA_DOUBLE);
      if (ierr!=CTA_OK) return ierr;
      ierr=CTA_Vector_GetVal(vObs,         iObs, &vObservations, CTA_DOUBLE);
      if (ierr!=CTA_OK) return ierr;
      ierr=CTA_Vector_GetVal(stationNames, iObs, &vStationNames, CTA_STRING);
      if (ierr!=CTA_OK) return ierr;
      /* Write Line of output */

      sprintf(outStr,"%18.8le,  %18s, %18.8le,  %18.8le,  %18.8le ", vTime,
              CTAI_String_GetPtr(vStationNames),  vFgModel, vBgModel,
              vObservations);
       ierr=CTA_File_WriteStr(file, outStr, CTA_TRUE);
       if (ierr!=CTA_OK) return ierr;
   }

   /* Free Local work variables */
   ierr=CTA_String_Free(&vStationNames);
   if (ierr!=CTA_OK) return ierr;

   return CTA_OK;
}


#undef METHOD
#define METHOD "MethodsPrintObservations"
int CTA_Util_MethodsPrintObservations(CTA_Handle fgModel, CTA_Handle bgModel,
   CTA_StochObs sObs, CTA_Time time, CTA_File file, int printHeader){

   CTA_Datatype tFgModel, tBgModel;
   int ierr;
   BOOL lConvertFg, lConvertBg;
   int nobs;
   CTA_ObsDescr obsDescr;
   CTA_Vector vStationNames, vObs, vFgModel, vBgModel;
   double NaN;
   char outStr[1024];

   /* print header */
   if (printHeader) {
      sprintf(outStr,"%18s,  %18s, %18s,  %18s,  %18s ", "Time", "Station Name", "Fg-prediction", "Bg-prediction", "Observed");
      ierr=CTA_File_WriteStr(file, outStr, CTA_TRUE);
      if (ierr!=CTA_OK) {
         CTA_WRITE_ERROR("cannot write struct to file");
         return ierr;
      }
   }

   /* quick return when sobs is CTA_NULL */
   if (sObs==CTA_NULL) return CTA_OK;

   /* Set value NaN */
   NaN = 0; NaN = 1.0/NaN; NaN=NaN/NaN;

   /* Get the types of the various handles */
   ierr=CTA_Handle_GetDatatype(fgModel, &tFgModel);
   if (ierr!=CTA_OK) {
       CTA_WRITE_ERROR("Cannot get datatype from fgModel");
       return ierr;
   }
   ierr=CTA_Handle_GetDatatype(bgModel, &tBgModel);
   if (ierr!=CTA_OK) {
       CTA_WRITE_ERROR("Cannot get datatype from bgModlel");
       return ierr;
   }

   /* set number of observations (nobs) */
   nobs=0;
   ierr=CTA_SObs_Count(sObs,&nobs);
   if (ierr!=CTA_OK) {
       CTA_WRITE_ERROR("Cannot set number of observations");
       return ierr;
   }
   if (nobs==0) return CTA_OK;

   /* Do we need to convert handles */
   if (fgModel==CTA_NULL || tFgModel==CTA_MODEL){
      lConvertFg=TRUE;
   } else if (tFgModel==CTA_VECTOR) {
      lConvertFg=FALSE;
   } else {
      printf("Error in CTA_Util_MethodsPrintObservations:\n");
      printf("  illegal input argument fgModel \n");
      return CTA_ILLEGAL_HANDLE;
   }
    if (bgModel==CTA_NULL || tBgModel==CTA_MODEL){
      lConvertBg=TRUE;
   } else if (tBgModel==CTA_VECTOR) {
      lConvertBg=FALSE;
   } else {
      printf("Error in CTA_Util_MethodsPrintObservations:\n");
      printf("  illegal input argument bgModel \n");
      return CTA_ILLEGAL_HANDLE;

   }

   /* Get the observation description */
   ierr=CTA_SObs_GetDescription(sObs, &obsDescr);
   if (ierr!=CTA_OK) {
       CTA_WRITE_ERROR("CAnnot get observation description");
       return ierr;
   }

   /* Create and set work arrays */
   /* - Station names */
   ierr=CTA_Vector_Create(CTA_DEFAULT_VECTOR,nobs,CTA_STRING,CTA_NULL,
                         &vStationNames);
   if (ierr!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return ierr;
   }
   ierr=CTA_ObsDescr_Get_ValueProperties(obsDescr, "NAME", vStationNames, CTA_STRING);
   if (ierr!=CTA_OK) {
       CTA_WRITE_ERROR("Error using CTA_ObsDescr_Get_ValueProperties");
       return ierr;
   }

   /*- observed values */
   ierr=CTA_Vector_Create(CTA_DEFAULT_VECTOR,nobs,CTA_DOUBLE,CTA_NULL,
                         &vObs);
   if (ierr!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return ierr;
   }
   ierr=CTA_SObs_GetVal (sObs, vObs);
   if (ierr!=CTA_OK) {
       CTA_WRITE_ERROR("Cannot get values form observation");
       return ierr;
   }

   /*- predicted values: */
   /* Foreground run */
   if (lConvertFg) {
      ierr=CTA_Vector_Create(CTA_DEFAULT_VECTOR,nobs,CTA_DOUBLE,CTA_NULL,
                            &vFgModel);
      if (ierr!=CTA_OK) {
         CTA_WRITE_ERROR("Cannot create vector");
         return ierr;
      }
      if (fgModel==CTA_NULL) {
         ierr=CTA_Vector_SetConstant(vFgModel,&NaN,CTA_DOUBLE);
         if (ierr!=CTA_OK) {
            CTA_WRITE_ERROR("Cannot set constant in vector");
            return ierr;
         }
      } else {
         ierr=CTA_Model_GetObsValues (fgModel, time, obsDescr, vFgModel);
         if (ierr!=CTA_OK) {
            CTA_WRITE_ERROR("Cannot get observed values from fgModel");
            return ierr;
         }
      }
   } else {
      vFgModel     = fgModel;
   }

   /* Background run */
   if (lConvertBg) {
      ierr=CTA_Vector_Create(CTA_DEFAULT_VECTOR,nobs,CTA_DOUBLE,CTA_NULL,
                            &vBgModel);
      if (ierr!=CTA_OK) {
         CTA_WRITE_ERROR("Cannot create an vector");
         return ierr;
      }
      if (bgModel==CTA_NULL) {
         ierr=CTA_Vector_SetConstant(vBgModel,&NaN,CTA_DOUBLE);
         if (ierr!=CTA_OK) {
            CTA_WRITE_ERROR("Cannot set constants in vector");
            return ierr;
         }
      } else {
         ierr=CTA_Model_GetObsValues (bgModel, time, obsDescr, vBgModel);
         if (ierr!=CTA_OK) {
            CTA_WRITE_ERROR("Cannot get observations from bgModel");
            return ierr;
         }
      }
   } else {
      vBgModel     = bgModel;
   }

   /* print output */
   ierr=CTAI_Util_MethodsPrintObservations(vFgModel, vBgModel, vObs,
                           vStationNames,  time, file);
   if (ierr!=CTA_OK) {
      CTA_WRITE_ERROR("Error using CTAI_Util_MethodsPrintObservations");
      return ierr;
   }

   /* Free work arrays */
   if (lConvertFg) {
      CTA_Vector_Free(&vFgModel);
   }
   if (lConvertBg) {
      CTA_Vector_Free(&vBgModel);
   }
   CTA_Vector_Free(&vStationNames);
   CTA_Vector_Free(&vObs);

   CTA_ObsDescr_Free(&obsDescr);

   return CTA_OK;

}

#undef METHOD
#define METHOD "Util_MethodsSelectObservations"
int CTA_Util_MethodsSelectObservations(CTA_Model model, CTA_StochObs sObsAll, CTA_Time spanSim, CTA_StochObs *sObsSel){

CTA_StochObs sObsSpanSim;
CTA_ObsDescr obsDescrSpanSim;
CTA_String modSelect;
int ierr;

   *sObsSel=CTA_NULL;

   if (sObsAll == CTA_NULL) {
     *sObsSel = CTA_NULL;
     return CTA_OK;
   }

   if (IDEBUG>0) {printf("CTA_Util_MethodsSelectObservations START");}

/* Select all observations that fit in the simulation time span */
   ierr=CTA_SObs_CreateTimSel(sObsAll,spanSim,&sObsSpanSim);
   if (ierr!=CTA_OK) {
      char message[1024];
      sprintf(message, "Error %d selecting observations for simulation timespan",
      ierr);
	  CTA_WRITE_ERROR(message);
      return ierr;
   }

   ierr=CTA_SObs_GetDescription(sObsSpanSim,&obsDescrSpanSim);
   if (ierr!=CTA_OK) {
      char message[1024];
      sprintf(message, "Error %d in CTA_SObs_GetDescription", ierr);
	  CTA_WRITE_ERROR(message);
      return ierr;
   }
   ierr=CTA_String_Create(&modSelect);

   if (IDEBUG>0) {printf("CTA_Util_MethodsSelectObservations 2 \n");}

   ierr=CTA_Model_GetObsSelect(model,spanSim,obsDescrSpanSim,modSelect);

   if (IDEBUG>0) {printf("CTA_Util_MethodsSelectObservations 3 \n");}

   if (ierr!=CTA_OK) {
      char message[1024];
      sprintf(message, "Error %d in CTA_Model_GetObsSelect", ierr);
	  CTA_WRITE_ERROR(message);
      return ierr;
   }


   ierr=CTA_SObs_CreateSel(sObsSpanSim,modSelect,sObsSel);
   if (ierr!=CTA_OK) {
      char message[1024];
      sprintf(message, "Error %d in CTA_SObs_CreateSel", ierr);
	  CTA_WRITE_ERROR(message);
      return ierr;
   }

   if (IDEBUG>0) {printf("CTA_Util_MethodsSelectObservations 4 \n");}

   CTA_String_Free(&modSelect);
   CTA_ObsDescr_Free(&obsDescrSpanSim);
   CTA_SObs_Free(&sObsSpanSim);

   return CTA_OK;
}


#undef METHOD
#define METHOD "Util_MethodsOpenResultFile"
int CTA_Util_MethodsOpenResultFile(char *stationFile,
                                   CTA_File *fStationFile){

   CTA_String sfile;
   int ierr;

   CTA_String_Create(&sfile);
   CTA_String_Set(sfile,stationFile);
   CTA_File_Create(fStationFile);
   ierr=CTA_File_Open(*fStationFile ,sfile,CTA_NULL);
   CTA_String_Free(&sfile);
   if (ierr!=CTA_OK) {
       CTA_WRITE_ERROR("Cannot free string");
       return ierr;
   }
   ierr=CTA_Util_MethodsPrintObservations(CTA_NULL, CTA_NULL, CTA_NULL,
                                     CTA_NULL, *fStationFile, CTA_TRUE);
   return ierr;
}


//int CTA_Util_RankReduction(CTA_State scaling, int *rankIn, int *rankOut, CTA_State *QmatL){
//
//  int icol, ierr;
//  CTA_Datatype datatype;
//  double *LTL;
//
//   /* Check Handles */
//   for (icol=0;icol<rankIn;icol++){
//      ierr=CTA_Handle_Check(QmatL[icol], &datatype);
//      if (ierr!=CTA_OK) return ierr;
//   }
//
//  ierr=CTA_TreeVector_GetSize (QmatL[0], &nState);
//  if (ierr!=CTA_OK) return ierr;
//
//  /* Copy states into work array */
//    for (icol=0;icol<rankIn;icol++){
//       ierr=CTA_TreeVector_GetVals (QmatL[icol], L[indx], nState, CTA_DOUBLE);
//       if (ierr!=CTA_OK) return ierr;
//   }
//
//
//
//
//
//
//
//
//
//
//   /* allocate work array */
//   LTL=CTA_Malloc(rankIn*rankIn*sizeof(double))
//
//   if (scaling==CTA_NULL){
//      do (j=0;j<rankIn;j++){
//         do (i=j;i<rankIn;i++)
//            indx=i+j*rankIn;
//            ierr=CTA_TreeVector_Dot  	(   	CTA_TreeVector   	 treevec1,
//		CTA_TreeVector  	treevec2,
//		double *  	dotprod
//	)
//
//
//
//      }
//   }
//
//
//
//
//
//
//
//}
//





/* Interfacing with Fortran */
CTAEXPORT void CTA_UTIL_METHODSPRINTOBSERVATIONS_F77(int *fgModel,
      int *bgModel, int *sObs, int *time, int *file, int * printHeader, int *ierr){
  *ierr=CTA_Util_MethodsPrintObservations((CTA_Handle) *fgModel,
        (CTA_Handle) *bgModel, (CTA_StochObs) *sObs, (CTA_Time) *time,
        (CTA_File) *file, *printHeader);
}

CTAEXPORT void CTA_UTIL_METHODSSELECTOBSERVATIONS_F77(int *model, int *sObsAll, int *spanSim, int *sObsSel, int *ierr){
   *ierr=CTA_Util_MethodsSelectObservations((CTA_Model) *model, (CTA_StochObs) *sObsAll, (CTA_Time) *spanSim, (CTA_StochObs*) sObsSel);
}

CTAEXPORT void CTA_UTIL_METHODSOPENRESULTFILE_F77(char *stationFile,
                                   int *fStationFile, int *ierr, int len_str){

   char  *c_file;

   c_file=CTA_Malloc((len_str+1)*sizeof(char));
   CTA_fstr2cstr(stationFile,c_file,len_str);
    *ierr=CTA_Util_MethodsOpenResultFile(c_file,
                                   (CTA_File *) fStationFile);
   free(c_file);
}

