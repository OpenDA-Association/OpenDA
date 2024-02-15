/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_time.c $
$Revision: 2751 $, $Date: 2011-09-09 08:58:46 +0200 (Fri, 09 Sep 2011) $

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
#include <math.h>
#include "cta_mem.h"
#include "f_cta_utils.h"
#include "ctai.h"
#include "cta_time.h"
#include "cta_pack.h"
#include "cta_errors.h"
#include "cta_file.h"
#include "cta_message.h"

#define CTA_TIME_CREATE_F77  F77_CALL(cta_time_create,CTA_TIME_CREATE)
#define CTA_TIME_FREE_F77    F77_CALL(cta_time_free,CTA_TIME_FREE)
#define CTA_TIME_SETSPAN_F77  F77_CALL(cta_time_setspan,CTA_TIME_SETSPAN)
#define CTA_TIME_GETSPAN_F77  F77_CALL(cta_time_getspan,CTA_TIME_GETSPAN)
#define CTA_TIME_SETSTEP_F77  F77_CALL(cta_time_setstep,CTA_TIME_SETSTEP)
#define CTA_TIME_GETSTEP_F77  F77_CALL(cta_time_getstep,CTA_TIME_GETSTEP)
#define CTA_TIME_GETSTEP_F77  F77_CALL(cta_time_getstep,CTA_TIME_GETSTEP)
#define CTA_TIME_INSPAN_F77   F77_CALL(cta_time_inspan,CTA_TIME_INSPAN)
#define CTA_TIME_ISSTEP_F77   F77_CALL(cta_time_isstep,CTA_TIME_ISSTEP)
#define CTA_TIME_COPY_F77        F77_CALL(cta_time_copy,CTA_TIME_COPY)
#define CTA_TIME_COUNTSTEPS_F77  F77_CALL(cta_time_countsteps,CTA_TIME_COUNTSTEPS)
#define CTA_TIME_GETTIMESTEP_F77 F77_CALL(cta_time_gettimestep,CTA_TIME_GETTIMESTEP)
#define CTA_TIME_EXPORT_F77 F77_CALL(cta_time_export,CTA_TIME_EXPORT)
#define CTA_TIME_IMPORT_F77 F77_CALL(cta_time_import,CTA_TIME_IMPORT)
#define CTA_TIME_ISSPAN_F77 F77_CALL(cta_time_isspan,CTA_TIME_ISSPAN)


#define CLASSNAME "CTA_Time"

/* Struct holding all data associated to an COSTA time span */

typedef struct {
BOOL isspan;
double t1;
double t2;
double step;
} CTAI_Time;


#undef METHOD
#define METHOD "Create"
int CTA_Time_Create(CTA_Time *htime){

   CTAI_Time *time;
   int retval;

   /* allocate memory for new time object */
   time=CTA_Malloc(sizeof(CTAI_Time));
   time->isspan=FALSE;
   time->t1=0.0;
   time->t2=0.0;
   time->step=1.0;

   /* Allocate new handle and return error when unsuccesfull */
   retval=CTA_Handle_Create("time",CTA_TIME,time,htime);
   if (retval) {
	   CTA_WRITE_ERROR("Cannot create time handle");
	   return retval;
   }
   return CTA_OK;
}

#undef METHOD
#define METHOD "Free"
int CTA_Time_Free(CTA_Time *htime){

   CTAI_Time *time;
   int retval;

   if (*htime==CTA_NULL) return CTA_OK;

   retval=CTA_Handle_Check((CTA_Handle) *htime,CTA_TIME);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_time handle");
       return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) *htime,(void**) &time);
   if (retval!=CTA_OK) {
	   	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   free(time);
   retval=CTA_Handle_Free(htime);
   return retval;
}

#undef METHOD
#define METHOD "IsSpan"
int CTA_Time_IsSpan(CTA_Time htime, int *isspan){

   CTAI_Time *time;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) htime,CTA_TIME);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_Time handle");
	   return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) htime,(void**) &time);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }


    if (time->isspan){
       *isspan=CTA_TRUE;
    } else {
       *isspan=CTA_FALSE;
    }
   return CTA_OK;
}

#undef METHOD
#define METHOD "SetSpan"
int CTA_Time_SetSpan(CTA_Time htime,double tstart, double tend){

   CTAI_Time *time;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) htime,CTA_TIME);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_time handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) htime,(void**) &time);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   time->t1     = tstart;
   time->t2     = tend;
   time->isspan = TRUE;

   if (tstart==tend){
      time->isspan = FALSE;
   }


   return CTA_OK;
}

#undef METHOD
#define METHOD "GetSpan"
int CTA_Time_GetSpan(CTA_Time htime,double *tstart, double *tend){

   CTAI_Time *time;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) htime,CTA_TIME);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_time handle");	   
	   return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) htime,(void**) &time);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   *tstart=time->t1;
   *tend=time->t2;

   return CTA_OK;
}

#undef METHOD
#define METHOD "SetStep"
int CTA_Time_SetStep(CTA_Time htime, double tstep){

   CTAI_Time *time;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) htime,CTA_TIME);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_time handle");
       return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) htime,(void**) &time);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   time->step=tstep;
   return CTA_OK;
}

#undef METHOD
#define METHOD "CountSteps"
int CTA_Time_CountSteps(CTA_Time htime, int *nsteps){

   CTAI_Time *time;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) htime,CTA_TIME);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_time handle");
       return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) htime,(void**) &time);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   if (time->isspan) {
      *nsteps = (int) ((time->t2-time->t1)/time->step +0.5);
   } else {
      *nsteps = 0;
   }
   return CTA_OK;
}
#undef METHOD
#define METHOD "GetTimeStep"
int CTA_Time_GetTimeStep(CTA_Time htime, int istep, CTA_Time htime_step){
   CTAI_Time *time1, *time2;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) htime,CTA_TIME);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_time handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) htime,(void**) &time1);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   retval=CTA_Handle_Check((CTA_Handle) htime_step,CTA_TIME);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_time handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) htime_step,(void**) &time2);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   time2->step=time1->step;
   if (time1->isspan) {
      time2->t1=time1->t1+(double)(istep-1)*time1->step;
      time2->t2=time1->t1+(double)(istep)  *time1->step;
      if (time2->t2>time1->t2) time2->t2=time1->t2;
   } else {
      time2->step=time1->step;
      time2->t1= 0.0;
      time2->t2=-1.0;
   }
   return CTA_OK;

}

#undef METHOD
#define METHOD "GetStep"
int CTA_Time_GetStep(CTA_Time htime, double *tstep){

   CTAI_Time *time;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) htime,CTA_TIME);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_time handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) htime,(void**) &time);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   if (time->isspan) {
      *tstep = time->step;
   } else {
      *tstep = 0;
   }
   return CTA_OK;
}

#undef METHOD
#define METHOD "InSpan"
/* is timesub inside time */
int CTA_Time_InSpan(CTA_Time htimesub, CTA_Time htime,  BOOL *inspan){

   CTAI_Time *time ;
   CTAI_Time *timesub ;
   int retval;
   double tstartsub, tendsub, tstart, tend;

   retval=CTA_Handle_Check((CTA_Handle) htimesub,CTA_TIME);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_time handle");
       return retval;
   }
   retval=CTA_Handle_Check((CTA_Handle) htime,CTA_TIME);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_time handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) htime,(void**) &time);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) htimesub,(void**) &timesub);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   tstartsub = timesub->t1;
   tendsub = timesub->t2;
   tstart = time->t1;
   tend = time->t2;

   if (tstartsub>tendsub || tstart> tend){
      *inspan=FALSE;
   }
   else {
      *inspan = (tstartsub >= tstart && tendsub <= tend) ;
   }
   return CTA_OK;
}

#undef METHOD
#define METHOD "IsStep"
 /* is timesub inside time */
int CTA_Time_IsStep(CTA_Time htime, double t,  BOOL *isstep){

   CTAI_Time *time ;
   int retval, ifrac;
   double eps, frac, diff;

   retval=CTA_Handle_Check((CTA_Handle) htime,CTA_TIME);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_time handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) htime,(void**) &time);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* Don't know what is really nice just try something */
   eps=M_EPS*100.0*MAX(fabs(time->t1),fabs(time->t2))+M_EPS;

   *isstep=FALSE;
   if ((time->t1-eps)<t && t< (time->t2+eps)){
      frac=(t-time->t1)/time->step;
      ifrac=(int) (frac +0.5);
      diff=fabs(((double) ifrac) - frac);
      if ( diff<eps) {
        *isstep=TRUE;
      }
   }
   return CTA_OK;
}

#undef METHOD
#define METHOD "Copy"
int CTA_Time_Copy(CTA_Time hfrom, CTA_Time hto) {
   int retval;
   double tstart, tend, tstep;
   CTAI_Time *to;

   retval=CTA_Handle_Check((CTA_Handle) hfrom,CTA_TIME);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_time handle");
       return retval;
   }

   retval=CTA_Handle_Check((CTA_Handle) hto,CTA_TIME);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_time handle");
       return retval;
   }

   retval = CTA_Time_GetSpan(hfrom, &tstart, &tend);
   if (retval != CTA_OK) {
       CTA_WRITE_ERROR("Cannot get timespan");
       return retval;
   }

   retval = CTA_Time_GetStep(hfrom, &tstep);
   if (retval != CTA_OK) {
       CTA_WRITE_ERROR("Cannot get timespan");
       return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) hto,(void**) &to);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   to->t1     = tstart;
   to->t2     = tend;
   to->step   = tstep;
   to->isspan = TRUE;
   retval = CTA_OK;

   return retval;
     }

#undef METHOD
#define METHOD "Export"
int CTA_Time_Export(CTA_Time htime, CTA_Handle hexport){
   CTA_Datatype datatype;
   int retval;
   FILE *file;          //File pointer
   CTAI_Time *time;

   /* Check handle */
   retval=CTA_Handle_Check((CTA_Handle) htime,CTA_TIME);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_time handle");
       return retval;
   }

   /* Get data */
   retval=CTA_Handle_GetData((CTA_Handle) htime,(void**) &time);
   if (retval!=CTA_OK) return retval;

   /* Get type of handle of hexport */
   retval=CTA_Handle_GetDatatype(hexport, &datatype);
   if (retval != CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* perform the export depending on hexport */
   if (datatype==CTA_PACK){
      retval=CTA_Pack_Add(hexport,time,sizeof(CTAI_Time));
   }
   else if (datatype==CTA_FILE){
      retval=CTA_File_Get(hexport,&file);
      if (retval != CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get file");
	   return retval;
   }
      fprintf(file,"[%lg %lg %lg %d];\n", time->t1, time->t2, time->step, time->isspan);
   }
   else {
      return CTA_FORMAT_NOT_SUPPORTED;
   }
   return CTA_OK;
}

#undef METHOD
#define METHOD "Import"
int CTA_Time_Import(CTA_Time htime, CTA_Handle himport){
   CTA_Datatype datatype;
   int retval;
   CTAI_Time *time;

   /* Check handle */
   retval=CTA_Handle_Check((CTA_Handle) htime,CTA_TIME);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_time handle");
       return retval;
   }

   /* Get data */
   retval=CTA_Handle_GetData((CTA_Handle) htime,(void**) &time);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* Get type of handle of himport */
   retval=CTA_Handle_GetDatatype(himport, &datatype);
   if (retval != CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle datatype");
	   return retval;
   }

   /* perform the export depending on hexport */
   if (datatype==CTA_PACK){
      retval=CTA_Pack_Get(himport,time,sizeof(CTAI_Time));
   }
   else if (datatype==CTA_FILE){
      return CTA_FORMAT_NOT_SUPPORTED;
   }
   else {
      return CTA_FORMAT_NOT_SUPPORTED;
   }
   return CTA_OK;
}


void ctai_gregor(int ijuld, int *iy, int *im, int *id){
/* id      o   gregorian day   */
/* ijuld   i   julian day      */
/* im      o   gregorian month */
/* iy      o   gregorian year  */

  int l,n;
//    put julian day in auxiliary variable
      l  = ijuld + 68569;
      n  = 4*l / 146097;
      l  = l - ( 146097*n + 3 ) / 4;
      *iy = 4000 * ( l+1 ) / 1461001;
      l  = l - 1461 * *iy / 4 + 31;
      *im = 80 * l / 2447;

//     calculate gregorian day
      *id = l - 2447 * *im / 80;
      l  = *im / 11;

//     calculate gregorian month
      *im = *im + 2 - 12 * l;

//     calculate gregorian year
      *iy = 100 * ( n- 49 ) + *iy + l;
}

int ctai_julian ( int iyear, int imonth, int iday ){

/*     iday    i   gregorian day
     imonth  i   gregorian month
     iyear   i   gregorian year
     julian      name of this function
*/
      int ijuld, imon1;
/*
     id      gregorian day
     ijuld   julian daynumber
     im      gregorian month
     imon1   auxiliary variable
     iy      gregorian year
*/

      imon1 = (imonth-14)/12;

//    calculate julian day number

      ijuld = iday - 32075 + 1461 * ( iyear + 4800 + imon1 ) / 4
               + 367 * ( imonth - 2 - imon1 * 12 ) / 12
               - 3 * ( ( iyear + 4900 + imon1 ) / 100 ) / 4;
      return ijuld;
}

/* Interfacing with Fortran */

CTAEXPORT void CTA_TIME_CREATE_F77(int *htime, int *ierr){
   *ierr=CTA_Time_Create((CTA_Time *) htime);
}

CTAEXPORT void CTA_TIME_FREE_F77(int *htime, int *ierr){
   *ierr=CTA_Time_Free((CTA_Time *) htime);
}
CTAEXPORT void CTA_TIME_SETSPAN_F77(int *htime,double *tstart, double *tend, int *ierr){
   *ierr=CTA_Time_SetSpan((CTA_Time) *htime,*tstart,*tend);
}
CTAEXPORT void CTA_TIME_GETSPAN_F77(int *htime,double *tstart, double *tend, int *ierr){
   *ierr=CTA_Time_GetSpan((CTA_Time) *htime,tstart, tend);
}
CTAEXPORT void CTA_TIME_SETSTEP_F77(int *htime,double *tstep, int *ierr){
   *ierr=CTA_Time_SetStep((CTA_Time) *htime,*tstep);
}
CTAEXPORT void CTA_TIME_GETSTEP_F77(int *htime,double *tstep, int *ierr){
   *ierr=CTA_Time_GetStep((CTA_Time) *htime,tstep);
}

CTAEXPORT void CTA_TIME_INSPAN_F77(int *htimesub, int *htime,  int *inspan, int *ierr){
   *ierr=CTA_Time_InSpan((CTA_Time) *htimesub, (CTA_Time) *htime,  (BOOL*) inspan);
}


CTAEXPORT void CTA_TIME_ISSTEP_F77(int *htime, double *t,  int *isstep, int *ierr){
   *ierr=CTA_Time_IsStep((CTA_Time) *htime, *t,  (BOOL*) isstep);
}

CTAEXPORT void CTA_TIME_COPY_F77(int *hfrom, int *hto, int *ierr){
  *ierr=CTA_Time_Copy((CTA_Time) *hfrom, (CTA_Time) *hto);
}

CTAEXPORT void CTA_TIME_COUNTSTEPS_F77(int *htime, int *nsteps, int *ierr){
   *ierr=CTA_Time_CountSteps((CTA_Time) *htime, nsteps);
}


CTAEXPORT void CTA_TIME_GETTIMESTEP_F77(int *htime, int *istep, int *htime_step,
                              int *ierr){
   *ierr=CTA_Time_GetTimeStep((CTA_Time) *htime, *istep,
                              (CTA_Time) *htime_step);
}


CTAEXPORT void CTA_TIME_EXPORT_F77(int *htime, int *hexport, int *ierr){
   *ierr=CTA_Time_Export((CTA_Time) *htime, (CTA_Handle) *hexport);
}


CTAEXPORT void CTA_TIME_IMPORT_F77(int *htime, int *himport, int *ierr){
   *ierr=CTA_Time_Import((CTA_Time) *htime, (CTA_Handle) *himport);
}



CTAEXPORT void CTA_TIME_ISSPAN_F77(int *htime, int *isspan, int *ierr){
   *ierr=CTA_Time_IsSpan((CTA_Time) *htime, isspan);
}



