/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_obsdescr.c $
$Revision: 3730 $, $Date: 2012-12-19 11:07:29 +0100 (Wed, 19 Dec 2012) $

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
#include "f_cta_utils.h"
#include "cta_mem.h"
#include "cta_sobs.h"
#include "cta_obsdescr.h"
#include "cta_errors.h"
#include "cta_handles.h"
#include "ctai.h"
#include "cta_message.h"

#define CTA_OBSDESCR_DEFINECLASS_F77          F77_CALL(cta_obsdescr_defineclass,CTA_OBSDESCR_DEFINECLASS)
#define CTA_OBSDESCR_CREATE_F77               F77_CALL(cta_obsdescr_create,CTA_OBSDESCR_CREATE)
#define CTA_OBSDESCR_CHECK_SOBS_F77           F77_CALL(cta_obsdescr_check_sobs,CTA_OBSDESCR_CHECK_SOBS)
#define CTA_OBSDESCR_GET_VALUEPROPERTIES_F77  F77_CALL(cta_obsdescr_get_valueproperties,CTA_OBSDESCR_GET_VALUEPROPERTIES)
#define CTA_OBSDESCR_GET_PROPERTY_KEYS_F77    F77_CALL(cta_obsdescr_get_propertykeys,CTA_OBSDESCR_GET_PROPERTYKEYS)
#define CTA_OBSDESCR_PROPERTY_COUNT_F77       F77_CALL(cta_obsdescr_property_count,CTA_OBSDESCR_PROPERTY_COUNT)
#define CTA_OBSDESCR_OBSERVATION_COUNT_F77    F77_CALL(cta_obsdescr_observation_count,CTA_OBSDESCR_OBSERVATION_COUNT)
#define CTA_OBSDESCR_EXPORT_F77               F77_CALL(cta_obsdescr_export,CTA_OBSDESCR_EXPORT)
#define CTA_OBSDESCR_FREE_F77                 F77_CALL(cta_obsdescr_free,CTA_OBSDESCR_FREE)

#define CLASSNAME "CTA_Obsdescr"

/* Struct holding all data associated to an COSTA Vector */
typedef struct {
CTA_Func functions[I_CTA_OBSDESCR_NUMFUNC];
} CTAI_ObsDescrClass; // A ObsDescrClass contains a list of the member-functions


typedef struct {
CTA_Func functions[I_CTA_OBSDESCR_NUMFUNC]; // See cta_obsdescr.h for a list of
                           //     available stochobs-functions
CTA_ObsDescrClass hdescrcl; // ObsDescr-class
void *data;                // pointer to the implementation-specific data.
//CTA_StochObs hsobs;        // The observer of which this is a description
} CTAI_ObsDescr;



#undef METHOD
#define METHOD "DefineClass"
int CTA_ObsDescr_DefineClass(
   // INPUTS:
      const char *name,                // Name of the new stochobs class
      const CTA_Func h_func[I_CTA_OBSDESCR_NUMFUNC],  // function handles to
                                       // the implementations of the
                                       // stochobs-class' functions.
   // OUTPUTS:
      CTA_ObsDescrClass  *hdescrcl)    // The (handle to) the new
                                       //   observation descriptor-class
{

   CTAI_ObsDescrClass *data;
   int retval;
   int i;

   /* Allocate new ObsDescr object */
   data=CTA_Malloc(sizeof(CTAI_ObsDescrClass));

   for (i=0;i<I_CTA_OBSDESCR_NUMFUNC;i++)
      {data->functions[i]=h_func[i];}

   // Allocate new handle
   retval=CTA_Handle_Create(name,CTA_OBSDESCRCLASS,data,hdescrcl);
   retval=CTA_Handle_GetData((CTA_Handle) *hdescrcl,(void**) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   // return error when unsuccesfull
   return retval;
}


int CTAI_ObsDescr_member_function(
        // INPUTS
        CTA_ObsDescr hdescr,    /* Handle of the stochastic observer of
                                  which a member function is wanted */
        int member,            /* Code of the member function */
        // OUTPUT
        CTAI_ObsDescr **descr,      /* All data of obs-descr hdescr  */
        CTA_Function **memfun  /* Member-Function pointer */
)
{
   int retval;

   if (hdescr==CTA_NULL){
      printf("CTAI_ObsDescr_member_function WARNING NULL handle\n");
   }
   /* Check that the given handle is indeed an observation description */
   retval=CTA_Handle_Check((CTA_Handle) hdescr, CTA_OBSDESCR);
   if (retval!=CTA_OK) return retval;

   /* Get pointer to struct with observation description data */
   retval=CTA_Handle_GetData((CTA_Handle) hdescr,(void**) descr);
   if (retval!=CTA_OK) return retval;

   /* Get pointer to implementation of this function */
   retval=CTA_Func_GetFunc((*descr)->functions[member],memfun);
   return retval;
}


#undef METHOD
#define METHOD "Create"  
int CTA_ObsDescr_Create(
    // INPUTS:
        CTA_ObsDescrClass hdescrcl,       // class of observer hsobs
        CTA_Handle usrdat,           // Data of the stochastic observer for which
                              //   a descriptor is to be created
    // OUTPUTS:
        CTA_ObsDescr *hdescr)       // The new COSTA-stochastic observer
                                   // (handle)
{
   CTAI_ObsDescr *descr;
   int memsize;
   int retval;
   CTAI_ObsDescrClass *clsdata;
   CTA_Function *my_Create_Size, *my_Create_Init;
   int i;

   /* Get class data containing all function pointers */
   retval=CTA_Handle_Check((CTA_Handle) hdescrcl,CTA_OBSDESCRCLASS);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a CTA_OBSDESCRCLASS handle");
       return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) hdescrcl,(void**) &clsdata);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* determine size of data object (CTA_OBSDESCR_CREATE_SIZE)*/
   retval=CTA_Func_GetFunc(clsdata->functions[I_CTA_OBSDESCR_CREATE_SIZE],
                           &my_Create_Size);

   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function CTA_OBSDESCR_CREATE_SIZE");
	   return retval;
   }

   my_Create_Size(&memsize,&retval);
   if (retval) {
	   CTA_WRITE_ERROR("Error in Create_Size");
	   return retval;
   }

   /* allocate memory for new observation description object */
   descr=CTA_Malloc(sizeof(CTAI_ObsDescr));
   descr->data=CTA_Malloc(memsize);

   /* copy function pointers */
   for (i=0;i<I_CTA_OBSDESCR_NUMFUNC;i++){
      descr->functions[i]=clsdata->functions[i];
   }

   /* set other general information */
   descr->hdescrcl=hdescrcl;
//   descr->hsobs=hsobs;

   /* Initialise and fill new obsdescr */
   retval=CTA_Func_GetFunc(clsdata->functions[I_CTA_OBSDESCR_CREATE_INIT],
                           &my_Create_Init);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function CTA_OBSDESCR_CREATE_INIT");
	   return retval;
   }

   /* Allocate new handle and return error when unsuccesfull */
   retval=CTA_Handle_Create("obsdescr",CTA_OBSDESCR,descr,hdescr);
   if (retval) {
	   CTA_WRITE_ERROR("Cannot create handle");
	   return retval;
   }

   my_Create_Init(hdescr, descr->data, &usrdat, &retval);
   if (retval!=CTA_OK) {
       printf("error in create init: %d \n",retval);  
	   CTA_WRITE_ERROR("Error in Create Init");
	   return retval;
   }

   return CTA_OK;
}

#undef METHOD
#define METHOD "CreateSel"
int CTA_ObsDescr_CreateSel( CTA_StochObs hobsdescr, CTA_String selection,
                            CTA_RelTable reltab, CTA_StochObs *hobsdescrout)
{
   /* Local variables */
   int retval;                 /* Return value of COSTA call    */
   CTAI_ObsDescr* sobsin;          /* All data of observer hsobsin  */
   CTAI_ObsDescr* sobsout;         /* All data of observer hsobsout */
   CTA_Function *memfun;       /* Function that must be called  */
   CTA_Function *my_Create_Size; /* Function for determining mem-block */
   int memsize,i;

   /* Look up the Create-size function and the data of the input observer*/
   retval=CTA_Handle_Check((CTA_Handle) hobsdescr,CTA_OBSDESCR);
   if (retval!=CTA_OK)  {
       CTA_WRITE_ERROR("Handle is not a cta_obsdescr handle");
       return retval;
   }

   /* Get the create-size member function */
   retval = CTA_Handle_GetData(hobsdescr,(void **) &sobsin);
   retval = CTA_Func_GetFunc(sobsin->functions[I_CTA_OBSDESCR_CREATE_SIZE],
                             &my_Create_Size);

   if (retval!=CTA_OK)  {
       CTA_WRITE_ERROR("Cannot get function CTA_OBSDESCR_CREATE_SIZE");
       return retval;
   }

   my_Create_Size(&memsize,&retval);
   if (retval) {
       CTA_WRITE_ERROR("Error in Create_Size");
       return retval;
   }

   sobsout=CTA_Malloc(sizeof(CTAI_ObsDescr));
   sobsout->data=CTA_Malloc(memsize);

   /* copy function pointers and observation class handle */
   for (i=0;i<I_CTA_OBSDESCR_NUMFUNC;i++)
      { sobsout->functions[i]=sobsin->functions[i]; }


   /* Look up member function and observer data */
   retval = CTA_Func_GetFunc(sobsin->functions[I_CTA_OBSDESCR_SELECTION],
                             &memfun);
   if (retval!=CTA_OK)  {
       CTA_WRITE_ERROR("Cannot get function CTA_OBSDESCR_SELECTION");
       return retval;
   }

   /* Allocate new handle and return error when unsuccesfull */
   retval=CTA_Handle_Create("obsdescr",CTA_OBSDESCR,sobsout,hobsdescrout);
   if (retval!=CTA_OK)  {
       CTA_WRITE_ERROR("Cannot create handle");
       return retval;
   }

   /* Call (user) implementation */
   memfun(sobsin->data,&selection,&reltab, hobsdescrout, sobsout->data,&retval);

   return retval;
};

#undef METHOD
#define METHOD "CreateTimeSe1"
int CTA_ObsDescr_CreateTimSel(CTA_ObsDescr hobsdescr, CTA_Time timespan,
                              CTA_RelTable reltab, CTA_ObsDescr *hobsdescrout)
{
   int ierr;
   char str[80];
   double t1,t2;
   double eps;

   CTA_String sselect;


   *hobsdescrout=CTA_NULL;
   if (hobsdescr!=CTA_NULL){
      // Get interval of timespan (t1,t2)
      ierr=CTA_Time_GetSpan(timespan,&t1,&t2); 
	  if (ierr!=CTA_OK) {
	     char message[1024];
	     sprintf(message,"Cannot get interval of timespan (%g,%g)",t1,t2);
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
      ierr=CTA_ObsDescr_CreateSel(hobsdescr, sselect, reltab, hobsdescrout);
      if (ierr!=CTA_OK) {
         CTA_WRITE_ERROR("Cannot create selection");
         return ierr;
      }

      // Free work variables
      ierr=CTA_String_Free(&sselect); 
	  if (ierr!=CTA_OK) {
         CTA_WRITE_ERROR("Cannot free string");
         return ierr;
      }
   }
   return CTA_OK;
};




#undef METHOD
#define METHOD "Get_PropertyKeys"
int CTA_ObsDescr_Get_PropertyKeys(
   // INPUTS:
   CTA_ObsDescr hobsdescr,   /* Handle of the observation description of
                                    which a property is to be returned */
   // OUTPUTS:
   CTA_Vector Keys)          /* Name of the key */
{
   /* Local variables */
   int retval;                 /* Return value of COSTA call    */
   CTAI_ObsDescr* obsdescr;    /* All data of observer hobsdescr  */
   CTA_Function *memfun;       /* Function that must be called  */

   /* trivial return we accept CTA_NULL -> empty vector */
   if (hobsdescr==CTA_NULL){
     Keys = CTA_NULL;
     return CTA_OK;
   }

   /* Look up the member function and the data of the observation descriptor*/
   retval = CTAI_ObsDescr_member_function(hobsdescr, I_CTA_OBSDESCR_GET_KEYS,
                                          &obsdescr, &memfun);
   if (retval) {
       CTA_WRITE_ERROR("Error in function CTA_OBSDESCR_GET_KEYS");
       return retval;
   }

   /* Call (user) implementation */
   memfun(obsdescr->data,&Keys,&retval);

   return retval;
}

#undef METHOD
#define METHOD "Property_Count"
int CTA_ObsDescr_Property_Count(CTA_ObsDescr hobsdscr, int *nkeys){
   /* Local variables */
   int retval;                 /* Return value of COSTA call    */
   CTAI_ObsDescr* obsdescr;    /* All data of observer hobsdescr  */
   CTA_Function *memfun;       /* Function that must be called  */
   char msg[256];

   /* trivial return: CTA_NULL means empty description: no keys */
   if (hobsdscr==CTA_NULL){
      *nkeys=0;
      return CTA_OK;
   }

   /* Look up the member function and the data of the observation descriptor*/
   retval = CTAI_ObsDescr_member_function(hobsdscr, I_CTA_OBSDESCR_COUNT_PROPERTIES,
                                          &obsdescr, &memfun);
   if (retval) {
       sprintf(msg,"Cannot find the member function I_CTA_OBSDESCR_COUNT_PROPERTIES retval=%d",retval);
       CTA_WRITE_ERROR(msg);
       return retval;
   }

   /* Call (user) implementation */
   memfun(obsdescr->data,nkeys,&retval);

   return retval;
}

#undef METHOD
#define METHOD "Observation_Count"
int CTA_ObsDescr_Observation_Count(CTA_ObsDescr hobsdscr, int *nobs){
   /* Local variables */
   int retval;                 /* Return value of COSTA call    */
   CTAI_ObsDescr* obsdescr;    /* All data of observer hobsdescr  */
   CTA_Function *memfun;       /* Function that must be called  */
   char msg[256];

   /* trivial return we accept CTA_NULL -> nobs=0 */
   if (hobsdscr==CTA_NULL){
      *nobs=0;
      return CTA_OK;
   }

   /* Look up the member function and the data of the observation descriptor*/
   retval = CTAI_ObsDescr_member_function(hobsdscr, I_CTA_OBSDESCR_COUNT_OBSERVATIONS,
                                          &obsdescr, &memfun);
   if (retval) {
       sprintf(msg,"Cannot find the member function I_CTA_OBSDESCR_COUNT_OBSERVATIONS retval=%d",retval);
       CTA_WRITE_ERROR(msg);
       return retval;
   }

   /* Call (user) implementation */
   memfun(obsdescr->data,nobs,&retval);

   return retval;
}

#undef METHOD
#define METHOD "Get_ValueProperties"
int CTA_ObsDescr_Get_ValueProperties(
          CTA_ObsDescr hobsdscr,
          const char* Key,
          CTA_Vector Properties,
          CTA_Datatype datatype)
{
   /* Local variables */
   int retval;                 /* Return value of COSTA call    */
   CTAI_ObsDescr* obsdescr;    /* All data of observer hobsdescr  */
   CTA_Function *memfun;       /* Function that must be called  */
   char msg[256];


   /* Look up the member function and the data of the observation descriptor*/
   retval = CTAI_ObsDescr_member_function(hobsdscr, I_CTA_OBSDESCR_GET_PROPERTIES,
                                          &obsdescr, &memfun);
   if (retval) {
       sprintf(msg,"Cannot find the member function I_CTA_OBSDESCR_GET_PROPERTIES retval=%d",retval);
       CTA_WRITE_ERROR(msg);
       return retval;
   }

 
   /* Call (user) implementation */
   memfun(obsdescr->data,Key,&Properties,&datatype,&retval);

   return retval;
}


#undef METHOD
#define METHOD "Export"
int CTA_ObsDescr_Export(CTA_ObsDescr hdescr, CTA_Handle usrdat){
   int retval;                 /* Return value of COSTA call    */
   CTAI_ObsDescr* descr;    /* All data of observer hobsdescr  */
   CTA_Function *memfun;       /* Function that must be called  */
   char msg[256];

   /* Look up the member function and the data of the observation descriptor*/
   retval = CTAI_ObsDescr_member_function(hdescr, I_CTA_OBSDESCR_EXPORT,
                                          &descr, &memfun);
   if (retval) {
       sprintf(msg,"Cannot find the member function I_CTA_OBSDESCR_EXPORT retval=%d",retval);
       CTA_WRITE_ERROR(msg);
       return retval;
   }

   /* Call (user) implementation */
   memfun(descr->data,&usrdat,&retval);
   return retval;
}




#undef METHOD
#define METHOD "Free"
int CTA_ObsDescr_Free(CTA_ObsDescr *hdescr){
   int retval;                 /* Return value of COSTA call    */
   CTAI_ObsDescr* descr;    /* All data of observer hobsdescr  */
   CTA_Function *memfun;       /* Function that must be called  */
   char msg[256];

   /* Check for quick return */
   if (*hdescr==CTA_NULL) return CTA_OK;

   /* Look up the member function and the data of the observation descriptor*/
   retval = CTAI_ObsDescr_member_function(*hdescr, I_CTA_OBSDESCR_FREE,
                                          &descr, &memfun);
   if (retval) {
       sprintf(msg,"Cannot find the member function I_CTA_OBSDESCR_FREE retval=%d",retval);
       CTA_WRITE_ERROR(msg);
       return retval;
   }

   /* Call (user) implementation */
   memfun(descr->data,&retval);

   free(descr->data);
   free(descr);
   retval = CTA_Handle_Free(hdescr);
   return retval;
}

/* Interfacing with Fortran */

CTAEXPORT void CTA_OBSDESCR_DEFINECLASS_F77( char *name, int *h_func, int *hobsdscrcl,int *ierr, int len_name){
//CTAEXPORT void cta_obsdescr_defineclass_( char *name, int *h_func, int *hobsdscrcl,int *ierr, int len_name){

   char  *c_name;
   // create a c-string equivalent to name
   c_name=CTA_Malloc((len_name+1)*sizeof(char));
   CTA_fstr2cstr(name,c_name,len_name);

   *ierr=CTA_ObsDescr_DefineClass(name, (CTA_Func*) h_func, (CTA_ObsDescrClass*) hobsdscrcl);

   free(c_name);
}

CTAEXPORT void CTA_OBSDESCR_CREATE_F77(int *hsobscl, int *usrdat,
              int *hobsdescr, int *ierr){

   *ierr=CTA_ObsDescr_Create((CTA_ObsDescrClass) *hsobscl, (CTA_Handle) *usrdat,
                             (CTA_ObsDescr*) hobsdescr);
}

//void CTA_OBSDESCR_CHECK_SOBS_F77(int *hobsdescr, int *hsobs ,int *ierr){
//   *ierr=CTA_ObsDescr_Check_SObs((CTA_ObsDescr) *hobsdescr, (CTA_StochObs) *hsobs);
//}

CTAEXPORT void CTA_OBSDESCR_GET_VALUEPROPERTIES_F77(
           int *hobsdscr, char *key, int *Property, int *datatype,
           int *ierr, int len_key)
{
   char *c_Key;
   c_Key=CTA_Malloc((len_key+1)*sizeof(char));
   CTA_fstr2cstr(key,c_Key,len_key);
   *ierr=CTA_ObsDescr_Get_ValueProperties(
                    (CTA_ObsDescr) *hobsdscr,
                    c_Key,
                    (CTA_Vector)   *Property,
                    (CTA_Datatype) *datatype);
   free(c_Key);
}

CTAEXPORT void CTA_OBSDESCR_GET_PROPERTY_KEYS_F77(int *hobsdscr, int *Keys, int *ierr){
   *ierr=CTA_ObsDescr_Get_PropertyKeys( (CTA_ObsDescr) *hobsdscr, (CTA_Vector) *Keys);
}

CTAEXPORT void CTA_OBSDESCR_PROPERTY_COUNT_F77(int *hobsdscr, int *nkeys, int *ierr){
   *ierr=CTA_ObsDescr_Property_Count( (CTA_ObsDescr) *hobsdscr, nkeys);
}

CTAEXPORT void CTA_OBSDESCR_OBSERVATION_COUNT_F77(int *hobsdscr, int *nobs, int *ierr){
   *ierr=CTA_ObsDescr_Observation_Count((CTA_ObsDescr) *hobsdscr, nobs);
}

CTAEXPORT void CTA_OBSDESCR_EXPORT_F77(int *hdescr, int *usrdat, int *ierr){
   *ierr=CTA_ObsDescr_Export((CTA_ObsDescr) *hdescr, (CTA_Handle) *usrdat);
}

CTAEXPORT void CTA_OBSDESCR_FREE_F77(int *hobsdscr, int *ierr){
   *ierr=CTA_ObsDescr_Free((CTA_ObsDescr *) hobsdscr);
}



