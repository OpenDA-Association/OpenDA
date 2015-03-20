/*
$URL: https://repos.deltares.nl/repos/openda/trunk/costa/src/cta/cta_obsdescr_netcdf.c $
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
#include "cta.h"
#include "cta_mem.h"
#include "cta_sobs_netcdf.h"
#include "cta_defaults.h"

#define IDEBUG (0)

#define MAX(a,b) (a>b ? a: b)
#define MIN(a,b) (a>b ? b: a)


/* Dit is een struct om wat op te slaan wat we nodig hebben */
typedef struct {
#if HAVE_LIBNETCDF
  CTA_Handle myhandle;
  char* condition;
  int n_keys;
  int size_1Dproperties;
  int nmeasr;
  int nmeasr_orig;
  CTA_String *Keys;
  CTA_RelTable selectionReltab;
  CTA_Time tspan;
  float bb_lon[2];
  float bb_lat[2];
  CTAI_OMI_database *database;
  CTAI_SObs_netcdf *sobs;  
#else
	int dum;
#endif
} CTAI_ObsDescr_netcdf;




void CTAI_ObsDescr_netcdf_Create_Size(
   // OUTPUTS:
      int *memsize,           // The number of bytes which are necessary to 
                              //     store one CTAI_SObs_netcdf, with a 
                              //     pointer to the contents (data), 
                              //     but without the contents themelves.
      int *retval             // error code (see cta_datatypes.h for possible 
                              //     error codes)
   ){
#if HAVE_LIBNETCDF

//   *memsize=(int) sizeof(CTAI_SObs_netcdf);
   *memsize=(int) sizeof(CTAI_ObsDescr_netcdf);
   *retval=CTA_OK;
#else
   printf("Error: CTAI_ObsDescr_netcdf_Create_Size: COSTA is compiled without NETCD suport\n");  
#endif
};

void CTAI_ObsDescr_netcdf_Create_Init(
/*
   Allocate the memory which is necesary to store the data necessary for a
   netcdf-observer
*/
     // INPUT:
      CTA_ObsDescr *myhandle,  /* Handle assigned by COSTA */
     // IN-OUTPUTS
        CTAI_ObsDescr_netcdf *descr,// The netcdf-observation description
                                 //     for which the memory must be
                                 //     allocated
     // INPUTS:
        CTA_Handle *usrdat, // User data: database-name
     // OUTPUTS
        int *retval)             // Error code.
{
#if HAVE_LIBNETCDF

   CTAI_SObs_netcdf *sobs; // User data: database-name
   int ierr,i,j ;
   CTA_Time tspan0;
   char buffer[20];

   descr->myhandle=*myhandle;

   *retval= CTA_Handle_GetData(*usrdat, (void **) &sobs);
   if (*retval!=CTA_OK) {
     printf("error: ctai_obsdescr-netcdf_create_init : handle_getdata \n");
     return;
   }

   ierr=CTA_Time_Create(&tspan0);
   ierr=CTA_Time_SetSpan(tspan0, 0.0, 24.0);
   descr->tspan = tspan0;

   // store the handle of the stochastic observer
   descr->sobs = sobs;

   descr->database = sobs->database;
   (descr->database->nusers)++;

   // Copy the conditionCTA_RelTable *hreltable ?
   ierr = CTA_RelTable_Create(&(descr->selectionReltab));
   descr->selectionReltab = sobs->selectionReltab;

   // Store the keys ?
   // Here: do it ad hoc since our OMI-netcdf database is very specific
   descr->size_1Dproperties = 30; //here: for kernel and pressure_levels
   descr->n_keys=2 + descr->size_1Dproperties + descr->size_1Dproperties+1 ;
   descr->Keys = CTA_Malloc(sizeof(CTA_String)* (descr->n_keys));
   for (i=0; i< descr->n_keys; i++) 
   {
     ierr = CTA_String_Create( &(descr->Keys[i]) );
     if (ierr != CTA_OK) {printf("error in cta_obsdecr_init %d \n",ierr);return ; }
   }
   /*   Key: 0:long
             1: lat
             2-31 kernel
             32-62 pressure_levels
   */

   ierr = CTA_String_Set(descr->Keys[0],"longitude");
   ierr = CTA_String_Set(descr->Keys[1],"latitude");
   for (j=1; j<= descr->size_1Dproperties; j++){
     sprintf(buffer,"kernel_%d",j); 
     ierr = CTA_String_Set(descr->Keys[1+j],buffer);
   }
   for (j=1; j<= descr->size_1Dproperties + 1; j++){
     sprintf(buffer,"pressure_levels_%d",j); 
     ierr = CTA_String_Set(descr->Keys[31+j],buffer);
   }

   if (ierr  != CTA_OK) {printf("error cta_string_set%d\n",ierr ); return;}

   // Copy the dimension of the observer
   descr->nmeasr = sobs->nmeasr;
   descr->nmeasr_orig = sobs->nmeasr_orig;
   *retval=CTA_OK;
#else
   printf("Error: CTAI_ObsDescr_netcdf_Create_Init: COSTA is compiled without NETCD suport\n");
  *retval=CTA_NOT_IMPLEMENTED;
#endif

};


void CTAI_ObsDescr_netcdf_CreateSel(CTAI_ObsDescr_netcdf *descr,
          CTA_String *selection, CTA_RelTable *reltab, 
          CTA_ObsDescr *myhandle_out, 
          CTAI_ObsDescr_netcdf *descrout, int *retval){

#if HAVE_LIBNETCDF
			  
	  int len, ierr,i ;
      char *condition;
      double t1_in,t2_in, t1,t2, t1_out, t2_out; 
      int nstations, count_out;
      CTA_Vector hvec_time;
      double *vec_time;
      int  *flag;

      // Get the condition
      // Allocate a name-string 
      *retval = CTA_String_GetLength(*selection, &len);
      if (*retval!=CTA_OK) return;

      // Get the condition
      condition = CTA_Malloc((len+1)*sizeof(char));

      *retval = CTA_String_Get(*selection, condition);
      if (IDEBUG>0) printf("CTAI_ObsDescr_netcdf_CreateSel: given    condition is '%s'\n",condition);
      if (*retval!=CTA_OK) return;

   // Set handle of new observation description
      descrout->myhandle=*myhandle_out;


      // IF the callling sequence has been : cta_sobs_createtimsel -> cta_sobs_creatsel -> 
      // ctai_sobs_netcdf_createsel, THEN
      // The condition is the string starting with 'time BETWEEN'
      if (strstr(condition,"time BETWEEN") != NULL) {
        ierr = sscanf(condition,"%*s %lf %lf",&t1,&t2);

      } else {
        t1 = 0.0; t2 = 24.0;
      }
      printf("ctai_obsdescr_createsel: time selection: %f %f \n",t1,t2);

      // It may be possible that a spatial restriction is given. We will implement that later. TODO
  
     
   // Combine the input-condition and the condition of the input observer.
   // In our netcdf-case, the condition is a combination of spatial window and timespan.
      ierr =  CTA_Time_GetSpan(descr->tspan, &t1_in, &t2_in);
      t1_out = MAX(t1_in,t1);
      t2_out = MIN(t2_in,t2);
      ierr =  CTA_Time_SetSpan(descrout->tspan, t1_out, t2_out);



      // first get the time vector. 
      ierr = CTA_Vector_Create(CTA_DEFAULT_VECTOR, descr->nmeasr, CTA_DOUBLE, CTA_NULL, &hvec_time);
      CTAI_SObs_netcdf_GetTimes(descr->sobs, &hvec_time, &ierr);

      printf("cta_obsdescr_netcdf: manipulation sobs %p %d \n",descr->sobs,ierr);

      vec_time = CTA_Malloc(descr->sobs->nmeasr * sizeof(double));
      ierr=CTA_Vector_GetVals(hvec_time,vec_time, descr->sobs->nmeasr, CTA_DOUBLE);

      // We use the relation table to investigate if the current selection should be restricted more.
      count_out = 0;
      flag=CTA_Malloc((descr->sobs->nmeasr)*sizeof(int));
      for (i=0; i < descr->sobs->nmeasr; i++) {
        flag[i]= 0;
        if (vec_time[i] >= t1_out && vec_time[i] <=t2_out) {
          flag[i] = 1;

          count_out = count_out + 1;
        }
      }
      nstations = count_out;

      descrout->nmeasr    = nstations;
      descrout->n_keys = descr->n_keys;
      descrout->size_1Dproperties = descr->size_1Dproperties;

      if (*reltab!=CTA_NULL) {
        *retval=CTAI_ObsDescr_CreateRelTable(descr->myhandle,
                                             descrout->myhandle, *reltab);
      } else {
        *retval=CTA_OK;
      }

      descrout->database = descr->database;
      descrout->database->nusers++;

      free(vec_time);
      free(flag);
      *retval = CTA_OK;

#else
   printf("Error: CTAI_ObsDescr_netcdf_CreateSel: COSTA is compiled without NETCD support\n");
  *retval=CTA_NOT_IMPLEMENTED;
#endif


}
         


void CTAI_ObsDescr_netcdf_Get_Keys(
         CTAI_ObsDescr_netcdf *descr,
         CTA_Vector *Keys,
         int *retval)
{ 
#if HAVE_LIBNETCDF	
   int i;
   for (i=1; i<=descr->n_keys; i++)
   {
      *retval = CTA_Vector_SetVal(*Keys,i,&(descr->Keys[i-1]),CTA_STRING);
   };

   *retval = CTA_OK;


#else
   printf("Error: CTAI_ObsDescr_netcdf_Get_Keys: COSTA is compiled without NETCD suport\n");
  *retval=CTA_NOT_IMPLEMENTED;
#endif

};

void CTAI_ObsDescr_netcdf_Property_Count(
         CTAI_ObsDescr_netcdf *descr, 
         int *nkeys, 
         int *retval)
{
#if HAVE_LIBNETCDF
   *nkeys = descr->n_keys;
   *retval = CTA_OK;
#else
   printf("Error: CTAI_ObsDescr_netcdf_Property_Count: COSTA is compiled without NETCD suport\n");
  *retval=CTA_NOT_IMPLEMENTED;
#endif
};

void CTAI_ObsDescr_netcdf_Observation_Count(
         CTAI_ObsDescr_netcdf *descr, 
         int *nobs, 
         int *retval)
{
#if HAVE_LIBNETCDF
   *nobs = descr->nmeasr;
   *retval = CTA_OK;
#else
   printf("Error: CTAI_ObsDescr_netcdf_Observation_Count: COSTA is compiled without NETCD suport\n");
  *retval=CTA_NOT_IMPLEMENTED;
#endif
};


void CTAI_ObsDescr_netcdf_Get_Properties(
         CTAI_ObsDescr_netcdf *descr, 
         const char *Key,
         CTA_Vector *Properties,
         CTA_Datatype *datatype,
         int *retval)
{
#if HAVE_LIBNETCDF

  int ierr,ncid,varid;
  int i, ival, dim2;
  float *nc_values, *selected_nc_values, *nc_tot_values, *nc_values_lon,*nc_values_lat;
  char tmpchar[20];
  CTA_String hstr;

  if (IDEBUG>0) {printf("ctai_obsdescr_netcdf_get_properties; START\n");}
  if (IDEBUG>0) {printf("ctai_obsdescr_netcdf_get_properties: key: %s  of: |%s|  \n",Key,descr->database->dbname);}


   // first allocate space for temporary arrays
  dim2 = 1;
  if (strstr(Key,"kernel")!=NULL){dim2 = descr->size_1Dproperties;}
  if (strstr(Key,"pressure_levels")!=NULL){dim2 = descr->size_1Dproperties+1 ;}
  nc_tot_values = CTA_Malloc(descr->nmeasr_orig * dim2 * sizeof(float));


   nc_values = CTA_Malloc(descr->nmeasr_orig  * sizeof(float));
   nc_values_lon = CTA_Malloc(descr->nmeasr_orig  * sizeof(float));
   nc_values_lat = CTA_Malloc(descr->nmeasr_orig  * sizeof(float));
   selected_nc_values = CTA_Malloc(descr->nmeasr * sizeof(float));


  // Get the netcdf-id of the database
   ncid = descr->database->ncid;


  /* now obtain the desired secondary variable, given by the keyname */
  /* For our specific OMI-netcdf-file, we need:    */

  /* always: get the lon and lat */
   ierr = nc_inq_varid(ncid, "longitude", &varid);
   ierr = nc_get_var_float(ncid, varid, &nc_values_lon[0]);
   ierr = nc_inq_varid(ncid, "latitude", &varid);
   ierr = nc_get_var_float(ncid, varid, &nc_values_lat[0]);
   if (ierr != CTA_OK)
     {printf("Error: could not read the property lat/lon \n");
       *retval = -1; return;
     }

  if (strcmp(Key,"NAME")==0){
    /* this key should not be necessary, but it is naively asked by certain COSTA-routines 
           like CTA_Util_MethodsPrintObservations */
    for (i=0; i < descr->nmeasr_orig; i++){
      nc_values[i] = i*1.0;
    }

} else if (strcmp(Key,"longitude")==0){

    if (IDEBUG>0) {printf("ctai_obsdescr_netcdf_get_properties: LONGITUDE    \n");}

    // read the lon vector. We know the size: nmeasr_orig. 
    //    ierr = nc_inq_varid(ncid, "longitude", &varid);
    //ierr = nc_get_var_float(ncid, varid, &nc_values[0]);
    for (i=0; i < descr->nmeasr_orig; i++){
      nc_values[i] = nc_values_lon[i]; }

  } else if (strcmp(Key,"latitude")==0) {
    // ierr = nc_inq_varid(ncid, "latitude", &varid);
    //ierr = nc_get_var_float(ncid, varid, &nc_values[0]);
    for (i=0; i < descr->nmeasr_orig; i++){
      nc_values[i] = nc_values_lat[i]; }

  } else if (strstr(Key,"pressure_levels")!=NULL) {
    ierr = nc_inq_varid(ncid, "pressure_levels", &varid);
    sscanf(&Key[16] ,"%d",&ival);
    if (IDEBUG>0) {printf("Getting key %s, number: %d \n",Key,ival);}

    ierr = nc_get_var_float(ncid, varid, &nc_tot_values[0]);

     if (ierr != CTA_OK)
      {printf("Error: could not read the property %s %d \n",Key,ierr);
        *retval = -1; return;
      }
    for (i=0; i < descr->nmeasr_orig; i++){
      nc_values[i] = nc_tot_values[ival-1 + (i* (descr->size_1Dproperties+1))];
    }



  } else if (strstr(Key,"kernel")!=NULL) {

    ierr = nc_inq_varid(ncid, "kernel", &varid);
    sscanf(&Key[7] ,"%d",&ival);
    if (IDEBUG>0) {printf("Getting key %s, number: %d \n",Key,ival);}

    ierr = nc_get_var_float(ncid, varid, &nc_tot_values[0]);

    if (ierr != CTA_OK)
      {printf("Error: could not read the property %s %d \n",Key,ierr);
        *retval = -1; return;
      }
    for (i=0; i < descr->nmeasr_orig; i++){
      nc_values[i] = nc_tot_values[ival-1 + (i* descr->size_1Dproperties)];
    }


  }else {
    printf("Key %s  is NOT YET IMPLEMENTED!!! \n",Key);
    *retval = CTA_NOT_IMPLEMENTED;
    return;

  }

  // now fill the properties vector with the obtained values. The relation table is used to get only the selection.
  ierr = CTA_RelTable_ApplyVal(descr->selectionReltab, nc_values,descr->nmeasr_orig,
                                 selected_nc_values,descr->nmeasr,CTA_REAL );
   
  if (IDEBUG>0) {printf("ctai_obsdescr_netcdf_get_properties:ierr %d   \n",ierr);}
  if (IDEBUG>0) {printf("selected values %f %f %d %d  \n",selected_nc_values[0], selected_nc_values[1],descr->nmeasr, *datatype);}



  if (*datatype == CTA_STRING){
    if (IDEBUG>0) {printf("cta-string-expected;workaround \n");}
    ierr = CTA_String_Create(&hstr);
    for (i=0; i<descr->nmeasr; i++){
      sprintf(tmpchar,"%f",selected_nc_values[i]);
      ierr = CTA_String_Set(hstr,tmpchar);
      ierr=CTA_Vector_SetVal(*Properties,i+1,&hstr, CTA_STRING);
    }
    ierr = CTA_String_Free(&hstr);
  } else {

    ierr=CTA_Vector_SetVals(*Properties,selected_nc_values, descr->nmeasr, CTA_REAL);
  }

  if (IDEBUG>0) {printf("ctai_obsdescr_netcdf_get_properties:setvals %d   \n",ierr);}

  if (ierr != CTA_OK)
    {printf("Error: could not fill the property vector %s \n",Key);
      *retval = -1; return;
    }

  free(nc_values);
  free(nc_values_lon);
  free(nc_values_lat);
  free(selected_nc_values);
  free(nc_tot_values);

  if (*retval!=CTA_OK) return;

  *retval = CTA_OK;
#else
   printf("CTAI_ObsDescr_netcdf_Get_Properties :Version is compiled without NETCDF support.\n");
   *retval=CTA_NOT_IMPLEMENTED;
#endif

};





void CTAI_ObsDescr_netcdf_Free(
         CTAI_ObsDescr_netcdf *descr, 
         int *retval)
{
#if HAVE_LIBNETCDF
  int i, ierr;
 

   descr->database->nusers--;

   //  printf("cta_obsdescr_free (netcdf): now %d users remaining of database %s \n",descr->database->nusers, descr->database->dbname);

    if (descr->database->nusers==0)
    {

    if ((ierr = nc_close(descr->database->ncid))){
        printf("CTA_Sobs_netcdf_Free: cannot close netCDF-file: %d\n",
               nc_strerror(ierr));
    }
    free(descr->database->dbname);
    free(descr->database);

    }


 
  for (i=0; i<descr->n_keys; i++){ CTA_String_Free(&(descr->Keys[i]));}
  free(descr->Keys);

  *retval = CTA_OK;
#else
   printf("CTAI_ObsDescr_netcdf_Get_Properties :Version is compiled without NETCDF support.\n");
   *retval=CTA_NOT_IMPLEMENTED;
#endif
}


void CTA_ObsDescr_netcdf_initialise(CTA_ObsDescrClass *hobsdescrcl)
{
   CTA_Intf hintf=0;
   CTA_Func h_func[I_CTA_OBSDESCR_NUMFUNC];

   // The vector h_func is filled with COSTA-function handles of the 
   // implementations in this file.
   CTA_Func_Create(" ",&CTAI_ObsDescr_netcdf_Create_Size,    hintf,
                            &h_func[I_CTA_OBSDESCR_CREATE_SIZE]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_netcdf_Create_Init,    hintf,
                            &h_func[I_CTA_OBSDESCR_CREATE_INIT]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_netcdf_Property_Count,    hintf,
                       &h_func[I_CTA_OBSDESCR_COUNT_PROPERTIES]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_netcdf_Get_Properties,    hintf,
                       &h_func[I_CTA_OBSDESCR_GET_PROPERTIES]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_netcdf_Observation_Count, hintf,
                       &h_func[I_CTA_OBSDESCR_COUNT_OBSERVATIONS]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_netcdf_Get_Keys,    hintf,
                       &h_func[I_CTA_OBSDESCR_GET_KEYS]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_netcdf_Free,    hintf,
                       &h_func[I_CTA_OBSDESCR_FREE]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_netcdf_CreateSel,    hintf,
                       &h_func[I_CTA_OBSDESCR_SELECTION]);
   CTA_ObsDescr_DefineClass("cta_obsdescr_netcdf",h_func,hobsdescrcl);

}



