/*
$URL: https://repos.deltares.nl/repos/openda/trunk/costa/src/cta/cta_stoch_observer_netcdf.c $
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


#include "cta.h"
#include "cta_mem.h"
#include "ctai.h"
#include <string.h>
#include <memory.h>
#include "cta_flush.h"

#if HAVE_LIBNETCDF
#include <netcdf.h>
#endif

#include "cta_sobs_netcdf.h"


#define MAX(a,b) (a>b ? a: b)
#define IDEBUG (0)

/* This is a struct to store what we need */
/* Specific for our case of Netcdf-OMI-data: The model will only ask two aspects: 
   - certain time selections (typically one-hour intervals)
   -It should be situated in a certain rectangular area (lat/lon)
*/
 



void CTAI_SObs_netcdf_Create_Size(
   // OUTPUTS:
      int *memsize,           // The number of bytes which are necessary to store one 
                              //     CTAI_SObs_netcdf, with a pointer to the contents (data), 
                              //     but without the contents themselves.
      int *retval             // error code (see cta_datatypes.h for possible error codes)
   ){
   *memsize=(int) sizeof(CTAI_SObs_netcdf);
   *retval=CTA_OK;
};





void CTAI_SObs_netcdf_Create_Init(



/*
   Allocate the memory which is neccesary to store the data necessary for a
   netcdf-observer
*/
     // IN-OUTPUTS
        CTAI_SObs_netcdf *x,   // The netcdf-observer for which the memory must 
                                //     be allocated
     // INPUTS:
        CTA_Handle userdata,   // User data: database-name. Note: the string ends with the date
                               // It is possible that userdata is vector of two handles: (name,timeoffset)
     // OUTPUTS
        int *retval)            // Error code. Possible error: Illegal data type
{
#if HAVE_LIBNETCDF
  char *dbname;
  int   ncid, varid, nmeasr, intNStations;
  size_t nstations;
  int ierr, i, len;
  int dimids[1];
  CTA_Time tspan0;
  float lon1,lon2,lat1,lat2;
  int *flag;
  CTAI_OMI_database *database;
  double timeoffset;
  CTA_Handle  userdata1, userdata2;

  database = CTA_Malloc(sizeof(CTAI_OMI_database));  

  // Get the name of the database file:

  // Check if userdata is a string (database name) or a vector (name and timeoffset)
   ierr=CTA_Handle_Check((CTA_Handle) userdata,CTA_VECTOR);
   if (ierr!=CTA_OK) {
     //then it should be only the string with the database name
     ierr=CTA_Handle_Check((CTA_Handle) userdata,CTA_STRING);
     if (ierr!=CTA_OK) { printf("sobs_netcdf: create :: userdata is neither a database name or vector\n");exit(-1);}
     userdata1 = userdata; timeoffset = 0.0;

     if (IDEBUG>0) {printf("sobs_netcdf_create 1: timeoffset %f ",timeoffset);}

   } else { //it is a vector: deconstruct it into the database string and the timeoffset
   
     CTA_Vector_GetVal(userdata,1,&userdata1,CTA_HANDLE);
     CTA_Vector_GetVal(userdata,2,&userdata2,CTA_HANDLE);
     ierr=CTA_Vector_GetVal(userdata2,1,&timeoffset,CTA_DOUBLE);
     if (ierr!=CTA_OK) {
       printf("CTAI_SObs_netcdf :: Create: userdata2 is not a vector of doubles! \n");
   }
     if (IDEBUG>0) {printf("sobs_netcdf_create 2: timeoffset %f ",timeoffset);}
   }


  // Allocate a name-string 
  *retval = CTA_String_GetLength(userdata1, &len);
  if (*retval!=CTA_OK) return;
  // Get the name of the database
  dbname = CTA_Malloc((len+1)*sizeof(char));
  *retval = CTA_String_Get(userdata1, dbname);
  if (*retval!=CTA_OK) return;

  // Open the database
  ierr = nc_open(dbname, NC_NOWRITE, &ncid);
  if (ierr != CTA_OK)
    {printf("Error: could not open netcdf-file \n");
      *retval = -1;
      return;}

   // Count the stations in this observer. 
  // Note: we denote each valid observation of the satellite as one station with a specific pace and time. 
  // The number of valid observations is O(10^4), while the total grid is typical O(10^5), in the case of 
  //   OMI-data and our part of Europe: 350x501 and a time series of length 8000.
  ierr = nc_inq_varid(ncid, "longitude", &varid);
  if (ierr != CTA_OK)
    {printf("Error: could not find datetime \n");*retval = -1; return;}

  ierr = nc_inq_vardimid(ncid, varid, dimids); // get ID of time dimension
  if (ierr != CTA_OK)
    {printf("Error: could not find dimension ID \n");*retval = -1; return;}

  ierr = nc_inq_dimlen(ncid, dimids[0], &nstations); /* get length of time series */
  if (ierr != CTA_OK)
    {printf("Error: could not length of time series \n");*retval = -1; return;}

 
  // initialise the selection relation table
  CTA_RelTable_Create(&(x->selectionReltab));

  flag=CTA_Malloc(nstations*sizeof(int));
  for (i=0; i < (int) nstations; i++) {flag[i] = i+1;}

  intNStations=(int) nstations; 
  CTA_RelTable_SetSelectVal(x->selectionReltab, flag, intNStations,CTA_INTEGER);
  if (IDEBUG>0) {printf("ctai_sobs_netcdf_init: number of stations/length of time series: %d \n",intNStations);}
  

  // initialise the time
   CTA_Time_Create(&tspan0);
   CTA_Time_SetSpan(tspan0, 0.0, 24.0);

   // initialise the spatial window. Maybe it should be read from netcdf-file (global attribute)
   lon1 = -90.0; // far west
   lon2 = 90.0;  // far east
   lat1 = 0.0;  //equator
   lat2= 90.0;  //north pole   

   // Measurements in this observer = Number of stations in our case! 
   nmeasr = nstations;

   // Set the observer-fields
   x->tspan = tspan0;    
   x->bb_lon[0] = lon1;   x->bb_lon[1] = lon2;
   x->bb_lat[0] = lat1;   x->bb_lat[1] = lat2;



   database->dbname = dbname;
   database->ncid = ncid;
   database->nusers = 1;

   x->database = database;
   x->nstations = nstations;
   x->nmeasr    = nmeasr;
   x->nmeasr_orig = nmeasr;


   x->timeoffset = timeoffset;

if (IDEBUG>0) {printf("END of cta_sobs_netcdf_create_init \n");}
   *retval=CTA_OK;

#else
   printf("Error: CTAI_SObs_netcdf_Create_Init: COSTA is compiled without NETCDF support\n");
  *retval=CTA_NOT_IMPLEMENTED;
#endif

};



void CTAI_SObs_netcdf_CreateSelString(
/*
   Allocate the memory which is necessary to store the data necessary for a
   netcdf-observer
*/
     // INPUTS
        CTAI_SObs_netcdf *obsin,// The netcdf-observer of which a selection 
                                 // is to be made
        CTA_Handle userdata,   // User data: condition
     // OUTPUTS
        CTAI_SObs_netcdf *obsout, // The netcdf-observer which is a selection 
                                // of observer obsin
        int *retval)            // Error code
{
  #if HAVE_LIBNETCDF
  int len, ierr,i ;
      char *condition;
      double t1_in,t2_in, t1,t2, t1_out, t2_out; 
      float t1_f, t2_f;
      int nstations, count_out, varid;
      CTA_Vector hvec_time;
      double *vec_time;
      int *selvec, *flag;
      CTA_RelTable reltab1;
      float bbx_1, bbx_2, bby_1, bby_2;
      float *nc_values1, *nc_values2, *nc_sel_values1, *nc_sel_values2;

      if (IDEBUG>0) {printf("DEBUG:  CTAI_SObs_netcdf_CreateSelString START\n");}
      if (IDEBUG>0) {printf("DEBUG: *obsin: %p  \n", obsin);}
      if (IDEBUG>0) {printf("DEBUG: *obsout: %p \n", obsout);}

      // Allocate a name-string 
      *retval = CTA_String_GetLength(userdata, &len);
      if (*retval!=CTA_OK) return;

      // Get the condition
      condition = CTA_Malloc((len+1)*sizeof(char));
      ierr = CTA_String_Get(userdata, condition);
      if (ierr!=CTA_OK) return;

   // Link the database also to this observer and keep track of the number of 
   // observers using this database
      obsout->database = obsin->database;
      obsout->database->nusers++;


      // IF the callling sequence has been : cta_sobs_createtimsel -> cta_sobs_createsel -> 
      // ctai_sobs_netcdf_createsel, THEN
      // The condition is the string starting with 'time BETWEEN'
      if (strstr(condition,"time BETWEEN") != NULL) {
        //printf("condition string is: |%s| \n",condition);
        ierr = sscanf(condition,"%*s %*s %f %*s %f",&t1_f,&t2_f); t1 = t1_f; t2=t2_f;

      } else {
        t1 = 0.0; t2 = 24.0;
      }
      if (IDEBUG>0){
        printf("ctai_sobs_createselstring: time selection: %s %f %f %d \n",obsout->database->dbname,t1,t2,ierr);
      }

      // It may be possible that a spatial restriction is given. 
      for (i=0; i < 2; i++) {
        obsout->bb_lon[i] = obsin->bb_lon[i];
        obsout->bb_lat[i] = obsin->bb_lat[i]; 
      }

      /* spatial selection: */
      if (strstr(condition,"bounding box") !=NULL) {
        ierr = sscanf(condition,"%*s %*s  %f  %f %f %f ",&bbx_1,&bbx_2, &bby_1,&bby_2);
        obsout->bb_lon[0] = bbx_1;        obsout->bb_lon[1] = bbx_2;
        obsout->bb_lat[0] = bby_1;        obsout->bb_lat[1] = bby_2;
        //        printf("SOBS_NETCDF_CREATESEL: condition |%s| \n",condition);
        //printf("SOBS_NETCDF_CREATESEL: condition window is: %f %f %f %f \n",bbx_1,bbx_2,bby_1,bby_2);
      }
     
   // Combine the input-condition and the condition of the input observer.
   // In our netcdf-case, the condition is a combination of spatial window and timespan.
      ierr =  CTA_Time_GetSpan(obsin->tspan, &t1_in, &t2_in);
      t1_out = MAX(t1_in,t1);
      t2_out = MIN(t2_in,t2);
      ierr = CTA_Time_Create(&(obsout->tspan));
      ierr =  CTA_Time_SetSpan(obsout->tspan, t1_out, t2_out);
      if (ierr != CTA_OK) {printf("time_create in ctai_sobs_netcdf_createsel FAILED!\n");}



      // We must recompute the number of stations after selection. Also, the relation table should be adjusted. 


      // first get the time vector. 
      ierr = CTA_Vector_Create(CTA_DEFAULT_VECTOR, obsin->nmeasr, CTA_DOUBLE, CTA_NULL, &hvec_time);
      CTAI_SObs_netcdf_GetTimes(obsin, &hvec_time, &ierr);
      vec_time = CTA_Malloc(obsin->nmeasr * sizeof(double));
      ierr=CTA_Vector_GetVals(hvec_time,vec_time, obsin->nmeasr, CTA_DOUBLE);


      if (IDEBUG>5){
        printf("createsel: ierr time vector %d ; timespan:  %f %f \n",ierr, t1_out, t2_out);
        printf("vectime, %f  %f \n",vec_time[0],vec_time[1]);
      }

      // We use the relation table to investigate if the current selection should be restricted more.


      nc_values1 = CTA_Malloc(obsin->nmeasr_orig * sizeof(float));
      nc_values2 = CTA_Malloc(obsin->nmeasr_orig * sizeof(float));
      nc_sel_values1 = CTA_Malloc(obsin->nmeasr * sizeof(float));
      nc_sel_values2 = CTA_Malloc(obsin->nmeasr * sizeof(float));
      nc_inq_varid(obsin->database->ncid, "longitude", &varid);
      nc_get_var_float(obsin->database->ncid, varid, &nc_values1[0]);
      nc_inq_varid(obsin->database->ncid, "latitude", &varid);
      nc_get_var_float(obsin->database->ncid, varid, &nc_values2[0]);
      CTA_RelTable_ApplyVal(obsin->selectionReltab, nc_values1, obsin->nmeasr_orig, 
                                nc_sel_values1,obsin->nmeasr, CTA_REAL);
      CTA_RelTable_ApplyVal(obsin->selectionReltab, nc_values2, obsin->nmeasr_orig, 
                                nc_sel_values2,obsin->nmeasr, CTA_REAL);

      if (IDEBUG>10){
        printf("latitudes of obsin: \n");
        for (i=0; i < MIN(15,obsin->nmeasr); i++) {
                  printf(" %d %f\n ",i, nc_values2[i]);
                }
        printf("------------%f %f ------\n",obsout->bb_lat[0],obsout->bb_lat[1]);
      }

      count_out = 0;
      flag=CTA_Malloc((obsin->nmeasr)*sizeof(int));
      for (i=0; i < obsin->nmeasr; i++) {
        flag[i]= 0;
        if ((vec_time[i] >= t1_out) && (vec_time[i] <=t2_out) && 
            (nc_sel_values1[i] > obsout->bb_lon[0]) && (nc_sel_values1[i] < obsout->bb_lon[1]) 
            && (nc_sel_values2[i] > obsout->bb_lat[0]) && (nc_sel_values2[i] < obsout->bb_lat[1]) 
            ) {
          flag[i] = 1;
          //printf("selected: %d = %f ; lat %f \n",i,vec_time[i], nc_sel_values2[i]);
          count_out = count_out + 1;
        }
      }
      
      free(nc_values1); free(nc_values2);free(nc_sel_values1);free(nc_sel_values2);

      nstations = count_out;


      obsout->nstations = nstations;
      obsout->nmeasr    = nstations;


      if (IDEBUG>0) {printf("selection made: now %d stations \n",nstations);}

      // make the relation table for this restriction
      ierr = CTA_RelTable_Create(&reltab1);
      selvec = CTA_Malloc(obsout->nmeasr * sizeof(int));

      
      count_out = 0;
      for (i=0; i < obsin->nmeasr; i++) {
        if (flag[i] > 0 && count_out < obsout->nmeasr) {
          selvec[count_out] = i+1;
          count_out = count_out + 1;
        }
      }

      ierr = CTA_RelTable_SetSelectVal(reltab1, selvec, obsout->nmeasr, CTA_INTEGER);                

      if (IDEBUG>5){printf("createselstring: setselect ierr  %d \n",ierr);}

      // now combine the two relation tables to get the whole selection
      ierr = CTA_RelTable_Create(&(obsout->selectionReltab)); //moet dit?????

      ierr=CTA_RelTable_SetTableCombine(obsout->selectionReltab, obsin->selectionReltab, CTA_FALSE,
                                                reltab1, CTA_FALSE);

      obsout->nmeasr_orig = obsin->nmeasr_orig;
      obsout->timeoffset = obsin->timeoffset;

      //printf("createsel: ierr reltable_combine: end  %d \n",ierr);

      free(vec_time);free(selvec);
      free(flag);
      ierr = CTA_RelTable_Free(&reltab1);
      *retval = CTA_OK;

      if (IDEBUG>0){printf("createselstring %s : END:  from  %d to %d \n",obsin->database->dbname,obsin->nmeasr, obsout->nmeasr);}
      if (IDEBUG>10){printf("createsel: END: lat.window  %f to %f \n",obsout->bb_lat[0], obsout->bb_lat[1]);}
#else
   printf("Error: CTAI_SObs_netcdf_CreateSelString: COSTA is compiled without NETCDF support\n");
  *retval=CTA_NOT_IMPLEMENTED;
#endif

};


void CTAI_SObs_netcdf_CreateSelRelTab(CTAI_SObs_netcdf *obsin, CTA_RelTable reltab, 
        CTAI_SObs_netcdf *obsout, int *retval) 
{
#if HAVE_LIBNETCDF
  int nsel, ierr, i;
  double t1_in,t2_in;


  //   printf(" CTAI_SObs_netcdf_CreateSelRelTab: START: NMEASR %d \n",obsin->nmeasr);
   CTA_RelTable_Count(obsin->selectionReltab,&nsel);

  /* Note: this is a quick implementation. The selection is not again checked w.r.t. the spatial window. Use with care!   */

   // First: combine the two relation tables to get the whole selection
   ierr = CTA_RelTable_Create(&(obsout->selectionReltab)); //moet dit?????
   //   printf("createselRelTab: ierr reltable_create van obsout:  %d \n",ierr);
   ierr=CTA_RelTable_SetTableCombine(obsout->selectionReltab, obsin->selectionReltab, CTA_FALSE,
                                                reltab, CTA_FALSE);

   if (ierr != CTA_OK) {
     printf("ERROR in  CTAI_SObs_netcdf_CreateSelRelTab: %d \n",ierr);
     *retval = ierr;
     return;
   }

   obsout->nmeasr_orig = obsin->nmeasr_orig;
   obsout->timeoffset = obsin->timeoffset;

   CTA_RelTable_Count(obsout->selectionReltab,&nsel);
   obsout->nmeasr = nsel; 
   // will this also work in case of empty selection?  Yes.


   //   printf(" CTAI_SObs_netcdf_CreateSelRelTab: %s SELECTION NMEASR %d \n",obsin->database->dbname,nsel);
   //  printf(" more: handles *obsin  %d  \n", *obsin);
   //printf(" more: handles *obsout %d   \n", *obsout);
   //printf(" more: handles  obsin->reltab ;reltab %d  %d \n",obsin->selectionReltab,reltab);

   for (i=0; i < 2; i++) {
     obsout->bb_lon[i] = obsin->bb_lon[i];
     obsout->bb_lat[i] = obsin->bb_lat[i]; 
   }


   // Link the database also to this observer and keep track of the number of 
   // observers using this database
   obsout->database = obsin->database;
   obsout->database->nusers++;

   obsout->nstations = obsout->nmeasr;


   CTA_Time_GetSpan(obsin->tspan, &t1_in, &t2_in);
   CTA_Time_Create(&(obsout->tspan));
   CTA_Time_SetSpan(obsout->tspan, t1_in, t2_in);



#else
   printf("Error: CTAI_SObs_netcdf_CreateSelRelTab: COSTA is compiled without NETCDF support\n");
  *retval=CTA_NOT_IMPLEMENTED;
#endif

};





void CTAI_SObs_netcdf_CreateSel(
/*
   Allocate the memory which is neccesary to store the data necessary for a
   netcdf-observer
*/
     // INPUTS
        CTAI_SObs_netcdf *obsin,// The netcdf-observer of which a selection 
                                 // is to be made
        CTA_Handle *userdata_in,   // User data: condition
     // OUTPUTS
        CTAI_SObs_netcdf *obsout, // The netcdf-observer which is a selection 
                                // of observer obsin
        int *retval)            // Error code
{
   CTA_Handle userdata=*userdata_in;
#if HAVE_LIBNETCDF
   CTA_Datatype datatype;

   if (IDEBUG > 0) { printf("ctai_sobs_netcf_createsel: obsin : %p  \n", obsin);}
   if (IDEBUG > 0) { printf("ctai_sobs_netcf_createsel: obsout : %p \n", obsout);}


   CTA_Handle_GetDatatype(userdata, &datatype);
   if (datatype==CTA_STRING){
   if (IDEBUG > 0) { printf("ctai_sobs_netcf_createsel: userdata STRING : %d   \n",userdata);}
      CTAI_SObs_netcdf_CreateSelString(obsin,userdata,obsout,retval); 
   } else if (datatype==CTA_RELTABLE){
   if (IDEBUG > 0) { printf("ctai_sobs_netcf_createsel: userdata RELTAB : %d  \n",userdata);}
      CTAI_SObs_netcdf_CreateSelRelTab(obsin,userdata,obsout,retval); 
   } else {
      printf("Error in CTAI_SObs_netcdf_CreateSel: datatype (%d) of selection is not supported\n", datatype);
      *retval=CTA_INPUT_OBJECT_NOT_SUPPORTED;
   }
#else
   printf("Error: CTAI_SObs_netcdf_CreateSel: COSTA is compiled without NETCDFsupport\n");
  *retval=CTA_NOT_IMPLEMENTED;
#endif
};


void CTAI_SObs_netcdf_Count(
// Return the number of measurements in the observer
   // INPUTS
      CTAI_SObs_netcdf *x,  // The StochObserver of which the number of measurements is
                                  //     returned 
   // OUTPUTS 
      int *nmeasr,             
      int *retval
   )
{
#if HAVE_LIBNETCDF
  *nmeasr = x->nmeasr;
  *retval = CTA_OK;
#else
   printf("Error: CTAI_SObs_netcdf_Count: COSTA is compiled without NETCDFsupport\n");
  *retval=CTA_NOT_IMPLEMENTED;
#endif
};


void CTAI_SObs_netcdf_GetVals(
// Get all the values from a netcdf-StochObserver
   // INPUTS
   CTAI_SObs_netcdf *x, // StochObserver from which the measurements are 
                         //    returned
   // OUTPUTS
   CTA_Vector *hvec,      // COSTA-vector containing the values
   int *retval
   )
{
#if HAVE_LIBNETCDF
   int ierr, ncid, varid;
   CTA_Datatype datatype;
   float *nc_vcd_values;
   CTA_Vector hvcd;

   /* Get datatype of vector */
   *retval=CTA_Vector_GetDatatype(*hvec,&datatype);
   if (*retval!=CTA_OK) return;


   /* read values from database. First read entire  vcd-array. Note: netcdf file should first be opened */
  // Get the netcdf-id of the database
   ncid = x->database->ncid;
   // read the vcd vector. We know the size: nmeasr_orig. OMI-vcd vector contains one float per entry.
   
   // first allocate space for temporary arrays
   nc_vcd_values = CTA_Malloc(x->nmeasr_orig * sizeof(float));

   ierr = nc_inq_varid(ncid, "vcd_trop", &varid);
   ierr = nc_get_var_float(ncid, varid, &nc_vcd_values[0]);
   if (ierr != CTA_OK)
     {printf("Error: could not read vcd_trop \n");
       *retval = -1;
       return;}

// now fill the values array with the vcd_trop. The  
//     relation table is used to get only the selection.
   ierr = CTA_Vector_Create(CTA_DEFAULT_VECTOR, x->nmeasr_orig, CTA_REAL, CTA_NULL,&hvcd);
   ierr = CTA_Vector_SetVals(hvcd, nc_vcd_values, x->nmeasr_orig, CTA_REAL);
   ierr = CTA_RelTable_Apply(x->selectionReltab, hvcd, *hvec);
   

   // TODO: use the kernel and lon/lat info to couple the no2-conc to specific imerid,izonal and iverti  
   // This is done in the model_obs! 



   CTA_Vector_Free(&hvcd);
   free(nc_vcd_values);
   if (*retval!=CTA_OK) return;

   *retval = CTA_OK;
#else
   printf("Error: CTAI_SObs_netcdf_GetVals: COSTA is compiled without NETCDFsupport\n");
  *retval=CTA_NOT_IMPLEMENTED;
#endif



};

void CTAI_SObs_netcdf_GetTimes(
// Get all the time values from a netcdf-StochObserver
   // INPUTS
   CTAI_SObs_netcdf *x, // StochObserver from which the measurements are 
                         //    returned
   // OUTPUTS
   CTA_Vector *hvec,      // COSTA-vector containing the values
   int *retval
   )
{
#if HAVE_LIBNETCDF
  int i, ierr, ncid, varid, idx;
   CTA_Datatype datatype;
   int * nc_time_values;
   double  *selected_values;
   int * shadow_vals;
   int * selected_shadow_vals;

   /* Get datatype of vector */
   *retval=CTA_Vector_GetDatatype(*hvec,&datatype);
   if (*retval!=CTA_OK) return;

   /* read values from database. First read entire time array. Note: netcdf file should first be opened */
  // Get the netcdf-id of the database
   ncid = x->database->ncid;
   // read the time vector. We know the size: nmeasr_orig. OMI-Time vector contains of six integers per entry.
   
   // first allocate space for temporary arrays
   nc_time_values = CTA_Malloc(x->nmeasr_orig * 6 * sizeof(int));
   selected_values = CTA_Malloc(x->nmeasr * sizeof(double));
   shadow_vals = CTA_Malloc(x->nmeasr_orig * sizeof(int));
   selected_shadow_vals = CTA_Malloc(x->nmeasr * sizeof(int));

   // use the relation table
   for (i=0; i < x->nmeasr_orig; i++) { shadow_vals[i] = i+1 ;}

   ierr = CTA_RelTable_ApplyVal(x->selectionReltab, shadow_vals, x->nmeasr_orig, 
                                           selected_shadow_vals,x->nmeasr, CTA_INTEGER);

   ierr = nc_inq_varid(ncid, "date_time", &varid);
   if (ierr != CTA_OK)
     {printf("Error: could not read date_time : %d error ;  ncid %d \n",ierr,ncid);
       *retval = -1;
       return;}
   ierr = nc_get_var_int(ncid, varid, &nc_time_values[0]);


// now fill the values array with the times. The times have to be re-computed to hours and 
//     the relation table is used to get only the selection.


       /* NOTE: times are adjusted with a certain offset! */
   for (i=0; i < x->nmeasr; i++) {
     idx = selected_shadow_vals[i]  ;       
     selected_values[i] = 1.0*nc_time_values[(idx-1)*6+3] + 
                              nc_time_values[(idx-1)*6+4]/60.0 + 
                              nc_time_values[(idx-1)*6+5]/3600.0  + x->timeoffset ;


   }

   //   printf("selected_shadow_vals %d \n",selected_shadow_vals[0]);
   if (IDEBUG>0) {printf("sobs_netcdf_gettimes: time values: count, first 3: %s %d %f %f %f \n",
             x->database->dbname,x->nmeasr, selected_values[0],selected_values[1],selected_values[2]);}


  /* Set values in vector and clean work-memory */
   *retval = CTA_Vector_SetVals( *hvec, selected_values, x->nmeasr, datatype);

   free(selected_values); 
   free(nc_time_values);
   free(selected_shadow_vals);
   free(shadow_vals);

   if (*retval!=CTA_OK) return;

   *retval = CTA_OK;
#else
   printf("Error: CTAI_SObs_netcdf_GetTimes: COSTA is compiled without NETCDFsupport\n");
  *retval=CTA_NOT_IMPLEMENTED;
#endif

};

void CTAI_SObs_netcdf_GetVariances(
// Get all the variances of the measurements in a netcdf-StochObserver
   // INPUTS
   CTAI_SObs_netcdf *x, // StochObserver from which the measurements are 
                        //    returned
   int *returnvar,      //return variance (CTA_TRUE) or std (CTA_FALSE)
   // OUTPUTS
   CTA_Vector *hvec,      // COSTA-vector containing the values

   int *retval
   )
{
#if HAVE_LIBNETCDF
   int ierr, ncid, varid;
   float *stdv;
   float *nc_values;

   // first allocate space for temporary arrays
   nc_values = CTA_Malloc(x->nmeasr_orig * sizeof(float));
   stdv=CTA_Malloc(x->nmeasr*sizeof(float));

   // Look up all the standard deviations of the measurements in the observer:

   /* read values from database. First read entire  vcd-array. Note: netcdf file should first be opened */

  // Get the netcdf-id of the database
   ncid = x->database->ncid;
   // read the vcd vector. We know the size: nmeasr_orig. OMI-vcd vector contains one float per entry.
   ierr = nc_inq_varid(ncid, "sigma_vcd_trop", &varid);
   if (ierr != 0) ierr = nc_get_var_float(ncid, varid, &nc_values[0]);
   if (ierr != 0)
     {printf("Error: could not read sigma_vcd_trop \n");
       *retval = -1; return;}

// now fill the stdv array. The relation table is used to get only the selection.
   ierr = CTA_RelTable_ApplyVal(x->selectionReltab, nc_values, x->nmeasr_orig, 
                                           stdv, x->nmeasr, CTA_REAL);
   if (ierr != CTA_OK)
     {printf("Error: reltable_applyval in netcdf_getvariances \n");
       *retval = -1; return;}

   // Fill in the variances in the variable *hvec

   *retval = CTA_Vector_SetVals( *hvec, stdv, x->nmeasr, CTA_REAL);
   if (*retval!=CTA_OK) return;

   *returnvar = CTA_FALSE;  //it is the std, not the var !
   *retval = CTA_OK;
#else
   printf("Error: CTAI_SObs_netcdf_GetVariances: COSTA is compiled without NETCDFsupport\n");
  *retval=CTA_NOT_IMPLEMENTED;
#endif

};


void CTAI_SObs_netcdf_GetRealisation(
// Calculate stochastic realizations for all the measurements in a netcdf-StochObserver
   // INPUTS
   CTAI_SObs_netcdf *x,  // StochObserver from which the measurements are 
                         //    returned
   // OUTPUTS
   CTA_Vector *hvec,      // COSTA-vector containing the realizations
   int *retval
   )
{
#if HAVE_LIBNETCDF
   double *values, *stdv;
   int i, ierr, returnvar;
   CTA_Vector  hvalues, hstdv;

   values=CTA_Malloc(x->nmeasr*sizeof(double));
   stdv=CTA_Malloc(x->nmeasr*sizeof(double));

   ierr = CTA_Vector_Create(CTA_DEFAULT_VECTOR, x->nmeasr, CTA_DOUBLE, CTA_NULL, &hvalues);
   ierr = CTA_Vector_Create(CTA_DEFAULT_VECTOR, x->nmeasr, CTA_DOUBLE, CTA_NULL, &hstdv);

   // Look up the expectations
   CTAI_SObs_netcdf_GetVals( x, &hvalues,&ierr);
   if (ierr != CTA_OK) return;

   // Look up the standard deviations
   CTAI_SObs_netcdf_GetVariances( x, &hstdv,&returnvar, &ierr);
   if (ierr!=CTA_OK) return;

   ierr = CTA_Vector_GetVals(hvalues, values, x->nmeasr, CTA_DOUBLE);
   ierr = CTA_Vector_GetVals(hstdv, stdv, x->nmeasr, CTA_DOUBLE);

   // Calculate realizations of all the measurements
   for (i=0;i<x->nmeasr;i++)
   {
      double r;
      *retval = CTA_rand_n(&r);
      if (*retval!=CTA_OK) return;

      if (IDEBUG>0) {printf("cta_sobs_getrealisations:%d:  val: %f std: %f r: %f \n", i, values[i],stdv[i],r);}

      if (returnvar == CTA_TRUE) {
        values[i] = values[i] + r*stdv[i];}
      else {
        values[i] = values[i] + r*stdv[i]*stdv[i];
      }


   }

   // Fill in the results in the vector
   *retval = CTA_Vector_SetVals( *hvec, values, x->nmeasr, CTA_DOUBLE);
   free(values);
   free(stdv);
   ierr = CTA_Vector_Free(&hvalues);
   ierr = CTA_Vector_Free(&hstdv);

   
   if (*retval!=CTA_OK) return;

  *retval = CTA_OK;
#else
   printf("Error: CTAI_SObs_netcdf_GetRealisation: COSTA is compiled without NETCDFsupport\n");
  *retval=CTA_NOT_IMPLEMENTED;
#endif
};

void CTAI_SObs_netcdf_GetCovMat(
// Get all the variances of the measurements in a netcdf-StochObserver
   // INPUTS
   CTAI_SObs_netcdf *x,  // StochObserver from which the measurements are 
                         //    returned
   // OUTPUTS
   CTA_Matrix *hmat,      /* Covariance matrix */
   int *retval
   )
{
#if HAVE_LIBNETCDF
   double *matval;
   int i, ierr, ncid, varid;
   float *stdv;
   float *nc_values;

   // first allocate space for temporary arrays
   nc_values = CTA_Malloc(x->nmeasr_orig * sizeof(float));
   stdv=CTA_Malloc(x->nmeasr*sizeof(float));

   // Look up all the standard deviations of the measurements in the observer:

   /* read values from database. First read entire  vcd-array. Note: netcdf file should first be opened */
  // Get the netcdf-id of the database
   ncid = x->database->ncid;
   // read the vcd vector. We know the size: nmeasr_orig. OMI-vcd vector contains one float per entry.
   ierr = nc_inq_varid(ncid, "sigma_vcd_trop", &varid);
   ierr = nc_get_var_float(ncid, varid, &nc_values[0]);
   if (IDEBUG>0) {printf("variance has been read %f \n",nc_values[0]);} 


   if (ierr != CTA_OK)
     {printf("Error: could not read sigma_vcd_trop \n");
       *retval = -1; return;}

// now fill the stdv array. The relation table is used to get only the selection.
   ierr = CTA_RelTable_ApplyVal(x->selectionReltab, nc_values, x->nmeasr_orig, 
                                           stdv, x->nmeasr, CTA_REAL);
   if (ierr != CTA_OK)
     {printf("Error: reltable_applyval in netcdf_getcovmat; nobs:%d of %d  \n",x->nmeasr,x->nmeasr_orig);
       *retval = -1; return;}

   // Fill in the variances in the variable matval (matrix values)

   if (IDEBUG>0) {printf("filling matrix of size %d...%f  %f \n",x->nmeasr,stdv[0],stdv[1]);} 

   matval=CTA_Malloc(x->nmeasr * x->nmeasr * sizeof(double));
   for (i=0;i < x->nmeasr * x->nmeasr ; i++){matval[i]=0.0;}

   for (i=0;i < x->nmeasr ; i++){
     matval[i * x->nmeasr + i] = stdv[i]*stdv[i];
   }
   free(stdv);
   free(nc_values);

   *retval = CTA_Matrix_SetVals( *hmat, matval, x->nmeasr, x->nmeasr,
                                 CTA_DOUBLE);
   free(matval);

   if (IDEBUG>0) {printf("getcovmat completed!  \n");} 

   if (*retval!=CTA_OK) return;
   *retval=CTA_OK;

   *retval = CTA_OK;
return ;
#else
   printf("Error: CTAI_SObs_netcdf_GetCovMat: COSTA is compiled without NETCDFsupport\n");
  *retval=CTA_NOT_IMPLEMENTED;
#endif
};


void CTAI_SObs_netcdf_export(
   CTAI_SObs_netcdf *x,
   CTA_Handle *userdata,
   int *retval
  )
{
#if HAVE_LIBNETCDF
   // The user-data must be 0, 1 or 2 items: 
   //  the output file (default: stdout)
   //  and a format-string (default: empty).
   //
   // Look up the output file
   FILE *file;
   int i, ierr;
   CTA_Vector hvec_time, hvec_vals;
   double dval1,dval2;

   if (CTA_Handle_Check(*userdata,CTA_FILE)==CTA_OK) {
      *retval = CTA_File_Get(*userdata,&file);

      if (CTA_FLUSH_ALWAYS) CTA_Flush();


      // Print the contents of the observer.
      fprintf(file,"%% This Observer has the following properties: \n");

      if (x->nmeasr == 0 ) {
        fprintf(file,"This is observer of an EMPTY database\n");}
      else { 


      fprintf(file,"   dbasename='%s';\n",x->database->dbname);
      fprintf(file,"   nusers=%d;\n",x->database->nusers);
      fprintf(file," nmeasr_orig; %d  nmeasr=%d;\n\n",x->nmeasr_orig, x->nmeasr);

      ierr = CTA_Vector_Create(CTA_DEFAULT_VECTOR, x->nmeasr, CTA_DOUBLE, CTA_NULL,&hvec_time);
      ierr = CTA_Vector_Create(CTA_DEFAULT_VECTOR, x->nmeasr, CTA_DOUBLE, CTA_NULL,&hvec_vals);
      CTAI_SObs_netcdf_GetTimes(x, &hvec_time, &ierr);
      CTAI_SObs_netcdf_GetVals(x, &hvec_vals, &ierr);

      for (i=1; i <= x->nmeasr; i++) {

        CTA_Vector_GetVal(hvec_time, i, &dval1, CTA_DOUBLE);
        CTA_Vector_GetVal(hvec_vals, i, &dval2, CTA_DOUBLE);

        fprintf(file," nr:%d  time: %f  value: %f \n",i,dval1, dval2);        
      }
      CTA_Vector_Free(&hvec_time);
      CTA_Vector_Free(&hvec_vals);

      }
   };
#else
   printf("Error: CTAI_SObs_netcdf_export: COSTA is compiled without NETCDFsupport\n");
  *retval=CTA_NOT_IMPLEMENTED;
#endif

};

void CTAI_SObs_netcdf_Free(
   CTAI_SObs_netcdf *x,
   int *retval
  )
{
#if HAVE_LIBNETCDF
  int ierr;
  /* We only are allowed to close the database if all users (including all selectioned observers) are gone! */
  //  if (x->ncid != 0) {
  //    if ((ierr = nc_close(x->ncid))){
  //      printf("CTA_Sobs_netcdf_Free: cannot close netCDF-file: %s\n",
  //             nc_strerror(retval));
  //    }
  //  }
  x->database->nusers--;
  //  printf("cta_sobs_free (netcdf): now %d users remaining of database %s \n",x->database->nusers, x->database->dbname);
 
  ierr = CTA_RelTable_Free(&(x->selectionReltab));
  if (ierr !=CTA_OK)
    {printf("CTA_Sobs_netcdf_Free: cannot close relation table \n");}

  if (x->database->nusers == 0) {
    if ((ierr = nc_close(x->database->ncid))){
        printf("CTA_Sobs_netcdf_Free: cannot close netCDF-file: %s\n",
               nc_strerror(ierr));
    }
    free(x->database->dbname);
    free(x->database);

  }
  
  *retval = CTA_OK;
#else
   printf("Error: CTAI_SObs_netcdf_Free: COSTA is compiled without NETCDFsupport\n");
  *retval=CTA_NOT_IMPLEMENTED;
#endif
}



/*

*/
void CTA_SObs_netcdf_initialise(CTA_SObsClass *hsobscl)
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
   CTA_Func_Create(" ",&CTAI_SObs_netcdf_Create_Size,    hintf,
                       &h_func[CTA_SOBS_CREATE_SIZE]);
   CTA_Func_Create(" ",&CTAI_SObs_netcdf_Create_Init,    hintf,
                       &h_func[CTA_SOBS_CREATE_INIT]);
   CTA_Func_Create(" ",&CTAI_SObs_netcdf_CreateSel  ,    hintf,
                       &h_func[CTA_SOBS_CREATE_SELECTION]);
   CTA_Func_Create(" ",&CTAI_SObs_netcdf_Count,          hintf,
                       &h_func[I_CTA_SOBS_COUNT]      );
   CTA_Func_Create(" ",&CTAI_SObs_netcdf_GetVals,        hintf,
                       &h_func[CTA_SOBS_GET_VALUES] );
   CTA_Func_Create(" ",&CTAI_SObs_netcdf_GetTimes,       hintf,
                       &h_func[CTA_SOBS_GET_TIMES] );
   CTA_Func_Create(" ",&CTAI_SObs_netcdf_GetVals,        hintf,
                       &h_func[CTA_SOBS_GET_EXPECTATION]);
   CTA_Func_Create(" ",&CTAI_SObs_netcdf_GetRealisation, hintf,
                       &h_func[CTA_SOBS_GET_REALISATION]);
   CTA_Func_Create(" ",&CTAI_SObs_netcdf_GetVariances,   hintf,
                       &h_func[CTA_SOBS_GET_VARIANCE]);
   CTA_Func_Create(" ",&CTAI_SObs_netcdf_GetCovMat,      hintf,
                       &h_func[CTA_SOBS_GET_COV_MATRIX]);
   CTA_Func_Create(" ",&CTAI_SObs_netcdf_export,     hintf,
                       &h_func[I_CTA_SOBS_EXPORT]     );
   CTA_Func_Create(" ",&CTAI_SObs_netcdf_Free,     hintf,
                            &h_func[I_CTA_SOBS_FREE]     );
   
   CTA_ObsDescr_netcdf_initialise(&descrcl);

   CTA_SObs_DefineClass("cta_sobs_netcdf",h_func,descrcl,hsobscl);


}





