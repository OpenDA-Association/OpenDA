/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_sobs_sqlite3.c $
$Revision: 4304 $, $Date: 2014-01-14 07:29:40 +0100 (Tue, 14 Jan 2014) $

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
#include "cta_defaults.h"
#include "cta_flush.h"
#include "cta_util_statistics.h"
#include "cta_util_sqlite3.h"
#include "cta_file.h"
#include "cta_string.h"
#include "cta_obsdescr_sqlite3.h"
#include "cta_sobs_sqlite3.h"
#include "cta_errors.h"
#include "cta_message.h"

#define IDEBUG (0)

#define CLASSNAME "CTA_SObs_sqlite3"
/*

*/
void CTA_SObs_sqlite3_initialise(CTA_SObsClass *hsobscl)
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
   CTA_Func_Create(" ",&CTAI_SObs_sqlite3_Create_Size,    hintf,
                       &h_func[CTA_SOBS_CREATE_SIZE]);
   CTA_Func_Create(" ",&CTAI_SObs_sqlite3_Create_Init,    hintf,
                       &h_func[CTA_SOBS_CREATE_INIT]);
   CTA_Func_Create(" ",&CTAI_SObs_sqlite3_CreateSel  ,    hintf,
                       &h_func[CTA_SOBS_CREATE_SELECTION]);
   CTA_Func_Create(" ",&CTAI_SObs_sqlite3_Count,          hintf,
                       &h_func[I_CTA_SOBS_COUNT]      );
   CTA_Func_Create(" ",&CTAI_SObs_sqlite3_GetVals,        hintf,
                       &h_func[CTA_SOBS_GET_VALUES] );
   CTA_Func_Create(" ",&CTAI_SObs_sqlite3_GetTimes,       hintf,
                       &h_func[CTA_SOBS_GET_TIMES] );
   CTA_Func_Create(" ",&CTAI_SObs_sqlite3_GetVals,        hintf,
                       &h_func[CTA_SOBS_GET_EXPECTATION]);
   CTA_Func_Create(" ",&CTAI_SObs_sqlite3_GetRealisation, hintf,
                       &h_func[CTA_SOBS_GET_REALISATION]);
   CTA_Func_Create(" ",&CTAI_SObs_sqlite3_GetVariances,   hintf,
                       &h_func[CTA_SOBS_GET_VARIANCE]);
   CTA_Func_Create(" ",&CTAI_SObs_sqlite3_GetCovMat,      hintf,
                       &h_func[CTA_SOBS_GET_COV_MATRIX]);
   CTA_Func_Create(" ",&CTAI_SObs_sqlite3_export,     hintf,
                       &h_func[I_CTA_SOBS_EXPORT]     );
   CTA_Func_Create(" ",&CTAI_SObs_sqlite3_Free,     hintf,
                            &h_func[I_CTA_SOBS_FREE]     );
   
   CTA_ObsDescr_sqlite3_initialise(&descrcl);

   CTA_SObs_DefineClass("cta_sobs_sqlite3",h_func,descrcl,hsobscl);


}


void CTAI_SObs_sqlite3_Create_Size(
   // OUTPUTS:
      int *memsize,           // The number of bytes which are necessary to store one 
                              //     CTAI_SObs_sqlite3, with a pointer to the contents (data), 
                              //     but without the contents themselves.
      int *retval             // error code (see cta_datatypes.h for possible error codes)
   ){
   *memsize=(int) sizeof(CTAI_SObs_sqlite3);
   *retval=CTA_OK;
};





#define METHOD "CTAI_SObs_sqlite3_Create_Init"
void CTAI_SObs_sqlite3_Create_Init(
/*
   Allocate the memory which is necessary to store the data necessary for a
   sqlite3-observer
*/
     // IN-OUTPUTS
        CTAI_SObs_sqlite3 *x,   // The sqlite3-observer for which the memory must 
                                //     be allocated
     // INPUTS:
        CTA_Handle userdata,   // User data: database-name
     // OUTPUTS
        int *retval)            // Error code. Possible error: Illegal data type
{
   int len;
   char *dbname;
   char *condition=CTA_Malloc(sizeof(char));
   int nstations,nmeasr;
   CTAI_util_sqlite3_database *database;
   sqlite3 *db;
   int rc;
   int *stations;
   char message[256];
   FILE *file;

   condition[0]=0;
   database = CTA_Malloc(sizeof(CTAI_util_sqlite3_database));


   // Get the name of the database file:
   // Check that indeed only one userdata-item is passed: the database name
   // Allocate a name-string 
   *retval = CTA_String_GetLength(userdata, &len);
   if (*retval!=CTA_OK) return;
   // Get the name of the database
   dbname = CTA_Malloc((len+1)*sizeof(char));
   *retval = CTA_String_Get(userdata, dbname);
   if (*retval!=CTA_OK) return;

   // Open the database
   //
   // First check whether the file exists
   //
   if ((file = fopen(dbname, "r"))){
      fclose(file);
   }
   else {
      sprintf(message,"Cannot open observation database file %s",dbname);
      CTA_WRITE_ERROR(message);		   
      *retval=CTA_CANNOT_OPEN_FILE;
      free(database);
      free(dbname);
      return;
   }



   rc = sqlite3_open(dbname, &db);
   if ( rc!=SQLITE_OK ){
      sprintf(message,"Cannot open observation database file %s",dbname);
      CTA_WRITE_ERROR(message);		   
      sqlite3_close(db);
      *retval=CTA_CANNOT_OPEN_FILE;
      free(database);
      free(dbname);
      return;
   }


   // Count the stations in this observer
   *retval = CTAI_util_sqlite3_select_values(&nstations, 1, CTA_INTEGER, db,
       "count(distinct stations.station_id)",condition);
   if (*retval!=CTA_OK) return;

   // Save the stations_ids of this observer in array stations
   stations=CTA_Malloc(nstations*sizeof(int));
   *retval = CTAI_util_sqlite3_select_values(stations, nstations, CTA_INTEGER, db,
       "distinct stations.station_id",condition);
   if (*retval!=CTA_OK) return;
   
   //printf("sobs_sqlite_create_init: debug3: %d \n ",*retval);

   // Count the measurements in this observer
   *retval = CTAI_util_sqlite3_select_values(&nmeasr, 1, CTA_INTEGER, db,
       "count(stations.station_id)",condition);
   if (*retval!=CTA_OK) return;

   // Set the observer-fields
    
   database->db        = db;
   database->name      = dbname;
   database->nusers    = 1;

   x->database  = database;
   x->nstations = nstations;
   x->stations  = stations;
   x->nmeasr    = nmeasr;
   x->condition = condition;
   *retval=CTA_OK;
};


void CTAI_SObs_sqlite3_CreateSelString(
/*
   Allocate the memory which is neccesary to store the data necessary for a
   sqlite3-observer
*/
     // INPUTS
        CTAI_SObs_sqlite3 *obsin,// The sqlite3-observer of which a selection 
                                 // is to be made
        CTA_Handle userdata,   // User data: condition
     // OUTPUTS
        CTAI_SObs_sqlite3 *obsout, // The sqlite3-observer which is a selection 
                                // of observer obsin
        int *retval)            // Error code
{
      int len;
      char *condition;

   // Get the condition
      // Allocate a name-string 
      *retval = CTA_String_GetLength(userdata, &len);
      if (*retval!=CTA_OK) return;

      // Get the condition
      condition = CTA_Malloc((len+1)*sizeof(char));
      *retval = CTA_String_Get(userdata, condition);
      if (*retval!=CTA_OK) return;

   // Link the database also to this observer and keep track of the number of 
   // observers using this database
   obsout->database = obsin->database;
   obsout->database->nusers++;

   // Combine the input-condition and the condition of the input observer.
   if (strcmp(obsin->condition,"")==0)
   {
      obsout->condition = condition;
   }
   else
   {
      obsout->condition = CTA_Malloc(sizeof(char)*
                                  (    strlen(condition) 
                                    +  strlen(obsin->condition)
                                    +  strlen("  (  ) AND (  )  ")
                                ) );
      sprintf(obsout->condition,"(%s) AND (%s)",condition,obsin->condition);
      if (IDEBUG) printf("CTAI_SObs_sqlite3_CreateSel: DEBUG condition=%s\n",
                         obsout->condition);
      free(condition);
   }

   // Count the stations in this observer
   *retval = CTAI_util_sqlite3_select_values(
       &(obsout->nstations), 1, CTA_INTEGER, obsout->database->db,
       "count(distinct stations.station_id)",obsout->condition);
   if (*retval!=CTA_OK) return;

   // Save the stations_ids of this observer in array stations
   obsout->stations=CTA_Malloc((obsout->nstations)*sizeof(int));
   *retval = CTAI_util_sqlite3_select_values(
       obsout->stations, obsout->nstations, CTA_INTEGER, obsout->database->db,
       "distinct stations.station_id",obsout->condition);
   if (*retval!=CTA_OK) return;
   
   // Count the measurements in this observer
   *retval = CTAI_util_sqlite3_select_values(
       &(obsout->nmeasr), 1, CTA_INTEGER, obsout->database->db,
       "count(stations.station_id)",obsout->condition);

   if (*retval!=CTA_OK) return;

   *retval=CTA_OK;
};


void CTAI_SObs_sqlite3_CreateSelRelTab( CTAI_SObs_sqlite3 *obsin, CTA_RelTable reltab, 
        CTAI_SObs_sqlite3 *obsout, int *retval) 
{
   int nmeasr;
   int isel, nsel;
   int *rowIds, *selRowIds;
   CTA_Handle hdum=0;
   CTA_Vector vRowIds, vSelRowIds;
   CTA_String sSel;
   char sNum[10];
   char *sel;

   /* Get number of observations */
   CTAI_SObs_sqlite3_Count(obsin, &nmeasr, retval);
   if (*retval!=CTA_OK) return;
    
   /* Create a vector with the rowID's */
   rowIds=CTA_Malloc(nmeasr*sizeof(int));
    *retval = CTAI_util_sqlite3_select_values(
               rowIds,  nmeasr,  CTA_INTEGER,  obsin->database->db,
               "data._ROWID_", obsin->condition);
   if (*retval!=CTA_OK) return;

   *retval=CTA_Vector_Create(CTA_DEFAULT_VECTOR, nmeasr, CTA_INTEGER, hdum, &vRowIds);
   if (*retval!=CTA_OK) return;
   *retval=CTA_Vector_SetVals(vRowIds,rowIds,nmeasr,CTA_INTEGER);
   if (*retval!=CTA_OK) return;
   free(rowIds);

   /* Create a vector with the selected rowID's */
   CTA_RelTable_Count(reltab,&nsel);
   selRowIds=CTA_Malloc(nsel*sizeof(int));
   *retval=CTA_Vector_Create(CTA_DEFAULT_VECTOR, nsel, CTA_INTEGER, hdum, &vSelRowIds);
   if (*retval!=CTA_OK) return;

   /* Apply the relation table */
   *retval=CTAI_RelTable_Apply(reltab, vRowIds, vSelRowIds, CTA_FALSE);
   if (*retval!=CTA_OK){
      printf("Error in CTAI_SObs_sqlite3_CreateSelRelTab: Cannot apply selection relation table \n");
      return;
   }

   /* Create a selection criterion for */
   *retval=CTA_Vector_GetVals(vSelRowIds,selRowIds,nsel,CTA_INTEGER);
   if (*retval!=CTA_OK) return;

   sel=CTA_Malloc((2+nsel*(4+10+16)+2+1)*sizeof(char));
   strcpy(sel,"( ");                       //2
   for (isel=0;isel<nsel;isel++){
      if (isel>0) {
         strcat(sel," OR ");               //4
      }
      sprintf(sNum,"%d",selRowIds[isel]);  //10
      strcat(sel," data._ROWID_ = ");      //16
      strcat(sel,sNum);
   }
   strcat(sel," )");                       //2

   printf("========================================\n");
   printf("%s\n",sel);
   printf("========================================\n");

   /* Create the actual selection */   
   CTA_String_Create(&sSel);
   CTA_String_Set(sSel,sel);
   CTAI_SObs_sqlite3_CreateSelString(obsin, sSel, obsout, retval);
   CTA_String_Free(&sSel);
   if (*retval !=CTA_OK){
      printf("CTAI_SObs_sqlite3_CreateSelRelTab: Error creating selection '%s' \n",sel);
   }

   /* Free work variables */
   free(selRowIds);
   free(sel);
};












void CTAI_SObs_sqlite3_CreateSel(
/*
   Allocate the memory which is neccesary to store the data necessary for a
   sqlite3-observer
*/
     // INPUTS
        CTAI_SObs_sqlite3 *obsin,// The sqlite3-observer of which a selection 
                                 // is to be made
        CTA_Handle *userdata_in,   // User data: condition
     // OUTPUTS
        CTAI_SObs_sqlite3 *obsout, // The sqlite3-observer which is a selection 
                                // of observer obsin
        int *retval)            // Error code
{
   CTA_Handle userdata=*userdata_in;
   CTA_Datatype datatype;

   CTA_Handle_GetDatatype(userdata, &datatype);
   if (datatype==CTA_STRING){
      CTAI_SObs_sqlite3_CreateSelString(obsin,userdata,obsout,retval); 
   } else if (datatype==CTA_RELTABLE){
      CTAI_SObs_sqlite3_CreateSelRelTab(obsin,userdata,obsout,retval); 
   } else {
      printf("Error in CTAI_SObs_sqlite3_CreateSel: datatype (%d) of selection is not supported\n",datatype);
      *retval=CTA_INPUT_OBJECT_NOT_SUPPORTED;
   }
};


void CTAI_SObs_sqlite3_Count(
// Return the number of measurements in the observer
   // INPUTS
      CTAI_SObs_sqlite3 *x,  // The StochObesrver of which the number of measurements is
                                  //     returned 
   // OUTPUTS 
      int *nmeasr,
      int *retval
   )
{
   /* Local variables */
   *nmeasr = x->nmeasr;
   *retval = CTA_OK;
};


void CTAI_SObs_sqlite3_GetVals(
// Get all the values from a sqlite3-StochObserver
   // INPUTS
   CTAI_SObs_sqlite3 *x, // StochObserver from which the measurements are 
                         //    returned
   // OUTPUTS
   CTA_Vector *hvec,      // COSTA-vector containing the values
   int *retval
   )
{
   void * values;
   int size;
   CTA_Datatype datatype;

   /* Get datatype of vector */
   *retval=CTA_Vector_GetDatatype(*hvec,&datatype);
   if (*retval!=CTA_OK) return;

   /* allocate space for holding result */
   *retval = CTA_SizeOf(datatype,&size);
   if (*retval!=CTA_OK) return;

   values=CTA_Malloc(x->nmeasr*size);

   /* read values from database */
   *retval = CTAI_util_sqlite3_select_values(
                  values,  x->nmeasr,  datatype,  x->database->db,
                  "value", x->condition);
   if (*retval!=CTA_OK) return;

  /* Set values in vector en clean work-memory */
   *retval = CTA_Vector_SetVals( *hvec, values, x->nmeasr, datatype);
   free(values);
   if (*retval!=CTA_OK) return;



   *retval = CTA_OK;
};

void CTAI_SObs_sqlite3_GetTimes(
// Get all the times from a sqlite3-StochObserver
   // INPUTS
   CTAI_SObs_sqlite3 *x, // StochObserver from which the measurements are 
                         //    returned
   // OUTPUTS
   CTA_Vector *hvec,      // COSTA-vector containing the values
   int *retval
   )
{
   void * values;
   int size;
   CTA_Datatype datatype;

   /* Get datatype of vector */
   *retval=CTA_Vector_GetDatatype(*hvec,&datatype);
   if (*retval!=CTA_OK) return;

   /* allocate space for holding result */
   *retval = CTA_SizeOf(datatype,&size);
   if (*retval!=CTA_OK) return;

   values=CTA_Malloc(x->nmeasr*size);

   /* read values from database */
   *retval = CTAI_util_sqlite3_select_values(
                  values,  x->nmeasr,  datatype,  x->database->db,
                  "time", x->condition);
   if (*retval!=CTA_OK) return;

  /* Set values in vector en clean work-memory */
   *retval = CTA_Vector_SetVals( *hvec, values, x->nmeasr, datatype);
   free(values);
   if (*retval!=CTA_OK) return;

   *retval = CTA_OK;
};

void CTAI_SObs_sqlite3_GetVariances(
// Get all the variances of the measurements in a sqlite3-StochObserver
   // INPUTS
   CTAI_SObs_sqlite3 *x, // StochObserver from which the measurements are 
                         //    returned
   int *returnvar,        //return variance (CTA_TRUE) or std (CTA_FALSE)
   // OUTPUTS
   CTA_Vector *hvec,      // COSTA-vector containing the values
   int *retval
   )
{
   double *var;
   float  *rvar;
   int i;
   int size;
   CTA_Datatype datatype;

   /* Get datatype of vector */
   *retval=CTA_Vector_GetDatatype(*hvec,&datatype);
   if (*retval!=CTA_OK) return;

   /* allocate space for holding result */
   *retval = CTA_SizeOf(datatype,&size);
   if (*retval!=CTA_OK) return;

   var=CTA_Malloc(x->nmeasr*size);
   // Look up all the standard deviations of the measurements in the observer
   *retval = CTAI_util_sqlite3_select_values(
                  var,  x->nmeasr,  datatype,  x->database->db,
                  "standarddeviation",x->condition);
   if (*retval!=CTA_OK) return;

   // Calculate the variances (square of the standard deviations)
   if (*returnvar==CTA_TRUE){
      if (datatype==CTA_REAL) {
         rvar=(float*) var;
         for (i=0;i<x->nmeasr;i++)
            { rvar[i] = rvar[i]*rvar[i]; };
      }else
      {
         for (i=0;i<x->nmeasr;i++)
            { var[i] = var[i]*var[i]; };
      }
   }

   // Fill in the variances in the vector
   *retval = CTA_Vector_SetVals( *hvec, var, x->nmeasr, datatype);
   free(var);
   if (*retval!=CTA_OK) return;
   *retval=CTA_OK;
   
};


void CTAI_SObs_sqlite3_GetRealisation(
// Calculate stochastic realizations for all the measurements in a sqlite3-StochObserver
   // INPUTS
   CTAI_SObs_sqlite3 *x,  // StochObserver from which the measurements are 
                         //    returned
   // OUTPUTS
   CTA_Vector *hvec,      // COSTA-vector containing the realizations
   int *retval
   )
{
   double *values, *stdv;
   int i;

   values=CTA_Malloc(x->nmeasr*sizeof(double));
   stdv=CTA_Malloc(x->nmeasr*sizeof(double));
   // Look up the expectations
   *retval = CTAI_util_sqlite3_select_values(
                  values,  x->nmeasr,  CTA_DOUBLE,  x->database->db,
                  "value",x->condition);
   if (*retval!=CTA_OK) return;

   // Look up the standard deviations
   *retval = CTAI_util_sqlite3_select_values(
                  stdv,  x->nmeasr,  CTA_DOUBLE,  x->database->db,
                  "standarddeviation",x->condition);
   if (*retval!=CTA_OK) return;

   // Calculate realizations of all the measurements
   for (i=0;i<x->nmeasr;i++)
   {
      double r;
      *retval = CTA_rand_n(&r);
      if (*retval!=CTA_OK) return;
      values[i] = values[i] + r*stdv[i];
   }

   // Fill in the results in the vector
   *retval = CTA_Vector_SetVals( *hvec, values, x->nmeasr, CTA_DOUBLE);
   free(values);
   free(stdv);
   
   if (*retval!=CTA_OK) return;
   
   *retval=CTA_OK;
};

void CTAI_SObs_sqlite3_GetCovMat(
// Get all the variances of the measurements in a sqlite3-StochObserver
   // INPUTS
   CTAI_SObs_sqlite3 *x,  // StochObserver from which the measurements are 
                         //    returned
   // OUTPUTS
   CTA_Matrix *hmat,      /* Covariance matrix */
   int *retval
   )
{
   double *matval;
   int i;
   double *stdv;
   stdv=CTA_Malloc(x->nmeasr*sizeof(double));

   // Look up all the standard deviations of the measurements in the observer
   *retval = CTAI_util_sqlite3_select_values(
                  stdv,  x->nmeasr,  CTA_DOUBLE,  x->database->db,
                  "standarddeviation",x->condition);
   if (*retval!=CTA_OK) return;

   // Fill in the variances in the variable matval (matrix values)

   matval=CTA_Malloc(x->nmeasr * x->nmeasr * sizeof(double));
   for (i=0;i < x->nmeasr * x->nmeasr ; i++){matval[i]=0.0;}

   for (i=0;i < x->nmeasr ; i++){
      matval[i * x->nmeasr + i] = stdv[i]*stdv[i];
   }
   free(stdv);
   *retval = CTA_Matrix_SetVals( *hmat, matval, x->nmeasr, x->nmeasr,
                                 CTA_DOUBLE);
   free(matval);
   if (*retval!=CTA_OK) return;
   *retval=CTA_OK;
   
};




static int CTAI_SObs_printen(void *vfile, int argc, char **argv, char **azColName){
  int i;
  FILE * file = *((FILE **) vfile);
  for(i=0; i<argc; i++){
    fprintf(file,"%s = %s\n", azColName[i], argv[i] ? argv[i] : "NULL");
  }
  fprintf(file,"\n");
  return 0;
}

void CTAI_SObs_sqlite3_export(
   CTAI_SObs_sqlite3 *x,
   CTA_Handle *userdata,
   int *retval
  )
{
   // The user-data must be 0, 1 or 2 items: 
   //  the output file (default: stdout)
   //  and a format-string (default: empty).
   //
   // Look up the output file
   FILE *file;
   int i;

   if (CTA_Handle_Check(*userdata,CTA_FILE)==CTA_OK) {
      *retval = CTA_File_Get(*userdata,&file);

      if (CTA_FLUSH_ALWAYS) CTA_Flush();


      // Print the contents of the observer.
      fprintf(file,"%% This Observer has the following properties: \n");

      if (x->nmeasr == 0 ) {
        fprintf(file,"This is observer of an EMPTY database\n");}
      else { 


      fprintf(file,"   dbasename='%s';\n",x->database->name);
      fprintf(file,"   nusers=%d;\n",x->database->nusers);
      fprintf(file,"   condition='%s';\n",x->condition);
      fprintf(file,"   stations=");

      for (i=0; i < x->nstations; i++)
         { fprintf(file,"%d ",x->stations[i]); }
      fprintf(file,"\n");
      fprintf(file,"   nmeasr=%d;\n\n",x->nmeasr);

      // Optionally, print also the values, using the appropriate SQL-command
      //if (strcmp(format,"values also")==0)
      {
        char *command;
        char *zErrMsg;

        int rc;
        // Set the constants necessary 
        const char *whole_table = // The expression which 
                       // produces the complete obs-table
           "stations inner join data on stations.station_id = data.station_id";

        command=CTA_Malloc((
                   strlen("select * from where") +
                   strlen(whole_table)     +
                   strlen(x->condition))*sizeof(char));

        if (strcmp(x->condition,"")==0)
        { 
            sprintf(command,"select * from %s",whole_table); 
        }
        else
        { 
           sprintf(command,"select * from %s where %s",
                     whole_table,x->condition); 
        }

        zErrMsg = NULL;

        rc = sqlite3_exec(x->database->db, command, CTAI_SObs_printen,
                      (void *) &file, &zErrMsg);
      //  if (zErrMsg!=NULL){
      //     printf("Error message from sqlite3: %s\n", &zErrMsg);
      //  }
        free(command);
        if( rc != SQLITE_OK ) {*retval=CTA_INVALID_COMMAND;return;};

      }
      }

      if (CTA_FLUSH_ALWAYS) CTA_Flush();

      *retval=CTA_OK;
   }
   else {
      *retval=CTA_FORMAT_NOT_SUPPORTED;
   }
};

void CTAI_SObs_sqlite3_Free(
   CTAI_SObs_sqlite3 *x,
   int *retval
  )
{
    x->database->nusers--;
    if (x->database->nusers==0)
    {
       sqlite3_close(x->database->db);
       free(x->database->name);
       free(x->database);
    }
    free(x->stations);
    free(x->condition);

    *retval = CTA_OK;
}
