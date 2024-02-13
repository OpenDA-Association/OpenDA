/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_util_sqlite3.c $
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

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "cta_mem.h"
#include "cta_flush.h"
#include "cta_util_sqlite3.h"
#include "cta_file.h"
#include "cta_string.h"
#include "cta_errors.h"

#define IDEBUG (0)


int CTAI_util_sqlite3_return_values(void *vout,
         int argc, char **argv, char **azColName)
// Callback-function for sqlite3
{
  int retval;
  int *values;

  // Cast the input to a counter-and-vector-struct
  CTAI_counter_vector *out = (CTAI_counter_vector*) vout;

  // Perform checks
  if (argc != 1) {return CTA_DIMENSION_ERROR;};
  if (out->index >= out->dimension) {return CTA_DIMENSION_ERROR;};

  if (out->datatype == CTA_INTEGER) 
  {
  // Add an integer to the output vector:

     // Read the value
     int iNAN =-987654;
     int value= iNAN;
     if (argv[0] != NULL) 
     { 
       int nscan = sscanf(argv[0],"%d",&value);
       if (nscan != 1) {return CTA_ILLEGAL_DATATYPE;}
     }

     // Cast the values-field to an integer-array and store the value
     values = (int *) out->values;
     values[out->index] = value;
  }
  else if(out->datatype == CTA_DOUBLE) 
  {
  // Add a double to the output vector:

     // Read the value
     double NaN;
     double value;
     double *values;

     NaN = 0; NaN = 1.0/NaN; NaN=NaN/NaN;
     value=NaN;

     if (argv[0] != NULL) 
     {
       int nscan = sscanf(argv[0],"%lf",&value);
       if (nscan != 1) {return CTA_ILLEGAL_DATATYPE;}
     }

     // Cast the values-field to a double-array and store the value
     values = (double *) out->values;
     values[out->index] = value;
  }
  else if(out->datatype == CTA_REAL) 
  {
  // Add a float to the output vector:

     // Read the value
     float NaN;
     float value;
     float *values;
     NaN = 0;
     NaN = (float) 1.0/NaN;
     NaN=NaN/NaN;
     value=NaN;
     if (argv[0] != NULL) 
     {
       int nscan = sscanf(argv[0],"%f",&value);
       if (nscan != 1) {return CTA_ILLEGAL_DATATYPE;}
     }

     // Cast the values-field to a double-array and store the value
     values = (float *) out->values;
     values[out->index] = value;
  }
  else if(out->datatype == CTA_STRING) 
  {
  // Add a string to the output vector:

     // Read the value
     CTA_String *values = (CTA_String *) out->values;
     if (argv[0] != NULL) 
     {
        // Cast the values-field to a double-array and store the value
        retval = CTA_String_Set(values[out->index],argv[0]);
        if (retval!=CTA_OK) return retval;
     }
     else
     {
        retval = CTA_String_Set(values[out->index],"");
        if (retval!=CTA_OK) return retval;
     }
  }
  else
  {
  // No other data types supported (yet)
     return CTA_ILLEGAL_DATATYPE;
  };

  // Increase the counter, so next value is put in the next spot
  out->index++;
  return SQLITE_OK;
}





int CTAI_util_sqlite3_keynames(void *vout,
         int argc, char **argv, char **azColName)
// Callback-function for sqlite3
{
  // Cast the input to a COSTA-string array
  CTA_String *out = (CTA_String*) vout;

  // Copy all the column names into the string array
  int i;
  for (i=0; i<argc; i++) {CTA_String_Set(out[i],azColName[i]);};

  // Return an error code on purpose, so that the query is done.
  return -1;
}




int CTAI_util_sqlite3_count_columns(void *vout,
         int argc, char **argv, char **azColName)
// Callback-function for sqlite3
{
  // Cast the input to an integer
  int *out = (int*) vout;

  // Fill the column-counter
  *out = argc;

  // Return an error code on purpose, so that the query is done.
  return -1;
}





int CTAI_util_sqlite3_select_values(
/* 
   Return the integers found in the sqlite3-database
*/
    // OUTPUTS:
    void *out, 
    // INPUTS:
    int nout,              // dimension of the output array
    CTA_Datatype datatype, // data type of the output array
    sqlite3 *db,           // the database
    const char *selection, // the column-name which is returned 
    const char *condition) // the selection creterion
{
   char *command;
   CTAI_counter_vector cvout;
   char *zErrMsg;
   int rc;
   
   // Set the constants necessary 
   const char *whole_table = // The expression which produces the complete obs-table
      "stations inner join data on stations.station_id = data.station_id";
   command=CTA_Malloc((strlen(selection)+strlen(whole_table)+strlen(condition)+100)*sizeof(char));

   // Construct the query-command
   if (strcmp(condition,"") == 0)
   {
      sprintf(command,"select %s from %s",selection,whole_table);
   }
   else
   {
      sprintf(command,"select %s from %s where %s",
              selection,whole_table,condition);
   };

   if (IDEBUG) printf("CTAI_util_sqlite3_select_values: DEBUG command=%s\n",command);

   // Arrange a counter-vector to store the results   
   cvout.dimension = nout;
   cvout.index     = 0;
   cvout.datatype  = datatype;
   cvout.values    = out;

   // Carry out the query
   if (IDEBUG>0) printf("Doing query '%s'\n",command);
   zErrMsg=NULL;
   rc = sqlite3_exec(db, command, CTAI_util_sqlite3_return_values,
                     (void *) &cvout, &zErrMsg);
//   if (zErrMsg!=NULL){
//      printf("Error message from sqlite3: %s\n", zErrMsg);
//   }
   free(command);
   if( rc != SQLITE_OK )
     {return CTA_INVALID_COMMAND;};
   return CTA_OK;
}




int CTAI_util_sqlite3_return_keys(
/* 
   Return the keys in the sqlite3-database. Allocate a string-array
   to store the names
*/
    // OUTPUTS:
    int *n_keys, 
    CTA_String **Keys,
    // INPUTS:
    sqlite3 *db,           // the database
    const char *condition) // the selection creterion
{
   // Set the constants necessary 
   const char *whole_table = // The expression which produces the 
                             //     complete obs-table
      "stations inner join data on stations.station_id = data.station_id";

   char *command;
   char *zErrMsg;
   int rc;
   int i;

   command=CTA_Malloc( sizeof(char) * (   strlen("select * from   where  ")
                                    + strlen(whole_table)
                                    + strlen(condition)
                                    + 100
                 )                ); 

   // Construct the query-command
   if (strcmp(condition,"") == 0)
   {
      sprintf(command,"select * from %s",whole_table);
   }
   else
   {
      sprintf(command,"select * from %s where %s",
              whole_table,condition);
   };

   // Carry out the query
   zErrMsg = NULL;

   CTA_Flush();
//   printf("Doing query '%s'\n",command);
   CTA_Flush();  
   rc = sqlite3_exec(db, command, CTAI_util_sqlite3_count_columns,
                     (void *) n_keys, &zErrMsg);
//   if (zErrMsg!=NULL){
//      printf("Error message from sqlite3: %s\n", zErrMsg);
//   }
//   printf("Return code '%d'\n",rc);
   CTA_Flush();
   if ( rc != 4 ) {
     printf("Return code of sqlite3_exec %d see sqlite.h\n",rc);
     return CTA_INVALID_COMMAND;
   };

   *Keys = CTA_Malloc(sizeof(CTA_String)* (*n_keys));
   for (i=0; i<*n_keys; i++) 
   {
     rc = CTA_String_Create( (*Keys+i) );
     if (rc != CTA_OK) return rc;
   }

   // Carry out the query
   zErrMsg = 0;
   //printf("Doing query '%s'\n",command);
   rc = sqlite3_exec(db, command, CTAI_util_sqlite3_keynames,
                     (void *) *Keys, &zErrMsg);
//   if (zErrMsg!=NULL){
//      printf("Error message from sqlite3: %s\n", zErrMsg);
//   }
   free(command);
   if( rc != 4 )
     {return CTA_INVALID_COMMAND;};

   // Remove the double entries and the VALUE-entry
   for (i=0; i<*n_keys; i++)
   {
     int ilen;
     char *stri;

     rc = CTA_String_GetLength( *(*Keys+i), &ilen);
     if (rc != CTA_OK) return rc;
     stri=CTA_Malloc((ilen+1)*sizeof(char));
     rc = CTA_String_Get( *(*Keys+i), stri);
     if (rc != CTA_OK) return rc;

     // Remove word <i> if it is the key 'VALUE'
     if (strcmp(stri,"VALUE")==0)
     {
        rc = CTA_String_Free(*Keys+i);
        if (rc != CTA_OK) return rc;
        if (i<*n_keys-1) { *(*Keys+i) = *(*Keys+*n_keys-1); }
        (*n_keys)--;
        i--;
     }
     else
     {
        // Check all the words up to <i> to see if word <i> is already in the list
        int j;
        for (j=0; j<i; j++)
        {
          int jlen;
          rc = CTA_String_GetLength( *(*Keys+j), &jlen);
          if (jlen==ilen)
          {
             char *strj;

             strj=CTA_Malloc((jlen+1)*sizeof(char));
             rc = CTA_String_Get( *(*Keys+j), strj);
             if (rc != CTA_OK) return rc;
             if (strcmp(stri,strj)==0)
             {
             // Remove word <i> from the list: it is already there.
                rc = CTA_String_Free(*Keys+i);
                if (rc != CTA_OK) return rc;
                if (i<*n_keys-1) { *(*Keys+i) = *(*Keys+*n_keys-1); }
                (*n_keys)--;
                i--;
                j=i;
             }
             free(strj);
          }
        }
      }
     free(stri);
   }

   *Keys = realloc(*Keys, sizeof(CTA_String)* (*n_keys));

   return CTA_OK;
}

