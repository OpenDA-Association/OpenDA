/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_obsdescr_sqlite3.c $
$Revision: 3361 $, $Date: 2012-07-04 16:52:30 +0200 (Wed, 04 Jul 2012) $

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
#include "cta_mem.h"
#include "cta_file.h"
#include "cta_errors.h"
#include "cta_string.h"
#include "cta_util_sqlite3.h"
#include "cta_obsdescr_sqlite3.h"
#include "cta_defaults.h"

#define IDEBUG (0)
/*
   Fills the relation table for the elements in hobsdescr2 that are also in 
   hobsdescr1.

   Note hobsdescr2 MUST be a subset of hobsdescr1!

*/
int CTAI_ObsDescr_CreateRelTable(CTA_ObsDescr hobsdescr1, CTA_ObsDescr hobsdescr2,
                             CTA_RelTable reltab){
   int nobs1, nobs2;
   CTA_Vector vtime1, vtime2, vid1, vid2, vselect;
   int ilo, iup, imid, i2;
   double t1, t2;
   int id1, id2;
   int retval;
   BOOL found;

   /* Count number of observations */
   CTA_ObsDescr_Observation_Count(hobsdescr1,&nobs1);
   CTA_ObsDescr_Observation_Count(hobsdescr2,&nobs2);

   /* Check whether nobs1>=nobs2 since hobsdecr2 must be a subset of
    * hobsdecr1
    */
   if (nobs1<nobs2){
      return CTA_INTERNAL_ERROR;
   }

   /* Allocate arrays for holding primary keys */
   CTA_Vector_Create(CTA_DEFAULT_VECTOR, nobs1, CTA_DOUBLE,  CTA_NULL, &vtime1);
   CTA_Vector_Create(CTA_DEFAULT_VECTOR, nobs2, CTA_DOUBLE,  CTA_NULL, &vtime2);
   CTA_Vector_Create(CTA_DEFAULT_VECTOR, nobs1, CTA_INTEGER, CTA_NULL, &vid1);
   CTA_Vector_Create(CTA_DEFAULT_VECTOR, nobs2, CTA_INTEGER, CTA_NULL, &vid2);
   CTA_Vector_Create(CTA_DEFAULT_VECTOR, nobs2, CTA_INTEGER, CTA_NULL, &vselect);

   /* Get Time and ID from both */
   CTA_ObsDescr_Get_ValueProperties(hobsdescr1, "TIME", vtime1, CTA_DOUBLE);
   CTA_ObsDescr_Get_ValueProperties(hobsdescr2, "TIME", vtime2, CTA_DOUBLE);
   CTA_ObsDescr_Get_ValueProperties(hobsdescr1, "STATION_ID",vid1,CTA_INTEGER);
   CTA_ObsDescr_Get_ValueProperties(hobsdescr2, "STATION_ID",vid2,CTA_INTEGER);

   /* now we have to figure out which elements from hobsdescr1 are also
    * present in hobsdescr2                                           */

    /* Note we know that elements are sorted first by time then by id! */
    for (i2=1;i2<=nobs2;i2++){
       CTA_Vector_GetVal(vtime2, i2, &t2,  CTA_DOUBLE);
       CTA_Vector_GetVal(vid2,   i2, &id2, CTA_INTEGER);

       if (IDEBUG) printf("==>looking for %f, %d\n",t2,id2);

       /* Use bi-section in order to find the elements */
       ilo=1;
       iup=nobs1;
       if (IDEBUG) printf("ilo=%d, iup=%d \n",ilo,iup);

       found=FALSE;
       for (;;){
          imid=(iup+ilo)/2;
          CTA_Vector_GetVal(vtime1, imid, &t1,  CTA_DOUBLE);
          CTA_Vector_GetVal(vid1,   imid, &id1, CTA_INTEGER);
          if (IDEBUG) printf("imid= %d found %f, %d\n",imid, t1,id1);
          if (t1==t2 && id1==id2){
             found=TRUE;
             break;
          }
     
          /* exit loop when element is not there :*/
          if(ilo==iup){break;}  /*Ony one element there */

          /* check whether element (t1,id1)<(t2,id2) and
           * set new values for ilo and iup
           */
          if (t1<t2 || (t1==t2 && id1<id2)) {
             if (ilo==imid) {ilo=iup;} else {ilo=imid;}
          } else {
             if (iup==imid) {iup=ilo;} else {iup=imid;}
          }
       }
       if (!found){
          /* INTERNAL ERROR */
          /* nicely Free memory */
          CTA_Vector_Free(&vtime1);
          CTA_Vector_Free(&vtime2);
          CTA_Vector_Free(&vid1);
          CTA_Vector_Free(&vid2);
          CTA_Vector_Free(&vselect);
          return CTA_INTERNAL_ERROR;
       }
       CTA_Vector_SetVal(vselect, i2, &imid, CTA_INTEGER);
    }
    retval=CTA_RelTable_SetSelect(reltab, vselect);
   
   /* Free memory */
   CTA_Vector_Free(&vtime1);
   CTA_Vector_Free(&vtime2);
   CTA_Vector_Free(&vid1);
   CTA_Vector_Free(&vid2);
   CTA_Vector_Free(&vselect);

   return retval;
}



void CTA_ObsDescr_sqlite3_initialise(CTA_ObsDescrClass *hobsdescrcl)
{
   CTA_Intf hintf=0;
   CTA_Func h_func[I_CTA_OBSDESCR_NUMFUNC];

   // The vector h_func is filled with COSTA-function handles of the 
   // implementations in this file.
   CTA_Func_Create(" ",&CTAI_ObsDescr_sqlite3_Create_Size,    hintf,
                       &h_func[I_CTA_OBSDESCR_CREATE_SIZE]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_sqlite3_Create_Init,    hintf,
                       &h_func[I_CTA_OBSDESCR_CREATE_INIT]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_sqlite3_Property_Count,    hintf,
                       &h_func[I_CTA_OBSDESCR_COUNT_PROPERTIES]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_sqlite3_Get_Properties,    hintf,
                       &h_func[I_CTA_OBSDESCR_GET_PROPERTIES]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_sqlite3_Observation_Count, hintf,
                       &h_func[I_CTA_OBSDESCR_COUNT_OBSERVATIONS]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_sqlite3_Get_Keys,    hintf,
                       &h_func[I_CTA_OBSDESCR_GET_KEYS]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_sqlite3_Free,    hintf,
                       &h_func[I_CTA_OBSDESCR_FREE]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_sqlite3_CreateSel,    hintf,
                       &h_func[I_CTA_OBSDESCR_SELECTION]);
   CTA_ObsDescr_DefineClass("cta_obsdescr_sqlite3",h_func,hobsdescrcl);

}



void CTAI_ObsDescr_sqlite3_Create_Size(
   // OUTPUTS:
      int *memsize,           // The number of bytes which are necessary to 
                              //     store one CTAI_SObs_sqlite3, with a 
                              //     pointer to the contents (data), 
                              //     but without the contents themelves.
      int *retval             // error code (see cta_datatypes.h for possible 
                              //     error codes)
   ){
//   *memsize=(int) sizeof(CTAI_SObs_sqlite3);
   *memsize=(int) sizeof(CTAI_ObsDescr_sqlite3);
   *retval=CTA_OK;
};

void CTAI_ObsDescr_sqlite3_Create_Init(
/*
   Allocate the memory which is necesary to store the data necessary for a
   sqlite3-observer
*/
     // INPUT:
      CTA_ObsDescr *myhandle,  /* Handle assigned by COSTA */
     // IN-OUTPUTS
        CTAI_ObsDescr_sqlite3 *descr,// The sqlite3-observation description
                                 //     for which the memory must be
                                 //     allocated
     // INPUTS:
        CTA_Handle *usrdat, // User data: database-name
     // OUTPUTS
        int *retval)             // Error code.
{

   CTAI_SObs_sqlite3 *sobs; // User data: database-name

   descr->myhandle=*myhandle;

   *retval= CTA_Handle_GetData(*usrdat, (void **) &sobs);
   if (*retval!=CTA_OK) return;

   // Link the data base to this descriptor; increase number of users
   descr->database = sobs->database;
   (descr->database->nusers)++;

   // Copy the conditionCTA_RelTable *hreltable
   descr->condition = CTA_Malloc(sizeof(char)*(1+strlen(sobs->condition)));
   strcpy(descr->condition,sobs->condition);

   // Store the keys
   *retval = CTAI_util_sqlite3_return_keys(
                  &(descr->n_keys),
                  &(descr->Keys),
                  descr->database->db,
                  descr->condition);

   if (*retval != CTA_OK) return;

   // Copy the dimension of the observer
   descr->nmeasr = sobs->nmeasr;
   *retval=CTA_OK;
};

void CTAI_ObsDescr_sqlite3_CreateSel(CTAI_ObsDescr_sqlite3 *descr,
          CTA_String *selection, CTA_RelTable *reltab, 
          CTA_ObsDescr *myhandle_out, 
          CTAI_ObsDescr_sqlite3 *descrout, int *retval){


   int i,len;
   char *condition;

   // Get the condition
   // Allocate a name-string 
   *retval = CTA_String_GetLength(*selection, &len);
   if (*retval!=CTA_OK) return;

   // Get the condition
   condition = CTA_Malloc((len+1)*sizeof(char));
   *retval = CTA_String_Get(*selection, condition);
   if (IDEBUG>0) printf("CTAI_ObsDescr_sqlite3_CreateSel: given    condition is '%s'\n",condition);
   if (IDEBUG>0) printf("CTAI_ObsDescr_sqlite3_CreateSel: Internal condition is '%s'\n",descr->condition);
   if (*retval!=CTA_OK) return;

   // Set handle of new observation description
   descrout->myhandle=*myhandle_out;

   // Link the database also to this observer and keep track of the number of 
   // observers using this database
   descrout->database = descr->database;
   descrout->database->nusers++;

   // Combine the input-condition and the condition of the input observer.
   if (strcmp(descr->condition,"")==0)
   {
      descrout->condition = condition;
   }
   else
   {
      descrout->condition = CTA_Malloc(sizeof(char)*
                                  (    strlen(condition) 
                                    +  strlen(descr->condition)
                                    +  strlen("  (  ) AND (  )  ")
                                ) );
      sprintf(descrout->condition,"(%s) AND (%s)",condition,
              descr->condition);
      free(condition);
   }

   // copy number of keys
   descrout->n_keys=descr->n_keys;

   // Count the measurements in this observer
   *retval = CTAI_util_sqlite3_select_values(
       &(descrout->nmeasr), 1, CTA_INTEGER, descrout->database->db,
       "count(stations.station_id)",descrout->condition);

   // copy the Keys 
   descrout->Keys=CTA_Malloc(descr->n_keys*sizeof(CTA_String));
   for (i=0;i<descr->n_keys;i++){
      *retval=CTA_String_Duplicate(descr->Keys[i],&descrout->Keys[i]);
      if (*retval!=CTA_OK) return;
   }
   
   if (*reltab!=CTA_NULL) {
      *retval=CTAI_ObsDescr_CreateRelTable(descr->myhandle,
                                              descrout->myhandle, *reltab);
   } else {
      *retval=CTA_OK;
   }
}
         


void CTAI_ObsDescr_sqlite3_Get_Keys(
         CTAI_ObsDescr_sqlite3 *descr,
         CTA_Vector *Keys,
         int *retval)
{
   int i;
   for (i=1; i<=descr->n_keys; i++)
   {
      *retval = CTA_Vector_SetVal(*Keys,i,&(descr->Keys[i-1]),CTA_STRING);
   };

   *retval = CTA_OK;
};

void CTAI_ObsDescr_sqlite3_Property_Count(
         CTAI_ObsDescr_sqlite3 *descr, 
         int *nkeys, 
         int *retval)
{
   *nkeys = descr->n_keys;
   *retval = CTA_OK;
};

void CTAI_ObsDescr_sqlite3_Observation_Count(
         CTAI_ObsDescr_sqlite3 *descr, 
         int *nobs, 
         int *retval)
{
   *nobs = descr->nmeasr;
   *retval = CTA_OK;
};


BOOL isNaN(double x)
{
   return (!(x>0 || x<=0));
}

void CTAI_ObsDescr_sqlite3_Get_Properties(
         CTAI_ObsDescr_sqlite3 *descr, 
         const char *Key,
         CTA_Vector *Properties,
         CTA_Datatype *datatype,
         int *retval)
{
    // Find the key among the key names
    int i;
    int len;
    BOOL found=0;
    int isize;
    void * StatProp;
    void * TimeProp;
    char *KeyLong;
    int retStat;
    int retTime;
    void * PropArray;


    for (i=0; !found && i<descr->n_keys; i++)
    {
       char *str;
       *retval = CTA_String_GetLength(descr->Keys[i],&len);
       if (*retval!=CTA_OK) return;

       str=CTA_Malloc((len+1)*sizeof(char));
       *retval = CTA_String_Get(descr->Keys[i],str);
       if (*retval!=CTA_OK){
          free(str);
          return;
       }
       found = strcmp(Key,str)==0;
       free(str);
    }
    i--;

    // Error if the key was not found
    if (!found) {*retval= CTA_ITEM_NOT_FOUND; return;}

    // Declare an array for the properties
    if     (*datatype == CTA_INTEGER) {isize = sizeof(int);}
    else if(*datatype == CTA_REAL)    {isize = sizeof(float);}
    else if(*datatype == CTA_DOUBLE)  {isize = sizeof(double);}
    else if(*datatype == CTA_STRING)  {isize = sizeof(CTA_String);}
    else   {*retval = CTA_ILLEGAL_DATATYPE; return;}

    StatProp = CTA_Malloc(isize*descr->nmeasr);
    TimeProp = CTA_Malloc(isize*descr->nmeasr);

    // In case of String-output, create the strings
    if (*datatype == CTA_STRING)
    {
       CTA_String * PropStr=StatProp;
       for (i=0; i<descr->nmeasr; i++)
       {
         *retval = CTA_String_Create(&PropStr[i]);
         if (*retval != CTA_OK) return;
       }

       PropStr=TimeProp;
       for (i=0; i<descr->nmeasr; i++)
       {
         *retval = CTA_String_Create(&PropStr[i]);
         if (*retval != CTA_OK) return;
       }
    }


    // Get the properties from the stationary information
    KeyLong=CTA_Malloc((strlen(Key)+strlen("stations.")+1));
    sprintf(KeyLong,"stations.%s",Key);
    retStat = CTAI_util_sqlite3_select_values(
              StatProp, descr->nmeasr, *datatype,
              descr->database->db, KeyLong, descr->condition);

    // Get the properties from the time dependent information
    sprintf(KeyLong,"data.%s",Key);
    retTime = CTAI_util_sqlite3_select_values(
              TimeProp, descr->nmeasr, *datatype,
              descr->database->db, KeyLong, descr->condition);

    free(KeyLong);
    if (retStat != CTA_OK && retTime != CTA_OK) {*retval = retStat; return;}

    // Merge the two answers
    
    if (retStat == CTA_OK && retTime == CTA_OK)
    {
    // Column found in stationary and in time-dependent information

       // Supplement the Stationary data array with instationary data, where
       // these are available. Check that the stationary and instationary data do
       // not conflict.
       if (*datatype == CTA_DOUBLE)
       {
          double * dStatProp = StatProp;
          double * dTimeProp = TimeProp;
          for (i=0; i<descr->nmeasr; i++)
          {
             if (isNaN(dStatProp[i]) && !isNaN(dTimeProp[i]))
                { dStatProp[i] = dTimeProp[i]; }
             else if ( !isNaN(dStatProp[i]) && !isNaN(dTimeProp[i]) &&
                        dStatProp[i] != dTimeProp[i] )
                { *retval = CTA_INCOMPATIBLE_VECTORS; return;}
          }

       }
       else if (*datatype == CTA_INTEGER)
       {
          int * iStatProp = StatProp;
          int * iTimeProp = TimeProp;
          int iNAN =-987654;
          for (i=0; i<descr->nmeasr; i++)
          {
             if (iStatProp[i] == iNAN && iTimeProp[i] != iNAN)
                { iStatProp[i] = iTimeProp[i]; }
             else if (iStatProp[i] != iNAN && iTimeProp[i] != iNAN &&
                      iStatProp[i] != iTimeProp[i] )
                { *retval = CTA_INCOMPATIBLE_VECTORS; return;}
          }
       }
       else if (*datatype == CTA_STRING)
       {
          CTA_String * sStatProp = StatProp;
          CTA_String * sTimeProp = TimeProp;
          for (i=0; i<descr->nmeasr; i++)
          {
             int statlen, timelen;
             *retval = CTA_String_GetLength(sStatProp[i],&statlen);
             if (*retval != CTA_OK) return;
             *retval = CTA_String_GetLength(sTimeProp[i],&timelen);
             if (*retval != CTA_OK) return;

             if (statlen == 0 && timelen != 0)
             { 
                 CTA_String help = sStatProp[i];
                 sStatProp[i] = sTimeProp[i]; 
                 sTimeProp[i] = help;
             }
             else if (statlen != 0 && timelen != 0 )
             {
                char *statstr;
                char *timestr;
                if (statlen != timelen)   
                   { *retval = CTA_INCOMPATIBLE_VECTORS; return;}

                statstr=CTA_Malloc((statlen+1)*sizeof(char));
                timestr=CTA_Malloc((timelen+1)*sizeof(char));
                *retval = CTA_String_Get(sStatProp[i],statstr);
                if (*retval != CTA_OK) return;
                *retval = CTA_String_Get(sTimeProp[i],timestr);
                if (*retval != CTA_OK) return;
                if (strcmp(statstr,timestr) != 0)
                   { *retval = CTA_INCOMPATIBLE_VECTORS; return;}
                free(statstr);
                free(timestr);
             } 
          }
       }
       else
       {  
          *retval = CTA_ILLEGAL_DATATYPE; return;
       }

       PropArray = StatProp;
    }
    else if (retStat == CTA_OK)
    {
       PropArray = StatProp;
    }
    else 
    {
       PropArray = TimeProp;
    }
    *retval = CTA_Vector_SetVals(*Properties, PropArray, descr->nmeasr, *datatype);
    if (*retval != CTA_OK) return;


    // In case of String-output, erase the strings
    if (*datatype == CTA_STRING)
    {
       CTA_String * PropStr=StatProp;
       for (i=0; i<descr->nmeasr; i++)
       {
         *retval = CTA_String_Free(&PropStr[i]);
         if (*retval != CTA_OK) return;
       }
       PropStr=TimeProp;
       for (i=0; i<descr->nmeasr; i++)
       {
         *retval = CTA_String_Free(&PropStr[i]);
         if (*retval != CTA_OK) return;
       }
    }

   free(StatProp);
   free(TimeProp); 
   *retval = CTA_OK;
}


void CTAI_ObsDescr_sqlite3_Free(
         CTAI_ObsDescr_sqlite3 *descr, 
         int *retval)
{
   int i;
   
   descr->database->nusers--;
    if (descr->database->nusers==0)
    {

       sqlite3_close(descr->database->db);
       free(descr->database->name);
       free(descr->database);

    }
    free(descr->condition);

    for (i=0; i<descr->n_keys; i++){ CTA_String_Free(&(descr->Keys[i]));}
    free(descr->Keys);


    *retval = CTA_OK;
}

