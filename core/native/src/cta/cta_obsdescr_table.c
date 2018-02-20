/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_obsdescr_table.c $
$Revision: 3407 $, $Date: 2012-08-17 13:50:50 +0200 (Fri, 17 Aug 2012) $
 
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

#define IDEBUG (0)

#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <math.h>
#include <memory.h>
#include "cta.h"
#include "cta_mem.h"

typedef struct {
int nkeys;
int nmeasr;
char **Keys;
char ***Columns;
} CTAI_ObsDescr_table;


void CTAI_ObsDescr_table_Create_Size(
   // OUTPUTS:
      int *memsize,           // The number of bytes which are necessary to 
                              //     store one CTAI_SObs_sqlite3, with a 
                              //     pointer to the contents (data), 
                              //     but without the contents themselves.
      int *retval             // error code (see cta_datatypes.h for possible 
                              //     error codes)
   ){
   *memsize=(int) sizeof(CTAI_ObsDescr_table);
   *retval=CTA_OK;
};

void CTAI_ObsDescr_table_Create_Init(
/*
   Allocate the memory which is necessary to store the data necessary for a
   table-observer
*/
     // INPUT:
      CTA_ObsDescr myhandle,  /* Handle assigned by COSTA */
     // IN-OUTPUTS
        CTAI_ObsDescr_table *descr,// The table-observation description
                                 //     for which the memory must be
                                 //     allocated
     // INPUTS:
        CTA_Handle          *usrdat, // User data
     // OUTPUTS
        int *retval)             // Error code.
{
   CTA_Datatype datatype;
   int ikey, imeasr, nkeys, nmeasr, lenkey, lencol, len;
   CTA_Vector vKeys, vCol;
   CTA_String sKey;
   CTA_String sCol;
   const char *key;

   if (IDEBUG>0) printf("CTAI_ObsDescr_table_Create_Init: Start handle=%d\n",*usrdat);
   /* Check input we only support a pack array input and an observation
    * description component */
   *retval=CTA_Handle_GetDatatype(*usrdat, &datatype);
   if (datatype==CTA_PACK) {
      
      /* Get number of keys and number of measurements */
      *retval=CTA_Pack_Get(*usrdat,&nkeys,sizeof(int));
      if (*retval!=CTA_OK) return;
      if (IDEBUG>0) printf("CTAI_ObsDescr_table_Create_Init: nkeys=%d\n",nkeys);
      *retval=CTA_Pack_Get(*usrdat,&nmeasr,sizeof(int));
      if (IDEBUG>0) printf("CTAI_ObsDescr_table_Create_Init: nmeasr=%d\n",nmeasr);
      if (*retval!=CTA_OK) return;
   } else if (datatype==CTA_OBSDESCR) {
      if (IDEBUG>0) printf("CTAI_ObsDescr_table_Create_Init: datatype==CTA_OBSDESCR\n");
      /* Get number of keys and number of measurements */
      *retval=CTA_ObsDescr_Property_Count(*usrdat, &nkeys);
      if (IDEBUG>0) printf("CTAI_ObsDescr_table_Create_Init: nkeys=%d ierr=%d\n",nkeys, *retval);
      if (*retval!=CTA_OK) return;
      *retval=CTA_ObsDescr_Observation_Count(*usrdat, &nmeasr);
      if (IDEBUG>0) printf("CTAI_ObsDescr_table_Create_Init: nmeasr=%d ierr=%d\n",nmeasr, *retval);
      if (*retval!=CTA_OK) return;
   } else {
      *retval=CTA_NOT_IMPLEMENTED;
      return;
   }
   /* Allocate arrays */
   descr->Keys=CTA_Malloc(nkeys*sizeof(char*));
   descr->Columns=CTA_Malloc(nkeys*sizeof(char**));

   for (ikey=0;ikey<nkeys;ikey++) {
      descr->Columns[ikey]=CTA_Malloc(nmeasr*sizeof(char*));
   }
   /* Nullify the strings */
   for (ikey=0;ikey<nkeys;ikey++) {
      descr->Keys[ikey]=NULL;
      for (imeasr=0;imeasr<nmeasr;imeasr++) {
         descr->Columns[ikey][imeasr]=NULL;
      }
   }
   /* Set nkeys and nmeasr */
   descr->nkeys=nkeys;
   descr->nmeasr=nmeasr;
 
   if (IDEBUG>0) printf("CTAI_ObsDescr_table_Create_Init: datatype=%d \n",datatype);
 
  
   if (datatype==CTA_PACK) {
      /* Unpack all keys */
     if (IDEBUG>0) printf("CTAI_ObsDescr_table_Create_Init: unpack-keys\n");
      for (ikey=0; ikey<descr->nkeys; ikey++){
         /* unpack stringlength */
         *retval=CTA_Pack_Get(*usrdat,&len,sizeof(int));
         if (*retval!=CTA_OK) return;
         /* pack string */
         descr->Keys[ikey]=CTA_Malloc(len*sizeof(char));
         *retval=CTA_Pack_Get(*usrdat,descr->Keys[ikey],len*sizeof(char));
         if (*retval!=CTA_OK) return;
         if (IDEBUG>0) printf("CTAI_ObsDescr_table_Create_Init: Key[%d]=%s\n",
                              ikey, descr->Keys[ikey]);
      }

      /* Unpack all Columns */
      for (ikey=0; ikey<descr->nkeys; ikey++){
         for (imeasr=0; imeasr<descr->nmeasr; imeasr++){
            if (IDEBUG>0) printf("CTAI_ObsDescr_table_Create_Init\n");
            /* pack stringlength */
            *retval=CTA_Pack_Get(*usrdat,&len,sizeof(int));
            if (*retval!=CTA_OK) return;
            /* pack string */
            descr->Columns[ikey][imeasr]=CTA_Malloc(len*sizeof(char));
            *retval=CTA_Pack_Get(*usrdat,descr->Columns[ikey][imeasr],
                                len*sizeof(char));
            if (*retval!=CTA_OK) return;
            if (IDEBUG>0) printf("CTAI_ObsDescr_table_Create_Init: Val[%d][%d]=%s\n",
                                 ikey,imeasr,descr->Columns[ikey][imeasr]);
         }
      }





   } else if (datatype==CTA_OBSDESCR) {
      

      if (IDEBUG>0) {printf("CTAI_ObsDescr_table_Create_Init: start \n");}

      /* Allocate vectors for getting out the information */

      *retval=CTA_Vector_Create(CTA_DEFAULT_VECTOR, nkeys,  CTA_STRING, CTA_NULL, &vKeys);
      if (*retval!=CTA_OK) return;
      *retval=CTA_Vector_Create(CTA_DEFAULT_VECTOR, nmeasr, CTA_STRING, CTA_NULL, &vCol);
      if (*retval!=CTA_OK) return;
 
       *retval=CTA_String_Create(&sKey);
       if (*retval!=CTA_OK) return;
       *retval=CTA_String_Create(&sCol);
       if (*retval!=CTA_OK) return;
       
       /* Get all Keys */
       *retval=CTA_ObsDescr_Get_PropertyKeys(*usrdat, vKeys);
       if (*retval!=CTA_OK) return;
       
       /* For all keys */
       for (ikey=0; ikey<nkeys; ikey++) {
          /* Copy Key into local administration */
          *retval=CTA_Vector_GetVal(vKeys, ikey+1, &sKey, CTA_STRING);
          if (*retval!=CTA_OK) return;
          *retval=CTA_String_GetLength(sKey,&lenkey);
          if (*retval!=CTA_OK) return;
          descr->Keys[ikey]=CTA_Malloc((lenkey+1)*sizeof(char));
          *retval=CTA_String_Get(sKey,descr->Keys[ikey]);
          if (*retval!=CTA_OK) return;
       
          /* Get the properties of this key */
          key=CTAI_String_GetPtr(sKey);
          if (IDEBUG>0) {printf("CTAI_ObsDescr_table_Create_Init: get data for key '%s'\n",key);}
          // NOTE this is were performance goes lost!
          *retval=CTA_ObsDescr_Get_ValueProperties(*usrdat, key, vCol, CTA_STRING);
          if (*retval!=CTA_OK) return;
//          printf("done\n");
       
          /* For all measurement */
          for (imeasr=0; imeasr<nmeasr; imeasr++) {
             *retval=CTA_Vector_GetVal(vCol, imeasr+1, &sCol, CTA_STRING);
             if (*retval!=CTA_OK) return;
             *retval=CTA_String_GetLength(sCol,&lencol);
             if (*retval!=CTA_OK) return;
                 descr->Columns[ikey][imeasr]=CTA_Malloc((lencol+1)*sizeof(char));
                 *retval=CTA_String_Get(sCol,descr->Columns[ikey][imeasr]);
                if (IDEBUG>0) printf("CTAI_ObsDescr_table_Create_Init: Val[%d][%d]=%s\n",
                                 ikey,imeasr,descr->Columns[ikey][imeasr]);  
             if (*retval!=CTA_OK) return;
          }
       }
       
      /* Deallocate work variables */
      CTA_Vector_Free(&vKeys);
      CTA_Vector_Free(&vCol);
      CTA_String_Free(&sKey);
      CTA_String_Free(&sCol);
   } else {
      *retval=CTA_NOT_IMPLEMENTED;
      return;
   }

   *retval=CTA_OK;
};

void CTAI_ObsDescr_table_Get_Keys(
         CTAI_ObsDescr_table *descr,
         CTA_Vector *Keys,
         int *retval)
{
   int i;
   CTA_String hlpstr;

   CTA_String_Create(&hlpstr);
   for (i=1; i<=descr->nkeys; i++)
   {  
      CTA_String_Set(hlpstr,descr->Keys[i-1]);     
      *retval = CTA_Vector_SetVal(*Keys,i,&hlpstr,CTA_STRING);
       if (IDEBUG>0) {printf("CTAI_ObsDescr_table_Get_Keys: set key '%s' ierr=%d",descr->Keys[i-1],*retval);}
   };
   CTA_String_Free(&hlpstr);

   *retval = CTA_OK;
};

void CTAI_ObsDescr_table_Property_Count(
         CTAI_ObsDescr_table *descr, 
         int *nkeys, 
         int *retval)
{
   *nkeys = descr->nkeys;
   *retval = CTA_OK;
};

void CTAI_ObsDescr_table_Observation_Count(
         CTAI_ObsDescr_table *descr, 
         int *nobs, 
         int *retval)
{
   *nobs = descr->nmeasr;
   *retval = CTA_OK;
};


void CTAI_ObsDescr_table_Get_Properties(
         CTAI_ObsDescr_table *descr, 
         const char *Key,
         CTA_Vector *Properties,
         CTA_Datatype *datatype,
         int *retval)
{
   // Find the key among the key names
   BOOL found=0;
   int isize, ikey, imeasr;
   void *vals;
   int  *ivals;
   float *rvals;
   double *dvals;
   CTA_String str;
   
   

   for (ikey=0; !found && ikey<descr->nkeys; ikey++)
   {
      found = strcmp(Key,descr->Keys[ikey])==0;
   }
   ikey--;

   // Error if the key was not found
   if (!found) {*retval= CTA_ITEM_NOT_FOUND; return;}

   // Declare an array for the properties
   if     (*datatype == CTA_INTEGER) {isize = sizeof(int);}
   else if(*datatype == CTA_REAL)    {isize = sizeof(float);}
   else if(*datatype == CTA_DOUBLE)  {isize = sizeof(double);}
   else if(*datatype == CTA_STRING)  {isize = 0;}
   else   {*retval = CTA_ILLEGAL_DATATYPE; return;}

   vals=NULL;
   if (isize>0) {
      isize=isize*descr->nmeasr;
      vals=CTA_Malloc(isize);
   } else if (*datatype == CTA_STRING) {
      CTA_String_Create(&str);
   }


   ivals=vals;
   rvals=vals;
   dvals=vals;

   for (imeasr=0;imeasr<descr->nmeasr;imeasr++){

      if (*datatype == CTA_INTEGER) {
         // Add an integer to the output vector:

         // Read the value
         if (descr->Columns[ikey][imeasr] != NULL) { 
            int nscan = sscanf(descr->Columns[ikey][imeasr],"%d",&ivals[imeasr]);
            if (nscan != 1) {
               *retval=CTA_ILLEGAL_DATATYPE;
               free(vals);
               return;
            }
         }
       } else if(*datatype == CTA_DOUBLE) {
          // Add a double to the output vector:

          // Read the value
          double NaN;

          NaN = 0; NaN = 1.0/NaN; NaN=(1.0+NaN)/NaN;
          dvals[imeasr]=NaN;

          if (descr->Columns[ikey][imeasr] != NULL) {
             int nscan = sscanf(descr->Columns[ikey][imeasr],"%lf",&dvals[imeasr]);
            if (nscan != 1) {
               *retval=CTA_ILLEGAL_DATATYPE;
               free(vals);
               return;
            }
          }
       } else if(*datatype == CTA_REAL) {
          // Add a float to the output vector:

          // Read the value
          float NaN;
          NaN = 0;
          NaN = (float) 1.0/NaN;
          NaN=NaN/NaN;
          rvals[imeasr]=NaN;
          if (descr->Columns[ikey][imeasr] != NULL) {
             int nscan = sscanf(descr->Columns[ikey][imeasr],"%f",&rvals[imeasr]);
            if (nscan != 1) {
               *retval=CTA_ILLEGAL_DATATYPE;
               free(vals);
               return;
            }
          }
       } else if(*datatype == CTA_STRING) {
          // Add a string to the output vector:
          if (descr->Columns[ikey][imeasr] != NULL) {
             // Set value in array of Strings
             *retval = CTA_String_Set(str, descr->Columns[ikey][imeasr]);
             if (*retval!=CTA_OK) return;
          } else {
             *retval = CTA_String_Set(str,"");
             if (*retval!=CTA_OK) return;
          }
          *retval=CTA_Vector_SetVal(*Properties,imeasr+1,&str,*datatype);
       } else  {
          // No other data types supported (yet)
          *retval=CTA_ILLEGAL_DATATYPE;
       };
    }
    /* Values in the return vector (except for characters) */
    if (*datatype == CTA_INTEGER || *datatype == CTA_REAL ||
        *datatype == CTA_DOUBLE){
       *retval=CTA_Vector_SetVals(*Properties,vals,descr->nmeasr,*datatype);
    }

    if (vals) {
       free(vals);
    } else if (*datatype == CTA_STRING) {
      CTA_String_Free(&str);
    }

}

void CTAI_ObsDescr_table_Export(CTAI_ObsDescr_table *descr, CTA_Handle
*usrdat, int *retval) 
{
   int ikey, imeasr, len;
   BOOL packout;

   packout = (CTA_Handle_Check(*usrdat,CTA_PACK)==CTA_OK);
   if (packout) {

      /* pack nkeys and nmeasr */
      *retval=CTA_Pack_Add(*usrdat,&descr->nkeys,sizeof(int));
      if (*retval!=CTA_OK) return;
      *retval=CTA_Pack_Add(*usrdat,&descr->nmeasr,sizeof(int));
      if (*retval!=CTA_OK) return;

      /* Pack all keys */
      for (ikey=0; ikey<descr->nkeys; ikey++){
         /* pack stringlength */
         len=strlen(descr->Keys[ikey])+1;
         *retval=CTA_Pack_Add(*usrdat,&len,sizeof(int));
         if (*retval!=CTA_OK) return;
         /* pack string */
         *retval=CTA_Pack_Add(*usrdat,descr->Keys[ikey],len*sizeof(char));
         if (*retval!=CTA_OK) return;
      }

      /* Pack all Columns */
      for (ikey=0; ikey<descr->nkeys; ikey++){
         for (imeasr=0; imeasr<descr->nmeasr; imeasr++){
            /* pack stringlength */
            len=strlen(descr->Columns[ikey][imeasr])+1;
            *retval=CTA_Pack_Add(*usrdat,&len,sizeof(int));
            if (*retval!=CTA_OK) return;
            /* pack string */
            *retval=CTA_Pack_Add(*usrdat,descr->Columns[ikey][imeasr],
                                len*sizeof(char));
            if (*retval!=CTA_OK) return;
         }
      }
   } else {
      *retval=CTA_FORMAT_NOT_SUPPORTED;
      return;
   }
   *retval=CTA_OK;
   return;
}
 
void CTAI_ObsDescr_table_Free(
         CTAI_ObsDescr_table *descr, 
         int *retval)
{
   int ikey, imeasr;

   for (ikey=0; ikey<descr->nkeys; ikey++) {
      
      free (descr->Keys[ikey]);
      for (imeasr=0; imeasr<descr->nmeasr; imeasr++) {
         free(descr->Columns[ikey][imeasr]);
      }
      free(descr->Columns[ikey]);
   }
   free(descr->Keys);
   free(descr->Columns);

   *retval=CTA_OK;
}



void CTAI_ObsDescr_table_CreateSel(CTAI_ObsDescr_table *descr,
          CTA_String *selection, CTA_RelTable *reltab, 
          CTA_ObsDescr *myhandle_out, 
          CTAI_ObsDescr_table *descrout, int *retval){


   int i, len, imeasr, nmeasr;
   int ikey, nkeys, irow;
   int datatype;
   int *index;
   char *condition;
   double t1,t2,t;
   CTA_Vector vtimes, vselect;

   // Get the condition
   // Allocate a name-string 
   *retval = CTA_String_GetLength(*selection, &len);
   if (*retval!=CTA_OK) return;

   // Get the condition
   condition = CTA_Malloc((len+1)*sizeof(char));
   *retval = CTA_String_Get(*selection, condition);
   if (*retval!=CTA_OK) return;

   *retval = CTA_String_Get(*selection, condition);
   if (*retval!=CTA_OK) return;


   // convert to upper case
   for (i=0;i<len;i++){
      condition[i]=toupper(condition[i]);
   }


   if (IDEBUG) {printf("CTAI_ObsDescr_table_CreateSel select=%s\n",condition);}

   // We only support the timeselect criterion whith the format:
   // TIME BETWEEN 0.000010 AND 0.100010
   
   t1=atof(&(condition[12]));
   if (IDEBUG) {printf("t1=%f\n",t1);}

   // find first second number
   for (i=0;12<len;i++){
      if (condition[i]=='D') {break;}
   }

   t2=atof(&(condition[i+1]));
   if (IDEBUG) {printf("t2=%f\n",t2);}

   // Get vector with all times
   datatype=CTA_DOUBLE;
   CTA_Vector_Create(CTA_DEFAULT_VECTOR, descr->nmeasr, datatype, CTA_NULL,
      &vtimes);
   CTAI_ObsDescr_table_Get_Properties(descr,"TIME", &vtimes, &datatype,
                                      retval);
   if (*retval!=CTA_OK) return;
   

   // find observations are in given timespan 
   nmeasr=-1;
   index=CTA_Malloc(descr->nmeasr*sizeof(int));
   for (i=1;i<=descr->nmeasr;i++){
      CTA_Vector_GetVal(vtimes, i, &t, CTA_DOUBLE);
      if (t1<t && t<=t2) {
         nmeasr++;
         index[nmeasr]=i;
      }
   }
   nmeasr++;
   if (IDEBUG) {printf("Number of measrements=%d\n",nmeasr);}

   // create the relation table
   if (*reltab!=CTA_NULL){
      CTA_Vector_Create(CTA_DEFAULT_VECTOR, nmeasr, CTA_INTEGER, CTA_NULL,
         &vselect);
      CTA_Vector_SetVals(vselect, index, nmeasr, CTA_INTEGER);
      CTA_RelTable_SetSelect(*reltab, vselect);
      CTA_Vector_Free(&vselect);
   }

   // create new stochastic observer
   nkeys=descr->nkeys;

   descrout->nkeys=nkeys;
   descrout->nmeasr=nmeasr;
   
   /* Allocate table */
   descrout->Keys=CTA_Malloc(nkeys*sizeof(char*));
   descrout->Columns=CTA_Malloc(nkeys*sizeof(char**));
   for (ikey=0;ikey<nkeys;ikey++) {
      descrout->Columns[ikey]=CTA_Malloc(nmeasr*sizeof(char*));
   }
   /* Copy Keys */
   for (ikey=0;ikey<nkeys;ikey++) {
      /* Copy Keys */
      len=strlen(descr->Keys[ikey]);
      descrout->Keys[ikey]=CTA_Malloc(sizeof(char)*(len+1));
      strcpy(descrout->Keys[ikey],descr->Keys[ikey]);
   }   
   
   /* Copy rows of table */
   for (imeasr=0;imeasr<nmeasr;imeasr++){
      irow=index[imeasr]-1;
      for (ikey=0;ikey<nkeys;ikey++){
         if (descr->Columns[ikey][irow]==NULL){
            descrout->Columns[ikey][imeasr]=NULL;
         } else {
            len=strlen(descr->Columns[ikey][irow]);
            descrout->Columns[ikey][imeasr]=CTA_Malloc(sizeof(char)*(len+1));
            strcpy(descrout->Columns[ikey][imeasr],
                   descr->Columns[ikey][irow]);
         }
      }
   }

   // Clean up memory
   free(index);
   CTA_Vector_Free(&vtimes);
}




void CTA_ObsDescr_table_initialise(CTA_ObsDescrClass *hobsdescrcl)
{
   CTA_Intf hintf=0;
   CTA_Func h_func[I_CTA_OBSDESCR_NUMFUNC];

   // The vector h_func is filled with COSTA-function handles of the 
   // implementations in this file.
   CTA_Func_Create(" ",&CTAI_ObsDescr_table_Create_Size,    hintf,
                       &h_func[I_CTA_OBSDESCR_CREATE_SIZE]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_table_Create_Init,    hintf,
                       &h_func[I_CTA_OBSDESCR_CREATE_INIT]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_table_Property_Count,    hintf,
                       &h_func[I_CTA_OBSDESCR_COUNT_PROPERTIES]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_table_Get_Properties,    hintf,
                       &h_func[I_CTA_OBSDESCR_GET_PROPERTIES]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_table_Observation_Count, hintf,
                       &h_func[I_CTA_OBSDESCR_COUNT_OBSERVATIONS]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_table_Get_Keys,    hintf,
                       &h_func[I_CTA_OBSDESCR_GET_KEYS]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_table_Export,    hintf,
                       &h_func[I_CTA_OBSDESCR_EXPORT]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_table_Free,    hintf,
                       &h_func[I_CTA_OBSDESCR_FREE]);
   CTA_Func_Create(" ",&CTAI_ObsDescr_table_CreateSel,    hintf,
                       &h_func[I_CTA_OBSDESCR_SELECTION]);

   CTA_ObsDescr_DefineClass("cta_obsdescr_table",h_func,hobsdescrcl);
}
