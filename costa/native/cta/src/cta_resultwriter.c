/*
$URL: https://repos.deltares.nl/repos/openda/openda_1/public/trunk/core/native/src/cta/cta_time.c $
$Revision: 2751 $, $Date: 2011-09-09 08:58:46 +0200 (Fri, 09 Sep 2011) $

COSTA: Problem solving environment for data assimilation
Copyright (C) 2013  Nils van Velzen

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
#include "cta_resultwriter.h"

#include "cta_mem.h"
#include "cta_tree.h"
#include "cta_string.h"
#include "cta_xml.h"
#include "f_cta_utils.h"
#include "cta_errors.h"
#include "cta_message.h"

/**
 * OpenDA support the concept of resultwriters. The Data assimilation algorithm
 * will presents intermediate results and information on the computations to the
 * result writer. Each implementation of a resultwriter can then handle this data
 * in its own way.
 *
 * See header file cta_resultwriter.h for more information
 *
 * @author nils van velzen
 *
 */
#define CLASSNAME "CTA_ResultWriter"
#ifdef WIN32
#define FILESEP "\\"
#else
#define FILESEP "/"
#endif


/* Struct holding all different data associated to an native array */
#define MaxNativeResultWriters (20)

static int not_initialized=1;

typedef struct {
   int not_initialized;
   int h_config;
   CTA_Func h_putmessage;
   CTA_Func h_putvalue;
   CTA_Func h_putiterationreport;
   CTA_Func h_free;
} CTAI_ResultWriterAdmin;

static CTAI_ResultWriterAdmin resultWriterAdmin[MaxNativeResultWriters];



#undef METHOD
#define METHOD "CTAI_Resultwriter_handleID"
int CTAI_Resultwriter_handleID(int iDWriter, char *config, char *workingDir){
  char msg[256];
  char filename[265];
  int iID, retval;
  CTA_String hfname;
  CTA_Tree htree=CTA_NULL;

  
  //Check Input
  if (iDWriter<0){
     sprintf(msg,"The Id of the resultwriter is negative (%d). This is a programming error in OpenDA",iDWriter);
     CTA_WRITE_ERROR(msg);
     return 0;
  }
  if (iDWriter>=MaxNativeResultWriters){
     sprintf(msg,"The ID of the resultwriter is %d >= MaxNativeResultWriters (=%d). This might be caused by a programming error in OpenDA or you are trying to use too many resultwriters.",iDWriter,MaxNativeResultWriters);
     CTA_WRITE_ERROR(msg);
     return 0;
  }

  //Initialize the local administration when needed
  if (not_initialized){
     for (iID=0;iID<MaxNativeResultWriters;iID++){
        resultWriterAdmin[iID].not_initialized      = 1;
        resultWriterAdmin[iID].h_config             = CTA_NULL; 
        resultWriterAdmin[iID].h_putmessage         = CTA_NULL; 
        resultWriterAdmin[iID].h_putvalue           = CTA_NULL; 
        resultWriterAdmin[iID].h_putiterationreport = CTA_NULL; 
        resultWriterAdmin[iID].h_free               = CTA_NULL; 
     }
     not_initialized=0;
  }
 
  //Initialize this resultwriter when needed
  if (resultWriterAdmin[iDWriter].not_initialized){
    resultWriterAdmin[iDWriter].not_initialized=0;

    //Parse the configuration
    CTA_String_Create(&hfname);
    sprintf(filename,"%s%s%s",workingDir,FILESEP,config);
    CTA_String_Set(hfname,filename);
    retval = CTA_XML_Read(hfname, &htree);
    CTA_String_Free(&hfname);
    if (retval !=CTA_OK){
       sprintf(msg,"Cannot read the configuration file of the native resultwriter (%s)",filename);
       CTA_WRITE_ERROR(msg);
       return 0;
    }
    resultWriterAdmin[iDWriter].h_config = htree;  //Store configuration tree in admin

    //Get the function handles
    retval=CTA_Tree_GetHandleStr(htree, "putmessage",         &(resultWriterAdmin[iDWriter].h_putmessage)        );
    if (retval!=CTA_OK){
       CTA_WRITE_ERROR("Cannot get the function handle of native putmessage");
       CTA_Tree_Free(&htree); //Cleanup
       return 0;
    }
    retval=CTA_Tree_GetHandleStr(htree, "putvalue",           &(resultWriterAdmin[iDWriter].h_putvalue)          );
    if (retval!=CTA_OK){
       CTA_WRITE_ERROR("Cannot get the function handle of native putvalue");
       CTA_Tree_Free(&htree); //Cleanup
       return 0;
    }
    retval=CTA_Tree_GetHandleStr(htree, "putiterationreport", &(resultWriterAdmin[iDWriter].h_putiterationreport));
    if (retval!=CTA_OK){ 
       CTA_WRITE_ERROR("Cannot get the function handle of native putiterationreport");
       CTA_Tree_Free(&htree); //Cleanup
       return 0;
    }

    retval=CTA_Tree_GetHandleStr(htree, "freewriter", &(resultWriterAdmin[iDWriter].h_free));
    if (retval!=CTA_OK){ 
       CTA_WRITE_ERROR("Cannot get the function handle of native freewriter");
       CTA_Tree_Free(&htree); //Cleanup
       return 0;
    }
  }
  return 1;
}


#undef METHOD
#define METHOD "CTA_Resultwriter_putmessage"
int CTA_Resultwriter_putmessage(int iDWriter, char* config, char *workingDir, char *message){
   CTA_Function *putmessage;
   int retval;
   if (CTAI_Resultwriter_handleID(iDWriter, config, workingDir)){
      // Fetch function pointer
      retval=CTA_Func_GetFunc(resultWriterAdmin[iDWriter].h_putmessage,&putmessage);
      if (retval!=CTA_OK){
         CTA_WRITE_ERROR("Cannot get the function pointer");
         return retval;
      }
      // Call user function
      putmessage(iDWriter, resultWriterAdmin[iDWriter].h_config, workingDir, message, &retval,
                 strlen(workingDir), strlen(message));
      return retval;
   }
   else {
      CTA_WRITE_ERROR("Cannot initialize or fetch data for this resultwriter");
      return CTA_RESULTWRITER_ERROR;
   }
}

#undef method
#define method "CTA_Resultwriter_putvalue"
int CTA_Resultwriter_putvalue(int iDWriter, char*config, char *workingDir, char *id, int handle, int outputlevel, char *context, int iteration){
   CTA_Function *putvalue;
   int retval;
   if (CTAI_Resultwriter_handleID(iDWriter, config, workingDir)){
      // Fetch function pointer
      retval=CTA_Func_GetFunc(resultWriterAdmin[iDWriter].h_putvalue,&putvalue);
      if (retval!=CTA_OK){
         CTA_WRITE_ERROR("Cannot get the function pointer");
         return retval;
      }
      // Call user function
      putvalue(iDWriter, resultWriterAdmin[iDWriter].h_config, workingDir, id, handle, outputlevel, context, iteration, &retval,
               strlen(workingDir), strlen(id), strlen(context));
      return retval;
   }
   else {
      CTA_WRITE_ERROR("Cannot initialize or fetch data for this resultwriter");
      return CTA_RESULTWRITER_ERROR;
   }
}

#undef method
#define method "CTA_Resultwriter_putiterationreport"
int CTA_Resultwriter_putiterationreport(int iDWriter, char*config, char *workingDir, int iteration, double cost, int handle){
   
   CTA_Function *putiterationreport;
   int retval;

   if (CTAI_Resultwriter_handleID(iDWriter, config, workingDir)){
      // Fetch function pointer
      retval=CTA_Func_GetFunc(resultWriterAdmin[iDWriter].h_putiterationreport,&putiterationreport);
      if (retval!=CTA_OK){
         CTA_WRITE_ERROR("Cannot get the function pointer");
         return retval;
      }
      // Call user function
      putiterationreport(iDWriter, resultWriterAdmin[iDWriter].h_config, workingDir, iteration, cost, handle, &retval,
                         strlen(workingDir));
      return retval;
   }
   else {
      CTA_WRITE_ERROR("Cannot initialize or fetch data for this resultwriter");
      return CTA_RESULTWRITER_ERROR;
   }
}


#undef method
#define method "CTA_Resultwriter_free"
int CTA_Resultwriter_free(int iDWriter){
   
   CTA_Function *freeWriter;
   int retval;

   if (CTAI_Resultwriter_handleID(iDWriter, NULL, NULL)){
      // Fetch function pointer
      retval=CTA_Func_GetFunc(resultWriterAdmin[iDWriter].h_free,&freeWriter);
      if (retval!=CTA_OK){
         CTA_WRITE_ERROR("Cannot get the function pointer");
         return retval;
      }
      // Call user function
      freeWriter(iDWriter);
      return CTA_OK;
   }
   else {
      CTA_WRITE_ERROR("Cannot initialize or fetch data for this resultwriter");
      return CTA_RESULTWRITER_ERROR;
   }
}


