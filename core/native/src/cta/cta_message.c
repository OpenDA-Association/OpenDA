/*
$URL: https://repos.deltares.nl/repos/openda/openda_1/public/trunk/openda_core/native/src/cta/cta_interface.c $
$Revision: 671 $, $Date: 2008-10-07 14:49:42 +0200 (di, 07 okt 2008) $

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

#include "cta_message.h"
#include "ctai_handles.h"
#include "cta_errors.h"


#define CTA_MESSAGE_QUIET_F77  F77_CALL(cta_message_quiet,CTA_MESSAGE_QUIET)

#define CLASSNAME "CTA_Message"

// Handle to an external writer
CTA_Func CTAI_Message_externalWriter=CTA_NULL;

int CTAI_Quiet=0;

void CTA_Message_Quiet(int setting){
   CTAI_Quiet=setting;
}


#define METHOD "Write"
void CTA_Message_Write(const char *className, const char *method, const char *message, char type){


   CTA_Function *externalFunction;
   int ierr;

   // if program is in quiet mode return directly
   if (CTAI_Quiet) return;

   if (CTAI_Message_externalWriter==CTA_NULL) {
      if (type=='m' || type=='M' ) {
         printf("%s\n",message);
      }
      else if (type=='i' || type=='I') {
         printf("Info: (%s.%s) %s\n",className, method, message);
      }
      else if (type=='w' || type=='W') {
         printf("Warning: (%s.%s) %s\n",className, method, message);
      }
      else if (type=='e' || type=='E') {
	          printf("Error: (%s.%s) %s\n",className, method, message);
      }
      else if (type=='f' || type=='F') {

		printf("FATAL ERROR: (%s.%s) %s\n",className, method, message);
        printf("APPLICATION WILL BE TERMINATED\n");
   		exit(-1);
      }
      else {
         printf("Error: (cta_msg.CTA_Message_Write) illegal message type (%c)\n", type);
         printf("Message: (%s.%s) %s\n",className, method, message);
      }
	}
   else {
      ierr=CTA_Func_GetFunc(CTAI_Message_externalWriter, &externalFunction);
      if (ierr != CTA_OK) {
          // Error, first "kill" external writer before writing error
          CTAI_Message_externalWriter=CTA_NULL;
          printf("Cannot get pointer to external writer");
      }
      else {
        // Call external writer
        (void) externalFunction(className, method, message, type);
      }
   }
}

#undef METHOD
#define METHOD "SetExternalWriter"
void CTA_Message_SetExternalWriter(CTA_Func externalWriter){

   if (externalWriter==CTA_NULL){
      CTAI_Message_externalWriter=CTA_NULL;
   }
   else {
      if (CTAI_Handle_GetDatatype(externalWriter)!=CTA_FUNCTION){
          CTA_WRITE_ERROR("Input handle is not a function handle.\n External writer is not changed\n");
      }
      else {
         CTAI_Message_externalWriter=externalWriter;
      }
   }
}

/* Interfacing with Fortran */

CTAEXPORT void CTA_MESSAGE_QUIET_F77(int *setting){
   CTA_Message_Quiet(*setting);
}
