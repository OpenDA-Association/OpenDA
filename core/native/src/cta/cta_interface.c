/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_interface.c $
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

#include "cta_mem.h"
#include "cta_interface.h"
#include "cta_errors.h"
#include "cta_message.h"

#define CLASSNAME "CTA_Interface"

/* Struct holding all data associated to an COSTA interface */
// Struct containing specific data associated to a COSTA interface
typedef struct {
int narg;
CTA_Datatype *argtyp;
} CTAI_Interface;


/** \brief Creates a new interface
 *
 * \param name     I name of the new interface
 * \param narg     I number of arguments of interface
 * \param argtyp   I list with the datatypes of arguments
 * \param hintf    O COSTA interface handle
 * \return error status: CTA_OK
 */
#undef METHOD
#define METHOD "Create"
int CTA_Intf_Create(const char *name, const CTA_Datatype *argtyp,
                    const int narg,CTA_Intf *hintf){

   int retval;           //Return value of a call
   CTAI_Interface *data; //Interface specific data
   int i;                //loop counter

   // Allocate data and set properties
   data=CTA_Malloc(sizeof(CTAI_Interface));

   // Set properties
   data->narg=narg;
   data->argtyp=CTA_Malloc(narg*sizeof(CTA_Datatype));

   // Copy values
   for (i=0;i<narg;i++){
      data->argtyp[i]=argtyp[i];
   };

   // Allocate new handle and return eror when unsuccesfull
   retval=CTA_Handle_Create(name,CTA_INTERFACE,data,hintf);
   if (retval) {
	   CTA_WRITE_ERROR("Cannot create handle");
	   return retval;
   }

   return CTA_OK;
}


/** \brief Frees an interface
 *
 *  \note Freeing CTA_NULL is allowed.
 *
 *  \param hintf  I/O  handle of interface. The value is 
 *                     CTA_NULL on return
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE, CTA_INCOMPATIBLE_HANDLE 
 */
#undef METHOD
#define METHOD "Free"
int CTA_Intf_Free(CTA_Intf *hintf){

   int retval;           //Return value of a call
   CTAI_Interface *data; //Interface specific data

   // If handle is CTA_NULL -> nothing to be done
   if (*hintf==CTA_NULL) return CTA_OK;

   // Check Handle and return error if handle is not valid
   retval=CTA_Handle_Check(*hintf,CTA_INTERFACE);
   if (retval) {
	   CTA_WRITE_ERROR("Handle is not a cta_interface handle");
	   return retval;
   }

   // Free data item
   CTA_Handle_GetData(*hintf,(void*) &data); 
   if (data->argtyp) free(data->argtyp);
   free(data);

   //Free Handle
   CTA_Handle_Free(hintf);
   
   return CTA_OK;
}

/** \brief Mactches two interfaces for compatibility argumentlist-argumentlist
 *
 *  \note two interfaces are compatible if all arguments have the same datatype
 *        CTA_VOID is compatible with all other arguments except for CTA_FSTRING

 *  \param argtyp1  I  list with the datatypes of arguments of first interface
 *  \param narg1    I  number of argumetns in first interface
 *  \param argtyp2  I  list with the datatypes of arguments of second interface
 *  \param narg2    I  number of argumetns in second interface
 *  \param flag     O  TRUE if interfaces are compatible FALSE ortherwise
 *  \return error status: CTA_OK
 */
int CTA_Intf_Match_aa(const CTA_Datatype *argtyp1, const int narg1, 
                      const CTA_Datatype *argtyp2, const int narg2,
                      BOOL *flag){

   // Check all arguments:
   // note CTA_VOID can be combined with all other datatypes except for
   // CTA_FSTRING
   *flag=FALSE;

   if (narg1==narg2){
      int i;
      BOOL allok=TRUE;
      for (i=0;i<narg1;i++){
         if (argtyp1[i]!=argtyp2[i]){
            if (argtyp1[i]!=CTA_VOID && argtyp2[i]!=CTA_VOID){
               allok=FALSE;
            }
            else if (argtyp1[i]==CTA_FSTRING ||
                     argtyp2[i]==CTA_FSTRING) { 
               allok=FALSE;
            };
         };
      }; 
      *flag=allok;
   };
   return CTA_OK;
}


/** \brief Mactches two interfaces for compatibility handle-argumentlist
 *
 *  \note two interfaces are compatible if all arguments have the same datatype
 *        CTA_VOID is compatible with all other arguments except for CTA_FSTRING

 *  \param hintf1   I  Handle of first interface
 *  \param argtyp2  I  list with the datatypes of arguments of second interface
 *  \param narg2    I  number of argumetns in second interface
 *  \param flag     O  TRUE if interfaces are compatible FALSE ortherwise
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE, CTA_INCOMPATIBLE_HANDLE 
 */
#undef METHOD
#define METHOD "Match_ha"
int CTA_Intf_Match_ha(const CTA_Intf hintf1,
                      const CTA_Datatype *argtyp2, const int narg2,
                      BOOL *flag){

   int retval;
   CTAI_Interface *data1;
   
   // Check Handle and return error if handle is not valid
   retval=CTA_Handle_Check(hintf1,CTA_INTERFACE);
   if (retval) {
	   CTA_WRITE_ERROR("Handle is not a cta_interface handle");
	   return retval;
   }

   // Get data for first  interface
   CTA_Handle_GetData(hintf1,(void*) &data1); 

   retval=CTA_Intf_Match_aa(data1->argtyp, data1->narg, 
                            argtyp2, narg2, flag);
   return retval;
}


/** \brief Mactches two interfaces for compatibility handle-handle
 *
 *  \note two interfaces are compatible if all arguments have the same datatype
 *        CTA_VOID is compatible with all other arguments except for CTA_FSTRING

 *  \param hintf1   I  Handle of first interface
 *  \param hintf2   I  Handle of second
 *  \param flag     O  TRUE if interfaces are compatible FALSE ortherwise
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE, CTA_INCOMPATIBLE_HANDLE 
 */
#undef METHOD
#define METHOD "Match_hh"
int CTA_Intf_Match_hh(const CTA_Intf hintf1, const CTA_Intf hintf2, BOOL *flag){

   int retval;
   CTAI_Interface *data1, *data2;
   
   // Check Handle and return error if handle is not valid
   retval=CTA_Handle_Check(hintf1,CTA_INTERFACE);
   if (retval) {
	   CTA_WRITE_ERROR("Handle is not a cta_interface handle");
	   return retval;
   }
   retval=CTA_Handle_Check(hintf2,CTA_INTERFACE);
   if (retval) {
	   CTA_WRITE_ERROR("Handle is not a cta_interface handle");	   
	   return retval;
   }

   // Get data for both interfaces
   CTA_Handle_GetData(hintf1,(void*) &data1); 
   CTA_Handle_GetData(hintf2,(void*) &data2); 

   retval=CTA_Intf_Match_aa(data1->argtyp, data1->narg, 
                            data2->argtyp, data2->narg, flag);
   return retval;
}

