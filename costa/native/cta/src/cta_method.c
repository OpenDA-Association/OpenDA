/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_method.c $
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
#include "cta_method.h"
#include "cta_message.h"

#define CLASSNAME "CTA_Method"


/* Struct holding all data associated to an COSTA Vector */
typedef struct {
CTA_Func functions[CTA_METH_NUMFUNC];
} CTAI_MethodClass;                    // A SObsClass contains a list of the member-functions



typedef struct {
CTA_Func functions[CTA_METH_NUMFUNC];
void *data;   // pointer to the implementation-specific data.
} CTAI_Method;



#undef METHOD
#define METHOD "DefineClass"
int CTA_Meth_DefineClass(const char *name, const CTA_Func h_func[CTA_METH_NUMFUNC],
                         CTA_MethClass *hmethcl){

   CTAI_MethodClass *data;
   int retval;

   /* Allocate new StochObs object */ 
   data=CTA_Malloc(sizeof(CTAI_MethodClass));
   data->functions[CTA_METH_CREATE_SIZE]=h_func[CTA_METH_CREATE_SIZE];
   data->functions[CTA_METH_CREATE_INIT]=h_func[CTA_METH_CREATE_INIT];
   data->functions[CTA_METH_RUN ]       =h_func[CTA_METH_RUN ];
   data->functions[CTA_METH_FREE       ]=h_func[CTA_METH_FREE       ];

   // Allocate new handle
   retval=CTA_Handle_Create(name,CTA_METHOD,data,hmethcl);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_method handle");
       return retval;
   }
   // return error when unsuccesfull 
   return retval;
}


#undef METHOD
#define METHOD "Create"
int CTA_Meth_Create(CTA_Method hmethcl, CTA_Handle userdata,
                    CTA_Method *hmeth){

   CTAI_Method *meth;
   int memsize;
   int retval;
   CTAI_MethodClass *clsdata;
   CTA_Function *my_Create_Size, *my_Create_Init;
   int i;

   /* Get class data containing all function pointers */
   retval=CTA_Handle_Check((CTA_Handle) hmethcl,CTA_METHODCLASS);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_methodclass handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hmethcl,(void**) &clsdata);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }


   /* determine size of data object (CTA_SOBS_CREATE_SIZE)*/
   retval=CTA_Func_GetFunc(clsdata->functions[CTA_METH_CREATE_SIZE],
                           &my_Create_Size);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function CTA_METH_CREATE_SIZE");
	   return retval;
   }
   my_Create_Size(&memsize,&retval);
   if (retval) {
	   CTA_WRITE_ERROR("Error in my_Create_Size");
	   return retval;
   }
   
   /* allocate memory for new stochobs object */
   meth=CTA_Malloc(sizeof(CTAI_Method));
   meth->data=CTA_Malloc(memsize);

   /* copy function pointers */
   for (i=0;i<CTA_METH_NUMFUNC;i++){
      meth->functions[i]=clsdata->functions[i];
   }

   /* Initialise and fill new stochobs */
   retval=CTA_Func_GetFunc(clsdata->functions[CTA_METH_CREATE_INIT],
                           &my_Create_Init);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function CTA_METH_CREATE_INIT");
	   return retval;
   }
   my_Create_Init(meth->data, userdata, &retval);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Error in my_Create_Init");
	   return retval;
   }

   /* Allocate new handle and return error when unsuccesfull */
   retval=CTA_Handle_Create("method",CTA_METHOD,meth,hmeth);
   if (retval) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   return CTA_OK;
}




int CTAI_Meth_member_function(
        // INPUTS
        CTA_Method hmeth,    /* Handle of the method of
                                  which a member function is wanted */
        int member,            /* Code of the member function */
        // OUTPUT
        CTAI_Method **meth,      /* All data of method  */
        CTA_Function **memfun  /* Member-Function pointer */
)
{
   int retval;
   /* Check that the given handle is indeed an observer */
   retval=CTA_Handle_Check((CTA_Handle) hmeth, CTA_METHOD);
   if (retval!=CTA_OK) return retval;

   /* Get pointer to struct with observer data */
   retval=CTA_Handle_GetData((CTA_Handle) hmeth,(void**) meth);
   if (retval!=CTA_OK) return retval;

   /* Get pointer to implementation of this function */
   retval=CTA_Func_GetFunc((*meth)->functions[member],memfun);
   return retval;
}


int CTA_Meth_Run(CTA_Method hmeth) {
   /* Local variables */
   int retval;                 /* Return value of COSTA call   */
   CTAI_Method *meth;            /* All data of method           */
   CTA_Function *memfun;       /* Function that must be called */

   /* Look up member function and method data */
   retval = CTAI_Meth_member_function(hmeth, CTA_METH_RUN,
                                      &meth, &memfun);
   if (retval!=CTA_OK) return retval;

   /* Call (user) implementation */
   memfun(meth->data,&retval);
   return retval;
};

int CTA_Meth_Free(CTA_Method *hmeth){
   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Method *meth;      /* All data of meth */
   CTA_Function *my_free;  /* Function that must be called */

   /* Get pointer to implementation of this function */
   
   if (*hmeth==CTA_NULL) return CTA_OK;

   /* Look up member function and method data */
   retval = CTAI_Meth_member_function(*hmeth, CTA_METH_FREE,
                                      &meth, &my_free);
   if (retval!=CTA_OK) return retval;

   /* Call (user) implementation */
   my_free(meth->data,&retval);
   free(meth->data);
   free(meth);
   retval=CTA_Handle_Free(hmeth);

   return retval;
};


