/*
$URL: https://repos.deltares.nl/repos/openda/trunk/costa/src/cta/cta_model_factory.c $
$Revision: 676 $, $Date: 2008-10-09 17:00:24 +0200 (Thu, 09 Oct 2008) $

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

//#include <libxml/encoding.h>
//#include <libxml/parser.h>
//#include <libxml/tree.h>
//#include <libxml/xinclude.h>
//#include <libxml/xmlwriter.h>

#include "cta.h"
#include "cta_mem.h"

#include "cta_datatypes.h"
#include "ctai_xml.h"
#include "cta_handles.h"
#include "ctai_handles.h"
#include "f_cta_utils.h"
#include "cta_model.h"
#include "cta_errors.h"
#include "cta_f77lapack.h"
#include "cta_reltable.h"
#include "cta_par.h"
#include "cta_message.h"

#define MAX(a,b) (a>b ? a: b)
#define IDEBUG (0)

#define CTA_SOBS_DEFINECLASS_F77      F77_CALL(cta_sobs_defineclass,CTA_SOBS_DEFINECLASS)

#define CLASSNAME "CTA_Sobs_Factory"

typedef struct {
CTA_Func functions[CTA_SOBS_NUMFUNC];
  CTA_ObsDescrClass descrcl;
} CTAI_SObsClass;


#undef METHOD
#define METHOD "CTAI_SObs_DuplicateClass"
int CTAI_SObs_DuplicateClass(CTA_SObsClass hsobscl,
                              CTA_SObsClass *hsobscl_dup){

   CTAI_SObsClass *clsdata;
   CTAI_SObsClass *clsdata_dup;
   CTA_Func hfunc;
   CTA_String sname;
   int retval;
   int i;
   char msg[256];

   /* Get class data containing all function pointers */
   retval=CTA_Handle_Check((CTA_Handle) hsobscl,CTA_SOBSCLASS);
   if (retval!=CTA_OK) return retval;
   retval=CTA_Handle_GetData((CTA_Handle) hsobscl,(void*) &clsdata);
   if (retval!=CTA_OK) return retval;


   /* Allocate new SObsClass object */
   clsdata_dup=CTA_Malloc(sizeof(CTAI_SObsClass));


   /* Duplicate all function pointers */
   for (i=0;i<CTA_SOBS_NUMFUNC;i++){
      CTA_Func_Duplicate(clsdata->functions[i], &hfunc);
      clsdata_dup->functions[i]=hfunc;
   }


   // Allocate new handle and return eror when unsuccesfull
   CTA_String_Create(&sname);
   CTA_Handle_GetName(hsobscl, sname);
   if (IDEBUG>0) printf("CTAI_SObs_DuplicateClass: NAME OF SOBS CLASS IS %s\n", CTAI_String_GetPtr(sname));


   retval=CTA_Handle_Create(CTAI_String_GetPtr(sname), CTA_SOBSCLASS,
                            clsdata_dup, hsobscl_dup);
   if (retval!=CTA_OK) {
      sprintf(msg,"Cannot create a handle of type CTA_SOBSCLASS with name '%s'",CTAI_String_GetPtr(sname));
      CTA_WRITE_ERROR(msg);
   }
   CTA_String_Free(&sname);
   return retval;
}


#undef METHOD
#define METHOD "DefineClass"
int CTA_SObs_DefineClass(
   // INPUTS:
      const char *name,                // Name of the new stochobs class
      const CTA_Func h_func[CTA_SOBS_NUMFUNC],  // function handles to
                                       // the implementations of the
                                       // stochobs-class' functions.
      CTA_ObsDescrClass descrcl,       //
   // OUTPUTS:
      CTA_SObsClass  *hsobscl          // The (handle to) the new stochobs-class
   ){

   CTAI_SObsClass *data;
   int retval;
   int i;


   /* Allocate new StochObs object */
   data=CTA_Malloc(sizeof(CTAI_SObsClass));

   data->descrcl = descrcl;
   for (i=0;i<CTA_SOBS_NUMFUNC;i++){
      data->functions[i]=h_func[i];
   }

   // Allocate new handle and return eror when unsuccesfull
   retval=CTA_Handle_Create(name,CTA_SOBSCLASS,data,hsobscl);
   retval=CTA_Handle_GetData((CTA_Handle) *hsobscl,(void**) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   // return error when unsuccesfull
   return retval;
}

#undef METHOD
#define METHOD "DefineClass2"
int CTA_SObs_DefineClass2(
   // INPUTS:
      const char *name,                // Name of the new stochobs class
      const CTA_Func h_func[CTA_SOBS_NUMFUNC],  // function handles to
                                       // the implementations of the
                                       // stochobs-class' functions.
      CTA_ObsDescrClass descrcl,       //
   // OUTPUTS:
      CTA_SObsClass  *hsobscl          // The (handle to) the new stochobs-class
   ){

   CTAI_SObsClass *data;
   int retval;
   int i;


   /* Allocate new StochObs object */
   data=CTA_Malloc(sizeof(CTAI_SObsClass));

   data->descrcl = descrcl;
   for (i=0;i<CTA_SOBS_NUMFUNC;i++){
      data->functions[i]=h_func[i];
   }

   // Allocate new handle and return eror when unsuccesfull
   retval=CTA_Handle_Create(name,CTA_SOBSCLASS,data,hsobscl);
   retval=CTA_Handle_GetData((CTA_Handle) *hsobscl,(void**) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   // return error when unsuccesfull
   return retval;
}


/* Interfacing with Fortran */

CTAEXPORT void  CTA_SOBS_DEFINECLASS_F77(char *name, int *h_func, int *descrcl,
                               int   *hstochobscl, int *ierr,
                               int len_name){

   char  *c_name;

   /* create a c-string equivalent to name */
   c_name=CTA_Malloc((len_name+1)*sizeof(char));
   CTA_fstr2cstr(name,c_name,len_name);

  *ierr=CTA_SObs_DefineClass( name, (CTA_Func*) h_func,
   (CTA_ObsDescrClass) *descrcl, (CTA_SObsClass*) hstochobscl);

   free(c_name);
}

/** \brief Create a COSTA sobsl class from XML
*          (load from methods from dynamic load library).
*
*  \param cur_node  I  Current XML node
*  \return             Handle to create or CTA_NULL in case of an error.
*/
CTA_SObsClass CTAI_XML_CreateSObsClass(xmlNode *cur_node) {

   CTA_Func      hfunc;                   /* the new function */
   xmlChar       *id = NULL;              /* id of function in XML-tree */
   xmlChar       *clsname = NULL;         /* (lookup) name of the sobs class */
   int           retval;                  /* return status of creation       */
   CTA_Func      h_func[CTA_SOBS_NUMFUNC];/* List of functions */
   xmlNode       *func_node = NULL;       /* values of children nodes */
   int i;
   CTA_SObsClass hsobscl;                 /* function class */
   CTA_SObsClass hsobscl_old;
   const char *name;
   CTA_String   sclsname;
   CTA_ObsDescrClass descrcl;


   if (IDEBUG>0) printf("CTAI_XML_CreateSObsClass: Start of function\n");
   /* Parse this node's attributes */
   /* Get id */
   id = xmlGetProp(cur_node, CTAI_XML_ID);

   /* Get class name */
   clsname = xmlGetProp(cur_node, CTAI_XML_NAME);

   /* Check whether this is a known sobs-class */
   CTA_String_Create(&sclsname);
   CTA_String_Set(sclsname,(char *) clsname);
   retval=CTA_Handle_Find(sclsname, CTA_SOBSCLASS, &hsobscl_old);
   CTA_String_Free(&sclsname);

   if (retval==CTA_OK) {
      /* duplicate sobs class */

      CTAI_SObs_DuplicateClass(hsobscl_old, &hsobscl);

      /* sobs-class is known */
      if (IDEBUG>0) printf("CTAI_XML_CreateSObsClass: found sobsclass '%s'\n",clsname);
      if (IDEBUG>0) printf("CTAI_XML_CreateSObsClass: duplication is created \n");

   } else {
      /* Set all function handles to CTA_NULL */
      for (i=0;i<CTA_SOBS_NUMFUNC;i++){
         h_func[i]=CTA_NULL;
      }

      /* Load all functions that are specified in input */
      for (func_node = cur_node->children; func_node;
              func_node = func_node->next) {
         if (0 == strcmp("CTA_FUNCTION", (char *) func_node->name)){
            hfunc=CTAI_XML_CreateFunc(func_node);
            if (hfunc!=CTA_NULL){
               name = CTAI_Handle_GetName(hfunc);
               if (0 == strcmp("CTA_SOBS_CREATE_SIZE", name)){
                  h_func[CTA_SOBS_CREATE_SIZE    ]=hfunc;
               } else if (0 == strcmp("CTA_SOBS_CREATE_INIT", name)){
                  h_func[CTA_SOBS_CREATE_INIT    ]=hfunc;
               } else if (0 == strcmp("CTA_SOBS_FREE", name)){
                  h_func[I_CTA_SOBS_FREE           ]=hfunc;
               } else if (0 == strcmp("CTA_SOBS_CREATE_SELECTION", name)){
                  h_func[CTA_SOBS_CREATE_SELECTION        ]=hfunc;
               } else if (0 == strcmp("CTA_SOBS_COUNT", name)){
                  h_func[I_CTA_SOBS_COUNT      ]=hfunc;
               } else if (0 == strcmp("CTA_SOBS_OBS_DESCRIPTION", name)){
                  h_func[CTA_SOBS_GET_OBS_DESCRIPTION      ]=hfunc;
               } else if (0 == strcmp("CTA_SOBS_GET_VALUES", name)){
                  h_func[CTA_SOBS_GET_VALUES     ]=hfunc;
               } else if (0 == strcmp("CTA_SOBS_GET_TIMES", name)){
                  h_func[CTA_SOBS_GET_TIMES     ]=hfunc;
               } else if (0 == strcmp("CTA_SOBS_GET_REALISATION", name)){
                  h_func[CTA_SOBS_GET_REALISATION       ]=hfunc;
               } else if (0 == strcmp("CTA_SOBS_GET_EXPECTATION", name)){
                  h_func[CTA_SOBS_GET_EXPECTATION       ]=hfunc;
               } else if (0 == strcmp("CTA_SOBS_EVALUATE_PDF", name)){
                  h_func[CTA_SOBS_EVALUATE_PDF       ]=hfunc;
               } else if (0 == strcmp("CTA_SOBS_GET_COV_MATRIX", name)){
                  h_func[CTA_SOBS_GET_COV_MATRIX       ]=hfunc;
               } else if (0 == strcmp("CTA_SOBS_GET_VARIANCE", name)){
                  h_func[CTA_SOBS_GET_VARIANCE       ]=hfunc;
               } else if (0 == strcmp("CTA_SOBS_EXPORT", name)){
                  h_func[I_CTA_SOBS_EXPORT       ]=hfunc;
               } else {
                  printf("CTAI_XML_CreateSObsClass :Warning found unknown node %s\n", func_node->name);
               }
            }
         }
      }

      /* Create a default observation description. Should be changed??? */
      CTA_ObsDescr_sqlite3_initialise(&descrcl);

      /* Create new function class */
      retval=CTA_SObs_DefineClass2((char *)clsname, h_func, descrcl, &hsobscl);

      /* Set id (=name) of handle */
      CTAI_Handle_SetName(hsobscl, (char *) id);

   }
   CTAI_Handle_SetName(hsobscl, (char *) id);
   xmlFree(id);
   xmlFree(clsname);

   if (IDEBUG>0) printf("CTAI_XML_CreateSObsClass: End of function\n");
   return hsobscl;
}



