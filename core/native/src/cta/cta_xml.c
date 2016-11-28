/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_xml.c $
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

#include <string.h>
#include "cta_mem.h"
#include "ctai_xml.h"
#include "f_cta_utils.h"
#include "cta_datatypes.h"
#include "cta_defaults.h"
#include "cta_errors.h"
#include "cta_time.h"
#include "cta_vector.h"
#include "cta_sobs.h"
#include "cta_xml.h"
#include "ctai.h"
#include "ctai_datatypes.h"
#include "ctai_handles.h"
#include "ctai_string.h"
#include "ctai_vector.h"
#include "ctai_sobs.h"
#include "cta_message.h"


#define CTA_XML_READ_F77   F77_CALL(cta_xml_read,CTA_XML_READ)
#define CTA_XML_WRITE_F77  F77_CALL(cta_xml_write,CTA_XML_WRITE)

#define CLASSNAME "CTA_XML"

/* Local interfaces */
CTA_Handle CTAI_XML_CreateObject_Comb(xmlNode *cur_node);
CTA_TreeVector CTAI_XML_CreatetreeVector(xmlNode *cur_node);
int CTAI_XML_TreeVector_regular_grid(CTA_TreeVector treevec, CTA_TreeVector root );
CTA_SObsClass CTAI_XML_CreateSObsClass(xmlNode *cur_node);
CTA_ModelClass CTAI_XML_CreateModelClass(xmlNode *cur_node);
void CTAI_XML_WriteTreeVector(int issubtree, int isxfv,CTA_TreeVector treevec, int level,xmlTextWriter *writer);

/***************************************************************
READ
***************************************************************/

/** \brief Get content of a text element; remove training spaces, tabs etc.
*
*  \param   cur_node  I  Text node.
*  \return  Text content of the given (TEXT) node. To free().
*/
static char *CTAI_XML_GetContent(xmlNode *cur_node) {
   char       *outtxt;
   const xmlChar *intxt;
   int         pos;

   intxt = cur_node->content;
   /* Find position of the last non-special character */
   pos = strlen((char *) intxt) - 1;
   while (pos >= 0 && intxt[pos] < 33) {
      --pos;
   }
   /* Allocate return string */
   if (pos < 0) {
      /* Empty string */
      outtxt = (char*)CTA_Malloc(1);
      outtxt[0] = '\0';
   } else {
      /* Non-empty string */
      outtxt = (char*)CTA_Malloc(2 + pos);
      memcpy(outtxt, intxt, pos + 1);
      outtxt[pos + 1]='\0';
   }
   return outtxt;
}


/** \brief Create a default COSTA tree.
*
*  \param cur_node  I  Current XML node
*  \return             Handle to create or CTA_NULL in case of an error.
*/
static CTA_Tree CTAI_XML_CreateDefaultTree(xmlNode *cur_node) {
   xmlChar       *val;        /* element  value */
   xmlAttr       *cur_prop;   /* property loop counter */
   CTA_Tree      htree;       /* COSTA tree */
   CTA_String    hstr;        /* COSTA string */
   int           retval;

   /* Get node text */
   retval = CTA_Tree_Create(&htree);
   if (retval == CTA_OK) {
      CTAI_Handle_SetName(htree, (char *) cur_node->name);


      if (0!=strcmp((char *) cur_node->name,"treeVectorFile")) {

        /* Parse this node's attributes (only if it is not treeVectorFile*/

        for (cur_prop = cur_node->properties; cur_prop; cur_prop = cur_prop->next) {

          val = xmlGetProp(cur_node, cur_prop->name);
          CTA_String_Create(&hstr);
          CTA_String_Set(hstr, (char *) val);
          xmlFree(val);
          CTA_Tree_AddHandle(htree, (char *) cur_prop->name, hstr);
        }
      }
      return htree;
   }
   return CTA_NULL;
}



static CTA_Tree CTAI_XML_Create_Combine_SObs(xmlNode *startnode, CTA_Tree hparent) {
   xmlNode       *cur_node;   /* node loop counter */
   CTA_Handle     hobj;       /* COSTA object */


   for (cur_node = startnode; cur_node; cur_node = cur_node->next) {
      if (cur_node->type == XML_ELEMENT_NODE || cur_node->type == XML_TEXT_NODE) {
         /* Create COSTA object  from the current node */
        hobj = CTAI_XML_CreateObject_Comb(cur_node);
         if (hobj != CTA_NULL) {
            if (hparent == CTA_NULL) {
               /* Create tree */
               hparent = hobj;
            } else {
               /* Add object to the parent */
               CTA_Tree_AddHandle(hparent, CTAI_Handle_GetName(hobj), hobj);
            }
            if (CTAI_Handle_GetDatatype(hobj) == CTA_TREE) {
               /* In case of a tree, handle nested COSTA objects */
               CTAI_XML_Create_Combine_SObs(cur_node->children, hobj);
            }
         }
      }
   }


   return hparent;

}


/** \brief Create a COSTA stochastic observer.
*
*  \param cur_node  I  Current XML node
*  \return             Handle to create or CTA_NULL in case of an error.
*/
static CTA_StochObs CTAI_XML_CreateSObs(xmlNode *cur_node) {
   CTA_StochObs  hnew, hnew2;             /* The new COSTA handle */
   int           retval;
   xmlChar       *name     = NULL;
   xmlChar       *database = NULL;
   xmlChar       *selection = NULL;
   xmlChar       *timeoffset = NULL;
   xmlChar       *sobs_class_name = NULL;
   CTA_SObsClass sobsclass = CTA_DEFAULT_SOBS;
   CTA_String    hsel;
   int           is_combined;
   CTA_Tree      hnew_sub;
   CTA_Handle    hdb;
   CTA_Vector    hvec1, htimeoffset;
   double        timeoffsetval;


   is_combined = 0;
   /* Parse this node's attributes */
   name     = xmlGetProp(cur_node, CTAI_XML_ID);
   sobs_class_name = xmlGetProp(cur_node, CTAI_XML_SOBSCLASSNAME);
   database = xmlGetProp(cur_node, CTAI_XML_DATABASE);
   selection = xmlGetProp(cur_node, CTAI_XML_SELECT);
   timeoffset = xmlGetProp(cur_node, CTAI_XML_TIMEOFFSET);

   /* first check if the stochobs is a 'combined' one */

   if (sobs_class_name) {
     if (0==strcmp((char *) sobs_class_name,"CTA_COMBINE_SOBS")) {
       sobsclass = CTA_COMBINE_SOBS ;
       printf("-- ctai_create_sobs: combiner! --\n");

       // read a subtree using the sobs-combiner-xml-read
       // hnew_sub is a valid cta_subtree with the cta_sobs already created
       hnew_sub = CTAI_XML_Create_Combine_SObs(cur_node->children,CTA_NULL);
        is_combined = 1;

     }
     else {    //sobsclass can be sql or netcdf or ...
       // TODO: better: a  handle_find or sobsclass_find action
       printf("-- ctai_create_sobs: class name given ,NO combiner! --\n");
       if (0==strcmp((char *) sobs_class_name,"CTA_NETCDF_SOBS")) {
       sobsclass = CTA_NETCDF_SOBS ;
     }
     }
   }
   else {   // sobsclass remains default
       printf("-- ctai_create_sobs: NO class name given! --\n");
     };


   hnew=CTA_NULL;


      /* Create stochastic observer */

   CTA_String_Create(&hdb);
      if (database) {


          CTA_String_Set(hdb, (char *) database);


          if (timeoffset){
             timeoffsetval = atof((char *) timeoffset);

             // make a vector of two handles: the subtree and the offset value
             retval = CTA_Vector_Create(CTA_DEFAULT_VECTOR, 2, CTA_HANDLE, CTA_NULL, &hvec1);
             // fill first element (name of database)
             retval = CTA_Vector_SetVal(hvec1,1,&hdb,CTA_HANDLE);
             //fill second element
             retval = CTA_Vector_Create(CTA_DEFAULT_VECTOR, 1, CTA_DOUBLE, CTA_NULL, &htimeoffset);

             retval = CTA_Vector_SetVal(htimeoffset,1,&timeoffsetval,CTA_DOUBLE);
             retval = CTA_Vector_SetVal(hvec1,2,&htimeoffset,CTA_HANDLE);
             // create sobs

             retval = CTA_SObs_Create(sobsclass, hvec1, &hnew);

          } else {

            // 'old' situation: userdata is name of database
          retval = CTA_SObs_Create(sobsclass, hdb, &hnew);
          }
      }
      else { //combined CTA_OBS
        if (is_combined == 1) {
          /* create the combined SObs  with for the 2nd argument not a database
             but the subsobs subtree and the timeoffset  */
          // make a vector of two handles: the subtree and the offset value
          retval = CTA_Vector_Create(CTA_DEFAULT_VECTOR, 2, CTA_HANDLE, CTA_NULL, &hvec1);
          // fill first element
          retval = CTA_Vector_SetVal(hvec1,1,&hnew_sub,CTA_HANDLE);
          //fill second element
          retval = CTA_Vector_Create(CTA_DEFAULT_VECTOR, 1, CTA_DOUBLE, CTA_NULL, &htimeoffset);
          if (timeoffset){
             timeoffsetval = atof((char *) timeoffset);
          } else {
             timeoffsetval = 0.0;
          }
          retval = CTA_Vector_SetVal(htimeoffset,1,&timeoffsetval,CTA_DOUBLE);
          retval = CTA_Vector_SetVal(hvec1,2,&htimeoffset,CTA_HANDLE);


          retval = CTA_SObs_Create(sobsclass, hvec1, &hnew);

        } else {
          printf("ERROR: neither database given nor combined observer\n");
          exit(-1);
        }

      }


      CTA_String_Free(&hdb);
      if (retval == CTA_OK) {
         /* Perform initial selection */
         if (selection) {
            CTA_String_Create(&hsel);
            CTA_String_Set(hsel, (char *) selection);
            retval=CTA_SObs_CreateSel(hnew, hsel, &hnew2);
            CTA_String_Free(&hsel);
            CTA_SObs_Free(&hnew);
            if (retval != CTA_OK){
               return CTA_NULL;
            }
            hnew=hnew2;
         }

         /* Set name (if any) */
         if (name) {
            CTAI_Handle_SetName(hnew, (char *) name);
           // xmlFree(name);
         } else {
            CTAI_Handle_SetName(hnew, "");
         }
      }

      //      if (is_combined == 1) {
        // attach combined-subtree to sobs

      //}

   if (name) xmlFree(name);
   if (database) xmlFree(database);
   if (selection) xmlFree(selection);

   return hnew;
}



   CTA_Handle CTAI_XML_CreateObject_Comb(xmlNode *cur_node) {
   xmlChar       *val;              /* Element value */
   CTA_Handle    hnew = CTA_NULL;   /* The return value */

   // read objects in part of the xml-file containing the subSObs. The objects are either SObs or default trees.

   if (cur_node->type == XML_TEXT_NODE) {
      /* If this node is a non-empty text node, create a string */
      val = (xmlChar *) CTAI_XML_GetContent(cur_node);


      if (strlen((char *) val) < 1) {
      if (val) xmlFree(val);
         return CTA_NULL;
      }
      CTA_String_Create(&hnew);
      CTA_String_Set(hnew, (char *) val);
      xmlFree(val);
      CTAI_Handle_SetName(hnew, "");
   } else {
      /* This is not a text node */
      switch (CTAI_String2Type((char *) cur_node->name)) {
      case CTA_SOBS:
        /* Create a COSTA stochastic observer */
        hnew = CTAI_XML_CreateSObs(cur_node);
        break;
      }
      if (hnew == CTA_NULL) {
        /* Default: create a COSTA subtree */
        hnew = CTAI_XML_CreateDefaultTree(cur_node);
      }
   }
   return hnew;
}




/** \brief Create a COSTA time.
*
*  \param cur_node  I  Current XML node
*  \return             Handle to create or CTA_NULL in case of an error.
*/
static CTA_Time CTAI_XML_CreateTime(xmlNode *cur_node) {
   CTA_Time      hnew;                    /* The new COSTA handle */
   int           retval;
   xmlChar       *name   = NULL;
   xmlChar       *sstart = NULL;
   xmlChar       *sstop  = NULL;
   xmlChar       *sstep  = NULL;
   double        dstart;
   double        dstop;
   double        dstep;
   double        tmp;

   /* Parse this node's attributes */
   name   = xmlGetProp(cur_node, CTAI_XML_ID);
   sstart = xmlGetProp(cur_node, CTAI_XML_START);
   sstop  = xmlGetProp(cur_node, CTAI_XML_STOP);
   sstep  = xmlGetProp(cur_node, CTAI_XML_STEP);

   /* Convert to numbers and/or fill defaults */
   if (sstart) {
      dstart = atof((char *) sstart);
      xmlFree(sstart);
   } else {
      dstart = 0.0;
   }
   if (sstop) {
      dstop = atof((char *) sstop);
      xmlFree(sstop);
   } else {
      dstop = dstart;
   }
   if (dstop < dstart) {
      tmp = dstart;
      dstart = dstop;
      dstop = tmp;
   }
   if (sstep) {
      dstep = atof((char *) sstep);
      xmlFree(sstep);
   } else {
      /* Default */
      dstep = MIN(1.0, (dstop - dstart));
   }

   /* Create time */
   retval = CTA_Time_Create(&hnew);
   if (retval == CTA_OK) {
      /* Set name (if any) */
      if (name) {
         CTAI_Handle_SetName(hnew, (char *) name);
         xmlFree(name);
      } else {
         CTAI_Handle_SetName(hnew, "");
      }

      /* Set values */
      CTA_Time_SetSpan(hnew, dstart, dstop);
      CTA_Time_SetStep(hnew, dstep);
      return hnew;
   }
   return CTA_NULL;
}




/** \brief Create a COSTA object.
*
*  \param cur_node  I  Current XML node
*  \return             Handle for a COSTA node to create or 0 in case of an error.
*/
 static CTA_Handle CTAI_XML_CreateObject(xmlNode *cur_node) {
   char       *val;              /* Element value */
   CTA_Handle    hnew = CTA_NULL;   /* The return value */
   CTA_Metainfo minfo;

   if (cur_node->type == XML_TEXT_NODE) {
      /* If this node is a non-empty text node, create a string */
      val = CTAI_XML_GetContent(cur_node);


      if (strlen(val) < 1) {
      if (val) free(val);
         return CTA_NULL;
      }
      CTA_String_Create(&hnew);
      CTA_String_Set(hnew, val);
      free(val);
      CTAI_Handle_SetName(hnew, "");
   } else {
      /* This is not a text node */
      switch (CTAI_String2Type((char *) cur_node->name)) {
      case CTA_TIME:
         /* Create a COSTA time */
         hnew = CTAI_XML_CreateTime(cur_node);
         break;
      case CTA_VECTOR:
         /* Create a COSTA vector */
        CTA_Metainfo_Create(&minfo);
        hnew = CTAI_XML_CreateVector_New(cur_node,minfo);
         break;
      case CTA_TREEVECTOR:
        /* Create a (general) treeVector */
        hnew = CTAI_XML_CreatetreeVector(cur_node);
        // retval = CTA_TreeVector_Export(hnew, CTA_FILE_STDOUT);
        CTAI_XML_TreeVector_regular_grid(hnew, hnew);
        break;
      case CTA_FUNCTION:
        /* Create a COSTA function */
        hnew = CTAI_XML_CreateFunc(cur_node);
        break;
      case CTA_SOBS:
        /* Create a COSTA stochastic observer */
        hnew = CTAI_XML_CreateSObs(cur_node);
        break;
      case CTA_SOBSCLASS:
        /* Create a COSTA stochastic observer class */
        hnew = CTAI_XML_CreateSObsClass(cur_node);
        break;
      case CTA_MODELCLASS:
        hnew = CTAI_XML_CreateModelClass(cur_node);
        break;
      }
      if (hnew == CTA_NULL) {
        /* Default: create a COSTA subtree */
        hnew = CTAI_XML_CreateDefaultTree(cur_node);
      }
   }
   return hnew;
}


/** \brief Recursively read elements
*
*  \param startnode  I  XML node to start from
*  \param hparent    I  parent COSTA handle
*  \return              tree root
*/
static CTA_Tree CTAI_XML_Read(xmlNode *startnode, CTA_Tree hparent) {
   xmlNode       *cur_node;   /* node loop counter */
   CTA_Handle     hobj;       /* COSTA object */

   for (cur_node = startnode; cur_node; cur_node = cur_node->next) {
      if (cur_node->type == XML_ELEMENT_NODE || cur_node->type == XML_TEXT_NODE) {
         /* Create COSTA object  from the current node */
        hobj = CTAI_XML_CreateObject(cur_node);
         if (hobj != CTA_NULL) {
            if (hparent == CTA_NULL) {
               /* Create tree */
               hparent = hobj;
            } else {
               /* Add object to the parent */
               CTA_Tree_AddHandle(hparent, CTAI_Handle_GetName(hobj), hobj);
            }
            if (CTAI_Handle_GetDatatype(hobj) == CTA_TREE) {
               /* In case of a tree, handle nested COSTA objects */
               CTAI_XML_Read(cur_node->children, hobj);
            }
         }
      }
   }
   return hparent;
}


/** \brief Read a COSTA XML file into a tree
*
*  \param hfname  I  file name of XML file to read
*  \param hroot   O  handle of a COSTA tree
*  \return CTA_OK if successful
*/
int CTA_XML_Read(CTA_String hfname, CTA_Tree *hroot) {
   char       *fname;
   xmlDoc     *doc;
   xmlNode    *root_element;
   int         retval;

   /* Check handle types */
   retval=CTA_Handle_Check((CTA_Handle) hfname, CTA_STRING);
   if (retval!=CTA_OK) return retval;
   /* Initialize libxml */
   LIBXML_TEST_VERSION;

   /* Parse the file and get the DOM */
   fname = CTAI_String_Allocate(hfname);
   doc = xmlReadFile(fname, NULL, 0);
   free(fname);
   if (!doc) {
      // OLD CODEhroot = NULL;
      *hroot = CTA_NULL;
      return CTA_CANNOT_OPEN_FILE;
   }

#ifdef LIBXML_XINCLUDE_ENABLED
   /* Handle xinclude */
   xmlXIncludeProcess(doc);
#endif

   /* Parse the XML tree */
   root_element = xmlDocGetRootElement(doc);
   *hroot = CTAI_XML_Read(root_element, CTA_NULL);

   /* Clean up */
   xmlFreeDoc(doc);
   return CTA_OK;
}


/***************************************************************
WRITE
***************************************************************/

/**
* ConvertInput:
* @in: string in a given encoding
* @encoding: the encoding used
*
* Converts @in into UTF-8 for processing with libxml2 APIs
*
* Returns the converted UTF-8 string, or NULL in case of error.
*/
xmlChar *ConvertInput(const char *in, const char *encoding)
{
   xmlChar *out;
   int ret;
   int size;
   int out_size;
   int temp;
   xmlCharEncodingHandlerPtr handler;

   if (in == 0)
      return 0;

   handler = xmlFindCharEncodingHandler(encoding);

   if (!handler) {
      printf("ConvertInput: no encoding handler found for '%s'\n",
         encoding ? encoding : "");
      return 0;
   }

   size = (int) strlen(in) + 1;
   out_size = size * 2 - 1;
   out = (unsigned char *) xmlMalloc((size_t) out_size);

   if (out != 0) {
      temp = size - 1;
      ret = handler->input(out, &out_size, (const xmlChar *) in, &temp);
      if ((ret < 0) || (temp - size + 1)) {
         if (ret < 0) {
            printf("ConvertInput: conversion wasn't successful.\n");
         } else {
            printf
               ("ConvertInput: conversion wasn't successful. converted: %i octets.\n",
               temp);
         }

         xmlFree(out);
         out = 0;
      } else {
         out = (unsigned char *) xmlRealloc(out, out_size + 1);
         out[out_size] = 0;  /*null terminating out */
      }
   } else {
      printf("ConvertInput: no mem\n");
   }

   return out;
}


/** \brief Generate XML from COSTA stochastic observer
*
*  \param h       I  COSTA handle
*  \param writer  I  the XML text writer
*/
static void CTAI_XML_WriteSObs(CTA_StochObs h, xmlTextWriter *writer) {
   const xmlChar   *name;
   CTA_String   hdb;
   /* Start an element the the name of the tree handle */
   xmlTextWriterStartElement(writer, (xmlChar *) CTAI_Type2String(CTA_SOBS));
   /* Write name (if any) */
   name = (xmlChar *) CTAI_Handle_GetName(h);
   if (name && *name) {
      xmlTextWriterWriteAttribute(writer, CTAI_XML_ID, name);
   }
   /* Write database */
   hdb = CTAI_SObs_GetUserData(h, 1);
   if (hdb == CTA_NULL) {
      xmlTextWriterWriteComment(writer,
         ConvertInput("Cannot find database in stochastic observer", (char *) MY_ENCODING));
   } else {
      xmlTextWriterWriteAttribute(writer, CTAI_XML_DATABASE, (xmlChar *) CTAI_String_GetPtr(hdb));
   }
   /* End the tree level elements */
   xmlTextWriterEndElement(writer);
}


/** \brief Generate XML from COSTA time span
*
*  \param htime   I  COSTA handle
*  \param writer  I  the XML text writer
*/
static void CTAI_XML_WriteTime(CTA_Time htime, xmlTextWriter *writer) {
   const xmlChar   *name;
   double       t1, t2, ts;
   xmlChar      sz[64];

   /* Start an element the the name of the tree handle */
   xmlTextWriterStartElement(writer, (xmlChar *) CTAI_Type2String(CTA_TIME));

   /* Write name (if any) */
   name = (xmlChar *) CTAI_Handle_GetName(htime);
   if (name && *name) {
      xmlTextWriterWriteAttribute(writer, CTAI_XML_ID, name);
   }

   /* Get time span */
   CTA_Time_GetSpan(htime, &t1, &t2);
   CTA_Time_GetStep(htime, &ts);

   /* Write time span */
   sprintf((char *) sz, "%lg", t1);
   xmlTextWriterWriteAttribute(writer, CTAI_XML_START, sz);
   sprintf((char *) sz, "%lg", t2);
   xmlTextWriterWriteAttribute(writer, CTAI_XML_STOP,  sz);
   sprintf((char *) sz, "%lg", ts);
   xmlTextWriterWriteAttribute(writer, CTAI_XML_STEP,  sz);

   /* End the tree level elements */
   xmlTextWriterEndElement(writer);
}





/** \brief Generate XML from one COSTA string
*
*  \param hstr   I  handle of a COSTA string
*  \param writer I  the XML text writer
*/
static void CTAI_XML_WriteString(CTA_String hstr, xmlTextWriter *writer) {
   const xmlChar *name;
   const xmlChar *text;

   name = (xmlChar *) CTAI_Handle_GetName(hstr);
   text = (xmlChar *) CTAI_String_GetPtr(hstr);
   if (!text) {
      xmlTextWriterWriteComment(writer,
         ConvertInput("Cannot retrieve string value", (char *) MY_ENCODING));
      return;
   }

   if (strlen((char *) name) == 0) {
      /* If the name is empty, it's a text element */
      xmlTextWriterWriteString(writer, text);
   } else {
      /* If the name is not empty, it's an attribute */
      xmlTextWriterWriteAttribute(writer, name, text);
   }
}


/** \brief Generate XML from one tree level (recursively)
*
*  \param htree  I  handle of a COSTA tree
*  \param writer I  the XML text writer
*/
static void CTAI_XML_WriteTree(CTA_Tree htree, xmlTextWriter *writer) {
   int        n;
   int        i;
   CTA_Handle hitem;

   /* Start an element the the name of the tree handle */
   xmlTextWriterStartElement(writer, (xmlChar *) CTAI_Handle_GetName(htree));

   /* Walk through all items on the current level */
   CTA_Tree_CountItems(htree, &n);
   for (i = 1; i <= n; ++i) {
      CTA_Tree_GetItem(htree, i, &hitem);
      if (hitem != CTA_NULL) {
         switch (CTAI_Handle_GetDatatype(hitem)) {
         case CTA_TREE:
            /* Recurse */
            CTAI_XML_WriteTree(hitem, writer);
            break;
         case CTA_STRING:
            /* Write a string */
            CTAI_XML_WriteString(hitem, writer);
            break;
         case CTA_TIME:
            /* Write a time span */
            CTAI_XML_WriteTime(hitem, writer);
            break;
         case CTA_VECTOR:
            /* Write a vector */
           CTAI_XML_WriteVector(hitem,"","",0,0, writer);
            break;
         case CTA_TREEVECTOR:
            /* Write a state vector */
           CTAI_XML_WriteTreeVector(0,0,hitem,0, writer);
            break;
         case CTA_SOBS:
            /* Write a stochastic observer */
            CTAI_XML_WriteSObs(hitem, writer);
            break;
         default:
            /* Write a comment */
            xmlTextWriterWriteComment(writer,
               ConvertInput("Cannot write unknown data type", (char *) MY_ENCODING));
            break;
         }
      }
   }

   /* End the tree level element */
   xmlTextWriterEndElement(writer);
}


/** \brief Build an XML document to write to a COSTA XML file
*
*  \param hroot  I  handle of a COSTA tree
*  \param writer I  the XML text writer
*/
static void CTAI_XML_WriteDocument(CTA_Tree hroot, xmlTextWriter *writer) {
/* Start the document with the xml default for the version,
* encoding ISO 8859-1 and the default for the standalone
   * declaration. */
   xmlTextWriterStartDocument(writer, NULL, (char *) MY_ENCODING, NULL);

   /* Write the tree */
   CTAI_XML_WriteTree(hroot, writer);

   /* End the document */
   xmlTextWriterEndDocument(writer);
}


/** \brief Write a tree to a COSTA XML file
*
*  \param fname  I  file name of XML file to write
*  \param hroot  I  handle of a COSTA tree
*  \return CTA_OK if successful
*/
#undef METHOD
#define METHOD "Write"
int CTA_XML_Write(CTA_String hfname, CTA_Tree hroot) {
   char          *fname;
   xmlDoc        *doc;
   xmlTextWriter *writer;
   int           retval;

   /* Check handle types */
   retval=CTA_Handle_Check((CTA_Handle) hfname, CTA_STRING);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vector handle");
       return retval;
   }
   retval=CTA_Handle_Check((CTA_Handle) hroot, CTA_TREE);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* Initialize libxml */
   LIBXML_TEST_VERSION;
   
   /* Create a new xmlWriter for DOM, with no compression. */
   writer = xmlNewTextWriterDoc(&doc, 0);
   if (!writer) {
      return CTA_EXTERNAL_ERROR;
   }

   /* Create document */
   CTAI_XML_WriteDocument(hroot, writer);

   /* Write document and clean up */
   xmlFreeTextWriter(writer);
   fname = CTAI_String_Allocate(hfname);
   xmlSaveFileEnc(fname, doc, (char *) MY_ENCODING);
   xmlFreeDoc(doc);
   free(fname);
   return CTA_OK;
}

/* Interfacing with Fortran */
CTAEXPORT void CTA_XML_READ_F77(int *hfname, int *hroot, int *ierr) {
   *ierr=CTA_XML_Read((CTA_String)*hfname, (CTA_Tree*)hroot);
}

CTAEXPORT void CTA_XML_WRITE_F77(int *hfname, int *hroot, int *ierr) {
   *ierr=CTA_XML_Write((CTA_String)*hfname, (CTA_Tree)*hroot);
}


