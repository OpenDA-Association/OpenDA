/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_tree.c $
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

#include <stdio.h>
#include <string.h>
#include "cta_mem.h"
#include "f_cta_utils.h"
#include "cta_defaults.h"
#include "cta_handles.h"
#include "cta_time.h"
#include "cta_vector.h"
#include "cta_errors.h"
#include "cta_tree.h"
#include "ctai_datatypes.h"
#include "ctai_handles.h"
#include "ctai_string.h"
#include "ctai_vector.h"
#include "ctai_sobs.h"
#include "cta_message.h"

#define SIGNAL_COUNT (-222)

#define CTA_TREE_CREATE_F77            F77_CALL(cta_tree_create,CTA_TREE_CREATE)
#define CTA_TREE_FREE_F77              F77_CALL(cta_tree_free,CTA_TREE_FREE)
#define CTA_TREE_ADDHANDLE_F77        F77_CALL(cta_tree_addhandle,CTA_TREE_ADDHANDLE)
#define CTA_TREE_COUNT_HANDLES_F77     F77_CALL(cta_tree_counthandles,CTA_TREE_COUNTHANDLES)
#define CTA_TREE_COUNT_HANDLESSTR_F77  F77_CALL(cta_tree_counthandlesstr,CTA_TREE_COUNTHANDLESSTR)
#define CTA_TREE_GET_HANDLESTR_F77     F77_CALL(cta_tree_gethandlestr,CTA_TREE_GETHANDLESTR)
#define CTA_TREE_GET_VALUESTR_F77      F77_CALL(cta_tree_getvaluestr,CTA_TREE_GETVALUESTR)
#define CTA_TREE_GET_HANDLE_F77        F77_CALL(cta_tree_gethandle,CTA_TREE_GETHANDLE)
#define CTA_TREE_GET_VALUE_F77         F77_CALL(cta_tree_getvalue,CTA_TREE_GETVALUE)
#define CTA_TREE_COUNT_ITEMS_F77       F77_CALL(cta_tree_countitems,CTA_TREE_COUNTITEMS)
#define CTA_TREE_GET_ITEM_F77          F77_CALL(cta_tree_getitem,CTA_TREE_GETITEM)
#define CTA_TREE_GET_ITEMVALUE_F77     F77_CALL(cta_tree_getitemvalue,CTA_TREE_GETITEMVALUE)
#define CTA_TREE_PRINT_F77             F77_CALL(cta_tree_print,CTA_TREE_PRINT)

#define CLASSNAME "CTA_Tree"


/* Struct holding all data associated to an COSTA tree */
typedef struct {
   CTA_Vector hvector;
} CTAI_Tree;


/* Create a COSTA tree */
int CTA_Tree_Create(CTA_Tree *htree){

   CTAI_Tree *tree;
   int retval;

   /* Allocate memory for new tree object */
   tree=CTA_Malloc(sizeof(CTAI_Tree));
   retval = CTA_Vector_Create(CTA_DEFAULT_VECTOR, 0, CTA_HANDLE, CTA_HANDLE, &(tree->hvector));

   /* Allocate new handle and return error when unsuccessful */
   retval=CTA_Handle_Create("tree",CTA_TREE,tree,htree);
   return retval;
}


/* Release a COSTA tree */
#undef METHOD
#define METHOD "Free"  
int CTA_Tree_Free(CTA_Tree *htree)
{
   CTAI_Tree *tree;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) *htree,CTA_TREE);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_tree handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) *htree,(void**) &tree);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* Remove the actual tree */
   retval = CTA_Vector_Free(&(tree->hvector));
   free(tree);
   retval=CTA_Handle_Free(htree);
   return retval;
}


/* Add a COSTA handle to the COSTA tree */
#undef METHOD
#define METHOD "AddHandle" 
int CTA_Tree_AddHandle(CTA_Tree htree, const char *name, CTA_Handle hitem)
{
   CTAI_Tree *tree;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) htree, CTA_TREE);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_tree handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) htree,(void**) &tree);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   retval = CTAI_Handle_SetName(hitem, name);
   if (retval != CTA_OK) {
	   CTA_WRITE_ERROR("Cannot set name");
	   return retval;
   }

   return CTA_Vector_AppendVal(tree->hvector, &hitem, CTA_HANDLE);
}


/* Get the next name from a path */
/* A path must be separated by / or \ */
static void CTAI_Tree_GetNextName(const char *path, char *name, const char **psplitpos) {
   const char *splitpos;
   const char *slashpos;
   const char *bslashpos;
   const char *srchpath;

   /* If the first character of the path is a separator, skip it */
   srchpath = path;
   if (*srchpath == '/' || *srchpath == '\\') {
      ++srchpath;
   }

   /* Find first path component */
   slashpos = strchr(srchpath, '/');
   bslashpos = strchr(srchpath, '\\');
   if (slashpos && !bslashpos) {
      splitpos = slashpos;
   } else if (bslashpos && !slashpos) {
      splitpos = bslashpos;
   } else if (slashpos && bslashpos) {
      splitpos = (slashpos < bslashpos ? slashpos : bslashpos);
   } else{
      splitpos = NULL;
   }

   if (splitpos) {
      /* Get the path part upto the split position */
      strncpy(name, srchpath, splitpos - srchpath);
      name[splitpos - srchpath] = '\0';
   } else {
      /* No split position, take the whole path */
      strcpy(name, srchpath);
   }

   *psplitpos = splitpos;
}


/* Find a COSTA handle in the COSTA tree */
/* A path must be separated by / or \ */
/* Returns the handle, or CTA_NULL if not found */
static CTA_Handle CTAI_Tree_FindHandle(CTA_Tree htree, const char *path, BOOL skiproot)
{
   const CTAI_Tree *tree;
   const char *splitpos;
   char *name;
   CTA_Handle h;
   CTA_Handle hdefault;
   CTA_Datatype dt;
   int retval;

   /* If path is NULL, return CTA_NULL */
   if (!path) return CTA_NULL;

   /* Get vector */
   retval=CTA_Handle_GetData((CTA_Handle) htree,(void**) &tree);
   if (retval!=CTA_OK) return retval;

   /* Allocate and get name */
   name = (char*)CTA_Malloc(1 + strlen(path));
   CTAI_Tree_GetNextName(path, name, &splitpos);

   /* If this name is the root, skip it */
   if (skiproot && splitpos && (*(splitpos + 1) != '\0') &&
       0 == strcmp(CTAI_Handle_GetName(htree), name)) {
      CTAI_Tree_GetNextName(splitpos + 1, name, &splitpos);
   }

   /* Find the given element on this level */
   h = CTAI_Vector_FindHandle(tree->hvector, name);
   free(name);
   if (h == CTA_NULL) return h;


   /* Check whether there is path remaining */
   if (splitpos && (*(splitpos + 1) != '\0'))
   {
      /* Check whether it's a tree */
      retval = CTA_Handle_GetDatatype(h, &dt);
      if (retval != CTA_OK) return retval;
      if (dt == CTA_TREE) {
         /* It's a tree, recurse */
         return CTAI_Tree_FindHandle(h, splitpos + 1, FALSE);
      }
   }

   /* If the handle found is a tree with a default value,
      return the default value */
   retval = CTA_Handle_GetDatatype(h, &dt);
   if (retval != CTA_OK) return retval;
   if (dt == CTA_TREE) {
      /* It's a tree, try to find a default value */
      hdefault = CTAI_Tree_FindHandle(h, "", FALSE);
      if (hdefault != CTA_NULL) {
         h = hdefault;
      }
   }

   /* Return the handle found */
   return h;
}



#undef METHOD
#define METHOD "GetHandleStr"
int CTA_Tree_GetHandleStr(CTA_Tree htree, char* str, CTA_Handle *hitem){
   CTA_String hstr;
   int retval;

   retval=CTA_String_Create(&hstr);
   if (retval != CTA_OK) {
       CTA_WRITE_ERROR("Cannot create string");
       return retval;
   }
   retval=CTA_String_Set(hstr,str);
   if (retval != CTA_OK) {
       CTA_WRITE_ERROR("Cannot set string");
       return retval;
   }
   retval=CTA_Tree_GetHandle(htree, hstr, hitem);
   if (retval != CTA_OK) {
	   char message[1024];
	   sprintf(message,"Cannot get tree handle: %s \n",str);
       CTA_WRITE_INFO(message);
       return retval;
   }
   retval=CTA_String_Free(&hstr);
   if (retval != CTA_OK) {
       CTA_WRITE_ERROR("Cannot free string");
       return retval;
   }
   return CTA_OK;
};


#undef METHOD
#define METHOD "GetValueStr"
int CTA_Tree_GetValueStr(CTA_Tree htree, char* str, void *value, CTA_Datatype datatype){
   CTA_String hstr;
   int retval;
   char msg[256];

   retval=CTA_String_Create(&hstr);
   if (retval != CTA_OK) {
       CTA_WRITE_ERROR("Cannot create string");
       return retval;
   }
   retval=CTA_String_Set(hstr,str);
   if (retval != CTA_OK) {
       CTA_WRITE_ERROR("Cannot set string");
       return retval;
   }
   retval=CTA_Tree_GetValue(htree, hstr, value, datatype);
   if (retval != CTA_OK) {
       sprintf(msg,"Cannot get value '%s' from tree",str);    
       CTA_WRITE_ERROR(msg);
       return retval;
   }
   retval=CTA_String_Free(&hstr);
   if (retval != CTA_OK) {
       CTA_WRITE_ERROR("Cannot free string");
       return retval;
   }
   return CTA_OK;
};

/* Find a COSTA handle in the COSTA tree */
/* A path must be separated by / or \ */
#undef METHOD
#define METHOD "GetHandle"
int CTA_Tree_GetHandle(CTA_Tree htree, CTA_String path, CTA_Handle *hitem)
{
   int retval;
   const char *pstr;


   retval=CTA_Handle_Check((CTA_Handle) htree, CTA_TREE);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_tree handle");
       return retval;
   }

   pstr = CTAI_String_GetPtr(path);
   if (pstr) {
      *hitem = CTAI_Tree_FindHandle(htree, pstr, TRUE);
      return (*hitem == CTA_NULL ? CTA_ITEM_NOT_FOUND : CTA_OK);
   }
   *hitem = 0;
   return CTA_ITEM_NOT_FOUND;
}


/** \brief Return the value of a COSTA handle from the COSTA tree (by path)
 *
 *  \note In case of trees with default values, returns the default value.
 *
 *  \param htree    I handle of the tree instance
 *  \param path     I path of the item, separated by / or \
 *  \param value    O value of the COSTA item, or CTA_NULL in case of not found
 *  \param datatype I datatype of the value specified
 *  \return CTA_OK if successful or CTA_ITEM_NOT_FOUND in case of not found
 */
int CTA_Tree_GetValue(CTA_Tree htree, CTA_String path, void *value, CTA_Datatype datatype)
{
   int        retval;
   CTA_Handle hitem;

   retval = CTA_Tree_GetHandle(htree, path, &hitem);
   if (retval == CTA_OK) {
      retval = CTA_Handle_GetValue(hitem, value, datatype);
   } else {
      *(CTA_Handle *)value = CTA_NULL;
   }
   return retval;
}

/* Count the number of COSTA handles in the COSTA tree */
/* A path must be separated by / or \ */
static int CTAI_Tree_CountHandles(CTA_Tree htree, const char *path, BOOL skiproot)
{
   const CTAI_Tree *tree;
   const CTAI_Tree *subtree;
   const char *splitpos;
   const char *skippos = NULL;
   char *name;
   CTA_Handle h;
   CTA_Datatype dt;
   int retval;
   int count = 0;
   BOOL root_skipped = FALSE;

   /* If path is NULL, return CTA_NULL */
   if (!path) return CTA_NULL;

   /* Get vector */
   retval=CTA_Handle_GetData((CTA_Handle) htree,(void**) &tree);
   if (retval!=CTA_OK) return -1;

   /* Allocate and get name */
   name = (char*)CTA_Malloc(1 + strlen(path));
   CTAI_Tree_GetNextName(path, name, &splitpos);

   /* If this name is the root, skip it */
   if (skiproot && splitpos && (*(splitpos + 1) != '\0') &&
      0 == strcmp(CTAI_Handle_GetName(htree), name)) {
      root_skipped = TRUE;
      skippos = splitpos;
      CTAI_Tree_GetNextName(splitpos + 1, name, &splitpos);
   }

   /* Find the given element on this level */
   h = CTAI_Vector_FindHandle(tree->hvector, name);
   if (h == CTA_NULL) {
      count = -1;
      goto return_pos;
   }

   /* Check whether there is path remaining */
   if (splitpos && (*(splitpos + 1) != '\0'))
   {
      /* Check whether it's a tree */
      retval = CTA_Handle_GetDatatype(h, &dt);
      if (retval != CTA_OK) {
         count = -1;
         goto return_pos;
      }
      if (dt == CTA_TREE) {
         /* It's a tree, recurse */
         count = CTAI_Tree_CountHandles(h, splitpos + 1, FALSE);
         if (count == SIGNAL_COUNT) {
            retval=CTA_Handle_GetData((CTA_Handle) h,(void**) &subtree);
            if (retval != CTA_OK) {
               count = -1;
               goto return_pos;
            }
            count = CTAI_Vector_CountHandles(subtree->hvector, splitpos + 1);
         }
      }
   } else {
      if (root_skipped) {
         if (skippos && (*(skippos + 1) != '\0')) {
            /* No recursion yet: count elements on root level */
            retval=CTA_Handle_GetData((CTA_Handle)htree, (void**)&subtree);
            if (retval != CTA_OK) {
               count = -1;
               goto return_pos;
            }
            count = CTAI_Vector_CountHandles(subtree->hvector, skippos + 1);
         } else {
            /* There is only one root */
            count = 1;
         }
      } else {
         /* Return SIGNAL_COUNT to signal that the count should be determined */
         count = SIGNAL_COUNT;
      }
   }

return_pos:
   free(name);
   return count;
}


/** \brief Counts the number of COSTA handles specified by the given path.
 *
 *  \param htree  I handle of the tree instance
 *  \param path   I path of the item, separated by / or \
 *  \param hitem  O number of items found
 *  \return CTA_OK if successful or CTA_ITEM_NOT_FOUND in case of not found
 */
#undef METHOD
#define METHOD "CountHandles" 
int CTA_Tree_CountHandles(CTA_Tree htree, CTA_String path, int *count) {
   int        retval;
   const char *pstr;

   retval=CTA_Handle_Check((CTA_Handle) htree, CTA_TREE);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_tree handle");
       return retval;
   }

   pstr = CTAI_String_GetPtr(path);
   if (pstr) {
      *count = CTAI_Tree_CountHandles(htree, pstr, TRUE);
      return (*count < 0 ? CTA_INTERNAL_ERROR : CTA_OK);
   }
   *count = -1;
   return CTA_INTERNAL_ERROR;
}

/** \brief Counts the number of COSTA handles specified by the given path.
 *
 *  \param htree  I handle of the tree instance
 *  \param path   I path of the item, separated by / or \
 *  \param hitem  O number of items found
 *  \return CTA_OK if successful or CTA_ITEM_NOT_FOUND in case of not found
 */
#undef METHOD
#define METHOD "CountHandleStr" 
int CTA_Tree_CountHandlesStr(CTA_Tree htree, char *path, int *count) {
   int        retval;

   retval=CTA_Handle_Check((CTA_Handle) htree, CTA_TREE);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_tree handle");
       return retval;
   }

   if (path) {
      *count = CTAI_Tree_CountHandles(htree, path, TRUE);
      return (*count < 0 ? CTA_INTERNAL_ERROR : CTA_OK);
   }
   *count = -1;
   return CTA_INTERNAL_ERROR;
}



/** \brief Return the number of elements on the current level of the COSTA tree
 *
 *  \param htree  I handle of the tree level
 *  \param count  O number of elements on the current tree level
 *  \return CTA_OK if successful
 */
#undef METHOD
#define METHOD "CountItems" 
int CTA_Tree_CountItems(CTA_Tree htree, int *count) {
   const CTAI_Tree *tree;
   int             retval;

   retval=CTA_Handle_Check((CTA_Handle) htree, CTA_TREE);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("cta_handle_check in cta_tree_countItems failed.");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) htree,(void**) &tree);
   if (retval!=CTA_OK) {
     CTA_WRITE_ERROR("cta_handle_getdata in cta_tree_countItems failed.");
     return retval;
   }
   return CTA_Vector_GetSize(tree->hvector, count);
 }


/** \brief Returns an element on the current level of the COSTA tree
 *
 *  \param htree  I handle of the tree level
 *  \param index  I index of the item to return. 1 <= index <= CTA_Tree_CountItems()
 *  \param hitem  O handle of the item to return
 *  \return CTA_OK if successful
 */
#undef METHOD
#define METHOD "GetItem"
int CTA_Tree_GetItem(CTA_Tree htree, int index, CTA_Handle *hitem) {
   const CTAI_Tree *tree;
   int             retval;

   retval=CTA_Handle_Check((CTA_Handle) htree, CTA_TREE);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_tree handle");
       return retval;
   };
   retval=CTA_Handle_GetData((CTA_Handle) htree,(void**) &tree);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   return CTA_Vector_GetVal(tree->hvector, index, hitem, CTA_HANDLE);
}


/** \brief Return the value of a COSTA handle from the COSTA tree (by index)
 *
 *  \note In case of trees with default values, returns the default value.
 *
 *  \param htree    I handle of the tree instance
 *  \param index    I index of the item
 *  \param value    O value of the COSTA item, or CTA_NULL in case of not found
 *  \param datatype I datatype of the value specified
 *  \return CTA_OK if successful or CTA_ITEM_NOT_FOUND in case of not found
 */
int CTA_Tree_GetItemValue(CTA_Tree htree, int index, void *value, CTA_Datatype datatype)
{
   int        retval;
   CTA_Handle hitem;

   retval = CTA_Tree_GetItem(htree, index, &hitem);
   if (retval == CTA_OK) {
      retval = CTA_Handle_GetValue(hitem, value, datatype);
   } else {
      *(CTA_Handle *)value = CTA_NULL;
   }
   return retval;
}


/** \brief Print a COSTA tree to STDOUT
*
*  \param htree  I handle of the tree
*  \return CTA_OK if successful
*/
static void ctatree_print(CTA_Handle h, int depth)
{
   int          i;
   int          j;
   int          n;
   int          count;
   double       t1, t2, ts;
   CTA_Datatype dt;
   CTAI_Tree*   tree;
   CTA_Handle   val;
   const char*  text;

   /* Print depth */


   for (i = 0; i < depth; ++i) {
      printf("| ");
   }

   CTA_Handle_GetDatatype(h, &dt);
   /* Print handle name */
   printf("name: '%s': ", CTAI_Handle_GetName(h));

   /* If this handle is a tree, handle each argument at depth + 1 */
   switch (dt) {
   case CTA_TREE:
      printf("tree\n");

      /* Get vector */
      CTA_Handle_GetData((CTA_Handle) h,(void**) &tree);

      // ce test of het wel vector is
      CTA_Handle_GetDatatype(h, &dt);
      //if (dt == CTA_TREE) printf(" inderdaad een tree %d \n",retval );

      CTA_Tree_CountItems(h, &count) ;

      /* Get number of elements in the current tree */
      CTA_Vector_GetSize(tree->hvector, &n);

      /* Recurse */
      for (i = 1; i <= n; ++i) {
         CTA_Vector_GetVal(tree->hvector, i, &val, CTA_HANDLE);
         ctatree_print(val, depth + 1);
      }
      break;
   case CTA_STRING:
      /* Print string text */
      printf("string: '%s'\n", CTAI_String_GetPtr(h));
      break;
   case CTA_TIME:
      /* Print time span */
      CTA_Time_GetSpan(h, &t1, &t2);
      CTA_Time_GetStep(h, &ts);
      printf("time: from %lg to %lg, step %lg\n", t1, t2, ts);
      break;
   case CTA_VECTOR:
      /* Print vector */
      CTA_Vector_GetSize(h, &n);
      CTA_Vector_GetDatatype(h, &dt);
      printf("vector: type='%s', dim=%d\n", CTAI_Type2String(dt), n);
      for (j = 1; j <= n; ++j) {
         for (i = 0; i <= depth; ++i) {
            printf("| ");
         }
         text = CTAI_Vector_GetStringVal(h, j);
         printf("element[%d] = '%s'\n", j, text);
         free((char*)text);
      }
      break;
   case CTA_SOBS:
      /* Print stochastic observer */
      val = CTAI_SObs_GetUserData(h, 1);
      if (val != CTA_NULL) {
         printf("stochastic observer: database='%s'\n", CTAI_String_GetPtr(val));
      } else {
         printf("stochastic observer\n");
      }
      break;
   default:
      printf("unknown data type\n");
      break;
   }
}


/** \brief Print a COSTA tree to STDOUT
*
*  \param htree  I handle of the tree
*  \return CTA_OK if successful
*/
#undef METHOD
#define METHOD "Print"
int CTA_Tree_Print(CTA_Tree htree)
{
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) htree, CTA_TREE);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_tree handle");
       return retval;
   }

   ctatree_print(htree, 0);
   return CTA_OK;
}

/* Interfacing with Fortran */
CTAEXPORT void CTA_TREE_CREATE_F77(int *htree, int *ierr) {
   *ierr=CTA_Tree_Create((CTA_Tree*) htree);
}

CTAEXPORT void CTA_TREE_FREE_F77(int *htree, int *ierr) {
   *ierr=CTA_Tree_Free((CTA_Tree*) htree);
}

CTAEXPORT void CTA_TREE_ADDHANDLE_F77(int *htree, int *hname, int *hitem, int *ierr) {
   *ierr=CTA_Tree_AddHandle((CTA_Tree)*htree, CTAI_String_GetPtr(*hname), (CTA_Handle)*hitem);
}

CTAEXPORT void CTA_TREE_COUNT_HANDLES_F77(int *htree, int *hpath, int *count, int *ierr) {
   *ierr=CTA_Tree_CountHandles((CTA_Tree)*htree, (CTA_String)*hpath, count);
}

CTAEXPORT void CTA_TREE_COUNT_HANDLESSTR_F77(int *htree, char *path, int *count, int *ierr, int len_str) {
   char *c_str;

   /* create a c-string equivalent to name */
   c_str=CTA_Malloc((len_str+1)*sizeof(char));
   CTA_fstr2cstr(path,c_str,len_str);


   *ierr=CTA_Tree_CountHandlesStr((CTA_Tree)*htree, c_str, count);
   free(c_str);
}



CTAEXPORT void CTA_TREE_GET_HANDLESTR_F77(int *htree, char* str, int *hitem, int* ierr, int len_str){
   char *c_str;

   /* create a c-string equivalent to name */
   c_str=CTA_Malloc((len_str+1)*sizeof(char));
   CTA_fstr2cstr(str,c_str,len_str);

   *ierr=CTA_Tree_GetHandleStr((CTA_Tree) *htree, c_str, (CTA_Handle*) hitem);

   free(c_str);
}


CTAEXPORT void CTA_TREE_GET_VALUESTR_F77(int *htree, char* str, void *value, int *datatype,
                         int *ierr, int len_str){
   char *c_str;

   /* create a c-string equivalent to name */
   c_str=CTA_Malloc((len_str+1)*sizeof(char));
   CTA_fstr2cstr(str,c_str,len_str);

  *ierr=CTA_Tree_GetValueStr( (CTA_Tree)*htree, c_str, value, (CTA_Datatype)*datatype);

   free(c_str);
}


CTAEXPORT void CTA_TREE_GETHANDLE_F77(int *htree, int *hpath, int *hitem, int *ierr) {
   *ierr=CTA_Tree_GetHandle((CTA_Tree)*htree, (CTA_String)*hpath, (CTA_Handle*)hitem);
}

CTAEXPORT void CTA_TREE_GET_VALUE_F77(int *htree, int *hpath, void *value, int *datatype, int *ierr) {
   *ierr=CTA_Tree_GetValue((CTA_Tree)*htree, (CTA_String)*hpath, value, *datatype);
}

CTAEXPORT void CTA_TREE_COUNT_ITEMS_F77(int *htree, int *count, int *ierr) {
   *ierr=CTA_Tree_CountItems((CTA_Tree)*htree, count);
}

CTAEXPORT void CTA_TREE_GET_ITEM_F77(int *htree, int *index, int *hitem, int *ierr) {
   *ierr=CTA_Tree_GetItem((CTA_Tree)*htree, *index, (CTA_Handle*)hitem);
}

CTAEXPORT void CTA_TREE_GET_ITEMVALUE_F77(int *htree, int *index, void *value, int *datatype, int *ierr) {
   *ierr=CTA_Tree_GetItemValue((CTA_Tree)*htree, *index, value, *datatype);
}

CTAEXPORT void CTA_TREE_PRINT_F77(int *htree, int *ierr) {
   *ierr=CTA_Tree_Print((CTA_Tree)*htree);
}

