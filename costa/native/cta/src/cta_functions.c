/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_functions.c $
$Revision: 3797 $, $Date: 2013-02-05 16:05:31 +0100 (Tue, 05 Feb 2013) $

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

// Header files for loading dynamic librarys and functions
#ifdef HAVE_DLFCN_H
#include<dlfcn.h>
#endif
#ifdef WIN32
#include <windows.h>
#endif

#include "ctype.h"
#include <stdio.h>
#include "cta_mem.h"
#include "f_cta_utils.h"
#include "cta_functions.h"
#include "cta_errors.h"
#include "ctai_xml.h"
#include "cta_message.h"

#define CTA_FUNC_CREATE_F77   F77_CALL(cta_func_create,CTA_FUNC_CREATE)
#define CTA_FUNC_FREE_F77     F77_CALL(cta_func_free,CTA_FUNC_FREE)
#define CTA_FUNC_GETINTF_F77  F77_CALL(cta_func_getintf,CTA_FUNC_GETINTF)
#define CTA_FUNC_GETNAME_F77  F77_CALL(cta_func_getname,CTA_FUNC_GETNAME)
#define CLASSNAME "CTA_Functions"


#define IDEBUG (0)
// Struct containing specific data associated to a COSTA function handle
typedef struct {
   void *function;        //pointer to function
   CTA_Intf hintf;        //handle of corresponding interface
   CTA_Handle userdata;   //userdata (array of COSTA handles)
   void *libhandle;       //Handle of dynamic library
   int  refcount;         //Number of references to the function
} CTAI_Function;


/** \brief Creates a new user defined function
 *
 * \param name     I name of the new user function
 * \param function I pointer to user funtion
 * \param hintf    I COSTA interface handle
 * \param hfunc    O COSTA user function handle
 * \return error status: CTA_OK
 */
#undef METHOD
#define METHOD "Func_Create"
int CTA_Func_Create(const char *name, CTA_Function *function,
                    const CTA_Intf hintf, CTA_Func *hfunc){

   int retval;            //Return value of a call
   CTAI_Function *data;   //Function specific data

   // Allocate data and set properties
   data=CTA_Malloc(sizeof(CTAI_Function));

   // Set properties
   if (IDEBUG>0) {
      printf("CTA_Func_Create: creating function name=%s\n",name);
      printf("function pointer is %p\n",function);
   }
   data->function=function;
   data->userdata=CTA_NULL;
   data->libhandle=NULL;
   data->refcount=1;

   // Allocate new handle and return eror when unsuccesfull
   retval=CTA_Handle_Create(name,CTA_FUNCTION,data,hfunc);
   if (retval) {
	   CTA_WRITE_ERROR("Cannot create handle");
	   return retval;
   }
   if (IDEBUG>0) printf("function handle is %d\n",*hfunc);

   return CTA_OK;
}

/** \brief Duplicates a user defined function
 *
 * \param hfunc      I COSTA user function handle
 * \param hdupl      I duplication of hfunc
 * \return error status: CTA_OK
 */
#undef METHOD
#define METHOD "Duplicate"
int CTA_Func_Duplicate(CTA_Func hfunc, CTA_Func *hdupl){

   int retval;            //Return value of a call
   CTAI_Function *data;   //Function specific data

   // Set default return value
   *hdupl=CTA_NULL;

   // If handle is CTA_NULL -> nothing to be done
   if (hfunc==CTA_NULL) return CTA_OK;

   // Check Handle and return error if handle is not valid
   retval=CTA_Handle_Check(hfunc,CTA_FUNCTION);
   if (retval) {
	   CTA_WRITE_ERROR("Handle is not a cta_function handle");
       return retval;
   }

   // get data
   CTA_Handle_GetData(hfunc,(void*) &data);

   // increase reference count
   data->refcount++;

   // copy handle
   *hdupl=hfunc;

   return CTA_OK;
}

/** \brief Frees a user defined function
 *
 *  \note Freeing CTA_NULL is allowed.
 *
 *  \param hfunc  IO  handle of user function. The value is
 *                   CTA_NULL on return
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE, CTA_INCOMPATIBLE_HANDLE
 */
#undef METHOD
#define METHOD "Free"
int CTA_Func_Free(CTA_Func *hfunc){

   int retval;            //Return value of a call
   CTAI_Function *data;   //Function specific data
   char msg[256];

   // If handle is CTA_NULL -> nothing to be done
   if (*hfunc==CTA_NULL) return CTA_OK;

   // Check Handle and return error if handle is not valid
   retval=CTA_Handle_Check(*hfunc,CTA_FUNCTION);
   if (retval) {
	   CTA_WRITE_ERROR("Handle is not a cta_function handle");
       return retval;
   }

   // get data
   CTA_Handle_GetData(*hfunc,(void*) &data);

   // decrease reference count
   data->refcount--;

   // If last reference is cleared, delete function
   if (data->refcount==0){
      // Check whether to unload dll
      if (data->libhandle) {
#ifdef HAVE_DLFCN_H
         if (dlclose(data->libhandle)){
            sprintf(msg, "Cannot close library: %s\n",dlerror());
	    CTA_WRITE_WARNING(msg);
         }
#endif
      }

      // Free data item
      free(data);

      //Free Handle
      CTA_Handle_Free(hfunc);
   } else {
      *hfunc=CTA_NULL;
   }
   return CTA_OK;
}


/** \brief Returns interface handle of a user defined function
 *
 *  \param hfunc  I  handle of user function
 *  \param hintf  O  handle of interface
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE, CTA_INCOMPATIBLE_HANDLE
 */
#undef METHOD
#define METHOD "GetIntf"
int CTA_Func_GetIntf(const CTA_Func hfunc, CTA_Intf *hintf){

   int retval;            //Return value of a call
   CTAI_Function *data;   //Function specific data

   // Check Handle and return error if handle is not valid
   retval=CTA_Handle_Check(hfunc,CTA_FUNCTION);
   if (retval)  {
	   CTA_WRITE_ERROR("Handle is not a cta_function handle");
       return retval;
   }

   // get data-item
   CTA_Handle_GetData(hfunc,(void*) &data);

   // return interface
   *hintf=data->hintf;

   return CTA_OK;
}

/** \brief Returns function pointer of a user function
 *
 *  \param hfunc    handle of user function
 *  \param function pointer to user defined function
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE, CTA_INCOMPATIBLE_HANDLE
 */
#undef METHOD
#define METHOD "GetFunc"
int CTA_Func_GetFunc(const CTA_Func hfunc, CTA_Function **function){

   int retval;            //Return value of a call
   CTAI_Function *data;   //Function specific data

   // Check Handle and return error if handle is not valid
   retval=CTA_Handle_Check(hfunc,CTA_FUNCTION);
   if (retval) {
	   CTA_WRITE_ERROR("Handle is not a cta_function handle");
	   return retval;
   }
   // Get data-item
   CTA_Handle_GetData(hfunc,(void*) &data);

   // Set function pointer
   *function= (CTA_Function*) data->function;
   return CTA_OK;
}

/** \brief Returns name of a user function
 *
 *  \param hfunc    I handle of user function
 *  \param name     O name of user function,
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE, CTA_INCOMPATIBLE_HANDLE
 */
#undef METHOD
#define METHOD "GetName"
int CTA_Func_GetName(const CTA_Func hfunc, CTA_String name){

   int retval;            //Return value of a call

   // Check Handle and return error if handle is not valid
   retval=CTA_Handle_Check(hfunc,CTA_FUNCTION);
   if (retval) {
	   CTA_WRITE_ERROR("Handle is not a cta_function handle");
	   return retval;
   }

   retval=CTA_Handle_GetName(hfunc,name);
   return retval;
}


/** \brief Set userdata for a user defined function
 *
 * \param hfunc    I handle of user function
 * \param userdata I handles to userdata
 * \return error status: CTA_OK, CTA_ILLEGAL_HANDLE, CTA_INCOMPATIBLE_HANDLE
 */
#undef METHOD
#define METHOD "SetUSerData"
int CTA_Func_SetUserdata(const CTA_Func hfunc, const CTA_Handle userdata ){

   int retval;            //Return value of a call
   CTAI_Function *data;   //Function specific data


   // Check Handle and return error if handle is not valid
   retval=CTA_Handle_Check(hfunc,CTA_FUNCTION);
   if (retval) {
	   CTA_WRITE_ERROR("Handle is not a cta_function handle");
	   return retval;
   }

   // Get data-item
   CTA_Handle_GetData(hfunc,(void*) &data);

   // Allocate userdata and copy values
   data->userdata=userdata;

   return CTA_OK;
}
/** \brief Returns the userdata array of a user defined function
 *
 * \param hfunc    I   handle of user function
 * \param userdata O   handles to userdata
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE, CTA_INCOMPATIBLE_HANDLE,
 *                        CTA_ARRAY_TOO_SHORT
 */
#undef METHOD
#define METHOD "GetUserdata"
int CTA_Func_GetUserdata(const CTA_Func hfunc, CTA_Handle userdata ){

   int retval;            //Return value of a call
   CTAI_Function *data;   //Function specific data

   // Check Handle and return error if handle is not valid
   retval=CTA_Handle_Check(hfunc,CTA_FUNCTION);
   if (retval) {
	   CTA_WRITE_ERROR("Handle is not a cta_function handle");
	   return retval;
   }

   // Get data-item
   CTA_Handle_GetData(hfunc,(void*) &data);

   // Copy userdata
   userdata=data->userdata;

   return CTA_OK;
}


/** \brief Change the name of the library into a platform specific name
*
*  \param libname_in   Suggested name of the library
*  \param libname_out  Name of library on platform
*  \return             Handle to create or CTA_NULL in case of an error.
*/
void CTAI_DLLName(const char *libname_in, char *libname_out){

   int i;
   /* step1 strip of extension (if any) */
   for (i= strlen(libname_in)-1;i>=0;i--){
      if (libname_in[i]=='.') {break;}
   }
   if (i>0) {
      strncpy(libname_out,libname_in,i);
      libname_out[i]='\0';
   }
   else {
      strcpy(libname_out,libname_in);
   }

#ifdef WIN32
   strcat(libname_out,".dll");
#else
#ifdef MAC_OS_X
   strcat(libname_out,".dylib");
#else
   strcat(libname_out,".so");
#endif
#endif

}


/** \brief Change the name of the function into a Fortran function name*
*  \param funcname   I name of the function
*  \param f_funcname O Fortran name of the function
*
*  \note f_funcname is allocated by this function and should be freed by
*  the calling routine.

*/
void CTAI_FunctionNameFortran(char *funcname, char **f_funcname, int mode){

   int i, lenName, lenName2;
   // Make different variants of the function name 
   /* 1: Name
      2: Name_
      3: name
      4: name_
      5: NAME
      6: NAME_
   */
   lenName=strlen(funcname);
   lenName2 = lenName + 1;
   if (mode == 2 || mode == 4 || mode ==6) {lenName2 = lenName + 2;}
   *f_funcname=CTA_Malloc((lenName2)*sizeof(char));

   for (i=0;i<lenName;i++){
	   if (mode == 3 || mode == 4) {
		   (*f_funcname)[i]=tolower(funcname[i]);}
	   else if (mode == 5 || mode == 6) {
		   (*f_funcname)[i]=toupper(funcname[i]);} 
	   else { (*f_funcname)[i]= (funcname[i]); }
   }
   if (mode == 2 || mode == 4 || mode ==6) {   
	   (*f_funcname)[lenName]='_';}

   (*f_funcname)[lenName2-1]='\0';
}


int CTAI_GetProcAddress(CTAI_Function *data, char *funcname){
  #ifdef HAVE_DLFCN_H  // linux!
    data->function = dlsym(data->libhandle, funcname);
  #else
              // windows
    data->function =  GetProcAddress(data->libhandle, funcname); 
  #endif
  return CTA_OK;
}


CTA_Func CTA_CreateFuncDynamicLib(char *libraryName, char *functionName, char *name, char *id){
   CTA_Func      hfunc;               /* the new function */
//   xmlChar       *id = NULL;          /* id of function in XML-tree */
//   xmlChar       *library_inp = NULL; /* name of the dynamic link library */
//   xmlChar       *funcname =NULL;     /* name of the function */
   char*         f_funcname=NULL;     /* Fortran name of funcname */
   int           retval;              /* return status of creation       */
   char          library[256];        /* name of the dynamic link library */
#if defined WIN32
   WCHAR wLibrary[256];
   int i;
#endif

   CTAI_Function *data;   //Function specific data
   int nerror;
   char msg[256];
   nerror=0;

   /* Add the dynamic library extention to the library name as used on this OS (so, dll, dylib,...) */
   CTAI_DLLName(libraryName, library);

   /* Create a new COSTA function instance */
   retval=CTA_Func_Create((char *) name, NULL , CTA_NULL, &hfunc);
   if (retval!=CTA_OK){
      CTA_WRITE_WARNING("Cannot create a new COSTA function");
      return CTA_NULL;
   }

   /* Get data block */
   CTA_Handle_GetData(hfunc,(void*) &data);

   /* WINDOWS VERSION FOR LOADING A FUNCTION FROM A SHARED LIBRARY: */
   #if defined HAVE_DLFCN_H || defined WIN32
     /* Open Dynamic link library */
      #ifdef HAVE_DLFCN_H
         data->libhandle=dlopen(library, RTLD_LAZY);
      #else
         /* convert string wLibrary to WCHAR, not sure this is the right way.
	    but it seems to work */
         for (i=0;i<256;i++){wLibrary[i]=library[i];}
         data->libhandle=LoadLibrary(wLibrary);
      #endif

      /* Check library handle */
      if (data->libhandle==NULL){
         sprintf(msg,"Error in CTAI_XML_CreateFunc:Cannot open library %s\n",library);
         CTA_WRITE_WARNING(msg);
         #ifdef HAVE_DLFCN_H
            sprintf(msg, "error is: |%s| \n",dlerror());
            CTA_WRITE_WARNING(msg);
         #else
            sprintf(msg, "GetLastError code is %d\n",GetLastError());
            CTA_WRITE_WARNING(msg);
         #endif
         nerror++;
      }

      /* Load function and try all kinds of typical fortran variants */
      /* 1: original approach: Name */
      retval = CTAI_GetProcAddress(data, (char *) functionName);
      if (data->function == NULL){  /* 2: Name_ */
        CTAI_FunctionNameFortran((char *) functionName, &f_funcname,2);
        retval=CTAI_GetProcAddress(data, f_funcname);
        free(f_funcname);
      }
      if (data->function == NULL){  /* 3: name */
        CTAI_FunctionNameFortran((char *) functionName, &f_funcname,3);
        retval=CTAI_GetProcAddress(data, f_funcname);
        free(f_funcname);
       }
      if (data->function == NULL){  /* 4: name_ */
         CTAI_FunctionNameFortran((char *) functionName, &f_funcname,4);
         retval=CTAI_GetProcAddress(data, f_funcname);
         free(f_funcname);
      }
      if (data->function == NULL){  /* 5: NAME */
         CTAI_FunctionNameFortran((char *) functionName, &f_funcname,5);
         retval=CTAI_GetProcAddress(data, f_funcname);
         free(f_funcname);
      }
      if (data->function == NULL){  /* 6: NAME_ */
         CTAI_FunctionNameFortran((char *) functionName, &f_funcname,6);
         retval=CTAI_GetProcAddress(data, f_funcname);
         free(f_funcname);
       }

       /* Check function pointer */
       if (data->function==NULL){
          sprintf(msg, "WARNING in CTAI_XML_CreateFunc:Cannot load function %s\n",functionName);
          CTA_WRITE_WARNING(msg);
          #ifdef HAVE_DLFCN_H
             sprintf(msg,"%s\n",dlerror());
             CTA_WRITE_WARNING(msg);
          #else
             sprintf(msg,"GetLastError code is %d\n",GetLastError());
             CTA_WRITE_WARNING(msg);
          #endif
          nerror++;
      }
   #else
      sprintf(msg, "Warning: Cannot load functions dynamically\n");
      CTA_WRITE_WARNING(msg);
      sprintf(msg, "Compile COSTA with support for Dynamic loading \n");
      CTA_WRITE_WARNING(msg);
      nerror++;
   #endif


   //finaly Free created function handle in case of an error or set the id of the handle
   if (nerror>0) {
      retval=CTA_Func_Free(&hfunc);
   } else {
      /* Set id (=name) of handle */
      CTAI_Handle_SetName(hfunc, (char *) id);
   }

   return hfunc;
} 


/** \brief Create a COSTA function from XML
*          (load from dynamic load library).
*
*  \param cur_node  I  Current XML node
*  \return             Handle to create or CTA_NULL in case of an error.
*/
CTA_Func CTAI_XML_CreateFunc(xmlNode *cur_node) {

   CTA_Func      hfunc;               /* the new function */
   xmlChar       *id = NULL;          /* id of function in XML-tree */
   xmlChar       *name = NULL;        /* (lookup) name of the function */
   xmlChar       *libraryName = NULL; /* name of the dynamic link library */
   xmlChar       *functionName =NULL;     /* name of the function */

   /* Parse this node's attributes */
   /* Get id */
   id = xmlGetProp(cur_node, CTAI_XML_ID);

   /* Get name */
   name = xmlGetProp(cur_node, CTAI_XML_NAME);

   /* Get tag */
   libraryName = xmlGetProp(cur_node,  CTAI_XML_LIBRARY);

   /* Get function name */
   functionName = xmlGetProp(cur_node, CTAI_XML_FUNCTION);

   hfunc=CTA_CreateFuncDynamicLib((char*) libraryName, (char*) functionName, (char*) name, (char*) id);

   return hfunc;
}

/* Interfacing with Fortran */

CTAEXPORT void CTA_FUNC_CREATE_F77(char *name, void *function , int *hintf, int *hfunc,
                     int *ierr, int len_name){
   char  *c_name;
   // create a c-string equivalent to name
   c_name=CTA_Malloc((len_name+1)*sizeof(char));
   CTA_fstr2cstr(name,c_name,len_name);

   *ierr=CTA_Func_Create(c_name, (CTA_Function*) function , (CTA_Intf) *hintf,
                         (CTA_Func *) hfunc);
   free(c_name);
}


CTAEXPORT void CTA_FUNC_FREE_F77(CTA_Func *hfunc, int* ierr){

   *ierr=CTA_Func_Free((CTA_Func*) hfunc);
}

CTAEXPORT void CTA_FUNC_GETINTF_F77(int *hfunc, int *hintf, int *ierr){

  *ierr=CTA_Func_GetIntf((CTA_Func) *hfunc, (CTA_Intf*) hintf);
};

// It is not possible to create a Fortran interface for CTA_Func_GetFunc
//int CTA_Func_GetFunc(const CTA_Func hfunc, CTA_Function **function);


CTAEXPORT void CTA_FUNC_GETNAME_F77(int *hfunc, int *hname, int *ierr){
/*
   char *c_name; //C-string copy of name
   c_name=CTA_Malloc((len_name+1)*sizeof(char));
   *ierr=CTA_Func_GetName((CTA_Func) *hfunc, c_name);
   CTA_cstr2fstr(c_name,name,len_name);
   free(c_name);
*/
   *ierr=CTA_Func_GetName((CTA_Func) *hfunc, (CTA_String) *hname);
};

