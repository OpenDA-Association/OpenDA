/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_handles.c $
$Revision: 3406 $, $Date: 2012-08-16 15:25:53 +0200 (Thu, 16 Aug 2012) $

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
#include <pthread.h>

#include "cta_mem.h"
#include "f_cta_utils.h"
#include "cta_string.h"
#include "cta_handles.h"
#include "cta_errors.h"
#include "cta_message.h"

#define CTA_HANDLE_CREATE_F77      F77_CALL(cta_handle_create,CTA_HANDLE_CREATE)
#define CTA_HANDLE_FREE_F77        F77_CALL(cta_handle_free,CTA_HANDLE_FREE)
#define CTA_HANDLE_CHECK_F77       F77_CALL(cta_handle_check,CTA_HANDLE_CHECK)
#define CTA_HANDLE_GETNAME_F77     F77_CALL(cta_handle_getname,CTA_HANDLE_GETNAME)
#define CTA_HANDLE_GETVALUE_F77    F77_CALL(cta_handle_getvalue,CTA_HANDLE_GETVALUE)
#define CTA_HANDLE_GETDATATYPE_F77 F77_CALL(cta_handle_getdatatype,CTA_HANDLE_GETDATATYPE)
#define CTA_HANDLE_FIND_F77        F77_CALL(cta_handle_find,CTA_HANDLE_FIND)
#define CTA_HANDLE_PRINTALL_F77    F77_CALL(cta_handle_printall,CTA_HANDLE_PRINTALL)
#define CTA_HANDLE_PRINTINFO_F77   F77_CALL(cta_handle_printinfo,CTA_HANDLE_PRINTINFO)

#define CLASSNAME "CTA_Handles"

#define IDEBUG (0)

int counter =0;
static pthread_mutex_t mutex =  PTHREAD_MUTEX_INITIALIZER;

CTAEXPORT extern int CTA_PAR_MY_RANK;


long CTAI_Vector_GetMemsize();




/* Struct holding all data associated to an COSTA Handle */
typedef struct {
   char *name;              // Name of handle for debugging and informational messages
   CTA_Datatype datatype;   // Data type associated with this handle
   void *data;              // Pointer to the type dependent data
   int refCount; 	        // Number of references from which we expect a free 
	                        // (only used for a limited number of components used from java) 
   int globCount;   //DEBUG
   char debugTag[80];
} CTAI_HandleInfo;

/*
   Handle administration: list of all COSTA handles and their data.
   The array of pointers to CTAI_Handles contains all data. The COSTA handles
   are the indices in this array.

   Finding our way in the administration:
   The size information is the most important and holds the current length of
   CTA_Handles. The other two CTAI_Handles_free, holding the number of free
   elements and CTAI_Handles_last are only usefull for performance.
   CTAI_Handles_free is used to easy check whether the array CTAI_HandleInfo must
   be enlarged when a new handle is to be created.
   CTAI_Handles_last points to the postion where the last element is added or one
   position before the last handle that has been freed. It is the starting position
   for finding a new free position in CTAI_Handles.

*/
static CTAI_HandleInfo **CTAI_Handles=NULL; //List of handles
static int CTAI_Handles_size=0;             // Size of list
static int CTAI_Handles_free=0;             // Number of free handles
static int CTAI_Handles_last=0;             // Position of last allocated handle handle
                                            // or just before last freed handle


/** \brief Creates a new COSTA handle
 *
 * \param name     I name associated to handle
 * \param datatype I datatype of handle
 * \param data     I block of data associated to handle
 * \param handle   O COSTA handle
 * \return error status: CTA_OK
 */

#undef METHOD
#define METHOD "Create"
int CTA_Handle_Create(const char *name, const CTA_Datatype datatype,
                      void *data, CTA_Handle *handle){

   int i; //index counter

   // Initialisation
   *handle=CTA_NULL;

   pthread_mutex_lock( &mutex );

   // Some additional debug stuff when we expect a memleak
   if (counter%10000 ==0){
      pthread_mutex_unlock( &mutex );
    //  CTA_Handle_PrintInfo("CTA_handle_Create ");
      pthread_mutex_lock( &mutex );
   }
   counter++;
   
   // Find a new handle:
   if (CTAI_Handles_size == 0)
   {
   // IF (list does not exist yet) THEN
      // Create a list
      int newlen=1000;
      //int newlen=100000;
      CTAI_Handles=CTA_Malloc(newlen*sizeof(CTA_Handle*));

      // Initialise list with null-pointers
      for (i=0;i<newlen;i++){
         CTAI_Handles[i]=NULL;
      }
      // Set handle and ''list'' administration
      *handle=1;
      CTAI_Handles_free=newlen-2;
      CTAI_Handles_size=newlen;
   }
   else if (CTAI_Handles_free == 0)
   {
      // No free entries extent list -> extent list and choose first new handle
      int oldlen=CTAI_Handles_size;
      //int newlen=oldlen*2;
      int newlen=oldlen*2;


      CTA_Handle **Handles_new=realloc(CTAI_Handles,newlen*sizeof(CTA_Handle*));
      if (Handles_new==NULL){
	CTA_WRITE_ERROR("FATAL Cannot allocate new memory to store handels");
        exit(-1);
      }
      else {
         CTAI_Handles = Handles_new;
      }
      
      // Initialise reallocated part
      for (i=oldlen;i<newlen;i++){
         CTAI_Handles[i]=NULL;
      }
      // Set handle and ''list'' administration
      *handle=oldlen;
      CTAI_Handles_free=newlen-oldlen-1;
      CTAI_Handles_size=newlen;
   } else {
      // Find a new free entry starting at last allocated position
      i=CTAI_Handles_last;
      while (*handle == CTA_NULL){
         i= i % (CTAI_Handles_size-1) + 1;
         if (CTAI_Handles[i]==NULL) *handle=i;
      }
      CTAI_Handles_free--;
   }
   // Allocate CTAI_HandleInfo-struct and fill it
   CTAI_Handles[*handle]=CTA_Malloc(sizeof(CTAI_HandleInfo));
   if (name) {
      CTAI_Handles[*handle]->name = CTA_Malloc(1 + strlen(name));
      strcpy(CTAI_Handles[*handle]->name, name);
      CTAI_Handles[*handle]->name[strlen(name)] = '\0';
   } else {
      CTAI_Handles[*handle]->name = CTA_Malloc(1);
      CTAI_Handles[*handle]->name[0] = '\0';
   }
   CTAI_Handles[*handle]->datatype=datatype;
   CTAI_Handles[*handle]->data=data;
   CTAI_Handles[*handle]->refCount=1;	

   pthread_mutex_unlock( &mutex );

   return CTA_OK;
}


#undef METHOD
#define METHOD "GetRefCount"
int CTA_Handle_GetRefCount(const CTA_Handle handle, int *refCount){

   pthread_mutex_lock( &mutex );

   if (handle>=0 && handle<CTAI_Handles_size && CTAI_Handles[handle]!=NULL){
      *refCount=CTAI_Handles[handle]->refCount;
      // else, function handle is not used ->return CTA_ILLEGAL_HANDLE
   }
   else {
      char message[1024];
      pthread_mutex_unlock( &mutex );
      sprintf(message,"Handle nr %d is NOT in list of %d handles",handle,CTAI_Handles_size);
      CTA_WRITE_ERROR(message);


      return CTA_ILLEGAL_HANDLE;
   }


   pthread_mutex_unlock( &mutex );

   return CTA_OK;
}

#undef METHOD
#define METHOD "IncRefCount"
int CTA_Handle_IncRefCount(const CTA_Handle handle){
   
   pthread_mutex_lock( &mutex );

   if (handle>=0 && handle<CTAI_Handles_size && CTAI_Handles[handle]!=NULL){
      CTAI_Handles[handle]->refCount++;

      // else, function handle is not used ->return CTA_ILLEGAL_HANDLE
   }
   else {

      char message[1024];
	  pthread_mutex_unlock( &mutex );
      sprintf(message,"Handle nr %d is not in list of %d handles",handle,CTAI_Handles_size);
      CTA_WRITE_ERROR(message);


      return CTA_ILLEGAL_HANDLE;
   }
   pthread_mutex_unlock( &mutex );

   return CTA_OK;
}

#undef METHOD
#define METHOD "DecrRefCount"
int CTA_Handle_DecrRefCount(const CTA_Handle handle){
   
   pthread_mutex_lock( &mutex );

   if (handle>=0 && handle<CTAI_Handles_size && CTAI_Handles[handle]!=NULL){
      CTAI_Handles[handle]->refCount--;
      // else, function handle is not used ->return CTA_ILLEGAL_HANDLE
   }
   else {

      char message[1024];
	  pthread_mutex_unlock( &mutex );

      sprintf(message,"Handle nr %d is not in list of %d handles",handle,CTAI_Handles_size);
      CTA_WRITE_ERROR(message);


      return CTA_ILLEGAL_HANDLE;
   }

   pthread_mutex_unlock( &mutex );

   return CTA_OK;
}


/** \brief Checks whether a handle is valid and checks type
 *
 *  \note The handle CTA_NULL is not valid.
 *
 * \param handle   I COSTA handle
 * \param datatype I The datatype the handle must be compatible with
 * \return error status: CTA_OK, CTA_ILLEGAL_HANDLE, CTA_INCOMPATIBLE_HANDLE
 *
 */

#undef METHOD
#define METHOD "Check"
int CTA_Handle_Check(const CTA_Handle handle,
                                 const CTA_Datatype datatype){

   pthread_mutex_lock( &mutex );

   if (handle>=0 && handle<CTAI_Handles_size &&
       CTAI_Handles[handle]!=NULL){
       // Handle is used, now check dataType
       if (datatype!=CTA_HANDLE && CTAI_Handles[handle]->datatype!=datatype){
          // datatype does not correspond -> return CTA_INCOMPATIBLE_HANDLE

             char message[1024];
		     pthread_mutex_unlock( &mutex );
             sprintf(message,"Handle is of type %d  instead of %d .\nSee cta_datatypes.h for more information.",CTAI_Handles[handle]->datatype, datatype);
             CTA_WRITE_ERROR(message);		   


             return CTA_INCOMPATIBLE_HANDLE;
       }
   // else, function handle is not used ->return CTA_ILLEGAL_HANDLE
   }else{

      char message[1024];
	  pthread_mutex_unlock( &mutex );

      sprintf(message,"Handle nr %d is not in list of %d handles",handle,CTAI_Handles_size);
      CTA_WRITE_ERROR(message);
      return CTA_ILLEGAL_HANDLE;
   }

   pthread_mutex_unlock( &mutex );

   return CTA_OK;
}

/** \brief Frees a COSTA handle
 *
 *  \note The data part of the handle is NOT freed, freeing CTA_NULL is
 *
 *  \param handle I/O handle that must be freed, CTA_NULL on return.
 *   allowed
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE
 */
#undef METHOD
#define METHOD "Free"
int CTA_Handle_Free(CTA_Handle *handle){

   // If handle is CTA_FUNC_NULL -> nothing to be done
   if (*handle==CTA_NULL) return CTA_OK;
   // if handle is not ok -> return CTA_ILLEGAL_HANDLE
   if (CTA_OK != CTA_Handle_Check(*handle,CTA_HANDLE)) {
	   CTA_WRITE_ERROR("Handle is not a cta_handle handle");
	   return CTA_ILLEGAL_HANDLE;
   }

   pthread_mutex_lock( &mutex );

   // Free associated data
   free(CTAI_Handles[*handle]->name);
   free(CTAI_Handles[*handle]);
   CTAI_Handles[*handle]=NULL;
   // Adjust pointer administration
   CTAI_Handles_free++;
   CTAI_Handles_last=*handle-1;
   // Set value of hfunc
   *handle=CTA_NULL;

   pthread_mutex_unlock( &mutex );

   return CTA_OK;
}


/** \brief Returns pointer to data element of handle
 *
 *  \param handle I COSTA handle
 *  \param data   O pointer to data element
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE
 */
#undef METHOD
#define METHOD "GetData"
int CTA_Handle_GetData(const CTA_Handle handle, void **data){

   // if handle is not ok -> return CTA_ILLEGAL_HANDLE
	if (CTA_OK != CTA_Handle_Check(handle,CTA_HANDLE)) {
		CTA_WRITE_ERROR("Cannot retrieve handle data");
		return CTA_ILLEGAL_HANDLE;
	}

   pthread_mutex_lock( &mutex );

   // return data
   *data=CTAI_Handles[handle]->data;

   pthread_mutex_unlock( &mutex );

   return CTA_OK;
}


/** \brief Returns data associated with handle (INTERNAL USE ONLY)
 *
 *  \param handle   I COSTA handle
 *  \return handle name
 */
const void *CTAI_Handle_GetData(const CTA_Handle handle){
   void *data;

   pthread_mutex_unlock( &mutex );

   data = CTAI_Handles[handle]->data;

   pthread_mutex_unlock( &mutex );

   return data;
}


/** \brief Returns name associated with handle
 *
 *  \param handle   I COSTA handle
 *  \param name     O name
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE
 */
#undef METHOD
#define METHOD "GetName"
int CTA_Handle_GetName(const CTA_Handle handle, CTA_String hname){
   int retVal;
   char *name;

   // if handle is not ok -> return CTA_ILLEGAL_HANDLE
   if (CTA_OK != CTA_Handle_Check(handle,CTA_HANDLE)) {
	  CTA_WRITE_ERROR("Handle is not a cta_handle handle");
	  return CTA_ILLEGAL_HANDLE;
   }

   if (CTA_OK != CTA_Handle_Check(hname,CTA_STRING)){
	   CTA_WRITE_ERROR("Handle is not a cta_string handle");
	   return CTA_ILLEGAL_HANDLE;
   }

   // return name

   pthread_mutex_lock( &mutex );
   name = CTAI_Handles[handle]->name;
   pthread_mutex_unlock( &mutex );
   
   retVal=CTA_String_Set(hname, name);


   return retVal;
}

/** \brief Set name associated with handle (INTERNAL ONLY)
 *
 *  \param handle   I COSTA handle
 *  \param hname    I COSTA string
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE
 */
int CTAI_Handle_SetName(const CTA_Handle handle, const char *name)
{
   pthread_mutex_lock( &mutex );

   if (strcmp(CTAI_Handles[handle]->name,name)!=0){
      CTAI_Handles[handle]->name = realloc(CTAI_Handles[handle]->name, 1 + strlen(name));
      strcpy(CTAI_Handles[handle]->name, name);
   }

   pthread_mutex_unlock( &mutex );

   return CTA_OK;
}


/** \brief Returns name associated with handle (INTERNAL USE ONLY)
 *
 *  \param handle   I COSTA handle
 *  \return handle name
 */
const char *CTAI_Handle_GetName(const CTA_Handle handle){
   char *retStr;

   pthread_mutex_lock( &mutex );

   retStr = CTAI_Handles[handle]->name;

   pthread_mutex_unlock( &mutex );

   return retStr;
}


/** \brief Returns datatype associated with handle
 *
 *  \param handle   I COSTA handle
 *  \param datatype O name of data type
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE
 */
#undef METHOD
#define METHOD "CGetDatatype"   
int CTA_Handle_GetDatatype(const CTA_Handle handle, CTA_Datatype *datatype){


   // if handle is not ok -> return CTA_ILLEGAL_HANDLE
	if (CTA_OK != CTA_Handle_Check(handle,CTA_HANDLE)) {
		CTA_WRITE_ERROR("Handle is not a cta_handle handle");


		return CTA_ILLEGAL_HANDLE;
	}
   pthread_mutex_lock( &mutex );

   // return datatype
   *datatype=CTAI_Handles[handle]->datatype;

   pthread_mutex_unlock( &mutex );

   return CTA_OK;
}


/** \brief Returns datatype associated with handle
 *
 *  \param handle   I COSTA handle
 *  \return           name of data type,
 */
CTA_Datatype CTAI_Handle_GetDatatype(const CTA_Handle handle){
   
   CTA_Datatype retVal;

   pthread_mutex_lock( &mutex );
   retVal = CTAI_Handles[handle]->datatype;


   pthread_mutex_unlock( &mutex );

   return retVal;
}


/** \brief Frees a COSTA handle and its associated data
 *
 *  \note The data part of the handle is also freed
 *
 *  \param handle I/O handle that must be freed, CTA_NULL on return.
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE
 */
int CTA_Intf_Free(CTA_Handle *handle);
int CTA_Func_Free(CTA_Handle *handle);
int CTA_Vector_Free(CTA_Handle *handle);
int CTA_TreeVector_Free(CTA_Handle *handle, int recursive);
int CTA_Matrix_Free(CTA_Handle *handle);
int CTA_Model_Free(CTA_Handle *handle);
int CTA_SObs_Free(CTA_Handle *handle);
int CTA_Tree_Free(CTA_Handle *handle);
int CTA_String_Free(CTA_Handle *handle);
int CTA_File_Free(CTA_Handle *handle);
int CTA_Time_Free(CTA_Handle *handle);
int CTA_ObsDescr_Free(CTA_Handle *handle);
int CTA_Pack_Free(CTA_Handle *handle);
int CTA_Metainfo_Free(CTA_Handle *handle);
int CTA_RelTable_Free(CTA_Handle *handle);
int CTA_Array_Free(CTA_Handle *handle);

int CTA_Handle_Free_All(CTA_Handle *handle){
    
   /* If handle is CTA_FUNC_NULL -> nothing to be done */
   if (*handle==CTA_NULL) return CTA_OK;

   switch (CTAI_Handles[*handle]->datatype){
      case CTA_HANDLE :
         if (IDEBUG>0) printf("Delete CTA_HANDLE\n");
         return CTA_Handle_Free(handle);
      case CTA_INTERFACE :
         if (IDEBUG>0) printf("Delete CTA_INTERFACE\n");
         return CTA_Intf_Free(handle);
      case CTA_FUNCTION :
         if (IDEBUG>0) printf("Delete CTA_FUNCTION\n");
         return CTA_Func_Free(handle);
      case CTA_VECTOR :
         if (IDEBUG>0) printf("Delete CTA_VECTOR\n");
         return CTA_Vector_Free(handle);
      case CTA_VECTORCLASS :
         if (IDEBUG>0) printf("Delete CTA_VECTORCLASS\n");
         printf("WARNING: Cannot delete CTA_VECTORCLASS\n");
         return CTA_Handle_Free(handle);
      case CTA_TREEVECTOR :
         if (IDEBUG>0) printf("Delete CTA_TREEVECTOR\n");
         return CTA_TreeVector_Free(handle,CTA_TRUE);
      case CTA_MATRIXCLASS :
         if (IDEBUG>0) printf("Delete CTA_MAXTRIXCLASS\n");
         printf("WARNING: Cannot delete CTA_MATRIXCLASS\n");
         return CTA_Handle_Free(handle);
      case CTA_MATRIX :
         if (IDEBUG>0) printf("Delete CTA_MATRIX\n");
         return CTA_Matrix_Free(handle);
      case CTA_COVMATCLASS :
         if (IDEBUG>0) printf("Delete CTA_COVMATCLASS\n");
         printf("WARNING: Cannot delete CTA_COVMATCLASS\n");
         return CTA_Handle_Free(handle);
      case CTA_COVMAT :
         if (IDEBUG>0) printf("Delete CTA_COVMAT\n");
         printf("WARNING: Cannot delete CTA_COVMAT\n");
         return CTA_OK;
      //   return CTA_Handle_Free(handle);
      case CTA_INTPOL :
         if (IDEBUG>0) printf("Delete CTA_INTPOL\n");
         printf("WARNING: Cannot delete CTA_INTPOL\n");
         return CTA_OK;
      //   return CTA_Handle_Free(handle);
      case CTA_OBS :
         if (IDEBUG>0) printf("Delete CTA_OBS\n");
         printf("WARNING: Cannot delete CTA_OBS\n");
         return CTA_OK;
      case CTA_MODELCLASS :
         if (IDEBUG>0) printf("Delete CTA_MODELCLASS\n");
         printf("WARNING: Cannot delete CTA_MODELCLASS\n");
         return CTA_OK;
      //   return CTA_Handle_Free(handle);
      case CTA_MODEL :
         if (IDEBUG>0) printf("Delete CTA_MODEL\n");
         return CTA_Model_Free(handle);
      case CTA_TIME :
         if (IDEBUG>0) printf("Delete CTA_TIME\n");
         return CTA_Time_Free(handle);
	   case CTA_SOBS :
         if (IDEBUG>0) printf("Delete CTA_SOBS\n");
		   return CTA_SObs_Free(handle);
      case CTA_SOBSCLASS :
         if (IDEBUG>0) printf("Delete CTA_SOBCLASS\n");
         printf("WARNING: Cannot delete CTA_OBSCLASS\n");
         return CTA_OK;
      case CTA_OBSDESCR :
         if (IDEBUG>0) printf("Delete CTA_OBSDESCR\n");
		   return CTA_ObsDescr_Free(handle);
      case CTA_METHODCLASS :
         if (IDEBUG>0) printf("Delete CTA_METHODCLASS\n");
         printf("WARNING: Cannot delete CTA_METHODCLASS\n");
         return CTA_OK;
      case CTA_METHOD :
         if (IDEBUG>0) printf("Delete CTA_METHOD\n");
         printf("WARNING: Cannot delete CTA_METHOD\n");
         return CTA_OK;
      case CTA_TREE :
         return CTA_Tree_Free(handle);
      case CTA_PACK :
         if (IDEBUG>0) printf("Delete CTA_PACK\n");
         return CTA_Pack_Free(handle);
      case CTA_DATABLOCK :
         if (IDEBUG>0) printf("Delete CTA_DATABLOCK\n");
         printf("WARNING: Cannot delete CTA_DATABLOCK\n");
         return CTA_OK;
      case CTA_METAINFO :
         if (IDEBUG>0) printf("Delete CTA_METAINFO\n");
         return CTA_Metainfo_Free(handle);
      case CTA_METAINFOCLASS :
         if (IDEBUG>0) printf("Delete CTA_METAINFOCLASS\n");
         printf("WARNING: Cannot delete CTA_METAINFOCLASS\n");
         return CTA_OK;
      case CTA_RELTABLE :
         if (IDEBUG>0) printf("Delete CTA_RELTABLE\n");
         return CTA_RelTable_Free(handle);
      case CTA_SUBTREEVECTOR :
         if (IDEBUG>0) printf("Delete CTA_SUBTREEVECTOR\n");
         printf("WARNING: Cannot delete CTA_SUBTREEVECTOR\n");
         return CTA_OK;
      case CTA_STRING :
         if (IDEBUG>0) printf("Delete CTA_STRING\n");
		 return CTA_String_Free(handle);
      case CTA_FILE :
         if (IDEBUG>0) printf("Delete CTA_FILE\n");
         return CTA_File_Free(handle);
      case CTA_ARRAY :
         if (IDEBUG>0) printf("Delete CTA_ARRAY\n");
         return CTA_Array_Free(handle);
      default:
          printf("INTERNAL ERROR IN CTA_Handle_Free_All\n");
          printf("WARNING: Cannot delete handle of datatype %d\n",
                  CTAI_Handles[*handle]->datatype);
          exit(-1);
   }
}




/** \brief Returns the value a handle points to
 *
 *  \param handle I COSTA handle
 *  \param data   O pointer to data element
 *  \return error status: CTA_OK, CTA_ILLEGAL_HANDLE, CTA_INCOMPATIBLE_HANDLE
 */
int CTA_Handle_GetValue(const CTA_Handle handle, void *value, CTA_Datatype datatype)
{
   CTA_Datatype mytype;

   /* If handle is CTA_FUNC_NULL -> nothing to be done */
   if (handle==CTA_NULL) return CTA_OK;

   mytype = CTAI_Handle_GetDatatype(handle);

   /* Return the handle value in case of identical types or CTA_HANDLE */
   if (mytype == datatype || datatype == CTA_HANDLE) {
      *(CTA_Handle*)value = handle;
      return CTA_OK;
   }

   /* In case of string, attempt to convert */
   if (mytype == CTA_STRING) {
      return CTA_String_GetValue(handle, value, datatype);
   }

   /* Otherwise, return CTA_INCOMPATIBLE_HANDLE */
   value = CTA_NULL;
   return CTA_INCOMPATIBLE_HANDLE;
}

#undef METHOD
#define METHOD "Find"
int CTA_Handle_Find(CTA_String sname, CTA_Datatype datatype, int *handlenr)
{
  int i,retval, len;
  char *sstr;


  /* Copy the input string into a C-string */
  retval = CTA_String_GetLength(sname,&len);
  if (retval!=CTA_OK) {
	  CTA_WRITE_ERROR("Cannot get Length");
	  return retval;
  }
  sstr=CTA_Malloc((len+1)*sizeof(char));
  retval = CTA_String_Get(sname,sstr);
  if (retval!=CTA_OK) {
	  CTA_WRITE_ERROR("Cannot get String");

          free(sstr);
	  return retval;
  }


  pthread_mutex_lock( &mutex );

  for (i=0; i < CTAI_Handles_size;i++){

     if (CTAI_Handles[i] != NULL) {

          if (!strncmp(CTAI_Handles[i]->name,sstr,len)) {

          //printf("gevonden! %d \n",i);
          *handlenr = i;
          
          pthread_mutex_unlock( &mutex );
          
          free(sstr);
          return CTA_OK; }

     }
  }


  pthread_mutex_unlock( &mutex );

  free(sstr);
  return CTA_HANDLE_NOT_FOUND;
}


int CTA_Handle_Printall()
{
  int i,ierr;
  char *sstr;
  int len;

  pthread_mutex_lock( &mutex );

  for (i=0; i < CTAI_Handles_size;i++)
  {
     if (CTAI_Handles[i] != NULL)
     {
        printf("Name of handle %d  is '%s'", i, CTAI_Handles[i]->name);

        if (CTAI_Handles[i]->datatype == CTA_STRING) {

          pthread_mutex_unlock( &mutex );
          ierr = CTA_String_GetLength(i,&len);
          pthread_mutex_lock( &mutex );

          sstr=CTA_Malloc((len+1)*sizeof(char));

          pthread_mutex_unlock( &mutex );

          ierr=CTA_String_Get(i, sstr);

          pthread_mutex_lock( &mutex );

          printf(": %s",sstr);
        }
        printf("\n");
     }
  }
  pthread_mutex_unlock( &mutex );
  return ierr;
}

int CTA_Handle_PrintInfo(const char *location)
{
   int i;
   int ntotal;
   int ncount[32];
   int nrefcount[32];
   int datatype;
   long memvec;
   float fmemvec;

   pthread_mutex_lock( &mutex );

   memvec=CTAI_Vector_GetMemsize();
   fmemvec=(float) memvec/(1024.0*1024.0);

   /* initialise counting variables */
   ntotal=0;
   for (i=0;i<32;i++){ncount[i]=0; nrefcount[i]=0;}

   for (i=0; i < CTAI_Handles_size;i++)
   {
      if (CTAI_Handles[i] != NULL)
      {
         ntotal++;
         datatype=-CTAI_Handles[i]->datatype;
         if (  datatype<0 || datatype >31 ){
            printf("ERROR HANDLE %d has datatype %d",i ,-datatype);
         }
         else {
            ncount[datatype]++;
            nrefcount[datatype] = nrefcount[datatype] + CTAI_Handles[i]->refCount;
         }
      }
      else{
         ncount[31]++;
      }
   }
   fflush(stdout);
   printf("#%d CTA_Handle_PrintInfo: NOTE: SPECIAL VERSION OF CTA_HANDLES.c:\n", CTA_PAR_MY_RANK);
   printf("#%d CTA_Handle_PrintInfo: Overview of handles:\n", CTA_PAR_MY_RANK);
   printf("#%d location=%s\n", CTA_PAR_MY_RANK,location);
   printf("#%d #handles including empty slots: %d\n", CTA_PAR_MY_RANK,CTAI_Handles_size);
   printf("#%d empty slots       : %d \n", CTA_PAR_MY_RANK,ncount[31]);
   printf("#%d type                number of handles  number of refcounts\n", CTA_PAR_MY_RANK  );
   printf("#%d CTA_HANDLE        : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_HANDLE]);
   printf("#%d CTA_INTERFACE     : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_INTERFACE   ]);
   printf("#%d CTA_FUNCTION      : %d %d\n", CTA_PAR_MY_RANK,ncount[-CTA_FUNCTION  ],  nrefcount[-CTA_FUNCTION    ]);
   printf("#%d CTA_VECTOR        : %d %d %f Mb\n", CTA_PAR_MY_RANK,ncount[-CTA_VECTOR    ], nrefcount[-CTA_VECTOR    ], fmemvec);
   printf("#%d CTA_VECTORCLASS   : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_VECTORCLASS ]);
   printf("#%d CTA_TREEVECTOR    : %d %d\n", CTA_PAR_MY_RANK,ncount[-CTA_TREEVECTOR], nrefcount[-CTA_TREEVECTOR ]);
   printf("#%d CTA_MATRIXCLASS   : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_MATRIXCLASS ]);
   printf("#%d CTA_MATRIX        : %d %d\n", CTA_PAR_MY_RANK,ncount[-CTA_MATRIX      ], nrefcount[-CTA_MATRIX      ]);
   printf("#%d CTA_COVMATCLASS   : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_COVMATCLASS ]);
   printf("#%d CTA_COVMAT        : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_COVMAT      ]);
   printf("#%d CTA_INTPOL        : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_INTPOL      ]);
   printf("#%d CTA_OBS           : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_OBS         ]);
   printf("#%d CTA_MODELCLASS    : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_MODELCLASS  ]);
   printf("#%d CTA_MODEL         : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_MODEL       ]);
   printf("#%d CTA_TIME          : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_TIME        ]);
   printf("#%d CTA_SOBS          : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_SOBS        ]);
   printf("#%d CTA_SOBSCLASS     : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_SOBSCLASS   ]);
   printf("#%d CTA_OBSDESCR      : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_OBSDESCR    ]);
   printf("#%d CTA_METHODCLASS   : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_METHODCLASS ]);
   printf("#%d CTA_METHOD        : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_METHOD      ]);
   printf("#%d CTA_TREE          : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_TREE        ]);
   printf("#%d CTA_PACK          : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_PACK        ]);
   printf("#%d CTA_DATABLOCK     : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_DATABLOCK   ]);
   printf("#%d CTA_METAINFO      : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_METAINFO    ]);
   printf("#%d CTA_METAINFOCLASS : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_METAINFOCLASS]);
   printf("#%d CTA_RELTABLE      : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_RELTABLE     ]);
   printf("#%d CTA_SUBTREEVECTOR : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_SUBTREEVECTOR]);
   printf("#%d CTA_STRING        : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_STRING       ]);
   printf("#%d CTA_FILE          : %d \n", CTA_PAR_MY_RANK,ncount[-CTA_FILE         ]);
   printf("#%d                     -----+\n", CTA_PAR_MY_RANK);
   printf("#%d Total             : %d \n", CTA_PAR_MY_RANK,ntotal);
   fflush(stdout);

   pthread_mutex_unlock( &mutex );

  return CTA_OK;
}




/* Interfacing with Fortran */
CTAEXPORT void CTA_HANDLE_CREATE_F77(char *name, int *datatype, int *data,
                         int *handle, int *ierr, int len_name){
  char  *c_name;
  // create a c-string equivalent to name
  c_name=CTA_Malloc((len_name+1)*sizeof(char));
  CTA_fstr2cstr(name,c_name,len_name);

  *ierr=CTA_Handle_Create(c_name, (CTA_Datatype) *datatype, (void *) data,
                          (CTA_Handle*) handle);

  free(c_name);
}

CTAEXPORT void CTA_HANDLE_FREE_F77(int *handle, int *ierr){

   *ierr=CTA_Handle_Free((CTA_Handle *) handle);
}

CTAEXPORT void CTA_HANDLE_FREE_ALLF77(int *handle, int *ierr){

   *ierr=CTA_Handle_Free_All((CTA_Handle *) handle);
}

CTAEXPORT void CTA_HANDLE_CHECK_F77(int *handle, int *datatype, int *ierr){

   *ierr=CTA_Handle_Check((CTA_Handle) *handle, (CTA_Datatype) *datatype);
}


// Note it is not possible to handle a CTA_Handle_GetData operation
// In fortran due to the lack of poiters
// void CTA_Handle_GetData(int *handle, void **data ,int *ierr);


CTAEXPORT void CTA_HANDLE_GETNAME_F77(int *handle, int *name, int*ierr){
/*
   char * c_name; //C-string copy of name
   c_name=CTA_Malloc((len_name+1)*sizeof(char));
   *ierr=CTA_Handle_GetName((CTA_Handle) *handle, name);
   CTA_cstr2fstr(c_name,name,len_name);
   free(c_name);
*/
   *ierr=CTA_Handle_GetName((CTA_Handle) *handle, (CTA_String)*name);
}

/* Try to get the value from the given handle */
CTAEXPORT void CTA_HANDLE_GETVALUE_F77(int *handle, void *value, int *datatype, int*ierr){
   *ierr=CTA_Handle_GetValue((CTA_Handle) *handle, value, *datatype);
}

/* Try to get the value from the given handle */
CTAEXPORT void CTA_HANDLE_GETDATATYPE_F77(int *handle, int *datatype, int*ierr){
   *ierr=CTA_Handle_GetDatatype((CTA_Handle) *handle, datatype);
}


CTAEXPORT void CTA_HANDLE_FIND_F77(int *sname, int *datatype, int *handlenr,
                         int *ierr){
   *ierr=CTA_Handle_Find((CTA_String) *sname, (CTA_Datatype) *datatype,
                         handlenr);
}

CTAEXPORT void CTA_HANDLE_PRINTALL_F77(int*ierr){
   *ierr=CTA_Handle_Printall();
}

CTAEXPORT void CTA_HANDLE_PRINTINFO_F77(char *location, int*ierr, int len_str){
   char *c_str;

   c_str=CTA_Malloc((len_str+1)*sizeof(char));
   CTA_fstr2cstr(location,c_str,len_str);

   *ierr=CTA_Handle_PrintInfo(c_str);
   free(c_str);
}
