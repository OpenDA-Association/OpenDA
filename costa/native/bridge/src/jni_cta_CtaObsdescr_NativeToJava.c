/*
$URL: https://repos.deltares.nl/repos/openda/trunk/costa/src/cta/cta_obsdescr_combine.c $
$Revision: 671 $, $Date: 2008-10-07 14:49:42 +0200 (Tue, 07 Oct 2008) $

COSTA: Problem solving environment for data assimilation
Copyright (C) 2010  Nils van Velzen

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

/**
\file  cta_handles.h
\brief Implemenation of a generic OpenDA observation description wrapper for use in native COSTA code 
*/


#include <string.h>
#include <stdio.h>
#include "cta.h"

#include "jni_datatypes.h"
#include "jni.h"
#include "jni_cta_utils.h"


#define CLASSNAME  "CtaObsDescr_nativeToJava"

#define IDEBUG (0)

typedef struct {
CTA_Handle myhandle;
sJni_Class classInstance;
} CTAI_ObsDescr_NativeToJava;


#define METHOD "CTAI_ObsDescr_nativeToJava_Create_Size"
void CTAI_ObsDescr_nativeToJava_Create_Size(int *memsize, int *retval){
   *memsize=(int) sizeof(CTAI_ObsDescr_NativeToJava);
   *retval=CTA_OK;
};

#undef METHOD
#define METHOD "CTAI_ObsDescr_nativeToJava_Create_Init"
void CTAI_ObsDescr_nativeToJava_Create_Init(CTA_ObsDescr *myhandle, CTAI_ObsDescr_NativeToJava *descr,
                                            CTA_Handle *usrdat, int *retval)
{
   JNIEnv *env;
   sJni_Class *classIn;

   //Set the runtime and class from userdata
   
   CTA_Handle_GetData(*usrdat, (void *) &classIn);
   env     = classIn->env;
 
   descr->classInstance.env = env;
   descr->classInstance.cls = (*env)->NewGlobalRef(env, classIn->cls);
   descr->classInstance.obj = (*env)->NewGlobalRef(env, classIn->obj);
   descr->myhandle          = *myhandle;
   
};

#undef METHOD
#define METHOD "CTAI_ObsDescr_nativeToJava_CreateSel"
void CTAI_ObsDescr_nativeToJava_CreateSel(CTAI_ObsDescr_NativeToJava *descr,
          CTA_String *selection, CTA_RelTable *reltab, 
          CTA_ObsDescr *myhandle_out, 
          CTAI_ObsDescr_NativeToJava *descrout, int *retval){

CTA_WRITE_ERROR("Method not implemented");
*retval=CTA_NOT_IMPLEMENTED;

}
         


#undef METHOD
#define METHOD "CTAI_ObsDescr_nativeToJava_Get_Keys"
void CTAI_ObsDescr_nativeToJava_Get_Keys(CTAI_ObsDescr_NativeToJava *descr,
                                         CTA_Vector *keys, int *retval)
{
   jobject jKeys;

   // Find method
   JNIEnv *env= descr->classInstance.env;
   jclass cls= descr->classInstance.cls;
   jobject obj = descr->classInstance.obj;
   jmethodID func;
   
   /* Gewoon aanroepen zoals in alle voorbeelden staat */
   func = (*env)->GetMethodID(env, cls, "getPropertyKeys", "()[Ljava/lang/String;");
   if (func) {
      // call method and get keys
      jKeys =(*env)->CallObjectMethod(env,obj,func);

      // convert java array of strings to native vector of strings
      *retval = cta_jni_JavaStringVecToNativeVec(env,  (jobjectArray) jKeys, *keys);
   }
   else {
      CTA_WRITE_ERROR("Cannot find java method name=getPropertyKeys signature=()[Ljava/lang/String;");
      *retval = CTA_INTERNAL_ERROR;
   } 
};

#undef METHOD
#define METHOD "CTAI_ObsDescr_nativeToJava_Property_Count"
void CTAI_ObsDescr_nativeToJava_Property_Count(
         CTAI_ObsDescr_NativeToJava *descr, 
         int *nkeys, 
         int *retval)
{
   // Find method
   JNIEnv *env = descr->classInstance.env;
   jobject obj = descr->classInstance.obj;
   jclass  cls = descr->classInstance.cls;

   jmethodID func = (*env)->GetMethodID(env, cls, "getPropertyCount", "()I");
   if (func) {
      // Call method
      *nkeys=(*env)->CallIntMethod(env,obj,func);
      *retval=CTA_OK;
   }
   else {
      CTA_WRITE_ERROR("Cannot find java method name=getPropertyCount signature=()I");
      *retval=CTA_INTERNAL_ERROR;
   }   
}
;

#undef METHOD
#define METHOD "CTAI_ObsDescr_nativeToJava_Observation_Count"
   void CTAI_ObsDescr_nativeToJava_Observation_Count(
      CTAI_ObsDescr_NativeToJava *descr, 
         int *nobs, 
         int *retval)
{

   JNIEnv *env= descr->classInstance.env;
   jobject obj = descr->classInstance.obj;
   jclass cls= descr->classInstance.cls;

   jmethodID func = (*env)->GetMethodID(env, cls, "getObservationCount", "()I");
   if (func) {

      // Call method
      *nobs=(*env)->CallIntMethod(env,obj,func);
      *retval=CTA_OK;
   }
   else {
      CTA_WRITE_ERROR("Cannot find java method name=getObservationCount signature=()I");
      *retval=CTA_INTERNAL_ERROR;
   }   
};


#undef METHOD
#define METHOD "CTAI_ObsDescr_nativeToJava_Get_Properties"
void CTAI_ObsDescr_nativeToJava_Get_Properties(
         CTAI_ObsDescr_NativeToJava *descr, 
         const char *Key,
         CTA_Vector *Properties,
         CTA_Datatype *datatype,
         int *retval)
{
   jobject jProperties;
   jstring jKey;

   // Find method
   JNIEnv *env= descr->classInstance.env;
   jclass cls= descr->classInstance.cls;
   jobject obj = descr->classInstance.obj;
   jmethodID func;
   
   func = (*env)->GetMethodID(env, cls, "getStringProperties", "(Ljava/lang/String;)[Ljava/lang/String;");
   if (func) {
      // call method and get properties
      jKey= (*env)->NewStringUTF(env, Key);

      jProperties =(*env)->CallObjectMethod(env,obj,func,jKey);
      
      // convert java array of strings to native vector
      if (jProperties){
         *retval = cta_jni_JavaStringVecToNativeVec(env,  (jobjectArray) jProperties, *Properties);
      } 
      else {
         CTA_WRITE_ERROR("getStringProperties returned no properties");
         *retval = CTA_JNI_INTERFACING_ERROR;
      }
   }
   else {
      CTA_WRITE_ERROR("Cannot find java method name=getStringProperties signature=(Ljava/lang/String;)[Ljava/lang/String;");
      *retval = CTA_INTERNAL_ERROR;
   }   
}


#undef METHOD
#define METHOD "CTAI_ObsDescr_nativeToJava_Free"
void CTAI_ObsDescr_nativeToJava_Free(
         CTAI_ObsDescr_NativeToJava *descr, 
         int *retval)
{
   // Release the global references
   JNIEnv *env = descr->classInstance.env;
   (*env)->DeleteGlobalRef(env, descr->classInstance.cls);
   (*env)->DeleteGlobalRef(env, descr->classInstance.obj);

   // Since we did not allocate any memory, there is nothing to do   
   *retval=CTA_OK;
}



#undef METHOD
#define METHOD "CTA_ObsDescr_nativeToJava_initialise"
void CTA_ObsDescr_nativeToJava_initialise(CTA_ObsDescrClass *hobsdescrcl)
{
   CTA_Intf hintf=0;
   CTA_Func h_func[I_CTA_OBSDESCR_NUMFUNC];

   int ierr;

   // The vector h_func is filled with COSTA-function handles of the 
   // implementations in this file.
   ierr=CTA_Func_Create(" ",&CTAI_ObsDescr_nativeToJava_Create_Size,    hintf,
                            &h_func[I_CTA_OBSDESCR_CREATE_SIZE]);
   ierr=CTA_Func_Create(" ",&CTAI_ObsDescr_nativeToJava_Create_Init,    hintf,
                            &h_func[I_CTA_OBSDESCR_CREATE_INIT]);
   ierr=CTA_Func_Create(" ",&CTAI_ObsDescr_nativeToJava_Property_Count,    hintf,
                            &h_func[I_CTA_OBSDESCR_COUNT_PROPERTIES]);
   ierr=CTA_Func_Create(" ",&CTAI_ObsDescr_nativeToJava_Get_Properties,    hintf,
                            &h_func[I_CTA_OBSDESCR_GET_PROPERTIES]);
   ierr=CTA_Func_Create(" ",&CTAI_ObsDescr_nativeToJava_Observation_Count, hintf,
                            &h_func[I_CTA_OBSDESCR_COUNT_OBSERVATIONS]);
   ierr=CTA_Func_Create(" ",&CTAI_ObsDescr_nativeToJava_Get_Keys,    hintf,
                            &h_func[I_CTA_OBSDESCR_GET_KEYS]);
   ierr=CTA_Func_Create(" ",&CTAI_ObsDescr_nativeToJava_Free,    hintf,
                            &h_func[I_CTA_OBSDESCR_FREE]);
   ierr=CTA_Func_Create(" ",&CTAI_ObsDescr_nativeToJava_CreateSel,    hintf,
                            &h_func[I_CTA_OBSDESCR_SELECTION]);
   ierr=CTA_ObsDescr_DefineClass("cta_obsdescr_nativeToJava",h_func,hobsdescrcl);

}




