/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/openda/bridge/jni_cta_utils.cpp $
$Revision: 2694 $, $Date: 2011-08-24 08:32:07 +0200 (Wed, 24 Aug 2011) $

OpenDA interface for COSTA.
Copyright (C) 2007  Stef Hummel / Nils van Velzen

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
#define CLASSNAME  "CtaObsDescr_nativeToJava"
#define IDEBUG 0

#define MAX_EXCEPTION_LEN 256

#include <stdio.h>
#include <math.h>
#include "jni_cta_utils.h"
#include "cta.h"


static jmethodID javaIDWriter;
static jclass    javaClass;
static JNIEnv*   javaEnv;


#define METHOD "cta_jni_setJavaEnv"
void cta_jni_setJavaEnv(JNIEnv *env){
   javaEnv=env;
}

#undef METHOD
#define METHOD "cta_jni_exception"
void cta_jni_exception(JNIEnv *env, const char *ctaClassName, const char *message, int retVal)
{
	char messageString[MAX_EXCEPTION_LEN+1];
	sprintf(messageString, "%s, Costa Error Code=%d", message, retVal);
	cta_jni_exception(env, ctaClassName, messageString);
	return;
}

#undef METHOD
#define METHOD "cta_jni_exception"
void cta_jni_exception(JNIEnv *env, const char *ctaClassName, const char *message)
{
	char messageString[MAX_EXCEPTION_LEN+1];
	sprintf(messageString, "%s: %s", ctaClassName, message);
	jclass cls = env->FindClass("java/lang/RuntimeException");
	/* if cls is NULL, an exception has already been thrown */
	if (cls != NULL) {
		env->ThrowNew(cls, messageString);
	} else {
		env->FatalError("Could not get RuntimeException class");	
	}
     /* free the local ref */
    env->DeleteLocalRef(cls);
}

#undef METHOD
#define METHOD "cta_jni_free"
void cta_jni_free(JNIEnv * env, jobject obj_this) {

	CTA_Handle ctaHandle = cta_jni_getCtaHandle(env, obj_this);

	if ( ctaHandle != CTA_NULL )
	{
		CTA_Handle_Free_All(&ctaHandle);
		jclass class_CtaObject = env->GetObjectClass(obj_this);
		jfieldID ctaHandleField = env->GetFieldID(class_CtaObject, "ctaHandle", "I");
		env->SetIntField(obj_this, ctaHandleField, ctaHandle);
	}

}



#undef METHOD
#define METHOD "cta_jni_getCtaHandle"
CTA_Handle cta_jni_getCtaHandle(JNIEnv * env, jobject obj_this) {

	jclass class_CtaObject = env->GetObjectClass(obj_this);
	jfieldID ctaHandleField = env->GetFieldID(class_CtaObject, "ctaHandle", "I");
	return env->GetIntField(obj_this, ctaHandleField);

}

#undef METHOD
#define METHOD "cta_jni_setCtaHandle"
void cta_jni_setCtaHandle(JNIEnv * env, jobject obj_this, CTA_Handle ctaHandle) {

	jclass class_CtaObject = env->GetObjectClass(obj_this);
	jfieldID ctaHandleField = env->GetFieldID(class_CtaObject, "ctaHandle", "I");

   env->SetIntField(obj_this, ctaHandleField, ctaHandle);
}

#undef METHOD
#define METHOD "cta_jni_ExternalMessageWriter"
void cta_jni_ExternalMessageWriter(char *className, char *method, char *message, char type){
   
   jstring jClassName, jMethod ,jMessage,jType;
   char sType[2];
   sType[0]=type;
   sType[1]=0;

   // Create 4 Java strings
   if (className){
      jClassName =(javaEnv)->NewStringUTF(className);
   }
   if (method) {
      jMethod    =(javaEnv)->NewStringUTF(method);
   }
   if (message){
      jMessage   =(javaEnv)->NewStringUTF(message);
   }
   jType      =(javaEnv)->NewStringUTF(sType);
   
   
   // Get the Java class of the message writer
   jclass cls = javaEnv->FindClass("org/openda/costa/CtaMessageWriter");
   jmethodID my_method=NULL;
   if (cls){
      my_method = javaEnv->GetStaticMethodID(cls, "Write", "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V");
   } 

   if (my_method){
      (javaEnv)->CallStaticVoidMethod(cls, my_method, jClassName, jMethod, jMessage, jType);
   } 
   else {
      printf("Null JNI pointers found. I cannot call the message writer; the message to write is\n");
      printf("class:%s method:%s message:%s type:%s\n", className, method, message, sType);
   }
}

#undef METHOD
#define METHOD "cta_jni_ExternalMessageWriterSetID"
void cta_jni_ExternalMessageWriterSetID(JNIEnv *env, jclass classID, jmethodID methodID){
   javaIDWriter =methodID;
   javaClass    =classID;
   javaEnv      =env;
};

#undef METHOD
#define METHOD "cta_jni_JavaStringVecToNativeStringVec"
int cta_jni_JavaStringVecToNativeVec(JNIEnv *env,  jobjectArray jArray, CTA_Vector cVec){
   CTA_String aString;
   jint lenArray;
   int i, nVec, retval, ival;
   jstring j_str;
   const char *c_str;
   jboolean isCopy;
   char message[256];
   CTA_Datatype datatype;
   double val;

      // Check dimensions
      CTA_Vector_GetSize(cVec, &nVec);
      lenArray = env->GetArrayLength(jArray);
      if (nVec == lenArray) {

         // Create work string
         CTA_String_Create(&aString);

         // Get the datatype of the vector
         CTA_Vector_GetDatatype(cVec, &datatype);

         // Unpack the java vector
         retval=CTA_OK;
         for (i=0; i<lenArray && retval==CTA_OK; i++){
            // Put Java string into COSTA string
            j_str = (jstring) env->GetObjectArrayElement(jArray, i);
            c_str = env->GetStringUTFChars(j_str,&isCopy);
            
         
            // Store java string in COSTA string or convert value to double
            if (datatype == CTA_STRING) {
               if (IDEBUG) printf("cta_jni_JavaStringVecToNativeStringVec :string %d is %s\n",i+1,c_str);
               CTA_String_Set(aString,c_str);
            }
            else {
               val=atof(c_str);
               if (IDEBUG) printf("cta_jni_JavaStringVecToNativeStringVec :value %d is %f\n",i+1,val);
               
            }
            env->ReleaseStringUTFChars(j_str, c_str);

            // Store value in COSTA vector
            if (datatype==CTA_STRING) {
               // Put COSTA string into array of strings
               retval=CTA_Vector_SetVal(cVec, i+1, &aString, CTA_STRING);
            }
            else {
               if (datatype == CTA_INTEGER){
                  ival = (int) val;
                  retval=CTA_Vector_SetVal(cVec, i+1, &ival, CTA_INTEGER);
               }
               else {
                  retval=CTA_Vector_SetVal(cVec, i+1, &val, CTA_DOUBLE);
               }
            }
         }
         CTA_String_Free(&aString);
         return retval;
      }
      else {
         sprintf(message,"Dimension error: the size of the native array is %d and the java array has size %d",nVec,lenArray);
         CTA_WRITE_ERROR(message);
         return CTA_DIMENSION_ERROR;
      }





}
