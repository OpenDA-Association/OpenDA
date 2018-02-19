/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/openda/bridge/org_openda_costa_CtaObservationDescriptions.cpp $
$Revision: 2557 $, $Date: 2011-07-05 10:39:56 +0200 (Tue, 05 Jul 2011) $

OpenDA interface for COSTA.
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


#include "org_openda_costa_CtaObservationDescriptions.h"
#include "cta.h"
#include "jni_cta_utils.h"
#include "jni_datatypes.h"
#define IDEBUG (0)

/*
 * Class:     org_openda_costa_CtaObservationDescriptions
 * Method:    getPropertyKeys
 * Signature: ()[Ljava/lang/String;
 */
JNIEXPORT jobjectArray JNICALL Java_org_openda_costa_CtaObservationDescriptions_getPropertyKeys
(JNIEnv *env, jobject jObsDescr){

   cta_jni_setJavaEnv(env);
   CTA_ObsDescr ctaObsDescr   = cta_jni_getCtaHandle(env, jObsDescr);

   int nKeys;
   CTA_ObsDescr_Property_Count (ctaObsDescr, &nKeys);

   CTA_Vector vKeys;
   CTA_Vector_Create(CTA_DEFAULT_VECTOR, nKeys, CTA_STRING, CTA_NULL, &vKeys);
   int retVal=CTA_ObsDescr_Get_PropertyKeys(ctaObsDescr, vKeys);
   if (retVal!=CTA_OK){
      cta_jni_exception(env, "CtaObservationDescriptions", "Could not get Keys", retVal);
	  return NULL;
   }


   /* Create and fill a Java array of strings */
   jclass clsString = env->FindClass("java/lang/String");
   jobjectArray results = env->NewObjectArray((long) nKeys, clsString, NULL);

   CTA_String sKey;
   CTA_String_Create(&sKey);
   for (int i=0; i<nKeys; i++){
      CTA_Vector_GetVal(vKeys,i+1,&sKey,CTA_STRING);
      jstring str = env->NewStringUTF(CTAI_String_GetPtr(sKey));
		env->SetObjectArrayElement(results, i, str);
   }

   CTA_String_Free(&sKey);
   CTA_Vector_Free(&vKeys);
   return results;
}


/*
 * Class:     org_openda_costa_CtaObservationDescriptions
 * Method:    ctaCreateSelection
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaObservationDescriptions_ctaCreateSelection
(JNIEnv *env, jobject jObsDescr, jstring jSelect){

   CTA_String select;

   printf("Debug we maken een selectie \n");
   cta_jni_setJavaEnv(env);
   /* create COSTA string with selection */
   CTA_String_Create(&select);
   const char *sel  = env->GetStringUTFChars(jSelect, 0);
   printf("Selectie is %s\n",sel);
   CTA_String_Set(select,sel);
   env->ReleaseStringUTFChars(jSelect, sel);

   CTA_ObsDescr descrOut;
   CTA_ObsDescr ctaObsDescr = cta_jni_getCtaHandle(env, jObsDescr);

   printf("De obs descr waar we het mee doen is %d\n",ctaObsDescr);
   int retVal = CTA_SObs_CreateSel (ctaObsDescr, select, &descrOut);
   if (retVal!=CTA_OK){
      cta_jni_exception(env, "CtaObservationDescriptions", "Could not create selection", retVal);
	  return 0;
   }
   CTA_String_Free(&select);

   return descrOut;
}


/*
 * Class:     org_openda_costa_CtaObservationDescriptions
 * Method:    ctaCreateTimeSelection
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaObservationDescriptions_ctaCreateTimeSelection
(JNIEnv *env, jobject jObsDescr, jint timesel){

   CTA_ObsDescr descrOut;
   CTA_ObsDescr ctaObsDescr;

   cta_jni_setJavaEnv(env);

   ctaObsDescr = cta_jni_getCtaHandle(env, jObsDescr);
   int retVal = CTA_SObs_CreateTimSel (ctaObsDescr, timesel, &descrOut);
   if (retVal!=CTA_OK){
      cta_jni_exception(env, "CtaStochObserver", "Could not create selection", retVal);
	  return 0;
   }

   return descrOut;
}


/*
 * Class:     org_costa_CtaObservationDescriptions
 * Method:    getObservationCount
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaObservationDescriptions_getObservationCount
(JNIEnv *env, jobject jObsDescr){

   CTA_ObsDescr ctaObsDescr;
   int nobs;
   int retVal;

   cta_jni_setJavaEnv(env);
   ctaObsDescr = cta_jni_getCtaHandle(env, jObsDescr);
   retVal = CTA_ObsDescr_Observation_Count(ctaObsDescr, &nobs);
   if (retVal!=CTA_OK){
      cta_jni_exception(env, "CtaStochObserver", "Could not get observation count", retVal);
	  return 0;
   }
   return nobs;
}



/*
 * Class:     org_costa_CtaObservationDescriptions
 * Method:    getPropertyCount
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaObservationDescriptions_getPropertyCount
(JNIEnv *env, jobject jObsDescr){
   CTA_ObsDescr ctaObsDescr;
   int nprop;
   int retVal;

   cta_jni_setJavaEnv(env);
   ctaObsDescr = cta_jni_getCtaHandle(env, jObsDescr);
   
   retVal = CTA_ObsDescr_Property_Count(ctaObsDescr, &nprop);
   if (retVal!=CTA_OK){
      cta_jni_exception(env, "CtaStochObserver", "Could not get property count", retVal);
	  return 0;
   }
   return nprop;
}


/*
 * Class:     org_costa_CtaObservationDescriptions
 * Method:    getStringProperties
 * Signature: (Ljava/lang/String;)[Ljava/lang/String;
 */
JNIEXPORT jobjectArray JNICALL Java_org_openda_costa_CtaObservationDescriptions_getStringProperties
(JNIEnv *env, jobject jObsDescr, jstring jKey){

   CTA_ObsDescr ctaObsDescr;
   int nMeasr;

   cta_jni_setJavaEnv(env);
   
   ctaObsDescr   = cta_jni_getCtaHandle(env, jObsDescr);
   CTA_ObsDescr_Observation_Count (ctaObsDescr, &nMeasr);

   CTA_Vector vProp;
   CTA_Vector_Create(CTA_DEFAULT_VECTOR, nMeasr, CTA_STRING, CTA_NULL, &vProp);


   const char *key=env->GetStringUTFChars(jKey, 0);
   int retVal = CTA_ObsDescr_Get_ValueProperties(ctaObsDescr, key, vProp, CTA_STRING);
   env->ReleaseStringUTFChars(jKey, key);
   if (retVal!=CTA_OK){
      printf("hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh\n");
      cta_jni_exception(env, "CtaObservationDescriptions", "Could not get String Properties", retVal);
	  return NULL;
   }


   /* Create and fill a Java array of strings */
   jclass clsString = env->FindClass("java/lang/String");
   jobjectArray results = env->NewObjectArray((long) nMeasr, clsString, NULL);

   CTA_String sProp;
   CTA_String_Create(&sProp);
   for (int i=0; i<nMeasr; i++){
      CTA_Vector_GetVal(vProp,i+1,&sProp,CTA_STRING);
      jstring str = env->NewStringUTF(CTAI_String_GetPtr(sProp));
		env->SetObjectArrayElement(results, i, str);
   }

   CTA_String_Free(&sProp);
   CTA_Vector_Free(&vProp);
   return results;
}



/*
 * Class:     org_openda_costa_CtaObservationDescriptions
 * Method:    ctaCreateNativeToJavaObserver
 * Signature: (Lorg/openda/interfaces/IObservationDescriptions;)I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaObservationDescriptions_ctaCreateNativeToJavaObserver
(JNIEnv *env, jobject jObsDescr, jobject jJavaObsDescr){

   cta_jni_setJavaEnv(env);

   if (IDEBUG>0) {printf("Debug: Creating observation descriptions \n");}
   // Get the class of the Java implementation
   jclass cls = env->GetObjectClass(jJavaObsDescr);

   // Find the COSTA class
   CTA_ObsDescrClass hobsdescrcl;
   CTA_String hClass;
   CTA_String_Create(&hClass);
   CTA_String_Set(hClass,"cta_obsdescr_nativeToJava");
   CTA_Handle_Find(hClass, CTA_OBSDESCRCLASS, &hobsdescrcl);

   // Create this new class
   CTA_ObsDescr hobsdscr;

   // create the user input argument
   sJni_Class jniClass;
   jniClass.env = env;
   jniClass.cls = cls;
   jniClass.obj = jJavaObsDescr;

   // Create the actual class
   CTA_Handle hJniClass;
   CTA_Handle_Create("Java class", CTA_DATABLOCK,&jniClass, &hJniClass);

   CTA_ObsDescr_Create( hobsdescrcl, hJniClass, &hobsdscr);
   if (IDEBUG>0) {printf("Debug: Handle of created observation descriptions is %d\n",hobsdscr);}



   //Free work objects / mem
   CTA_Handle_Free(&hJniClass);
   CTA_String_Free(&hClass);

   return hobsdscr;

}



/*
 * Class:     org_costa_CtaObservationDescriptions
 * Method:    ctaGetValueProperties
 * Signature: (Ljava/lang/String;I)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaObservationDescriptions_ctaGetValueProperties
(JNIEnv *env, jobject jObsDescr, jstring jKey, jint vProp){

   cta_jni_setJavaEnv(env);

   CTA_ObsDescr ctaObsDescr   = cta_jni_getCtaHandle(env, jObsDescr);
   printf("De handle van de obsdescr is %d\n",ctaObsDescr);

   const char *key=env->GetStringUTFChars(jKey, 0);
   printf("De key is %s\n",key);
   int retVal = CTA_ObsDescr_Get_ValueProperties(ctaObsDescr, key, vProp, CTA_DOUBLE);
   env->ReleaseStringUTFChars(jKey, key);
   if (retVal!=CTA_OK){
      printf("HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH\n");
      cta_jni_exception(env, "CtaObservationDescriptions", "Could not get String Properties", retVal);
	  return;
   }
}











