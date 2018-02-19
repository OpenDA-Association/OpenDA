/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/openda/bridge/org_openda_costa_CtaOpenDaModel.cpp $
$Revision: 3319 $, $Date: 2012-05-30 10:12:43 +0200 (Wed, 30 May 2012) $

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

#include "org_openda_costa_CtaOpenDaModel.h"
#include "org_openda_costa_CtaTreeVector.h"
#include "cta_model.h"
#include "cta_model_factory.h"
#include "jni_cta_utils.h"
#include "cta_errors.h"
#include "cta_defaults.h"
#include "cta_mem.h"
#include "string.h"

#define IDEBUG (0)

/* Store a list of all classes that are created */
static int            CTAI_nClasses          = 0;
static CTA_ModelClass *CTAI_Created_Classes  = NULL;
static char           **CTAI_ClassFile       = NULL;
static char           **CTAI_ClassConfigFile = NULL;


/*Private functions */

jintArray Java_org_openda_costa_CtaOpenDaModel_ctaGetObservedLocalization
  (JNIEnv *env, jobject jModel, jobject jObsDescr, jdouble distance, jint iDomain){

   int nObs;
   int retVal;
   CTA_Vector locVecs;

   cta_jni_setJavaEnv(env);
   CTA_Model ctaModel       = cta_jni_getCtaHandle(env, jModel);
   CTA_ObsDescr ctaObsDescr = cta_jni_getCtaHandle(env, jObsDescr);

   /* Get number of observations */
   CTA_ObsDescr_Observation_Count(ctaObsDescr, &nObs);

   /* Create array */
   jintArray jValues = env->NewIntArray(nObs);

   /* Create COSTA vector */
   retVal = CTA_Vector_Create(CTA_DEFAULT_VECTOR, nObs, CTA_HANDLE, CTA_NULL, &locVecs);

   /* Set all indices NULL */
   CTA_Handle nullHandle=CTA_NULL;
   retVal = CTA_Vector_SetConstant(locVecs, &nullHandle, CTA_HANDLE);

   if (iDomain >=0){
      /* Get localization from model */
      retVal = CTA_Model_GetObsLocalizationDomain(ctaModel, ctaObsDescr, distance, iDomain, locVecs);
   }
   else {
      retVal = CTA_Model_GetObsLocalization(ctaModel, ctaObsDescr, distance, locVecs);
   }

   /* Copy costa handles into Java integer array */
   int *cValues = new int[nObs];
   retVal = CTA_Vector_GetVals(locVecs, (void *) cValues, nObs, CTA_HANDLE);
   env->SetIntArrayRegion(jValues, 0, nObs, (jint *) cValues);

   /* Set all handles to CTA_NULL in order to avoid deallocation */
   retVal = CTA_Vector_SetConstant(locVecs, &nullHandle, CTA_HANDLE);

   /* Free work variables */
   delete [] cValues;
   retVal=CTA_Vector_Free(&locVecs);

   return jValues;
}






/*
 * Class:     org_costa_CtaOpenDaModel
 * Method:    CtaOpenDaModelCreate
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaOpenDaModel_ctaCreate
(JNIEnv *env, jobject, jstring jModelCls, jstring jConfigFile){

   cta_jni_setJavaEnv(env);

   /* Translate jModelCls and jConfigFile to C-strings */
   const char *modelClassConfigFile = env->GetStringUTFChars(jModelCls, 0);
   const char *ConfigFile = env->GetStringUTFChars(jConfigFile, 0);

   if (IDEBUG>0) {
     printf("Java_org_openda_costa_CtaOpenDaModel_ctaCreate: Start of function:\n");
     printf("modelClassConfigFile   =%s\n",modelClassConfigFile);
     printf("ConfigFile   =%s\n",ConfigFile);
   }

   /* Get the COSTA modelClass object */
   /* Figure out whether this class already exists*/
   CTA_ModelClass modelClass;
   bool newClass=true;
   for (int iClass=0; iClass<CTAI_nClasses; iClass++){
      if (strcmp(CTAI_ClassFile[iClass],modelClassConfigFile)==0 &&
          strcmp(CTAI_ClassConfigFile[iClass],ConfigFile)==0){
          newClass=false;
          modelClass=CTAI_Created_Classes[iClass];
      }
   }

   /* If the class does not exist yet, create it and administrate it in the list of all classes */
   if (newClass){
      /* extend the list */
      CTAI_nClasses++;
      CTAI_Created_Classes = (CTA_ModelClass *) CTA_Realloc(CTAI_Created_Classes, CTAI_nClasses*sizeof(CTA_ModelClass));
      CTAI_ClassFile       = (char**) CTA_Realloc(CTAI_ClassFile,       CTAI_nClasses*sizeof(char*));
      CTAI_ClassConfigFile = (char**) CTA_Realloc(CTAI_ClassConfigFile, CTAI_nClasses*sizeof(char*));

      /* store the name of the ClassFile and ClassConfigFile */
      CTAI_ClassFile[CTAI_nClasses-1]       = (char*) CTA_Malloc((strlen(modelClassConfigFile)+1)*sizeof(char));
      CTAI_ClassConfigFile[CTAI_nClasses-1] = (char*) CTA_Malloc((strlen(ConfigFile)+1)*sizeof(char));
      strcpy(CTAI_ClassFile[CTAI_nClasses-1],modelClassConfigFile);
      strcpy(CTAI_ClassConfigFile[CTAI_nClasses-1],ConfigFile);

      /* Create a new modelclass*/
      int retVal = CTA_ModelFactory_New(modelClassConfigFile, &modelClass);
      CTAI_Created_Classes[CTAI_nClasses-1] = modelClass;

      /* Release the jModelCls C-string */
      env->ReleaseStringUTFChars(jModelCls, modelClassConfigFile );
      if ( retVal != CTA_OK ) {
         cta_jni_exception(env, "CtaOpenDaModel", "Could create model class", retVal);
         return 0;
      }

      if (IDEBUG>0) {
         printf("Java_org_openda_costa_CtaOpenDaModel_ctaCreate: handle of modelClass=%d\n",modelClass);
      }
   }

   /* Create a model instance */

   /* -Create costa string with name of the model configuration */
   CTA_String ctaConfigFile;
   CTA_String_Create(&ctaConfigFile);
   CTA_String_Set(ctaConfigFile, ConfigFile);
   env->ReleaseStringUTFChars(jConfigFile, ConfigFile);

   /* -Create the model */
   CTA_Model ctaModel;
   if (IDEBUG>0) printf("Java_org_openda_costa_CtaOpenDaModel_ctaCreate: before model_create ");
   int retVal = CTA_Model_Create(modelClass,ctaConfigFile,&ctaModel);
   if (IDEBUG>0) printf("Java_org_openda_costa_CtaOpenDaModel_ctaCreate: after model_create ");
   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaOpenDaModel", "Could NOT perform CTA_Model_Create", retVal);
      return 0;
   }

   if (IDEBUG>0) {
      printf("Java_org_openda_costa_CtaOpenDaModel_ctaCreate: Handle of model is %d\n",ctaModel);
      printf("Java_org_openda_costa_CtaOpenDaModel_ctaCreate: End of function:\n");
   }
   return ctaModel;
}

/*
 * Class:     org_costa_CtaOpenDaModel
 * Method:    CtaModelAnnounceObservedValues
 * Signature: (Lorg/openda/interfaces/IObservationDescriptions;)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaOpenDaModel_ctaAnnounceObservedValues
(JNIEnv *env, jobject jModel, jobject jObsDescr){

   cta_jni_setJavaEnv(env);
   CTA_Model ctaModel       = cta_jni_getCtaHandle(env, jModel);
   CTA_ObsDescr ctaObsDescr = cta_jni_getCtaHandle(env, jObsDescr);

   int retVal=CTA_Model_AnnounceObsValues(ctaModel, ctaObsDescr);
   if ( retVal != CTA_OK ) {
     cta_jni_exception(env, "CtaOpenDaModel", "Could NOT perform CTA_Model_AnnounceObsValues", retVal);
     return;
   }
}

/*
 * Class:     org_costa_CtaOpenDaModel
 * Method:    CtaModelAxpyOnParameters
 * Signature: (DLorg/openda/interfaces/IVector;)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaOpenDaModel_ctaAxpyOnParameters
(JNIEnv *env, jobject jModel, jdouble alpha, jobject jX){

   cta_jni_setJavaEnv(env);
   CTA_Model ctaModel  = cta_jni_getCtaHandle(env, jModel);
   CTA_TreeVector ctaX = cta_jni_getCtaHandle(env, jX);

   int retVal=CTA_Model_AxpyParam(ctaModel, alpha, ctaX);
   if ( retVal != CTA_OK ) {
     cta_jni_exception(env, "CtaOpenDaModel", "Could perform CTA_Model_AxpyParam", retVal);
     return;
   }
}


/*
 * Class:     org_costa_CtaOpenDaModel
 * Method:    CtaModelCompute
 * Signature: (Lorg/openda/interfaces/ITime;)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaOpenDaModel_ctaCompute
(JNIEnv *env, jobject jModel, jobject jTime){

   double tstart, tstop;

   cta_jni_setJavaEnv(env);

   if (IDEBUG>0) {
     printf("Java_org_openda_costa_CtaOpenDaModel_ctaCompute: Start of function:\n");
   }

   CTA_Model ctaModel = cta_jni_getCtaHandle(env, jModel);
   CTA_Time  ctaTime  = cta_jni_getCtaHandle(env, jTime);


   if (IDEBUG>0) {
     printf("Java_org_openda_costa_CtaOpenDaModel_ctaCompute: model handle =%d:\n",ctaModel);
     printf("Java_org_openda_costa_CtaOpenDaModel_ctaCompute: time  handle =%d:\n",ctaTime);
     tstart=-9999.0;
     tstop=-9999.0;
     CTA_Time_GetSpan(ctaTime,&tstart,&tstop);
     printf("Java_org_openda_costa_CtaOpenDaModel_ctaCompute: from =%g %g :\n",tstart,tstop);
   }


   int retVal=CTA_Model_Compute(ctaModel, ctaTime);
   if ( retVal != CTA_OK ) {
     cta_jni_exception(env, "CtaOpenDaModel", "Could perform CTA_Model_Compute", retVal);
     return;
   }

   if (IDEBUG>0) {
     printf("Java_org_openda_costa_CtaOpenDaModel_ctaCompute: End of function:\n");
   }

}




/*
 * Class:     org_costa_CtaOpenDaModel
 * Method:    CtaModelgetObservedValues
 * Signature: (Lorg/openda/interfaces/IObservationDescriptions;Lorg/costa/CtaVector;)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaOpenDaModel_ctaGetObservedValues
(JNIEnv *env, jobject jModel, jobject jObsDescr, jobject jValues){

   cta_jni_setJavaEnv(env);
   CTA_Model ctaModel       = cta_jni_getCtaHandle(env, jModel);
   CTA_ObsDescr ctaObsDescr = cta_jni_getCtaHandle(env, jObsDescr);
   CTA_Vector ctaValues     = cta_jni_getCtaHandle(env, jValues);
   CTA_Time time;

   // Create an empty time interval
   CTA_Time_Create(&time);
   CTA_Time_SetSpan(time, 0.0, -1.0);

   int retVal=CTA_Model_GetObsValues (ctaModel, time, ctaObsDescr, ctaValues);

   CTA_Time_Free(&time);

   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaOpenDaModel", "Could not perform CTA_Model_GetObsValues", retVal);
      return;
   }
}

/*
 * Class:     org_costa_CtaOpenDaModel
 * Method:    ctaGetParameters
 * Signature: ()Lorg/openda/interfaces/IVector;
 */
JNIEXPORT jobject JNICALL Java_org_openda_costa_CtaOpenDaModel_ctaGetParameters
(JNIEnv *env, jobject jModel){

   cta_jni_setJavaEnv(env);

   CTA_Model ctaModel       = cta_jni_getCtaHandle(env, jModel);
   CTA_TreeVector ctaParams =CTA_NULL;

   int retVal=CTA_Model_GetParam (ctaModel, &ctaParams);
   if ( retVal != CTA_OK ) {
     cta_jni_exception(env, "CtaOpenDaModel", "Could perform CTA_Model_GetParam", retVal);
     return NULL;
   }

   /* Create a Java CtaTreeVector */
   jclass clsTreeVector = env->FindClass("org/openda/costa/CtaTreeVector");
   jmethodID constructorID = env->GetMethodID (clsTreeVector, "<init>", "()V");
   jobject jState = env->NewObject(clsTreeVector, constructorID);

   cta_jni_setCtaHandle(env, jState, ctaParams);

   return jState;
}

/*
 * Class:     org_costa_CtaOpenDaModel
 * Method:    ctaGetState
 * Signature: ()Lorg/openda/interfaces/IVector;
 */
JNIEXPORT jobject JNICALL Java_org_openda_costa_CtaOpenDaModel_ctaGetState
(JNIEnv *env, jobject jModel){

   cta_jni_setJavaEnv(env);
   CTA_Model ctaModel       = cta_jni_getCtaHandle(env, jModel);
   CTA_TreeVector ctaState =CTA_NULL;
   int retVal=CTA_Model_GetState (ctaModel, &ctaState);
   if ( retVal != CTA_OK ) {
     cta_jni_exception(env, "CtaOpenDaModel", "Could perform CTA_Model_GetState", retVal);
     return NULL;
   }

   /* Create a Java CtaTreeVector */
   jclass clsTreeVector = env->FindClass("org/openda/costa/CtaTreeVector");
   jmethodID constructorID = env->GetMethodID (clsTreeVector, "<init>", "()V");
   jobject jState = env->NewObject(clsTreeVector, constructorID);

   cta_jni_setCtaHandle(env, jState, ctaState);

   return jState;
}

/*
 * Class:     org_costa_CtaOpenDaModel
 * Method:    ctaGetStateScaling
 * Signature: ()Lorg/openda/interfaces/IVector;
 */
JNIEXPORT jobject JNICALL Java_org_openda_costa_CtaOpenDaModel_ctaGetStateScaling
(JNIEnv *env, jobject jModel){

   cta_jni_setJavaEnv(env);
   CTA_Model ctaModel       = cta_jni_getCtaHandle(env, jModel);
   CTA_TreeVector ctaScal =CTA_NULL;

   int retVal=CTA_Model_GetStateScaling (ctaModel, &ctaScal);
   if ( retVal != CTA_OK ) {
     cta_jni_exception(env, "CtaOpenDaModel", "Could perform CTA_Model_GetStateScaling", retVal);
     return NULL;
   }

   /* Create a Java CtaTreeVector */
   jclass clsTreeVector = env->FindClass("org/openda/costa/CtaTreeVector");
   jmethodID constructorID = env->GetMethodID (clsTreeVector, "<init>", "()V");
   jobject jState = env->NewObject(clsTreeVector, constructorID);

   cta_jni_setCtaHandle(env, jState, ctaScal);

   return jState;
}


/*
 * Class:     org_openda_costa_CtaOpenDaModel
 * Method:    ctaGetObservedLocalization
 * Signature: (Lorg/openda/interfaces/IObservationDescriptions;D)[I
 */
JNIEXPORT jintArray JNICALL Java_org_openda_costa_CtaOpenDaModel_ctaGetObservedLocalization
  (JNIEnv *env, jobject jModel, jobject jObsDescr, jdouble distance){

   return Java_org_openda_costa_CtaOpenDaModel_ctaGetObservedLocalization
      (env, jModel, jObsDescr, distance, -1);
}

/*
 * Class:     org_openda_costa_CtaOpenDaModel
 * Method:    ctaReleaseInternalState
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaOpenDaModel_ctaReleaseInternalState
  (JNIEnv *env, jobject jModel, jstring jID){
  // Release resources used to save a state at some earlier time.

   cta_jni_setJavaEnv(env);
   CTA_Model ctaModel       = cta_jni_getCtaHandle(env, jModel);
   if (IDEBUG>0) {
     printf("Java_org_openda_costa_CtaOpenDaModel_ctaReleaseInternalState: Start of function:\n");
   }

   // create costa string with ID of state to be released
   const char *ID = env->GetStringUTFChars(jID, 0);
   CTA_String cID;
   CTA_String_Create(&cID);
   CTA_String_Set(cID,ID);
   env->ReleaseStringUTFChars(jID, ID);

   int retVal = CTA_Model_ReleaseInternalState(ctaModel,cID);
   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaOpenDaModel", "Could perform CTA_Model_ReleaseInternalState", retVal);
      return;
   }
   if (IDEBUG>0) {
     printf("Java_org_openda_costa_CtaOpenDaModel_ctaReleaseInternalState: End of function:\n");
   }
}

/*
 * Class:     org_openda_costa_CtaOpenDaModel
 * Method:    ctaSaveInternalState
 * Signature: ()Lorg/openda/interfaces/IModelState;
 */
JNIEXPORT jobject JNICALL Java_org_openda_costa_CtaOpenDaModel_ctaSaveInternalState
  (JNIEnv *env, jobject jModel){

   cta_jni_setJavaEnv(env);
   if (IDEBUG>0) {
     printf("Java_org_openda_costa_CtaOpenDaModel_ctaSaveInternalState: Start of function:\n");
   }

   CTA_Model ctaModel = cta_jni_getCtaHandle(env, jModel);
   /* create costa string */
   CTA_String cID;
   CTA_String_Create(&cID);
   /* save the state and get the ID of it back */
   int retVal = CTA_Model_SaveInternalState(ctaModel,&cID);
   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaOpenDaModel", "Could perform CTA_Model_SaveInternalState", retVal);
      return NULL;
   }

   /* put the returned costa string into a java string */
   char *ID = CTAI_String_GetPtr(cID);
   jstring jID =(env)->NewStringUTF(ID);
   if (IDEBUG>0) {
      printf("# returned instanceID bridge = '%s'\n",ID);
   }

   /* create the ctaModelState object */
   jclass clsModelState = env->FindClass("org/openda/costa/CtaModelState");
   jmethodID constructorID = env->GetMethodID (clsModelState, "<init>", "(Ljava/lang/String;I)V");
   jobject jModelState = env->NewObject(clsModelState, constructorID, jID, (jint) ctaModel );
   if (IDEBUG>0) {
     printf("Java_org_openda_costa_CtaOpenDaModel_ctaSaveInternalState: End of function:\n");
   }
   CTA_String_Free(&cID);
   return jModelState;
}

/*
 * Class:     org_openda_costa_CtaOpenDaModel
 * Method:    ctaLoadPersistentState
 * Signature: (Ljava/lang/String;)Lorg/openda/interfaces/IModelState;
 */
JNIEXPORT jobject JNICALL Java_org_openda_costa_CtaOpenDaModel_ctaLoadPersistentState
  (JNIEnv *env, jobject jModel, jstring jFileName){

   cta_jni_setJavaEnv(env);
   if (IDEBUG>0) {
     printf("Java_org_openda_costa_CtaOpenDaModel_ctaLoadPersistentState: Start of function:\n");
   }

   CTA_Model ctaModel = cta_jni_getCtaHandle(env, jModel);
   /* create costa strings*/
   CTA_String cFileName;
   CTA_String cID;
   const char *fileName = env->GetStringUTFChars(jFileName, 0);

   CTA_String_Create(&cFileName);
   CTA_String_Create(&cID);
   CTA_String_Set(cFileName, fileName);
   env->ReleaseStringUTFChars(jFileName, fileName);

   /* load the state from file, get ID of loaded state back*/
   int retVal = CTA_Model_LoadPersistentState(ctaModel, cFileName, &cID);

   /* put the returned costa string into a java string */
   char *ID = CTAI_String_GetPtr(cID);
   jstring jID =(env)->NewStringUTF(ID);

   /* create the ctaModelState object */
   jclass clsModelState = env->FindClass("org/openda/costa/CtaModelState");
   jmethodID constructorID = env->GetMethodID (clsModelState, "<init>", "(Ljava/lang/String;I)V");
   jobject jModelState = env->NewObject(clsModelState, constructorID, jID, (jint) ctaModel );

   CTA_String_Free(&cID);
   return jModelState;
}

/*
 * Class:     org_costa_CtaOpenDaModel
 * Method:    setAutomaticNoiseGeneration
 * Signature: (Z)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaOpenDaModel_setAutomaticNoiseGeneration
(JNIEnv *env, jobject jModel, jboolean add_noise){

   cta_jni_setJavaEnv(env);
   CTA_Model ctaModel = cta_jni_getCtaHandle(env, jModel);
   CTA_Time timespan;
   double t1,t2;
   CTA_Time_Create(&timespan);
   if (add_noise){
      t1=-999e99;
      t2= 999e99;
   }
   else {
      t1=999e99;
      t2=-999e99;
   }

   CTA_Time_SetSpan(timespan,t1,t2);


   int retVal=CTA_Model_AddNoise (ctaModel, timespan);
   CTA_Time_Free(&timespan);
   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaOpenDaModel", "Could perform CTA_Model_AddNoise", retVal);
      return;
   }
}

/*
 * Class:     org_openda_costa_CtaOpenDaModel
 * Method:    ctaRestoreInternalState
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaOpenDaModel_ctaRestoreInternalState
   (JNIEnv *env, jobject jModel, jstring jID){
   //Restore a previously saved state of the model
   int retVal;

   cta_jni_setJavaEnv(env);
   if (IDEBUG>0) {
     printf("Java_org_openda_costa_CtaOpenDaModel_ctaRestoreInternalState: Start of function:\n");
   }
   CTA_Model ctaModel = cta_jni_getCtaHandle(env, jModel);
   const char*ID = env->GetStringUTFChars(jID, 0);
   CTA_String cID;
   CTA_String_Create(&cID);
   CTA_String_Set(cID,ID);

   retVal = CTA_Model_RestoreInternalState(ctaModel,cID);
   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaOpenDaModel", "Could perform CTA_Model_RestoreInternalState", retVal);
      return;
   }
}

/*
 * Class:     org_costa_CtaOpenDaModel
 * Method:    CtaModelSetParameters
 * Signature: (Lorg/openda/interfaces/IVector;)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaOpenDaModel_ctaSetParameters
(JNIEnv *env, jobject jModel, jobject jParams){

   cta_jni_setJavaEnv(env);
   CTA_Model ctaModel = cta_jni_getCtaHandle(env, jModel);
   CTA_TreeVector ctaParams = cta_jni_getCtaHandle(env, jParams);
   int retVal=CTA_Model_SetParam (ctaModel, ctaParams);
   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaOpenDaModel", "Could perform CTA_Model_AddNoise", retVal);
      return;
   }
}

/*
 * Class:     org_costa_CtaOpenDaModel
 * Method:    getTimeHorizon
 * Signature: ()Lorg/openda/interfaces/ITime;
 */
JNIEXPORT jobject JNICALL Java_org_openda_costa_CtaOpenDaModel_getTimeHorizon
   (JNIEnv *env, jobject jModel){

   CTA_Time timeHorizon;

   cta_jni_setJavaEnv(env);

   /* Create a new time instance */
   CTA_Time_Create(&timeHorizon);

   /* Get the handle of the model */
   CTA_Model ctaModel       = cta_jni_getCtaHandle(env, jModel);

   /* Call the method */
   int retVal=CTA_Model_GetTimeHorizon (ctaModel, timeHorizon);
   if ( retVal != CTA_OK ) {
         	cta_jni_exception(env, "CtaOpenDaModel", "Could perform CTA_Model_GetTimeHorizon", retVal);
         	return NULL;
    }

   /* Create Java Time */
   jclass clsTime = env->FindClass("org/openda/costa/CtaTime");
   jmethodID constructorID = env->GetMethodID (clsTime, "<init>", "()V");
   jobject jTime = env->NewObject(clsTime, constructorID);
   /* Set the handle */
   cta_jni_setCtaHandle(env, jTime, timeHorizon);

   return jTime;
}

/*
 * Class:     org_costa_CtaOpenDaModel
 * Method:    getCurrentTime
 * Signature: ()Lorg/openda/interfaces/ITime;
 */
JNIEXPORT jobject JNICALL Java_org_openda_costa_CtaOpenDaModel_getCurrentTime
   (JNIEnv *env, jobject jModel){

   CTA_Time currentTime;

   cta_jni_setJavaEnv(env);

   /* Create a new time instance */
   CTA_Time_Create(&currentTime);

   /* Get the handle of the model */
   CTA_Model ctaModel       = cta_jni_getCtaHandle(env, jModel);

   /* Call the method */
   int retVal=CTA_Model_GetCurrentTime (ctaModel, currentTime);
   if ( retVal != CTA_OK ) {
     cta_jni_exception(env, "CtaOpenDaModel", "Could perform CTA_Model_GetCurrentTime", retVal);
     return NULL;
   }

   /* Create Java Time */
   jclass clsTime = env->FindClass("org/openda/costa/CtaTime");
   jmethodID constructorID = env->GetMethodID (clsTime, "<init>", "()V");
   jobject jTime = env->NewObject(clsTime, constructorID);
   /* Set the handle */
   cta_jni_setCtaHandle(env, jTime, currentTime);

   return jTime;
}


/*
 * Class:     org_openda_costa_CtaOpenDaModel
 * Method:    ctaAxpyOnStateDomain
 * Signature: (DLorg/openda/interfaces/IVector;I)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaOpenDaModel_ctaAxpyOnStateDomain
  (JNIEnv *env, jobject jModel, jdouble alpha, jobject jX, jint iDomain){

   cta_jni_setJavaEnv(env);
   CTA_Model ctaModel  = cta_jni_getCtaHandle(env, jModel);
   CTA_TreeVector ctaX = cta_jni_getCtaHandle(env, jX);

   int retVal=CTA_OK;
   if (iDomain<0){
      retVal=CTA_Model_AxpyState(ctaModel, alpha, ctaX);
   }
   else {
      retVal=CTA_Model_AxpyStateDomain(ctaModel, alpha, iDomain, ctaX);
   }
   if ( retVal != CTA_OK ) {
     cta_jni_exception(env, "CtaOpenDaModel", "Could perform CTA_Model_AxpyState", retVal);
     return;
   }
}


/*
 * Class:     org_openda_costa_CtaOpenDaModel
 * Method:    ctaGetStateDomain
 * Signature: (I)Lorg/openda/interfaces/IVector;
 */
JNIEXPORT jobject JNICALL Java_org_openda_costa_CtaOpenDaModel_ctaGetStateDomain
  (JNIEnv *env, jobject jModel, jint iDomain){
   cta_jni_setJavaEnv(env);
   CTA_Model ctaModel       = cta_jni_getCtaHandle(env, jModel);
   CTA_TreeVector ctaState =CTA_NULL;
   int retVal=CTA_Model_GetStateDomain(ctaModel, iDomain, &ctaState);
   if ( retVal != CTA_OK ) {
     cta_jni_exception(env, "CtaOpenDaModel", "Could perform CTA_Model_GetState", retVal);
     return NULL;
   }

   /* Create a Java CtaTreeVector */
   jclass clsTreeVector = env->FindClass("org/openda/costa/CtaTreeVector");
   jmethodID constructorID = env->GetMethodID (clsTreeVector, "<init>", "()V");
   jobject jState = env->NewObject(clsTreeVector, constructorID);

   cta_jni_setCtaHandle(env, jState, ctaState);

   return jState;
}


/*
 * Class:     org_openda_costa_CtaOpenDaModel
 * Method:    ctaGetObservedLocalizationDomain
 * Signature: (Lorg/openda/interfaces/IObservationDescriptions;DI)[I
 */
JNIEXPORT jintArray JNICALL Java_org_openda_costa_CtaOpenDaModel_ctaGetObservedLocalizationDomain
  (JNIEnv *env, jobject jModel, jobject jObsDescr, jdouble distance, jint iDomain){

   return Java_org_openda_costa_CtaOpenDaModel_ctaGetObservedLocalization
      (env, jModel, jObsDescr, distance, iDomain);
}


/*
 * Class:     org_openda_costa_CtaOpenDaModel
 * Method:    ctaGetNumDomains
 * Signature: (D)I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaOpenDaModel_ctaGetNumDomains
  (JNIEnv *env, jobject jModel, jdouble distance){

   cta_jni_setJavaEnv(env);
   CTA_Model ctaModel  = cta_jni_getCtaHandle(env, jModel);

   int nDomains;
   int retVal=CTA_Model_GetNumDomains(ctaModel, distance, &nDomains);
   if ( retVal != CTA_OK ) {
     cta_jni_exception(env, "CtaOpenDaModel", "Could perform CTA_Model_AxpyState", retVal);
     return 0;
   }
   return nDomains;
}


/*
 * Class:     org_openda_costa_CtaOpenDaModel
 * Method:    ctaGetObservationSelector
 * Signature: (Lorg/openda/interfaces/IObservationDescriptions;DI)[I
 */
JNIEXPORT jintArray JNICALL Java_org_openda_costa_CtaOpenDaModel_ctaGetObservationSelector
  (JNIEnv *env, jobject jModel, jobject jObsDescr, jdouble distance, jint iDomain){

   cta_jni_setJavaEnv(env);
   CTA_Model ctaModel       = cta_jni_getCtaHandle(env, jModel);
   CTA_ObsDescr ctaObsDescr = cta_jni_getCtaHandle(env, jObsDescr);
   CTA_Time time;


   CTA_Vector ctaSelVec = CTA_NULL;
   int retVal=CTA_Model_GetObsSelector (ctaModel, ctaObsDescr, distance, iDomain, &ctaSelVec);
   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaModel", "ObservationSelector returned an error ", retVal);
      return NULL;           
   }
   int nSel;
   retVal=CTA_Vector_GetSize(ctaSelVec, &nSel);
   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaModel", "ObservationSelector cannot get length of returned selction. Error ", retVal);
      return NULL;           
   }
   int *iVals = new int[nSel];
   retVal = CTA_Vector_GetVals(ctaSelVec,iVals,nSel,CTA_INTEGER);
   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaModel", "ObservationSelector cannot get values from selection vector. Error ", retVal);
      delete [] iVals;
      return NULL;           
   }
   retVal= CTA_Vector_Free(&ctaSelVec);
   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaModel", "ObservationSelector cannot get values from selection vector. Error ", retVal);
      delete [] iVals;
      return NULL;           
   }

   /* Copy values to java integer array */
   jintArray jValues = env->NewIntArray(nSel);
   jint *jIVals= new jint[nSel];
   for (int i=0;i<nSel;i++){jIVals[i]=iVals[i];}

   env->SetIntArrayRegion(jValues, 0, nSel, jIVals);
   delete [] jIVals;
   delete [] iVals;
   return jValues;
}








