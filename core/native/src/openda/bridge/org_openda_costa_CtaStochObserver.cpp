/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/openda/bridge/org_openda_costa_CtaStochObserver.cpp $
$Revision: 3443 $, $Date: 2012-08-28 15:33:36 +0200 (Tue, 28 Aug 2012) $

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

#include <string.h>
#include "org_openda_costa_CtaStochObserver.h"
#include "cta_sobs.h"
#include "jni_cta_utils.h"
#include "cta_errors.h"
#include "cta_defaults.h"


/*
 * Class:     org_costa_CtaStochObserver
 * Method:    CtaStochObserver_createNative
 * Signature: (Ljava/lang/String;Ljava/lang/String)I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaStochObserver_ctaCreateNative
(JNIEnv *env, jobject, jstring jTypeObserver, jstring jFilename){

   cta_jni_setJavaEnv(env);
   printf("Debug: start: Java_org_openda_costa_CtaStochObserver_ctaCreateNative\n");
   CTA_StochObs hstochobs;
   CTA_String Filename,sTypeObserver;
   CTA_SObsClass hsobsclass;
   CTA_Tree htree;
   int retVal=0;
   
   CTA_String_Create(&Filename);
   const char *str  = env->GetStringUTFChars(jFilename, 0);
   CTA_String_Set(Filename, str);
   printf("Debug Observation filename=%s\n",str);
   env->ReleaseStringUTFChars(jFilename, str);

   const char *typeObserver = env->GetStringUTFChars(jTypeObserver, 0);
   if (strcmp(typeObserver,"SQLITE")==0){
      retVal = CTA_SObs_Create (CTA_DEFAULT_SOBS, Filename, &hstochobs);
   }
   else if (strcmp(typeObserver,"MAORI")==0) {
      printf("DEBUG vanuit bridge. Create de MAORI filename=%d\n",Filename);
      retVal = CTA_SObs_Create (CTA_MAORI_SOBS, Filename, &hstochobs);
   }
   else if (strcmp(typeObserver,"USER")==0) {
      printf("DEBUG vanuit bridge. Create de USER filename=%d\n",Filename);
      retVal = CTA_SObs_Create (CTA_USER_SOBS, Filename, &hstochobs);
   }
   else {
      char message[1000];

      CTA_String_Create(&sTypeObserver);
      CTA_String_Set(sTypeObserver,typeObserver);
      retVal=CTA_XML_Read(sTypeObserver,&htree);
      CTA_String_Free(&sTypeObserver);
      if (retVal != CTA_OK) {
         sprintf(message, "Error reading xml-file %s with Stochastic Observer class. Error code %d", typeObserver, retVal);
         cta_jni_exception(env, "CtaStochObserver", message, retVal);
         hstochobs = CTA_NULL;
      } else {
         retVal=CTA_Tree_GetHandleStr(htree, (char *) "COSTA/sobsclass", &hsobsclass);
         if (retVal != CTA_OK) {
            sprintf(message, "Error get class from tree (created from xml-file %s. Error code %d\n" ,typeObserver, retVal);
            cta_jni_exception(env, "CtaStochObserver", message, retVal);
            hstochobs = CTA_NULL;
        } else {
           retVal = CTA_SObs_Create (hsobsclass, Filename, &hstochobs);            
           if (retVal != CTA_OK) {
              sprintf(message, "Cannot create stochastic Observer Error code %d\n" ,retVal);
              cta_jni_exception(env, "CtaStochObserver", message, retVal);
              hstochobs = CTA_NULL;
           }
        }
      }
      CTA_Tree_Free(&htree);
   }
/*   else {
      char message[256];
      sprintf(message, "Unknown Observer type: %s", typeObserver);
      retVal    = CTA_NOT_YET_SUPPORTED;
      hstochobs = CTA_NULL;
      cta_jni_exception(env, "CtaStochObserver", message, retVal);
   } */

   printf("Debug Observation handle is =%d\n", hstochobs);
   env->ReleaseStringUTFChars(jTypeObserver, typeObserver);
   if (retVal!=CTA_OK){
      cta_jni_exception(env, "CtaStochObserver", "Could create new stoch observer", retVal);
		return 0;
   }

   CTA_String_Free(&Filename);
   printf("Debug: end: Java_org_openda_costa_CtaStochObserver_ctaCreateNative\n");
   return hstochobs;

}


/*
 * Class:     org_costa_CtaStochObserver
 * Method:    getCount
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaStochObserver_getCount
(JNIEnv *env, jobject jStochObs){

   cta_jni_setJavaEnv(env);
   
   CTA_StochObs ctaStochObs = cta_jni_getCtaHandle(env, jStochObs);
   int nmeasr;
   int retVal = CTA_SObs_Count(ctaStochObs, &nmeasr);
   if (retVal!=CTA_OK){
      cta_jni_exception(env, "CtaStochObserver", "Could not get Count", retVal);
		return 0;
   }
   return nmeasr;
}

/*
 * Class:     org_costa_CtaStochObserver
 * Method:    ctaStochObserverGetExpectations
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaStochObserver_ctaGetExpectations
(JNIEnv *env, jobject jStochObs, jint ctaVector){

   cta_jni_setJavaEnv(env);

   CTA_StochObs ctaStochObs = cta_jni_getCtaHandle(env, jStochObs);
   int retVal = CTA_SObs_GetExpectation(ctaStochObs, ctaVector);
   if (retVal!=CTA_OK){
      cta_jni_exception(env, "CtaStochObserver", "Could not get Expectation", retVal);
		return;
   }
 }

/*
 * Class:     org_costa_CtaStochObserver
 * Method:    CtagetObservationDescriptions
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaStochObserver_ctaGetObservationDescriptions
(JNIEnv *env, jobject jStochObs){

   cta_jni_setJavaEnv(env);
   
   CTA_StochObs ctaStochObs = cta_jni_getCtaHandle(env, jStochObs);
   CTA_ObsDescr hobsdescr;
   int retVal = CTA_SObs_GetDescription(ctaStochObs, &hobsdescr);
   if (retVal!=CTA_OK){
      cta_jni_exception(env, "CtaStochObserver", "Could not get Description", retVal);
		return 0;
   }         
   return hobsdescr;
}

/*
 * Class:     org_costa_CtaStochObserver
 * Method:    ctaStochObserverGetRealizations
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaStochObserver_ctaGetRealizations
(JNIEnv *env, jobject jStochObs, jint ctaVector){

   cta_jni_setJavaEnv(env);
   
   CTA_StochObs ctaStochObs = cta_jni_getCtaHandle(env, jStochObs);
   int retVal = CTA_SObs_GetRealisation(ctaStochObs, ctaVector);
   if (retVal!=CTA_OK){
      cta_jni_exception(env, "CtaStochObserver", "Could not get Realizations", retVal);
		return;
   }
 }

/*
 * Class:     org_costa_CtaStochObserver
 * Method:    ctaStochObserverGetValues
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaStochObserver_ctaGetValues
(JNIEnv *env, jobject jStochObs, jint ctaVector){

   cta_jni_setJavaEnv(env);

   CTA_StochObs ctaStochObs = cta_jni_getCtaHandle(env, jStochObs);
   int retVal = CTA_SObs_GetVal(ctaStochObs, ctaVector);
   if (retVal!=CTA_OK){
      cta_jni_exception(env, "CtaStochObserver", "Could not get Values", retVal);
		return;
   }
 }

/*
 * Class:     org_costa_CtaStochObserver
 * Method:    ctaStochObserverGetVariances
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaStochObserver_ctaGetVariances
(JNIEnv *env, jobject jStochObs, jint ctaVector){

   cta_jni_setJavaEnv(env);
   
   CTA_StochObs ctaStochObs = cta_jni_getCtaHandle(env, jStochObs);
   int retVal = CTA_SObs_GetVar(ctaStochObs, ctaVector);
   if (retVal!=CTA_OK){
      cta_jni_exception(env, "CtaStochObserver", "Could not get Variance", retVal);
		return;
   }
 }

/*
 * Class:     org_costa_CtaStochObserver
 * Method:    ctaGetStandardDeviation
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaStochObserver_ctaGetStandardDeviation
(JNIEnv *env, jobject jStochObs, jint ctaVector){
   
   cta_jni_setJavaEnv(env);

   CTA_StochObs ctaStochObs = cta_jni_getCtaHandle(env, jStochObs);
   int retVal = CTA_SObs_GetStd(ctaStochObs, ctaVector);
   if (retVal!=CTA_OK){
       cta_jni_exception(env, "CtaStochObserver", "Could not get Standard deviation", retVal);
     return;
   }
   return;
}







/*
 * Class:     org_costa_CtaStochObserver
 * Method:    ctaStochObserverCreateSelection
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaStochObserver_ctaCreateSelection
(JNIEnv *env, jobject jStochObs, jstring jSelect){

   cta_jni_setJavaEnv(env);
   
   CTA_String select;
   /* create COSTA string with selection */
   CTA_String_Create(&select);
   const char *sel  = env->GetStringUTFChars(jSelect, 0);
   CTA_String_Set(select,sel);
   env->ReleaseStringUTFChars(jSelect, sel);

   CTA_StochObs hsobsout;
   CTA_StochObs ctaStochObs = cta_jni_getCtaHandle(env, jStochObs);
   int retVal = CTA_SObs_CreateSel (ctaStochObs, select, &hsobsout);
   if (retVal!=CTA_OK){
      cta_jni_exception(env, "CtaStochObserver", "Could not create selection", retVal);
		return 0;
   }
   CTA_String_Free(&select); 

   return hsobsout;
}

/*
 * Class:     org_costa_CtaStochObserver
 * Method:    ctaStochObserverCreateTimeSelection
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaStochObserver_ctaCreateTimeSelection
(JNIEnv *env, jobject jStochObs, jint timesel){

   cta_jni_setJavaEnv(env);

   CTA_StochObs hsobsout;
   CTA_StochObs ctaStochObs = cta_jni_getCtaHandle(env, jStochObs);
   int retVal = CTA_SObs_CreateTimSel (ctaStochObs, timesel, &hsobsout);
   if (retVal!=CTA_OK){
      cta_jni_exception(env, "CtaStochObserver", "Could not create selection", retVal);
		return 0;
   }
   return hsobsout;

}

/*
 * Class:     org_costa_CtaStochObserver
 * Method:    ctaStochObserverEvaluatePDF
 * Signature: (II)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaStochObserver_ctaEvaluatePDF
(JNIEnv *env, jobject jStochObs, jint ctaVectorX, jint ctaVectorY){

   cta_jni_setJavaEnv(env);

   CTA_StochObs ctaStochObs = cta_jni_getCtaHandle(env, jStochObs);
   int retVal = CTA_SObs_EvalPDF(ctaStochObs, ctaVectorX, ctaVectorY);
   if (retVal!=CTA_OK){
      cta_jni_exception(env, "CtaStochObserver", "Could not get evaluate PDF", retVal);
		return;
   }
 }


