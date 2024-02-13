/*
$URL: https://repos.deltares.nl/repos/openda/openda_1/public/trunk/core/native/src/openda/bridge/org_openda_costa_CtaVector.cpp $
$Revision: 2738 $, $Date: 2011-09-05 10:48:32 +0200 (Mon, 05 Sep 2011) $

OpenDA interface for COSTA.
Copyright (C) 2013  Nils van Velzen

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
#include "org_openda_resultwriters_NetcdfResultWriterNative.h"
#include "jni_cta_utils.h"
#include "cta_resultwriter.h"


CTA_String copyJavaStringToCostaString(JNIEnv *env, jstring javaString){

   int ierr;
   CTA_String hString=CTA_NULL;

   if (javaString==NULL) return hString;
   const char *sJava=env->GetStringUTFChars(javaString,     0);
   size_t n=strlen(sJava)+1;
   char *sString = (char*) malloc(n*sizeof(char));
   for (size_t i=0;i<n;i++){
       sString[i]=sJava[i];
   }
   env->ReleaseStringUTFChars(javaString, sJava);
   
   ierr=CTA_String_Create(&hString);
   if (ierr!=CTA_OK){
      cta_jni_exception(env, "copyJavaStringToCostaString", "Cannot create COSTA string.", ierr);
   }
   ierr=CTA_String_Set(hString, sString);
   if (ierr!=CTA_OK){
      cta_jni_exception(env, "copyJavaStringToCostaString", "Cannot set value of COSTA string.", ierr);
   }
   
   free(sString);

   return hString;
}


/*
 * Class:     org_openda_resultwriters_NetcdfResultWriterNative
 * Method:    ctaNetcdfInit
 * Signature: (Ljava/lang/String;Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_org_openda_resultwriters_NetcdfResultWriterNative_ctaNetcdfInit
  (JNIEnv *env , jobject , jstring netcdfname, jstring action){

   cta_jni_setJavaEnv(env);

   int ierr;
   CTA_String hFilename    = copyJavaStringToCostaString(env, netcdfname);
   CTA_String hAction      = copyJavaStringToCostaString(env, action);
   CTA_File hFile;

   ierr=CTA_File_Create(&hFile);
   if (ierr!=CTA_OK){
      cta_jni_exception(env, "Java_org_openda_resultwriters_NetcdfResultWriterNative_ctaNetcdfInit",
                             "Cannot create COSTA file.", ierr);
   }

   ierr=CTA_File_Open(hFile,hFilename,hAction);
   if (ierr!=CTA_OK){
      cta_jni_exception(env, "Java_org_openda_resultwriters_NetcdfResultWriterNative_ctaNetcdfInit",
                             "Cannot open Netcdf file.", ierr);
   }

   CTA_String_Free(&hFilename);
   CTA_String_Free(&hAction);
   return hFile;

}

/*
 * Class:     org_openda_resultwriters_NetcdfResultWriterNative
 * Method:    ctaNetcdfClose
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_org_openda_resultwriters_NetcdfResultWriterNative_ctaNetcdfClose
  (JNIEnv *env, jobject, jint hFile){

   cta_jni_setJavaEnv(env);

   int ierr;
   int iHFile = hFile;
   ierr=CTA_File_Free(&iHFile);
   if (ierr!=CTA_OK){
      cta_jni_exception(env, "Java_org_openda_resultwriters_NetcdfResultWriterNative_ctaNetcdfInit",
                             "Cannot close Netcdf file.", ierr);
   }
   return ierr;
}


