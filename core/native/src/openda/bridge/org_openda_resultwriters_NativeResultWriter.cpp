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
#include "org_openda_resultwriters_NativeResultWriter.h"
#include "jni_cta_utils.h"
#include "cta_resultwriter.h"

char *copyJavaStringToNormalString(JNIEnv *env, jstring javaString){

   if (javaString==NULL) return NULL;
   const char *sJava=env->GetStringUTFChars(javaString,     0);
   size_t n=strlen(sJava)+1;
   char *sString = (char*) malloc(n*sizeof(char));
   for (size_t i=0;i<n;i++){
       sString[i]=sJava[i];
   }
   env->ReleaseStringUTFChars(javaString, sJava);
   return sString;

}



/*
 * Class:     org_openda_resultwriters_NativeResultWriter
 * Method:    putMessage
 * Signature: (ILjava/lang/String;Ljava/lang/String;Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_org_openda_resultwriters_NativeResultWriter_putMessage
  (JNIEnv *env, jobject, jint iDWriter, jstring config, jstring workingDir, jstring message){
   cta_jni_setJavaEnv(env);

   char *sConfig      = copyJavaStringToNormalString(env, config);
   char *sWorkingDir  = copyJavaStringToNormalString(env, workingDir);
   char *sMessage     = copyJavaStringToNormalString(env, message);

   // To do add method
   CTA_Resultwriter_putmessage(iDWriter, sConfig, sWorkingDir, sMessage); 

   if (sConfig)     free(sConfig);
   if (sWorkingDir) free(sWorkingDir);
   if (sMessage)    free(sMessage);
}

/*
 * Class:     org_openda_resultwriters_NativeResultWriter
 * Method:    putValue
 * Signature: (ILjava/lang/String;Ljava/lang/String;Ljava/lang/String;IILjava/lang/String;I)V
 */
JNIEXPORT void JNICALL Java_org_openda_resultwriters_NativeResultWriter_putValue
  (JNIEnv *env, jobject, jint iDWriter, jstring config, jstring workingDir, jstring id, jint handle, jint outputLevel, jstring context, jint iteration){
   cta_jni_setJavaEnv(env);

   char *sConfig      = copyJavaStringToNormalString(env, config);
   char *sWorkingDir  = copyJavaStringToNormalString(env, workingDir);
   char *sId          = copyJavaStringToNormalString(env, id);
   char *sContext     = copyJavaStringToNormalString(env, context);

   // To do add method
    CTA_Resultwriter_putvalue(iDWriter, sConfig, sWorkingDir, sId, handle, outputLevel, sContext, iteration);
   if (sConfig)     free(sConfig);
   if (sWorkingDir) free(sWorkingDir);
   if (sId)         free(sId);
   if (sContext)    free(sContext);
}
/*
 * Class:     org_openda_resultwriters_NativeResultWriter
 * Method:    putIterationReport
 * Signature: (ILjava/lang/String;Ljava/lang/String;IDI)V
 */
JNIEXPORT void JNICALL Java_org_openda_resultwriters_NativeResultWriter_putIterationReport
  (JNIEnv *env, jobject, jint iDWriter, jstring config, jstring workingDir, jint iteration, jdouble cost, jint handle){
   cta_jni_setJavaEnv(env);

   char *sConfig      = copyJavaStringToNormalString(env, config);
   char *sWorkingDir  = copyJavaStringToNormalString(env, workingDir);

   // To do add method
    CTA_Resultwriter_putiterationreport(iDWriter, sConfig, sWorkingDir, iteration, cost, handle);

   if (sConfig)     free(sConfig);
   if (sWorkingDir) free(sWorkingDir);
}

/*
 * Class:     org_openda_resultwriters_NativeResultWriter
 * Method:    free
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_org_openda_resultwriters_NativeResultWriter_free
  (JNIEnv *, jobject, jint iDWriter){

   // To do add method
    CTA_Resultwriter_free(iDWriter);
}







