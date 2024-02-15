/*
$URL: https://repos.deltares.nl/repos/openda/openda_1/public/trunk/core/native/src/openda/bridge/org_openda_costa_CtaObject.cpp $
$Revision: 2211 $, $Date: 2011-04-01 13:48:25 +0200 (Fri, 01 Apr 2011) $

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
#include "org_openda_costa_CtaInitialize.h"
#include "jni_cta_utils.h"
#include "cta.h"
#include "jni_cta_CtaObsdescr_NativeToJava.h"


/*
 * Class:     org_openda_costa_CtaInitialize
 * Method:    ctaInit
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaInitialize_ctaInit
  (JNIEnv *env, jclass){

   CTA_Func hMessageWriter;

	// Initialize the costa core environment
   CTA_Core_Initialise();
   
   // Create a COSTA function handle for the JNI-message writer
   CTA_Func_Create("CTA_cta_jni_ExternalMessageWriter", (CTA_Function*) &cta_jni_ExternalMessageWriter, 
                   CTA_NULL, &hMessageWriter);

   // Get the Java class of the message writer
   jclass clsMessageWriter = env->FindClass("org/openda/costa/CtaMessageWriter");
   

   if (!clsMessageWriter){
      CTA_Message_Write("org_costa_CtaObject", "ctaInit", "Cannot find Java class org/openda/costa/CtaMessageWriter", 'F');
   }
   // Get the Method ID of the Java write-function
   jmethodID javaIDWriter = env->GetStaticMethodID(clsMessageWriter, "Write", "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V");
   if (!javaIDWriter){
      CTA_Message_Write("org_costa_CtaObject", "ctaInit", "Member method Write in Java class org/openda/costa/CtaMessageWriter", 'F');
   }

   // Save ID's of the message writer
   cta_jni_ExternalMessageWriterSetID(env, clsMessageWriter, javaIDWriter);

   // Set the JNI-writer as message writer in COSTA 
   CTA_Message_SetExternalWriter(hMessageWriter);

   // Use the JNI-writer for informing that initialization is succesfull
   CTA_Message_Write("org_costa_CtaObject", "ctaInit", "Java message writer is initialized", 'I');

   // Administrate the CtaObsdescr_NativeToJava.c class
   CTA_ObsDescrClass hobsdescrcl;
   CTA_ObsDescr_nativeToJava_initialise(&hobsdescrcl);

}

/*
 * Class:     org_openda_costa_CtaInitialize
 * Method:    setRandomSeed
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaInitialize_setRandomSeed
  (JNIEnv *, jclass, jint seed){
  long lseed=seed;

 CTA_rand_seed(lseed); 

}

