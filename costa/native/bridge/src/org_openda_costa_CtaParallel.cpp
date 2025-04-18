/*
$URL: https://repos.deltares.nl/repos/openda/openda_1/public/trunk/core/native/src/openda/bridge/org_costa_CtaOpenDaModel.cpp $
$Revision: 1429 $, $Date: 2010-04-01 17:14:23 +0200 (Thu, 01 Apr 2010) $

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

#include "org_openda_costa_CtaParallel.h"
#include "cta.h"
#include "cta_modbuild_par.h"
#include "jni_cta_utils.h"

#define IDEBUG (0)

#ifdef __cplusplus
extern "C" {
#endif
   int CTA_Par_CreateGroups(CTA_Tree,int);
#ifdef __cplusplus
}
#endif


/*
 * Class:     org_openda_costa_CtaParallel
 * Method:    nativeInit
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaParallel_nativeInit
  (JNIEnv *env, jclass myClass, jstring jConfigName){

   CTA_String hfile;
   CTA_Tree htree;
   int retVal;

   /* Create C string of jave string of config file name */
   const char *configName = env->GetStringUTFChars(jConfigName, 0);

   /* Parse the input file */
   CTA_String_Create(&hfile);
   CTA_String_Set(hfile, configName);
   env->ReleaseStringUTFChars(jConfigName, configName);
   retVal=CTA_XML_Read(hfile,&htree);
   if (retVal!=CTA_OK) {
      cta_jni_exception(env, "CtaParallel",
                        "Could parse configuration file", retVal);
   }
   CTA_String_Free(&hfile);

   /* initialize the parallel environment */
   retVal=CTA_Par_CreateGroups(htree,CTA_TRUE);
   if (retVal!=CTA_OK) {
      cta_jni_exception(env, "CtaParallel",
                        "Could not initialize parallel environment", retVal);
   }
}

/*
 * Class:     org_openda_costa_CtaParallel
 * Method:    finalizeParallelEnvironment
 * Signature: ()V
 */

JNIEXPORT void JNICALL Java_org_openda_costa_CtaParallel_finalizeParallelEnvironment
  (JNIEnv *, jclass){
   CTA_Modbuild_par_Finalize();
}

