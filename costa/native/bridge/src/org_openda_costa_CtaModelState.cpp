/*
$URL:  $
$Revision: $, $Date: $

Copyright (c) 2012 OpenDA Association
All rights reserved.

This file is part of OpenDA.

OpenDA is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of
the License, or (at your option) any later version.

OpenDA is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "org_openda_costa_CtaModelState.h"
#include "cta_model.h"
#include "jni_cta_utils.h"
#include "cta_errors.h"
#include "cta_defaults.h"

#define IDEBUG (0)
/*
 * Class:     org_openda_costa_CtaModelState
 * Method:    nativeSavePersistentState
 * Signature: (Ljava/lang/String;Ljava/lang/String;I)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaModelState_nativeSavePersistentState
  (JNIEnv *env, jobject jPersisState, jstring jFileName, jstring jID, jint ctaModel){

   cta_jni_setJavaEnv(env);
   if (IDEBUG>0) {
     printf("Java_org_openda_costa_CtaModelState_nativeSavePersistentState: Start of function:\n");
   }

   const char *fileName = env->GetStringUTFChars(jFileName, 0);
   const char *ID       = env->GetStringUTFChars(jID, 0);
   CTA_String cFileName;
   CTA_String cID;
   CTA_String_Create(&cFileName);
   CTA_String_Create(&cID);
   CTA_String_Set(cFileName, fileName);
   CTA_String_Set(cID,ID);
   env->ReleaseStringUTFChars(jFileName, fileName);
   env->ReleaseStringUTFChars(jID, ID);

   int retVal = CTA_Model_SavePersistentState(ctaModel, cFileName, cID);
   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaModelState", "Error calling SavePersistentState ", retVal);
      return;
   }
   if (IDEBUG>0) {
     printf("Java_org_openda_costa_CtaModelState_nativeSavePersistentState: End of function:\n");
   }
}
