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

#include "org_openda_costa_CtaUtils.h"
#include "jni_cta_utils.h"
#include "cta_handles.h"

/*
 * Class:     org_openda_costa_CtaUtils
 * Method:    print_memory
 * Signature: (Ljava/lang/String;I)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaUtils_print_1memory
  (JNIEnv *env, jclass obj_this, jstring where, jint level){

   cta_jni_setJavaEnv(env);
   const char *swhere  = env->GetStringUTFChars(where, 0);
   CTA_Handle_PrintInfo(swhere);
   env->ReleaseStringUTFChars(where, swhere);
}
