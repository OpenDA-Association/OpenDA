/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/openda/bridge/org_openda_costa_CtaObject.cpp $
$Revision: 4061 $, $Date: 2013-07-15 11:45:09 +0200 (Mon, 15 Jul 2013) $

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


#include "org_openda_costa_CtaObject.h"
#include "jni_cta_utils.h"
#include "cta.h"
#include "jni_cta_CtaObsdescr_NativeToJava.h"

/*
 * Class:     org_costa_CtaObject
 * Method:    ctaFree
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaObject_ctaFree
  (JNIEnv * env, jobject obj_this)
{
	cta_jni_free(env, obj_this);
}

