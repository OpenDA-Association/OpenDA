/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/openda/bridge/org_openda_costa_CtaTime.cpp $
$Revision: 2872 $, $Date: 2011-11-07 10:48:40 +0100 (Mon, 07 Nov 2011) $

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


#include "org_openda_costa_CtaTime.h"
#include "cta_defaults.h"
#include "jni_cta_utils.h"
#include "cta_time.h"
#include "cta_errors.h"


/*
 * Class:     org_costa_CtaTime
 * Method:    create
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaTime_create
  (JNIEnv * env, jobject obj_this)
{

   cta_jni_setJavaEnv(env);
   
   int handle = CTA_NULL;

   int retVal = CTA_Time_Create((CTA_Time *) &handle);
   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaTime_Create", "", retVal);
      return 0;
   }
   jint handle2 = (jint) handle;
   return handle2;
}

/*
 * Class:     org_costa_CtaTime
 * Method:    getMJD
 * Signature: ()D
 */
JNIEXPORT jdouble JNICALL Java_org_openda_costa_CtaTime_getMJD
  (JNIEnv * env, jobject obj_this)
{
   cta_jni_setJavaEnv(env);

   CTA_Handle ctaHandle = cta_jni_getCtaHandle(env, obj_this);
   double t1,t2;
   int retVal=CTA_Time_GetSpan(ctaHandle, &t1, &t2);
   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaTime", "Could not getMJD ", retVal);
      return 0.0;
   }
   if (t1!=t2)  {
      cta_jni_exception(env, "CtaTime", "Could not getMJD this is a time span ",0);
      return 0.0;
   }
   return (jdouble) t2;
}

/*
 * Class:     org_costa_CtaTime
 * Method:    getUserTime
 * Signature: ()D
 */
JNIEXPORT jdouble JNICALL Java_org_openda_costa_CtaTime_getUserTime
  (JNIEnv * env, jobject obj_this)
{
   cta_jni_setJavaEnv(env);
	return 0;
}

/*
 * Class:     org_costa_CtaTime
 * Method:    setMJD
 * Signature: (D)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaTime_setMJD
  (JNIEnv *env, jobject obj_this, jdouble t)
{
   cta_jni_setJavaEnv(env);

   CTA_Handle ctaHandle = cta_jni_getCtaHandle(env, obj_this);
   int retVal=CTA_Time_SetSpan(ctaHandle, t, t);
   if ( retVal != CTA_OK ) {
		cta_jni_exception(env, "CtaTime", "Could not setMJD ", retVal);
		return;
	}
  //CTA_Time_Export(ctaHandle,CTA_FILE_STDOUT);
}

/*
 * Class:     org_costa_CtaTime
 * Method:    setUserTime
 * Signature: (D)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaTime_setUserTime
  (JNIEnv *env, jobject, jdouble)
{
   cta_jni_setJavaEnv(env);
}

/*
 * Class:     org_costa_CtaTime
 * Method:    getStep
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL Java_org_openda_costa_CtaTime_getStep
  (JNIEnv * env, jobject obj_this)
{
   cta_jni_setJavaEnv(env);
	return 0;
}

/*
 * Class:     org_costa_CtaTime
 * Method:    getBeginMJD
 * Signature: ()D
 */
JNIEXPORT jdouble JNICALL Java_org_openda_costa_CtaTime_getBeginMJD
  (JNIEnv * env, jobject obj_this)
{
   cta_jni_setJavaEnv(env);

   double t1,t2;
   CTA_Handle ctaHandle = cta_jni_getCtaHandle(env, obj_this);
   int retVal=CTA_Time_GetSpan(ctaHandle, &t1, &t2);
   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaTime", "Could not getBeginMJD ", retVal);
      return 0.0;
   }
   return (jdouble) t1;
}

/*
 * Class:     org_costa_CtaTime
 * Method:    getEndMJD
 * Signature: ()D
 */
JNIEXPORT jdouble JNICALL Java_org_openda_costa_CtaTime_getEndMJD
  (JNIEnv * env, jobject obj_this)
{
   cta_jni_setJavaEnv(env);

   double t1,t2;
   CTA_Handle ctaHandle = cta_jni_getCtaHandle(env, obj_this);
   int retVal=CTA_Time_GetSpan(ctaHandle, &t1, &t2);
   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaTime", "Could not getEndMJD ", retVal);
      return 0.0;
   }
   return (jdouble) t2;
}

/*
 * Class:     org_costa_CtaTime
 * Method:    getStepMJD
 * Signature: ()D
 */
JNIEXPORT jdouble JNICALL Java_org_openda_costa_CtaTime_getStepMJD
  (JNIEnv * env, jobject obj_this)
{
   cta_jni_setJavaEnv(env);

   double t1;
   CTA_Handle ctaHandle = cta_jni_getCtaHandle(env, obj_this);
   int retVal=CTA_Time_GetStep(ctaHandle, &t1);
   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaTime", "Could not getStepMJD ", retVal);
      return 0.0;
   }
   return (jdouble) t1;
}

/*
 * Class:     org_costa_CtaTime
 * Method:    getBeginUserTime
 * Signature: ()D
 */
JNIEXPORT jdouble JNICALL Java_org_openda_costa_CtaTime_getBeginUserTime
  (JNIEnv * env, jobject obj_this)
{
   cta_jni_setJavaEnv(env);
	return 0;
}

/*
 * Class:     org_costa_CtaTime
 * Method:    getEndUserTime
 * Signature: ()D
 */
JNIEXPORT jdouble JNICALL Java_org_openda_costa_CtaTime_getEndUserTime
  (JNIEnv * env, jobject obj_this)
{
   cta_jni_setJavaEnv(env);
	return 0;
}

/*
 * Class:     org_costa_CtaTime
 * Method:    getUserStep
 * Signature: ()D
 */
JNIEXPORT jdouble JNICALL Java_org_openda_costa_CtaTime_getUserStep
  (JNIEnv * env, jobject obj_this)
{
   cta_jni_setJavaEnv(env);
	return 0;
}

/*
 * Class:     org_costa_CtaTime
 * Method:    setSpanMJD
 * Signature: (DDD)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaTime_setSpanMJD
  (JNIEnv *env, jobject obj_this, jdouble tstart, jdouble tend, jdouble tstep)
{
   cta_jni_setJavaEnv(env);

   CTA_Handle ctaHandle = cta_jni_getCtaHandle(env, obj_this);
   int retVal=CTA_Time_SetSpan(ctaHandle, tstart, tend);
   if ( retVal != CTA_OK ) {
		cta_jni_exception(env, "CtaTime", "Could not setSpanMJD ", retVal);
		return;
	}

   retVal=CTA_Time_SetStep(ctaHandle, tstep);
   if ( retVal != CTA_OK ) {
		cta_jni_exception(env, "CtaTime", "Could not setSpanMJD ", retVal);
		return;
	}
}

/*
 * Class:     org_costa_CtaTime
 * Method:    setUserSpan
 * Signature: (DDD)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaTime_setUserSpan
  (JNIEnv *env, jobject, jdouble, jdouble, jdouble)
{
   cta_jni_setJavaEnv(env);

}

/*
 * Class:     org_costa_CtaTime
 * Method:    getStepCount
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL Java_org_openda_costa_CtaTime_getStepCount
  (JNIEnv * env, jobject obj_this)
{
   cta_jni_setJavaEnv(env);
	return 0;
}

/*
 * Class:     org_costa_CtaTime
 * Method:    clone
 * Signature: ()Lorg/openda/interfaces/ITime;
 */
JNIEXPORT jobject JNICALL Java_org_openda_costa_CtaTime_clone
  (JNIEnv * env, jobject obj_this)
{
   cta_jni_setJavaEnv(env);
	return 0;
}

