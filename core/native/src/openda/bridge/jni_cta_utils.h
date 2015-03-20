/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/openda/bridge/jni_cta_utils.h $
$Revision: 1553 $, $Date: 2010-05-07 14:55:23 +0200 (Fri, 07 May 2010) $

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


#include "jni.h"
#include "cta.h"
void cta_jni_setJavaEnv(JNIEnv *env);
#ifdef __cplusplus
void cta_jni_exception(JNIEnv *env, const char *name, const char *msg);
#endif
void cta_jni_exception(JNIEnv *env, const char *name, const char *msg, int retVal);
void cta_jni_free(JNIEnv * env, jobject obj_this);
CTA_Handle cta_jni_getCtaHandle(JNIEnv * env, jobject obj_this);

void cta_jni_setCtaHandle(JNIEnv * env, jobject obj_this, CTA_Handle ctaHandle);
void cta_jni_ExternalMessageWriter(char *className, char *method, char *message, char type);
void cta_jni_ExternalMessageWriterSetID(JNIEnv *env, jclass classID, jmethodID methodID);

#ifdef __cplusplus
extern "C" {
#endif
int cta_jni_JavaStringVecToNativeVec(JNIEnv *env,  jobjectArray jArray, CTA_Vector cVec);

#ifdef __cplusplus
}
#endif
