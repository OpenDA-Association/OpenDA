/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/openda/bridge/org_openda_costa_CtaVector.cpp $
$Revision: 4445 $, $Date: 2014-06-03 08:56:12 +0200 (Tue, 03 Jun 2014) $

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


#include "org_openda_costa_CtaVector.h"
#include "cta_vector.h"
#include "jni_cta_utils.h"
#include "cta_errors.h"
#include "cta_defaults.h"

/*
 * Class:     org_costa_CtaVector
 * Method:    setConstant
 * Signature: (D)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaVector_setConstant
  (JNIEnv * env, jobject obj_this, jdouble val){

   cta_jni_setJavaEnv(env);

   CTA_Handle ctaHandle = cta_jni_getCtaHandle(env, obj_this);

   CTA_Datatype datatype;
   CTA_Handle_GetDatatype (ctaHandle, &datatype);

   int retVal;
   if (datatype==CTA_VECTOR){
      retVal = CTA_Vector_SetConstant(ctaHandle, &val, CTA_DOUBLE);
   }
   else {
      retVal = CTA_TreeVector_SetConstant(ctaHandle, &val, CTA_DOUBLE);
   }

	if ( retVal != CTA_OK ) {
		cta_jni_exception(env, "CtaVector", "Could not set constant ", retVal);
		return;
	}
}

/*
 * Class:     org_costa_CtaVector
 * Method:    scale
 * Signature: (D)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaVector_scale
  (JNIEnv * env, jobject obj_this, jdouble val){

   cta_jni_setJavaEnv(env);

   CTA_Handle ctaHandle = cta_jni_getCtaHandle(env, obj_this);

   CTA_Datatype datatype;
   CTA_Handle_GetDatatype (ctaHandle, &datatype);

   int retVal;
   if (datatype==CTA_VECTOR){
      retVal = CTA_Vector_Scal(ctaHandle, val);
   }
   else {
      retVal = CTA_TreeVector_Scal(ctaHandle, val);
   }

	if ( retVal != CTA_OK ) {
		cta_jni_exception(env, "CtaVector", "Could not scale ", retVal);
		return;
	}
  }



/*
 * Class:     org_costa_CtaVector
 * Method:    ctaPrintHandles
 * Signature: (Z)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaVector_ctaPrintHandles
(JNIEnv *env, jobject jModel, jstring jlocation){

   cta_jni_setJavaEnv(env);

   const char *location = env->GetStringUTFChars(jlocation, 0);

   CTA_Handle_PrintInfo(location);

}

/*
 * Class:     org_costa_CtaVector
 * Method:    setValues
 * Signature: ([D)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaVector_setValues
  (JNIEnv * env, jobject obj_this, jdoubleArray jValues) {
   
    cta_jni_setJavaEnv(env);

	CTA_Handle ctaHandle = cta_jni_getCtaHandle(env, obj_this);

	int cCount = env->GetArrayLength(jValues);
	jboolean isCopy = JNI_FALSE; 
	jdouble * cValues = env->GetDoubleArrayElements(jValues, &isCopy);

    CTA_Datatype datatype;
    CTA_Handle_GetDatatype (ctaHandle, &datatype);

    int retVal;
    if (datatype==CTA_VECTOR){
	   retVal = CTA_Vector_SetVals(ctaHandle, (void *) cValues, cCount, CTA_DOUBLE);
    }
    else {
	   retVal = CTA_TreeVector_SetVals(ctaHandle, (void *) cValues, cCount, CTA_DOUBLE);
    }

     env->ReleaseDoubleArrayElements(jValues, cValues,0); 

   if ( retVal != CTA_OK ) {
		cta_jni_exception(env, "CtaVector", "Could not set values", retVal);
		return;
	}
}


/*
 * Class:     org_costa_CtaVector
 * Method:    getValues
 * Signature: ()[D
 */
JNIEXPORT jdoubleArray JNICALL Java_org_openda_costa_CtaVector_getValues
  (JNIEnv * env, jobject obj_this) {

	cta_jni_setJavaEnv(env);
   
   CTA_Handle ctaHandle = cta_jni_getCtaHandle(env, obj_this);

	int cCount = 0;

   CTA_Datatype datatype;
   CTA_Handle_GetDatatype (ctaHandle, &datatype);

   int retVal;
   if (datatype==CTA_VECTOR){
      retVal = CTA_Vector_GetSize(ctaHandle, &cCount);
   } else {
      retVal = CTA_TreeVector_GetSize(ctaHandle, &cCount);
   }

   if ( retVal != CTA_OK ) {
		cta_jni_exception(env, "CtaVector", "Could not get size", retVal);
		return NULL;
	}

   jdoubleArray jValues = env->NewDoubleArray(cCount);
   double * cValues = new double[cCount];

   if (datatype==CTA_VECTOR){
      retVal = CTA_Vector_GetVals(ctaHandle, (void *) cValues, cCount, CTA_DOUBLE);
   }
   else {
      retVal = CTA_TreeVector_GetVals(ctaHandle, (void *) cValues, cCount, CTA_DOUBLE);
   }

   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaVector", "Could not get values", retVal);
      return NULL;
   }
   else {
      env->SetDoubleArrayRegion(jValues, 0, cCount, cValues);
   }
   delete [] cValues;
   return jValues;
}

/*
 * Class:     org_costa_CtaVector
 * Method:    setValue
 * Signature: (ID)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaVector_setValue
  (JNIEnv * env, jobject obj_this, jint indx, jdouble val){

   cta_jni_setJavaEnv(env);
     
   CTA_Handle ctaHandle = cta_jni_getCtaHandle(env, obj_this);

   CTA_Datatype datatype;
   CTA_Handle_GetDatatype (ctaHandle, &datatype);

   int retVal;
   if (datatype==CTA_VECTOR){
      retVal = CTA_Vector_SetVal(ctaHandle, indx+1, &val, CTA_DOUBLE);
   }
   else {
      retVal = CTA_TreeVector_SetVal(ctaHandle, indx+1, &val, CTA_DOUBLE);
   }
   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaVector", "Could not set value ", retVal);
		return;
   }
}

/*
 * Class:     org_costa_CtaVector
 * Method:    getValue
 * Signature: (I)D
 */
JNIEXPORT jdouble JNICALL Java_org_openda_costa_CtaVector_getValue
  (JNIEnv * env, jobject obj_this, jint indx){

   cta_jni_setJavaEnv(env);
     
   CTA_Handle ctaHandle = cta_jni_getCtaHandle(env, obj_this);

   double val;
   CTA_Datatype datatype;
   CTA_Handle_GetDatatype (ctaHandle, &datatype);

   int retVal;
   if (datatype==CTA_VECTOR){
      retVal = CTA_Vector_GetVal(ctaHandle, indx+1, &val, CTA_DOUBLE);
   }
   else {
      retVal = CTA_TreeVector_GetVal(ctaHandle, indx+1, &val, CTA_DOUBLE);
   }


   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaVector", "Could not get value ", retVal);
		return 0.0;
   }
   return val;
}

/*
 * Class:     org_costa_CtaVector
 * Method:    getSize
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaVector_getSize
  (JNIEnv * env, jobject obj_this){

   cta_jni_setJavaEnv(env);

   CTA_Handle ctaHandle = cta_jni_getCtaHandle(env, obj_this);
   int n;

   CTA_Datatype datatype;
   CTA_Handle_GetDatatype (ctaHandle, &datatype);

   int retVal;
   if (datatype==CTA_VECTOR){
      retVal = CTA_Vector_GetSize(ctaHandle, &n);
   }
   else {
      retVal = CTA_TreeVector_GetSize(ctaHandle, &n);
   }


   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaVector", "Could not get value ", retVal);
		return 0;
   }
   return n;
}

/*
 * Class:     org_costa_CtaVector
 * Method:    norm2
 * Signature: ()D
 */
JNIEXPORT jdouble JNICALL Java_org_openda_costa_CtaVector_norm2
  (JNIEnv * env, jobject obj_this){

   cta_jni_setJavaEnv(env);
     
   CTA_Handle ctaHandle = cta_jni_getCtaHandle(env, obj_this);
   double nrm;

   CTA_Datatype datatype;
   CTA_Handle_GetDatatype (ctaHandle, &datatype);

   int retVal;
   if (datatype==CTA_VECTOR){
      retVal = CTA_Vector_Nrm2(ctaHandle, &nrm);
   }
   else {
      retVal = CTA_TreeVector_Nrm2(ctaHandle, &nrm);
   }


   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaVector", "Could not compute 2-norm ", retVal);
		return 0.0;
   }
   return nrm;
}

/*
 * Class:     org_costa_CtaVector
 * Method:    create
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaVector_ctaCreate
  (JNIEnv * env, jobject obj_this, jint length) {

   cta_jni_setJavaEnv(env);
   
   int handle = CTA_NULL;
	int retVal = CTA_Vector_Create(CTA_DEFAULT_VECTOR, length, CTA_DOUBLE, CTA_NULL, &handle);
	if ( retVal != CTA_OK ) {
		cta_jni_exception(env, "CtaVector", "Could not create vector", retVal);

		return 0;
	} else {
		double initialValue = 0;
		int retVal = CTA_Vector_SetConstant(handle, &initialValue, CTA_DOUBLE);
		if ( retVal != CTA_OK ) {
			cta_jni_exception(env, "CtaVector", "Could not set initial values", retVal);
			return 0;
		}
	}
	return handle;
}

/*
 * Class:     org_costa_CtaVector
 * Method:    ctaDotProduct
 * Signature: (I)D
 */
JNIEXPORT jdouble JNICALL Java_org_openda_costa_CtaVector_ctaDotProduct
  (JNIEnv * env, jobject obj_this, jint handleToOtherVector) {

   cta_jni_setJavaEnv(env);

   CTA_Handle ctaHandle = cta_jni_getCtaHandle(env, obj_this);
   double dot;

   CTA_Datatype datatype;
   CTA_Handle_GetDatatype (ctaHandle, &datatype);

   int retVal;
   if (datatype==CTA_VECTOR){
      retVal = CTA_Vector_Dot(ctaHandle, handleToOtherVector, &dot);
   }
   else {
      retVal = CTA_TreeVector_Dot(ctaHandle, handleToOtherVector, &dot);
   }
   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaVector", "Could not compute dot product", retVal);
		return 0.0;
   }
   return dot;
}

/*
 * Class:     org_costa_CtaVector
 * Method:    ctaPointwiseDivide
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaVector_ctaPointwiseDivide
  (JNIEnv *env, jobject obj_this, jint handleToOtherVector){

   cta_jni_setJavaEnv(env);

   CTA_Handle ctaHandle = cta_jni_getCtaHandle(env, obj_this);

   CTA_Datatype datatype;
   CTA_Handle_GetDatatype (ctaHandle, &datatype);

   int retVal;
   if (datatype==CTA_VECTOR){
      retVal = CTA_Vector_ElmDiv(ctaHandle, handleToOtherVector);
   }
   else {
      retVal = CTA_TreeVector_ElmDiv(ctaHandle, handleToOtherVector);
   }

   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaVector", "Could not compute pointwise division", retVal);
		return;
   }
}

/*
 * Class:     org_costa_CtaVector
 * Method:    ctaPointwiseMultiply
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaVector_ctaPointwiseMultiply
  (JNIEnv *env, jobject obj_this, jint handleToOtherVector){

   cta_jni_setJavaEnv(env);

   CTA_Handle ctaHandle = cta_jni_getCtaHandle(env, obj_this);

   CTA_Datatype datatype;
   CTA_Handle_GetDatatype (ctaHandle, &datatype);

   int retVal;
   if (datatype==CTA_VECTOR){
      retVal = CTA_Vector_ElmProd(ctaHandle, handleToOtherVector);
   }
   else {
      retVal = CTA_TreeVector_ElmProd(ctaHandle, handleToOtherVector);
   }

   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaVector", "Could not compute pointwise multiply", retVal);
		return;
   }
}

/*
 * Class:     org_costa_CtaVectorJava_org_openda_costa_CtaObject_ctaInit
 * Method:    ctaSetValues
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaVector_ctaSetValues
  (JNIEnv *env, jobject obj_this, jint handleToOtherVector){

   cta_jni_setJavaEnv(env);

   CTA_Handle ctaHandle = cta_jni_getCtaHandle(env, obj_this);

   CTA_Datatype datatype;
   CTA_Handle_GetDatatype (ctaHandle, &datatype);

   int retVal;
   if (datatype==CTA_VECTOR){
      retVal=CTA_Vector_Copy( handleToOtherVector, ctaHandle);
   }
   else {
      retVal=CTA_TreeVector_Copy( handleToOtherVector, ctaHandle);
   }

   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaVector", "Could not copy values of vectors", retVal);
		return;
   }

}

/*
 * Class:     org_costa_CtaVector
 * Method:    ctaAxpy
 * Signature: (DI)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaVector_ctaAxpy
  (JNIEnv *env, jobject obj_this, jdouble alpha, jint handleToOtherVector){

   cta_jni_setJavaEnv(env);

   CTA_Handle ctaHandle = cta_jni_getCtaHandle(env, obj_this);

   CTA_Datatype datatype;
   CTA_Handle_GetDatatype (ctaHandle, &datatype);

   int retVal;
   if (datatype==CTA_VECTOR){
      retVal=CTA_Vector_Axpy(ctaHandle, alpha, handleToOtherVector);
   }
   else {
      retVal=CTA_TreeVector_Axpy(ctaHandle, alpha, handleToOtherVector);
   }
   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaVector", "Could not compute axpy", retVal);
		return;
   }
}

/*
 * Class:     org_costa_CtaVector
 * Method:    clone
 * Signature: ()Lorg/openda/interfaces/IVector;
 */
JNIEXPORT jobject JNICALL Java_org_openda_costa_CtaVector_clone
  (JNIEnv *env, jobject obj_this){

   cta_jni_setJavaEnv(env);

   CTA_Vector ctaHandle = cta_jni_getCtaHandle(env, obj_this);
   CTA_Vector ctaDuplicate;
   int retVal=CTA_Vector_Duplicate(ctaHandle, &ctaDuplicate);
   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaVector", "Could not duplicate", retVal);
      return NULL;
   }

   /* Create a Java CtaVector */
   jclass clsVector = env->FindClass("org/openda/costa/CtaVector");
   jmethodID constructorID = env->GetMethodID (clsVector, "<init>", "()V");
   jobject jVec = env->NewObject(clsVector, constructorID);

   cta_jni_setCtaHandle(env, jVec, ctaDuplicate);

   return jVec;
  }


/*
 * Class:     org_costa_CtaVector
 * Method:    ctaSqrt
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaVector_ctaSqrt
  (JNIEnv *env, jobject obj_this){

   cta_jni_setJavaEnv(env);

   CTA_Handle ctaHandle = cta_jni_getCtaHandle(env, obj_this);

   CTA_Datatype datatype;
   CTA_Handle_GetDatatype (ctaHandle, &datatype);

   int retVal;
   if (datatype==CTA_VECTOR){
      retVal=CTA_Vector_ElmSqrt(ctaHandle);
   }
   else {
      retVal=CTA_TreeVector_ElmSqrt(ctaHandle);
   }
   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaVector", "Could compute sqrt", retVal);
		return;
   }
}



