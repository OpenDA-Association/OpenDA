/*
$URL: https://repos.deltares.nl/repos/openda/openda_1/public/trunk/core/native/src/openda/bridge/org_openda_costa_CtaVector.cpp $
$Revision: 2738 $, $Date: 2011-09-05 10:48:32 +0200 (Mon, 05 Sep 2011) $

OpenDA interface for COSTA.
Copyright (C) 2012  Nils van Velzen

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

#include "org_openda_costa_CtaArray.h"
#include "cta_array.h"
#include "jni_cta_utils.h"
#include "cta_errors.h"
#include "cta_defaults.h"

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    create
 * Signature: ([D[I)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaArray_create
  (JNIEnv *env, jobject obj_this, jdoubleArray jValues, jintArray jDimensions){

   cta_jni_setJavaEnv(env);

   // Get dimension and pointers to the data of the java arrays
   //int nValues = env->GetArrayLength(jValues);
   jboolean isCopy = JNI_FALSE; 
   jdouble * cValues = env->GetDoubleArrayElements(jValues, &isCopy);

   int nDimensions = env->GetArrayLength(jDimensions);
   jint * cDimensions = env->GetIntArrayElements(jDimensions, &isCopy);

   // Call native constructor
   CTA_Array h;  
   int ierr=CTA_Array_CreateAsDoubles(cValues, nDimensions, (int*) cDimensions, &h);
   if ( ierr != CTA_OK ) {
      cta_jni_exception(env, "CtaArray", "Could create new Array", ierr);
   }

   // Set the native handle in the java class
   cta_jni_setCtaHandle(env, obj_this, (CTA_Handle) h);

   // Release the data of the arrays
   env->ReleaseDoubleArrayElements(jValues, cValues,0);
   env->ReleaseIntArrayElements(jDimensions, cDimensions,0);
}

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    getNumberOfDimensions
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaArray_getNumberOfDimensions
  (JNIEnv *env, jobject obj_this){

   cta_jni_setJavaEnv(env);
   CTA_Array h = (CTA_Array) cta_jni_getCtaHandle(env, obj_this);
   return (jint) CTA_Array_getNumberOfDimensions(h);
}

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    getDimensions
 * Signature: ()[I
 */
JNIEXPORT jintArray JNICALL Java_org_openda_costa_CtaArray_getDimensions
  (JNIEnv *env, jobject obj_this){

   cta_jni_setJavaEnv(env);
   CTA_Array h = (CTA_Array) cta_jni_getCtaHandle(env, obj_this);

   int nDimensions=CTA_Array_getNumberOfDimensions(h);
   jintArray jDimensions = env->NewIntArray(nDimensions);
   jint * cDimensions = new jint[nDimensions];

   int ierr = CTA_Array_getDimensions(h, (int*) cDimensions);
   if (ierr == CTA_OK){
      env->SetIntArrayRegion(jDimensions, 0, nDimensions, cDimensions);
   }
   else {
      cta_jni_exception(env, "CtaArray", "could not call getDimensions", ierr);
   }
   delete [] cDimensions;
   return jDimensions;
}

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    length
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaArray_length
  (JNIEnv *env, jobject obj_this){

   cta_jni_setJavaEnv(env);
   CTA_Array h = (CTA_Array) cta_jni_getCtaHandle(env, obj_this);

   return (jint) CTA_Array_length(h);

}

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    getValuesAsDoubles
 * Signature: ()[D
 */
JNIEXPORT jdoubleArray JNICALL Java_org_openda_costa_CtaArray_getValuesAsDoubles__
  (JNIEnv *env, jobject obj_this){

   cta_jni_setJavaEnv(env);
   CTA_Array h = (CTA_Array) cta_jni_getCtaHandle(env, obj_this);
   int n = CTA_Array_length(h);
   jdoubleArray jValues = env->NewDoubleArray(n);
   double *cValues = new double[n];

   int ierr = CTA_Array_getValuesAsDoubles(h, cValues);
   if (ierr == CTA_OK){
      env->SetDoubleArrayRegion(jValues, 0, n, cValues);
   }
   else {
      cta_jni_exception(env, "CtaArray", "could not call getValuesAsDoubles", ierr);
   }
   delete [] cValues;
   return jValues;
}

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    getValuesAsDoubles
 * Signature: (Z)[D
 */
JNIEXPORT jdoubleArray JNICALL Java_org_openda_costa_CtaArray_getValuesAsDoubles__Z
  (JNIEnv *env, jobject obj_this, jboolean){

   //Note since this is a JNI binding we ignore the boolean

   cta_jni_setJavaEnv(env);
   CTA_Array h = (CTA_Array) cta_jni_getCtaHandle(env, obj_this);
   int n = CTA_Array_length(h);
   jdoubleArray jValues = env->NewDoubleArray(n);
   double *cValues = new double[n];

   int ierr = CTA_Array_getValuesAsDoubles(h, cValues);
   if (ierr == CTA_OK){
      env->SetDoubleArrayRegion(jValues, 0, n, cValues);
   }
   else {
      cta_jni_exception(env, "CtaArray", "could not call getValuesAsDoubles", ierr);
   }
   delete [] cValues;
   return jValues;
}

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    getValuesAsDoubles
 * Signature: (II)[D
 */
JNIEXPORT jdoubleArray JNICALL Java_org_openda_costa_CtaArray_getValuesAsDoubles__II
  (JNIEnv *env, jobject obj_this, jint firstIndex, jint lastIndex){

   cta_jni_setJavaEnv(env);
   CTA_Array h = (CTA_Array) cta_jni_getCtaHandle(env, obj_this);
   int n = lastIndex-firstIndex+1;
   jdoubleArray jValues = env->NewDoubleArray(n);
   double *cValues = new double[n];

   int ierr = CTA_Array_getValuesAsDoubles_indexrange(h, firstIndex, lastIndex, cValues);
   if (ierr == CTA_OK){
      env->SetDoubleArrayRegion(jValues, 0, n, cValues);
   }
   else {
      cta_jni_exception(env, "CtaArray", "could not call getValuesAsDoubles_indexrange", ierr);
   }
   delete [] cValues;
   return jValues;


}

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    getValueAsDouble
 * Signature: (I)D
 */
JNIEXPORT jdouble JNICALL Java_org_openda_costa_CtaArray_getValueAsDouble__I
  (JNIEnv *env, jobject obj_this, jint index){

   cta_jni_setJavaEnv(env);
   CTA_Array h = (CTA_Array) cta_jni_getCtaHandle(env, obj_this);
   jdouble cValue=-99.0;

   int ierr = CTA_Array_getValueAsDoubles_index(h, index, &cValue);
   if (ierr!=CTA_OK){
      cta_jni_exception(env, "CtaArray", "could not call getValuesAsDoubles_indexrange", ierr);
   }
   return cValue;
}

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    getValueAsDouble
 * Signature: ([I)D
 */
JNIEXPORT jdouble JNICALL Java_org_openda_costa_CtaArray_getValueAsDouble___3I
  (JNIEnv *env, jobject obj_this, jintArray jIndices){

   cta_jni_setJavaEnv(env);
   CTA_Array h = (CTA_Array) cta_jni_getCtaHandle(env, obj_this);

   jboolean isCopy = JNI_FALSE; 
   jint * cIndices = env->GetIntArrayElements(jIndices, &isCopy);
   int nIndices = env->GetArrayLength(jIndices);

   jdouble cValue;
   int ierr = CTA_Array_getValueAsDouble_indices(h, nIndices, (int *) cIndices, &cValue);
   if (ierr != CTA_OK){
      cta_jni_exception(env, "CtaArray", "could not call CTA_Array_getValueAsDouble_indices", ierr);
   }
   env->ReleaseIntArrayElements(jIndices, cIndices,0);

   return cValue;
}

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    setConstant
 * Signature: (D)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaArray_setConstant
  (JNIEnv *env, jobject obj_this, jdouble jValue){

   cta_jni_setJavaEnv(env);
   CTA_Array h = (CTA_Array) cta_jni_getCtaHandle(env, obj_this);
   int ierr = CTA_Array_setConstant(h, jValue);
   if (ierr != CTA_OK){
      cta_jni_exception(env, "CtaArray", "could not call CTA_Array_setConstant", ierr);
   }
}

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    setValuesAsDoubles
 * Signature: ([D)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaArray_setValuesAsDoubles___3D
  (JNIEnv *env, jobject obj_this, jdoubleArray jValues){

   cta_jni_setJavaEnv(env);
   CTA_Array h = (CTA_Array) cta_jni_getCtaHandle(env, obj_this);

   jboolean isCopy = JNI_FALSE; 
   jdouble * cValues = env->GetDoubleArrayElements(jValues, &isCopy);
   int length = env->GetArrayLength(jValues);
   
   int ierr = CTA_Array_setValuesAsDoubles(h, cValues, length);
   if ( ierr != CTA_OK ) {
      cta_jni_exception(env, "CtaArray", "Could call CTA_Array_setValuesAsDoubles", ierr);
   }
   env->ReleaseDoubleArrayElements(jValues, cValues,0);
}

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    setValuesAsDoubles
 * Signature: (II[D)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaArray_setValuesAsDoubles__II_3D
  (JNIEnv *env, jobject obj_this, jint firstIndex, jint lastIndex, jdoubleArray jValues){

   cta_jni_setJavaEnv(env);
   CTA_Array h = (CTA_Array) cta_jni_getCtaHandle(env, obj_this);

   jboolean isCopy = JNI_FALSE; 
   jdouble * cValues = env->GetDoubleArrayElements(jValues, &isCopy);

   int ierr = CTA_Array_setValuesAsDoubles_indexrange(h, firstIndex, lastIndex, cValues);
   if ( ierr != CTA_OK ) {
      cta_jni_exception(env, "CtaArray", "Could call CTA_Array_setValuesAsDoubles_indexrange", ierr);
   }
   env->ReleaseDoubleArrayElements(jValues, cValues,0);
}

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    setValueAsDouble
 * Signature: (ID)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaArray_setValueAsDouble__ID
  (JNIEnv *env, jobject obj_this, jint jIndex, jdouble jValue){

   cta_jni_setJavaEnv(env);
   CTA_Array h = (CTA_Array) cta_jni_getCtaHandle(env, obj_this);

   int ierr = CTA_Array_setValueAsDouble_index(h, jIndex, jValue);
   if ( ierr != CTA_OK ) {
      cta_jni_exception(env, "CtaArray", "Could call CTA_Array_setValueAsDouble", ierr);
   }
}

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    setValueAsDouble
 * Signature: ([ID)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaArray_setValueAsDouble___3ID
  (JNIEnv *env, jobject obj_this, jintArray jIndices, jdouble jValue){

   cta_jni_setJavaEnv(env);
   CTA_Array h = (CTA_Array) cta_jni_getCtaHandle(env, obj_this);

   jboolean isCopy = JNI_FALSE; 
   jint * cIndices = env->GetIntArrayElements(jIndices, &isCopy);
   int nIndices = env->GetArrayLength(jIndices);
 

   int ierr = CTA_Array_setValueAsDouble_indices(h, nIndices, (int *) cIndices, jValue);
   if ( ierr != CTA_OK ) {
      cta_jni_exception(env, "CtaArray", "Could call setValueAsDouble_indices", ierr);
   }
   env->ReleaseIntArrayElements(jIndices, cIndices,0);

}

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    axpyOnValues
 * Signature: (D[D)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaArray_axpyOnValues
  (JNIEnv *env, jobject obj_this, jdouble jAlpha, jdoubleArray jValues){

   cta_jni_setJavaEnv(env);
   CTA_Array h = (CTA_Array) cta_jni_getCtaHandle(env, obj_this);

   jboolean isCopy = JNI_FALSE; 
   jdouble * cValues = env->GetDoubleArrayElements(jValues, &isCopy);

   int ierr = CTA_Array_axpyOnValues(h, jAlpha, cValues);
   if ( ierr != CTA_OK ) {
      cta_jni_exception(env, "CtaArray", "Could call CTA_Array_axpyOnValues", ierr);
   }
   env->ReleaseDoubleArrayElements(jValues, cValues,0);
}

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    multiplyValues
 * Signature: ([D)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaArray_multiplyValues
  (JNIEnv *env, jobject obj_this, jdoubleArray jValues){

   cta_jni_setJavaEnv(env);
   CTA_Array h = (CTA_Array) cta_jni_getCtaHandle(env, obj_this);

   jboolean isCopy = JNI_FALSE; 
   jdouble * cValues = env->GetDoubleArrayElements(jValues, &isCopy);

   int ierr = CTA_Array_multiplyValues(h, cValues);
   if ( ierr != CTA_OK ) {
      cta_jni_exception(env, "CtaArray", "Could call CTA_Array_axpyOnValues", ierr);
   }
   env->ReleaseDoubleArrayElements(jValues, cValues,0);
}

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    reshape
 * Signature: ([I)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaArray_reshape
  (JNIEnv *env, jobject obj_this, jintArray jDimensions){

   cta_jni_setJavaEnv(env);
   CTA_Array h = (CTA_Array) cta_jni_getCtaHandle(env, obj_this);

   jboolean isCopy = JNI_FALSE; 
   int nDimensions = env->GetArrayLength(jDimensions);
   jint * cDimensions = env->GetIntArrayElements(jDimensions, &isCopy);

   int ierr = CTA_Array_reshape(h, nDimensions, (int *) cDimensions);
   if ( ierr != CTA_OK ) {
      cta_jni_exception(env, "CtaArray", "Could call CTA_Array_reshape", ierr);
   }
   env->ReleaseIntArrayElements(jDimensions, cDimensions,0);
}

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    getSlice
 * Signature: (II)Lorg/openda/interfaces/IArray;
 */
JNIEXPORT jobject JNICALL Java_org_openda_costa_CtaArray_getSlice__II
  (JNIEnv *env, jobject obj_this, jint jDimension, jint jIndex){

   cta_jni_setJavaEnv(env);
   CTA_Array h = (CTA_Array) cta_jni_getCtaHandle(env, obj_this);
    
   CTA_Array h_out;
   jobject jArray=NULL;

   int ierr = CTA_Array_getSlice(h, jDimension, jIndex, &h_out);
   if ( ierr == CTA_OK ) {
      /* Create a Java CtaVector */
      jclass clsArray = env->FindClass("org/openda/costa/CtaArray");
      jmethodID constructorID = env->GetMethodID (clsArray, "<init>", "()V");
      jArray = env->NewObject(clsArray, constructorID);

      cta_jni_setCtaHandle(env, jArray, h_out);
   }
   else {
      cta_jni_exception(env, "CtaArray", "Could call CTA_Array_getSlice", ierr);
   }
   return jArray;
}

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    getSlice
 * Signature: (III)Lorg/openda/interfaces/IArray;
 */
JNIEXPORT jobject JNICALL Java_org_openda_costa_CtaArray_getSlice__III
  (JNIEnv *env, jobject obj_this, jint jDimension, jint jMinIndex, jint jMaxIndex){

   cta_jni_setJavaEnv(env);
   CTA_Array h = (CTA_Array) cta_jni_getCtaHandle(env, obj_this);
    
   CTA_Array h_out;
   jobject jArray=NULL;

   int ierr = CTA_Array_getSlice_range(h, jDimension, jMinIndex, jMaxIndex, &h_out);
   if ( ierr == CTA_OK ) {
      /* Create a Java CtaVector */
      jclass clsArray = env->FindClass("org/openda/costa/CtaArray");
      jmethodID constructorID = env->GetMethodID (clsArray, "<init>", "()V");
      jArray = env->NewObject(clsArray, constructorID);

      cta_jni_setCtaHandle(env, jArray, h_out);
   }
   else {
      cta_jni_exception(env, "CtaArray", "Could call CTA_Array_getSlice_range", ierr);
   }
   return jArray;
}

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    getSliceAsDoubles
 * Signature: (III)[D
 */
JNIEXPORT jdoubleArray JNICALL Java_org_openda_costa_CtaArray_getSliceAsDoubles
  (JNIEnv *env, jobject obj_this, jint jDimension, jint jMinIndex, jint jMaxIndex){

   cta_jni_setJavaEnv(env);
   CTA_Array h = (CTA_Array) cta_jni_getCtaHandle(env, obj_this);

   int nDimensions;
   CTA_Array_getnDimensions(h, &nDimensions);
   int *dimensions = new int[nDimensions];
   CTA_Array_getDimensions(h, dimensions);

   int n=jMaxIndex-jMinIndex+1;
   int i;
   for (i=0;i<jDimension;i++){ n*=dimensions[i];}
   for (i=jDimension+1;i<nDimensions;i++){ n*=dimensions[i];}
   delete [] dimensions;

   jdoubleArray jValues = env->NewDoubleArray(n);
   double *cValues = new double[n];

   int ierr = CTA_Array_getSliceAsDoubles_range(h, jDimension, jMinIndex, jMaxIndex, cValues);
   if (ierr == CTA_OK){
      env->SetDoubleArrayRegion(jValues, 0, n, cValues);
   }
   else {
      cta_jni_exception(env, "CtaArray", "could not call CTA_Array_getSliceAsDoubles_range", ierr);
   }
   return jValues;

}

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    setSlice
 * Signature: ([DII)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaArray_setSlice___3DII
  (JNIEnv *env, jobject obj_this, jdoubleArray jValues, jint jDimension, jint jIndex){

   cta_jni_setJavaEnv(env);
   CTA_Array h = (CTA_Array) cta_jni_getCtaHandle(env, obj_this);

   jboolean isCopy = JNI_FALSE; 
   jdouble * cValues = env->GetDoubleArrayElements(jValues, &isCopy);

   int ierr = CTA_Array_setSliceAsDoubles(h, cValues, jDimension, jIndex);
   if (ierr != CTA_OK){
      cta_jni_exception(env, "CtaArray", "could not call CTA_Array_setSliceAsDoubles", ierr);
   }
   env->ReleaseDoubleArrayElements(jValues, cValues,0);
}

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    setSliceBySlice
 * Signature: (Lorg/openda/interfaces/IArray;II)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaArray_setSliceBySlice__Lorg_openda_interfaces_IArray_2II
  (JNIEnv *env, jobject obj_this, jobject jSlice, jint jDimension, jint jIndex){

   cta_jni_setJavaEnv(env);
   CTA_Array h       = (CTA_Array) cta_jni_getCtaHandle(env, obj_this);
   CTA_Array h_slice = (CTA_Array) cta_jni_getCtaHandle(env, jSlice);

   int ierr = CTA_Array_setSliceAsArray(h, h_slice, jDimension, jIndex);
   if (ierr != CTA_OK){
      cta_jni_exception(env, "CtaArray", "could not call CTA_Array_setSliceAsArray", ierr);
   }
}

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    setSliceBySlice
 * Signature: (Lorg/openda/interfaces/IArray;III)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaArray_setSliceBySlice__Lorg_openda_interfaces_IArray_2III
  (JNIEnv *env, jobject obj_this, jobject jSlice, jint jDimension, jint jMinIndex, jint jMaxIndex){

   cta_jni_setJavaEnv(env);
   CTA_Array h       = (CTA_Array) cta_jni_getCtaHandle(env, obj_this);
   CTA_Array h_slice = (CTA_Array) cta_jni_getCtaHandle(env, jSlice);

   int ierr = CTA_Array_setSliceAsArray_range(h, h_slice, jDimension, jMinIndex, jMaxIndex);
   if (ierr != CTA_OK){
      cta_jni_exception(env, "CtaArray", "could not call CTA_Array_setSliceAsArray_range", ierr);
   }
}

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    setSlice
 * Signature: ([DIII)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaArray_setSlice___3DIII
  (JNIEnv *env, jobject obj_this, jdoubleArray jValues, jint jDimension, jint jMinIndex, jint jMaxIndex){

   cta_jni_setJavaEnv(env);
   CTA_Array h = (CTA_Array) cta_jni_getCtaHandle(env, obj_this);

   jboolean isCopy = JNI_FALSE; 
   jdouble * cValues = env->GetDoubleArrayElements(jValues, &isCopy);

   int ierr = CTA_Array_setSliceAsDoubles_range(h, cValues, jDimension, jMinIndex, jMaxIndex);
   if (ierr != CTA_OK){
      cta_jni_exception(env, "CtaArray", "could not call CTA_Array_setSliceAsDoubles_range", ierr);
   }
   env->ReleaseDoubleArrayElements(jValues, cValues,0);


}

/*
 * Class:     org_openda_costa_CtaArray
 * Method:    valueIndex
 * Signature: ([I)I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaArray_valueIndex
  (JNIEnv *env, jobject obj_this, jintArray jIndices){

   cta_jni_setJavaEnv(env);
   CTA_Array h = (CTA_Array) cta_jni_getCtaHandle(env, obj_this);

   jboolean isCopy = JNI_FALSE; 
   jint * cIndices = env->GetIntArrayElements(jIndices, &isCopy);
   int nIndices = env->GetArrayLength(jIndices);
   jint jIndex;
   int ierr = CTA_Array_valueIndex(h, nIndices, (int *) cIndices, (int*) &jIndex);
   if (ierr != CTA_OK){
      cta_jni_exception(env, "CtaArray", "could not call CTA_Array_valueIndex", ierr);
   }

   env->ReleaseIntArrayElements(jIndices, cIndices,0);
   return jIndex;
}

