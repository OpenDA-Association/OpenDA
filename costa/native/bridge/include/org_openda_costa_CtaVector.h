/* DO NOT EDIT THIS FILE - it is machine generated */
#include <jni.h>
/* Header for class org_costa_CtaVector */

#ifndef _Included_org_costa_CtaVector
#define _Included_org_costa_CtaVector
#ifdef __cplusplus
extern "C" {
#endif


/*
 * Class:     org_costa_CtaVector
 * Method:    ctaPrint handles
 * Signature: ()Lorg/openda/interfaces/IVector;
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaVector_ctaPrintHandles
(JNIEnv *, jobject, jstring);



/*
 * Class:     org_costa_CtaVector
 * Method:    clone
 * Signature: ()Lorg/openda/interfaces/IVector;
 */
JNIEXPORT jobject JNICALL Java_org_openda_costa_CtaVector_clone
  (JNIEnv *, jobject);

/*
 * Class:     org_costa_CtaVector
 * Method:    setConstant
 * Signature: (D)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaVector_setConstant
  (JNIEnv *, jobject, jdouble);

/*
 * Class:     org_costa_CtaVector
 * Method:    scale
 * Signature: (D)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaVector_scale
  (JNIEnv *, jobject, jdouble);

/*
 * Class:     org_costa_CtaVector
 * Method:    setValues
 * Signature: ([D)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaVector_setValues
  (JNIEnv *, jobject, jdoubleArray);

/*
 * Class:     org_costa_CtaVector
 * Method:    getValues
 * Signature: ()[D
 */
JNIEXPORT jdoubleArray JNICALL Java_org_openda_costa_CtaVector_getValues
  (JNIEnv *, jobject);

/*
 * Class:     org_costa_CtaVector
 * Method:    setValue
 * Signature: (ID)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaVector_setValue
  (JNIEnv *, jobject, jint, jdouble);

/*
 * Class:     org_costa_CtaVector
 * Method:    getValue
 * Signature: (I)D
 */
JNIEXPORT jdouble JNICALL Java_org_openda_costa_CtaVector_getValue
  (JNIEnv *, jobject, jint);

/*
 * Class:     org_costa_CtaVector
 * Method:    getSize
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaVector_getSize
  (JNIEnv *, jobject);

/*
 * Class:     org_costa_CtaVector
 * Method:    norm2
 * Signature: ()D
 */
JNIEXPORT jdouble JNICALL Java_org_openda_costa_CtaVector_norm2
  (JNIEnv *, jobject);

/*
 * Class:     org_costa_CtaVector
 * Method:    ctaCreate
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaVector_ctaCreate
  (JNIEnv *, jobject, jint);

/*
 * Class:     org_costa_CtaVector
 * Method:    ctaDotProduct
 * Signature: (I)D
 */
JNIEXPORT jdouble JNICALL Java_org_openda_costa_CtaVector_ctaDotProduct
  (JNIEnv *, jobject, jint);

/*
 * Class:     org_costa_CtaVector
 * Method:    ctaPointwiseDivide
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaVector_ctaPointwiseDivide
  (JNIEnv *, jobject, jint);

/*
 * Class:     org_costa_CtaVector
 * Method:    ctaPointwiseMultiply
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaVector_ctaPointwiseMultiply
  (JNIEnv *, jobject, jint);

/*
 * Class:     org_costa_CtaVector
 * Method:    ctaSetValues
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaVector_ctaSetValues
  (JNIEnv *, jobject, jint);

/*
 * Class:     org_costa_CtaVector
 * Method:    ctaAxpy
 * Signature: (DI)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaVector_ctaAxpy
  (JNIEnv *, jobject, jdouble, jint);


/*
 * Class:     org_costa_CtaVector
 * Method:    ctaSqrt
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaVector_ctaSqrt
  (JNIEnv *, jobject);


#ifdef __cplusplus
}
#endif
#endif
