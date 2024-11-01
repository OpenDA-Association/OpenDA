/* DO NOT EDIT THIS FILE - it is machine generated */
#include <jni.h>
/* Header for class org_costa_CtaTreeVector */

#ifndef _Included_org_costa_CtaTreeVector
#define _Included_org_costa_CtaTreeVector
#ifdef __cplusplus
extern "C" {
#endif
/*
 * Class:     org_costa_CtaTreeVector
 * Method:    getId
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_org_openda_costa_CtaTreeVector_getId
  (JNIEnv *, jobject);

/*
 * Class:     org_costa_CtaTreeVector
 * Method:    ctaCreate_from_vector
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaTreeVector_ctaCreateFromVector
  (JNIEnv *, jobject, jstring, jstring, jint);

/*
 * Class:     org_costa_CtaTreeVector
 * Method:    ctaCreateFromSubtreevectors
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaTreeVector_ctaCreateFromSubtreevectors
  (JNIEnv *, jobject, jstring, jstring, jint, jintArray);

/*
 * Class:     org_costa_ctatreevector
 * Method:    ctaSetRegGrid
 * Signature: ()V
*/
 JNIEXPORT jint JNICALL Java_org_openda_costa_CtaTreeVector_ctaSetRegGrid
  (JNIEnv * , jobject, jint, jint, jint, jdouble, jdouble, jdouble,
                 jdouble, jdouble, jdouble);

/*
 * Class:     org_costa_CtaTreeVector
 * Method:    ctaGetSubTreeVector
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaTreeVector_ctaGetSubTreeVector
  (JNIEnv *, jobject, jstring);

/*
 * Class:     org_costa_CtaTreeVector
 * Method:    ctaGetSubTreeVectorId
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_org_openda_costa_CtaTreeVector_ctaGetSubTreeVectorId
(JNIEnv *env, jobject obj_this, jint index);


/*
 * Class:     org_costa_CtaTreeVector
 * Method:    ctaGetNumSubTreeVectors
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaTreeVector_ctaGetNumSubTreeVectors
(JNIEnv *env, jobject obj_this);

/*
 * Class:     org_costa_CtaTreeVector
 * Method:    ctaImport
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaTreeVector_ctaImport
(JNIEnv *, jobject , jint );

/*
 * Class:     org_costa_CtaTreeVector
 * Method:    ctaVImport
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaTreeVector_ctaVImport
(JNIEnv *, jobject , jint );



JNIEXPORT jint JNICALL Java_org_openda_costa_CtaTreeVector_ctaNetcdfClose
  (JNIEnv *, jobject, jint );

/*
 * Class:     org_resultwriters_NetcdfResultWriter
 * Method:    ctaNetcdfClose
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_org_openda_resultwriters_NetcdfResultWriter_ctaNetcdfClose
  (JNIEnv *, jobject, jint);



JNIEXPORT jint JNICALL Java_org_openda_costa_CtaTreeVector_ctaNetcdfInit
  (JNIEnv *, jobject, jstring, jstring);
/*
 * Class:     org_resultwriters_NetcdfResultWriter
 * Method:    ctaNetcdfInit
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_org_openda_resultwriters_NetcdfResultWriter_ctaNetcdfInit
  (JNIEnv *, jobject, jstring, jstring);

/*
 * Class:     org_costa_CtaTreeVector
 * Method:    ctaExport
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaTreeVector_ctaExport
  (JNIEnv *, jobject, jint);


/*
 * Class:     org_costa_CtaTreeVector
 * Method:    getCaption
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_org_openda_costa_CtaTreeVector_getCaption
  (JNIEnv *, jobject);

/*
 * Class:     org_costa_CtaTreeVector
 * Method:    clone
 * Signature: ()Lorg/openda/interfaces/ITreeVector;
 */
JNIEXPORT jobject JNICALL Java_org_openda_costa_CtaTreeVector_clone
  (JNIEnv *, jobject);

#ifdef __cplusplus
}
#endif
#endif
