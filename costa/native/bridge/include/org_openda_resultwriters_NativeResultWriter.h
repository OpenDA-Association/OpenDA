/* DO NOT EDIT THIS FILE - it is machine generated */
#include <jni.h>
/* Header for class org_openda_resultwriters_NativeResultWriter */

#ifndef _Included_org_openda_resultwriters_NativeResultWriter
#define _Included_org_openda_resultwriters_NativeResultWriter
#ifdef __cplusplus
extern "C" {
#endif
/*
 * Class:     org_openda_resultwriters_NativeResultWriter
 * Method:    putMessage
 * Signature: (ILjava/lang/String;Ljava/lang/String;Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_org_openda_resultwriters_NativeResultWriter_putMessage
  (JNIEnv *, jobject, jint, jstring, jstring, jstring);

/*
 * Class:     org_openda_resultwriters_NativeResultWriter
 * Method:    putValue
 * Signature: (ILjava/lang/String;Ljava/lang/String;Ljava/lang/String;IILjava/lang/String;I)V
 */
JNIEXPORT void JNICALL Java_org_openda_resultwriters_NativeResultWriter_putValue
  (JNIEnv *, jobject, jint, jstring, jstring, jstring, jint, jint, jstring, jint);

/*
 * Class:     org_openda_resultwriters_NativeResultWriter
 * Method:    putIterationReport
 * Signature: (ILjava/lang/String;Ljava/lang/String;IDI)V
 */
JNIEXPORT void JNICALL Java_org_openda_resultwriters_NativeResultWriter_putIterationReport
  (JNIEnv *, jobject, jint, jstring, jstring, jint, jdouble, jint);

/*
 * Class:     org_openda_resultwriters_NativeResultWriter
 * Method:    free
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_org_openda_resultwriters_NativeResultWriter_free
  (JNIEnv *, jobject, jint);

#ifdef __cplusplus
}
#endif
#endif