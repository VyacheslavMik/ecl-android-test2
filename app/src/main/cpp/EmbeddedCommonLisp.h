#ifndef _Included_com_example_ndktest_EmbeddedCommonLisp
#define _Included_com_example_ndktest_EmbeddedCommonLisp

extern "C" {

/*
 * Class:     com_example_ndktest_EmbeddedCommonLisp
 * Method:    start
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_com_example_ecl_1android_1test_EmbeddedCommonLisp_start
(JNIEnv *, jobject, jstring);

/*
 * Class:     com_example_ndktest_EmbeddedCommonLisp
 * Method:    exec
 * Signature: (Ljava/lang/String;)Ljava/lang/String;
 */
JNIEXPORT void JNICALL Java_com_example_ecl_1android_1test_EmbeddedCommonLisp_exec
        (JNIEnv *, jobject, jint);
}
#endif
