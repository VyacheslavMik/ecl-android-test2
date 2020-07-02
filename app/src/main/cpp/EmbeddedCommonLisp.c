#include <assert.h>
#include <android/log.h>
#include <string.h>
#include <jni.h>
#include <pthread.h>
#include <stdio.h>

#include <ecl/ecl.h>
#include <ecl/external.h>
#include <malloc.h>
#include "ecl_boot.h"

#define ECL_TAG "ecl-native"
#define LOGI(...) ((void)__android_log_print(ANDROID_LOG_INFO, ECL_TAG, __VA_ARGS__))
#define LOGW(...) ((void)__android_log_print(ANDROID_LOG_WARN, ECL_TAG, __VA_ARGS__))
#define LOGE(...) ((void)__android_log_print(ANDROID_LOG_ERROR, ECL_TAG, __VA_ARGS__))
#define LOGV(...) ((void)__android_log_print(ANDROID_LOG_VERBOSE, ECL_TAG, __VA_ARGS__))

/*
 * Class:     com_example_ndktest_EmbeddedCommonLisp
 * Method:    start
 * Signature: ()V
 */
JNIEXPORT void JNICALL
Java_com_example_ecl_1android_1test_EmbeddedCommonLisp_start(JNIEnv *env, jobject this, jstring path) {
    const char *lisp_dir = (*env)->GetStringUTFChars(env, path, NULL);
    LOGI("XXX: ECL starting: *default-pathname-defaults* to: %s\n", lisp_dir);
    ecl_boot(lisp_dir, env, this);
    LOGI("ECL started.");
};

/*
 * Class:     com_example_ndktest_EmbeddedCommonLisp
 * Method:    exec
 * Signature: (I)V;
 */
JNIEXPORT void JNICALL
Java_com_example_ecl_1android_1test_EmbeddedCommonLisp_exec(JNIEnv *env, jobject this, jint method) {
    char tmp[2048];

    LOGI("Exec method: %d\n", method);

    CL_CATCH_ALL_BEGIN(ecl_process_env())
    {
        si_select_package(ecl_make_simple_base_string("CL-USER", 7));
        sprintf(tmp, "(native/run-method %d)", method);
        si_safe_eval(3, c_string_to_object(tmp), Cnil, OBJNULL);
    }CL_CATCH_ALL_END;

    LOGI("Exec method finished\n");
};