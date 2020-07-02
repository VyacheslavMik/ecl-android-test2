#include <stdlib.h>
#include <ecl/ecl.h>
#include <android/log.h>
#include <jni.h>

#define LOGI(...) ((void)__android_log_print(ANDROID_LOG_INFO, "native-activity", __VA_ARGS__))
#define LOGW(...) ((void)__android_log_print(ANDROID_LOG_WARN, "native-activity", __VA_ARGS__))
#define LOGE(...) ((void)__android_log_print(ANDROID_LOG_ERROR, "native-activity", __VA_ARGS__))
#define LOGV(...) ((void)__android_log_print(ANDROID_LOG_VERBOSE, "native-activity", __VA_ARGS__))

#include "ecl_boot.h"
#include "logging.h"

#ifdef __cplusplus
#define ECL_CPP_TAG "C"
#else
#define ECL_CPP_TAG
#endif

/* extern ECL_CPP_TAG void init_lib_ASDF(); */
/* extern ECL_CPP_TAG void init_lib_SOCKETS(); */
/* extern ECL_CPP_TAG void init_lib_SB_BSD_SOCKETS(); */
/* extern ECL_CPP_TAG void init_lib_SERVE_EVENT(); */
/* extern ECL_CPP_TAG void init_lib_ECL_CDB(); */
/* extern ECL_CPP_TAG void init_lib_ECL_HELP() */;

extern void loadLispFromAssets(char* fn);

JavaVM  *jvm;
static jobject classLoader;
static jmethodID findClassMethod;

jclass findClass(JNIEnv *env, const char *name)
{
    LOGI("findClass: %p, %s", env, name);
    return (jclass)(*env)->CallObjectMethod(env, classLoader, findClassMethod, (*env)->NewStringUTF(env, name));
}

int ecl_boot(const char *root_dir, JNIEnv *env, jobject this)
{
    start_logger("ecl-standard-output");

    char *ecl = "ecl";
    char tmp[2048];

    LOGI("ECL boot beginning\n");

    LOGI("Setting directories\n");
    setenv("HOME", root_dir, 1);

    sprintf(tmp, "%s/lib/", root_dir);
    setenv("ECLDIR", tmp, 1);

    sprintf(tmp, "%s/etc/", root_dir);
    setenv("ETC", tmp, 1);

    sprintf(tmp, "%s/home/", root_dir);
    setenv("HOME", tmp, 1);

    LOGI("Directories set\n");

    /* ecl_set_option(ECL_OPT_TRAP_SIGFPE, 0); */
    /* ecl_set_option(ECL_OPT_TRAP_SIGSEGV, 0); */
    /* ecl_set_option(ECL_OPT_TRAP_SIGINT, 0); */
    /* ecl_set_option(ECL_OPT_TRAP_SIGILL, 0); */
    /* ecl_set_option(ECL_OPT_TRAP_SIGBUS, 0); */
    /* ecl_set_option(ECL_OPT_TRAP_INTERRUPT_SIGNAL, 0); */
    /* ecl_set_option(ECL_OPT_SIGNAL_HANDLING_THREAD, 0); */
    /* ecl_set_option(ECL_OPT_INCREMENTAL_GC, 0); */

    cl_boot(1, &ecl);

    LOGI("installing bytecodes compiler\n");
    si_safe_eval(3, c_string_to_object("(si:install-bytecodes-compiler)"), ECL_NIL, OBJNULL);

    /* LOGI("initializing linked modules\n"); */
    /* ecl_init_module(NULL, init_lib_ECL_HELP); */
    /* ecl_init_module(NULL, init_lib_ASDF); */
    /* ecl_init_module(NULL, init_lib_SOCKETS); */
    /* ecl_init_module(NULL, init_lib_SERVE_EVENT); */

    LOGI("writing some info to stdout\n");
    si_safe_eval
            (3, c_string_to_object
                     ("(format t \"ECL_BOOT, features = ~A ~%\" *features*)"),
             Cnil, OBJNULL);
    si_safe_eval
            (3, c_string_to_object
                     ("(format t \"(truename SYS:): ~A)\" (truename \"SYS:\"))"),
             Cnil, OBJNULL);

/*    cl_object cl_this_symbol = ecl_make_symbol("*THIS*", "CL-USER");
    cl_object cl_this_data = ecl_make_pointer(&this);
    ecl_setq(ecl_process_env(), cl_this_symbol, cl_this_data);*/

    LOGI("Getting jvm: %i", (*env)->GetJavaVM(env, &jvm));

//    JNIEnv *_env;
//    (*jvm)->GetEnv(jvm, (void **)&_env, JNI_VERSION_1_6);

    JNIEnv  *_env;
    JavaVMAttachArgs args;
    args.version = JNI_VERSION_1_6;
    (*jvm)->AttachCurrentThread(jvm, &_env, &args);

    jclass helloEclActivityClass = (*_env)->FindClass(_env, "com/example/ecl_android_test/HelloEclActivity");
    jclass classClass = (*_env)->GetObjectClass(_env, helloEclActivityClass);
    jclass classLoaderClass = (*_env)->FindClass(_env, "java/lang/ClassLoader");
    jmethodID getClassLoaderMethod = (*_env)->GetMethodID(_env, classClass, "getClassLoader", "()Ljava/lang/ClassLoader;");
    jobject classLoaderObj = (*_env)->CallObjectMethod(_env, helloEclActivityClass, getClassLoaderMethod);
    classLoader = (*_env)->NewGlobalRef(_env, classLoaderObj);
    findClassMethod = (*_env)->GetMethodID(_env, classLoaderClass, "findClass", "(Ljava/lang/String;)Ljava/lang/Class;");

    ecl_setq(ecl_process_env(), ecl_make_symbol("*JVM*", "CL-USER"), ecl_make_pointer(jvm));
    ecl_setq(ecl_process_env(), ecl_make_symbol("*CLASS-LOADER*", "CL-USER"), ecl_make_pointer(classLoader));
    ecl_setq(ecl_process_env(), ecl_make_symbol("*FIND-CLASS-PTR*", "CL-USER"), ecl_make_pointer(&findClass));

    ecl_toplevel(root_dir);

    LOGI("Debug print: %p, %p, %p, %p, %i, %p, %p", jvm, (*jvm)->AttachCurrentThread, *_env, (*_env)->GetVersion,
         (*_env)->GetVersion(_env),
         helloEclActivityClass,
         NULL
    );


    return 0;
}

void ecl_toplevel(const char *home)
{
    char tmp[2048];

    LOGI("Executing the init scripts\n");

    CL_CATCH_ALL_BEGIN(ecl_process_env())
    {
        sprintf(tmp, "(setq *default-pathname-defaults* #p\"%s/\")", home);
        si_safe_eval(3, c_string_to_object(tmp), Cnil, OBJNULL);
        si_select_package(ecl_make_simple_base_string("CL-USER", 7));
        si_safe_eval(3, c_string_to_object("(load \"etc/init\")"), Cnil, OBJNULL);
    } CL_CATCH_ALL_END;

    LOGI("Toplevel initialization done\n");
}
