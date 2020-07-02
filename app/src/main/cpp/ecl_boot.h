#ifndef _ECL_BOOT_H_
#define _ECL_BOOT_H_

int ecl_boot(const char *root_dir, JNIEnv *env, jobject this);
void ecl_toplevel(const char *home);

#endif
