/* ecl/config-internal.h.  Generated from configpre-int.h by configure.  */
/* -*- Mode: C; c-basic-offset: 8; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

/*
    config-internal.h.in -- Template configuration file (not exported options).

    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    See top-level file 'LICENSE' for license details.
*/

/* -------------------------------------------------------------------- *
 *      BUILD OPTIONS WHICH NEED NOT BE EXPORTED                        *
 * -------------------------------------------------------------------- */

/*
 * FEATURES LINKED IN:
 */

/* Define this if you want a runtime version only without compiler      */
/* #undef RUNTIME */
/* Profile tool                                                         */
/* #undef PROFILE */
/* Program Development Environment                                      */
/* #undef PDE */

/* Allow loading dynamically linked code                                */
#define ENABLE_DLOPEN 1

/* Undefine this if you do not want ECL to check for circular lists     */
#define ECL_SAFE

/* Activate Boehm-Weiser incremental garbage collector                  */
/* #undef GBC_BOEHM_GENGC */

/* Activate Boehm-Weiser precise garbage collector                      */
/* #undef GBC_BOEHM_PRECISE */

/* Weak hash tables need the garbage collector and weak pointers        */
#ifdef GBC_BOEHM
#define ECL_WEAK_HASH
#endif

/*
 * SYSTEM FEATURES:
 */

/* Most significant byte first                                          */
/* #undef WORDS_BIGENDIAN */
/* Has <sys/resource.h>                                                 */
#define HAVE_SYS_RESOURCE_H /**/
/* #undef HAVE_ULIMIT_H */
/* High precision timer                                                 */
#define HAVE_NANOSLEEP 1
/* Float version if isnan()                                             */
/* #undef HAVE_ISNANF */
/* float.h for epsilons, maximum real numbers, etc			*/
#define HAVE_FLOAT_H 1
/* select()								*/
#ifndef NACL
#define HAVE_SELECT 1
#endif
#define HAVE_SYS_SELECT_H 1
#define HAVE_SYS_IOCTL_H 1
/* putenv() or setenv()							*/
#define HAVE_SETENV 1
#define HAVE_PUTENV 1
/* times() and sys/times.h						*/
#ifndef NACL
#define HAVE_TIMES 1
#endif
/* gettimeofday() and sys/time.h					*/
#define HAVE_GETTIMEOFDAY 1
/* getrusage() and sys/resource.h					*/
#ifndef NACL
#define HAVE_GETRUSAGE 1
#endif
/* user home directory, user name, etc...				*/
/* #undef HAVE_PW_H */
/* symbolic links and checking their existence				*/
#ifndef NACL
#define HAVE_LSTAT 1
#endif
/* safe creation of temporary files					*/
#define HAVE_MKSTEMP 1
/* timer for userland threads						*/
#define HAVE_ALARM 1
/* filesytem                                                            */
#define HAVE_DIRENT_H 1
/* dynamic linking of libraries                                         */
#define HAVE_DLFCN_H 1
#define HAVE_LINK_H 1
/* #undef HAVE_MACH_O_DYLD_H */
/* POSIX signals							*/
#ifndef NACL
#define HAVE_SIGPROCMASK 1
#endif
/* isatty() checks whether a file is connected to a			*/
#define HAVE_ISATTY 1
/* do we want to have signed zeros                                      */
#define ECL_SIGNED_ZERO /**/
/* has support for large files                                          */
#define HAVE_FSEEKO 1
/* the tzset() function gets the current time zone                      */
#define HAVE_TZSET 1
/* several floating point functions (ISO C99)                           */
#define HAVE_EXPF 1
#define HAVE_LOGF 1
#define HAVE_SQRTF 1
#define HAVE_COSF 1
#define HAVE_SINF 1
#define HAVE_TANF 1
#define HAVE_SINHF 1
#define HAVE_COSHF 1
#define HAVE_TANHF 1
#define HAVE_FLOORF 1
#define HAVE_CEILF 1
#define HAVE_FABSF 1
#define HAVE_FREXPF 1
#define HAVE_LDEXPF 1
#define HAVE_LOG1P 1
#define HAVE_LOG1PF 1
#define HAVE_LOG1PL 1
/* #undef HAVE_COPYSIGNF */
#define HAVE_COPYSIGN 1
/* #undef HAVE_COPYSIGNL */
/* whether we have sched_yield() that gives priority to other threads   */
#define HAVE_SCHED_YIELD 1
/* whether we have a working sem_init()                                 */
/* #undef HAVE_SEM_INIT */
/* whether we have read/write locks                                     */
#define HAVE_POSIX_RWLOCK /**/
/* uname() for system identification                                    */
#define HAVE_UNAME 1
#define HAVE_UNISTD_H 1
#define HAVE_SYS_WAIT_H 1
#define HAVE_SYS_MMAN_H /**/
/* size of long long                                                    */
#define ECL_LONG_LONG_BITS 64
/* existence of char **environ                                          */
#define HAVE_ENVIRON /**/
/* existence of pointer -> function name functions                      */
/* #undef HAVE_BACKTRACE */
/* #undef HAVE_BACKTRACE_SYMBOLS */
#define HAVE_SCHED_H 1
#define HAVE_SYSTEM 1

/* what characters are used to mark beginning of new line               */
/* #undef ECL_NEWLINE_IS_CRLF */
/* #undef ECL_NEWLINE_IS_LFCR */

/*
 * PARAMETERS:
 */

/*
 * Memory limits for the old garbage collector.
 */
#define LISP_PAGESIZE   2048    /* Page size in bytes                   */
#define MAXPAGE 65536           /* Maximum Memory Size                  */

/* We allocate a number of strings in a pool which is used to speed up reading */
#define ECL_MAX_STRING_POOL_SIZE        10
#define ECL_BUFFER_STRING_SIZE          4192

/*
 * Macros that depend on these system features.
 */
#if defined(sparc) || defined(i386) || defined(mips)
#  define       stack_align(n)  (((n) + 0x7) & ~0x7)
#else
#  define       stack_align(n)  (((n) + 03) & ~03)
#endif

/* #undef FILE_CNT */
#if 0 == 1
#  define FILE_CNT(fp)  ((fp)->_IO_read_end - (fp)->_IO_read_ptr)
#endif
#if 0 == 2
#  define FILE_CNT(fp)  ((fp)->_r)
#endif
#if 0 == 3
#  define FILE_CNT(fp)  ((fp)->_cnt)
#endif

#if defined(cygwin) || defined(ECL_MS_WINDOWS_HOST)
#  define IS_DIR_SEPARATOR(x) ((x=='/')||(x=='\\'))
#  define DIR_SEPARATOR         '/'
#  define PATH_SEPARATOR        ';'
#else
#  define IS_DIR_SEPARATOR(x) (x=='/')
#  define DIR_SEPARATOR '/'
#  define PATH_SEPARATOR        ':'
#endif

#define ECL_ARCHITECTURE "X86_64"

#ifdef ECL_AVOID_FPE_H
# define ecl_detect_fpe()
#else
# include "arch/fpe_x86.c"
#endif

#ifdef ECL_INCLUDE_MATH_H
# include <math.h>
# ifdef _MSC_VER
/* #  undef complex */
#  define signbit(x) (copysign(1.0,(x)))
# endif
# ifndef isfinite
#  error "Function isfinite() is missing"
# endif
# ifndef signbit
#  ifndef ECL_SIGNED_ZERO
#   define signbit(x) ((x) < 0)
#  else
#   ifdef HAVE_COPYSIGN
#    define signbit(x) (copysign(1.0,(x)) < 0)
#   else 
     /* Fall back to no signed zero */
#    undef \
     ECL_SIGNED_ZERO
#    define signbit(x) ((x) < 0)
#   endif
#  endif
# endif
/*
 * GCC fails to compile the following code
 * if (f == 0.0) { if (signbit(f)) ... }
 */
# if defined(__sun__) && defined(__GNUC__)
#  undef \
   signbit /* Avoid autoconf removing this */
#  define signbit(x) (copysign(1.0,(x)) < 0)
# endif
#endif

#if defined(HAVE_LIBFFI) && defined(ECL_INCLUDE_FFI_H)
#include "ecl/ffi.h"
#endif

/* Can we determine and set the stack size at runtime? */
#define ECL_CAN_SET_STACK_SIZE /**/

#if defined(ECL_CAN_SET_STACK_SIZE)
#define ECL_DEFAULT_C_STACK_SIZE 0 /* Use the stack size provided by the OS */
#else
#define ECL_DEFAULT_C_STACK_SIZE 1048576
#endif
