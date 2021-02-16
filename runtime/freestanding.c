/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            */
/*                                                                        */
/*   Copyright 2001 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Unix-specific stuff */

#define _GNU_SOURCE
           /* Helps finding RTLD_DEFAULT in glibc */
           /* also secure_getenv */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#include "caml/config.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/osdeps.h"
#include "caml/signals.h"
#include "caml/sys.h"
#include "caml/io.h"
#include "caml/alloc.h"

#ifndef S_ISREG
#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)
#endif

#ifndef EINTR
#define EINTR (-1)
#endif
#ifndef EAGAIN
#define EAGAIN (-1)
#endif
#ifndef EWOULDBLOCK
#define EWOULDBLOCK (-1)
#endif

int caml_read_fd(int fd, int flags, void * buf, int n)
{
  int retcode;
  do {
    caml_enter_blocking_section();
    retcode = read(fd, buf, n);
    caml_leave_blocking_section();
  } while (retcode == -1 && errno == EINTR);
  if (retcode == -1) caml_sys_io_error(NO_ARG);
  return retcode;
}

int caml_write_fd(int fd, int flags, void * buf, int n)
{
  int retcode;
 again:
#if defined(NATIVE_CODE) && defined(WITH_SPACETIME)
  if (flags & CHANNEL_FLAG_BLOCKING_WRITE) {
    retcode = write(fd, buf, n);
  } else {
#endif
  caml_enter_blocking_section();
  retcode = write(fd, buf, n);
  caml_leave_blocking_section();
#if defined(NATIVE_CODE) && defined(WITH_SPACETIME)
  }
#endif
  if (retcode == -1) {
    if (errno == EINTR) goto again;
    if ((errno == EAGAIN || errno == EWOULDBLOCK) && n > 1) {
      /* We couldn't do a partial write here, probably because
         n <= PIPE_BUF and POSIX says that writes of less than
         PIPE_BUF characters must be atomic.
         We first try again with a partial write of 1 character.
         If that fails too, we'll return an error code. */
      n = 1; goto again;
    }
  }
  if (retcode == -1) caml_sys_io_error(NO_ARG);
  CAMLassert (retcode > 0);
  return retcode;
}

caml_stat_string caml_decompose_path(struct ext_table * tbl, char * path)
{
  return NULL;
}

caml_stat_string caml_search_in_path(struct ext_table * path, const char * name)
{
  return caml_stat_strdup(name);
}


caml_stat_string caml_search_exe_in_path(const char * name)
{
  caml_stat_string res;
  return res;
}

caml_stat_string caml_search_dll_in_path(struct ext_table * path,
                                         const char * name)
{
  caml_stat_string res;
  return res;
}

void * caml_dlopen(char * libname, int for_execution, int global)
{
  return NULL;
}

void caml_dlclose(void * handle)
{
}

void * caml_dlsym(void * handle, const char * name)
{
  return NULL;
}

void * caml_globalsym(const char * name)
{
  return NULL;
}

char * caml_dlerror(void)
{
  return "dynamic loading not supported on this platform";
}

/* Add to [contents] the (short) names of the files contained in
   the directory named [dirname].  No entries are added for [.] and [..].
   Return 0 on success, -1 on error; set errno in the case of error. */

CAMLexport int caml_read_directory(char * dirname, struct ext_table * contents)
{
    return -1;
}

/* Recover executable name from /proc/self/exe if possible */

char * caml_executable_name(void)
{
    return "ocaml";
}

char *caml_secure_getenv (char const *var)
{
    return "";
}

int caml_num_rows_fd(int fd)
{
    return -1;
}
