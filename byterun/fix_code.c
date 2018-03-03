/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Handling of blocks of bytecode (endianness switch, threading). */

#include "caml/config.h"

#ifdef HAS_UNISTD
#include <unistd.h>
#else
#include <io.h>
#endif

#include "caml/debugger.h"
#include "caml/fix_code.h"
#include "caml/instruct.h"
#include "caml/intext.h"
#include "caml/md5.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/reverse.h"

code_t caml_start_code;
asize_t caml_code_size;
unsigned char * caml_saved_code;

/* Read the main bytecode block from a file */

void caml_init_code_fragments(void) {
  struct code_fragment * cf;
  /* Register the code in the table of code fragments */
  cf = caml_stat_alloc(sizeof(struct code_fragment));
  cf->code_start = (char *) caml_start_code;
  cf->code_end = (char *) caml_start_code + caml_code_size;
  caml_md5_block(cf->digest, caml_start_code, caml_code_size);
  cf->digest_computed = 1;
  caml_ext_table_init(&caml_code_fragments_table, 8);
  caml_ext_table_add(&caml_code_fragments_table, cf);
}

void caml_load_code(int fd, asize_t len)
{
  int i;

  caml_code_size = len;
  caml_start_code = (code_t) caml_stat_alloc(caml_code_size);
  if (read(fd, (char *) caml_start_code, caml_code_size) != caml_code_size)
    caml_fatal_error("Fatal error: truncated bytecode file.\n");
  caml_init_code_fragments();
  /* Prepare the code for execution */
#ifdef ARCH_BIG_ENDIAN
#error "Big endian"
#endif
  if (caml_debugger_in_use) {
    len /= sizeof(opcode_t);
    caml_saved_code = (unsigned char *) caml_stat_alloc(len);
    for (i = 0; i < len; i++) caml_saved_code[i] = caml_start_code[i];
  }
}



int* caml_init_opcode_nargs()
{
  return NULL;
}

int caml_is_instruction(opcode_t instr1, opcode_t instr2)
{
#ifdef THREADED_CODE
  return instr1 == (opcode_t)(caml_instr_table[instr2] - caml_instr_base);
#else
  return instr1 == instr2;
#endif
}
