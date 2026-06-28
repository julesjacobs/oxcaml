/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   Copyright 2026 Jane Street Group LLC                                 */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <stdint.h>

#include "caml/config.h"
#include "caml/misc.h"

typedef int caml_llvm_unwind_action;
typedef int caml_llvm_unwind_reason_code;

struct caml_llvm_unwind_exception;
struct caml_llvm_unwind_context;

#define CAML_LLVM_URC_CONTINUE_UNWIND 8

CAMLexport caml_llvm_unwind_reason_code caml_llvm_eh_personality(
    int version, caml_llvm_unwind_action actions, uint64_t exception_class,
    struct caml_llvm_unwind_exception *exception_object,
    struct caml_llvm_unwind_context *context)
{
  (void) version;
  (void) actions;
  (void) exception_class;
  (void) exception_object;
  (void) context;
  return CAML_LLVM_URC_CONTINUE_UNWIND;
}
