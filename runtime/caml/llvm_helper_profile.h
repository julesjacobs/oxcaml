#ifndef CAML_LLVM_HELPER_PROFILE_H
#define CAML_LLVM_HELPER_PROFILE_H

#ifdef CAML_INTERNALS

#include "camlatomic.h"
#include "mlvalues.h"

struct caml_llvm_helper_profile {
  atomic_uintnat initialize_total;
  atomic_uintnat initialize_dest_young;
  atomic_uintnat initialize_val_immediate;
  atomic_uintnat initialize_val_old;
  atomic_uintnat initialize_old_to_young;

  atomic_uintnat modify_total;
  atomic_uintnat modify_dest_young;
  atomic_uintnat modify_old_immediate;
  atomic_uintnat modify_old_young;
  atomic_uintnat modify_old_old_no_mark;
  atomic_uintnat modify_old_old_marking;
  atomic_uintnat modify_new_immediate;
  atomic_uintnat modify_new_old;
  atomic_uintnat modify_new_young_record_ref;

  atomic_uintnat modify_local_total;
  atomic_uintnat modify_local_not_markable;
  atomic_uintnat modify_local_fallback;

  atomic_uintnat string_equal_total;
  atomic_uintnat string_equal_pointer_equal;
  atomic_uintnat string_equal_size_mismatch;
  atomic_uintnat string_equal_content_loop;
  atomic_uintnat string_equal_content_mismatch;
  atomic_uintnat string_equal_content_equal;

  atomic_uintnat string_notequal_total;
  atomic_uintnat string_compare_total;
  atomic_uintnat string_compare_pointer_equal;
  atomic_uintnat string_compare_memcmp;
  atomic_uintnat string_compare_memcmp_lt;
  atomic_uintnat string_compare_memcmp_gt;
  atomic_uintnat string_compare_len_lt;
  atomic_uintnat string_compare_len_gt;
  atomic_uintnat string_compare_equal;
  atomic_uintnat string_compare_min_len_0;
  atomic_uintnat string_compare_min_len_1;
  atomic_uintnat string_compare_min_len_2;
  atomic_uintnat string_compare_min_len_3;
  atomic_uintnat string_compare_min_len_4;
  atomic_uintnat string_compare_min_len_5_7;
  atomic_uintnat string_compare_min_len_8_15;
  atomic_uintnat string_compare_min_len_16_31;
  atomic_uintnat string_compare_min_len_32_63;
  atomic_uintnat string_compare_min_len_64_127;
  atomic_uintnat string_compare_min_len_128_plus;

  atomic_uintnat bytes_equal_total;
  atomic_uintnat bytes_compare_total;

  atomic_uintnat blit_bytes_total;
  atomic_uintnat blit_bytes_bytes;
  atomic_uintnat blit_string_total;
  atomic_uintnat blit_string_bytes;
  atomic_uintnat fill_bytes_total;
  atomic_uintnat fill_bytes_bytes;

  atomic_uintnat obj_tag_total;
};

CAMLextern int caml_llvm_helper_profile_enabled;
CAMLextern struct caml_llvm_helper_profile caml_llvm_helper_profile;

void caml_llvm_helper_profile_init(void);
void caml_llvm_helper_profile_dump(void);
void caml_llvm_helper_profile_record_initialize(volatile value *fp, value val);
void caml_llvm_helper_profile_record_modify(volatile value *fp, value val);

#define CAML_LLVM_HELPER_PROFILE_INC(field) do {                         \
  if (caml_llvm_helper_profile_enabled)                                  \
    atomic_fetch_add(&caml_llvm_helper_profile.field, 1);                \
} while (0)

#define CAML_LLVM_HELPER_PROFILE_ADD(field, amount) do {                 \
  if (caml_llvm_helper_profile_enabled)                                  \
    atomic_fetch_add(&caml_llvm_helper_profile.field, (uintnat)(amount));\
} while (0)

#else

#define CAML_LLVM_HELPER_PROFILE_INC(field) ((void)0)
#define CAML_LLVM_HELPER_PROFILE_ADD(field, amount) ((void)0)

#endif /* CAML_INTERNALS */

#endif /* CAML_LLVM_HELPER_PROFILE_H */
