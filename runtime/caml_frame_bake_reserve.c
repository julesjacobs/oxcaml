/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   C1 frame-table bake (bake-frame, task #63/#11): the opt-in reserve.   */
/*                                                                        */
/**************************************************************************/

/* This object is deliberately NOT part of libasmrun.a. It is installed
   beside the runtime libraries and linked ONLY into binaries that opt in to
   the baked frame-descriptor table (the compiler). frame_descriptors.c holds
   a WEAK reference to [caml_baked_frametable]; a binary linked without this
   object resolves that symbol to NULL and rebuilds the table at startup, at
   zero size/runtime cost. A binary linked WITH this object carries a
   page-aligned PROGBITS section that the post-link bake step (objcopy
   --update-section, same size) fills in place. Keeping the reserve here means
   only opted-in executables pay the ~8 MB, instead of every native binary.

   The size MUST equal FRAME_BAKED_RESERVE in frame_descriptors.c:
   4096 (header page) + (1<<20) slots * sizeof(frame_descr *). Kept in sync by
   the identical arithmetic below; frame_baked_load re-checks cap-fits so a
   mismatch fails closed to the rebuild path rather than corrupting memory. */

#define FRAME_BAKED_BODY_OFF 4096u
#define FRAME_BAKED_CAP_MAX (1u << 20)
#define FRAME_BAKED_RESERVE \
  (FRAME_BAKED_BODY_OFF + FRAME_BAKED_CAP_MAX * sizeof(void *))

#if defined(__ELF__)
__attribute__((section(".caml_frametable_baked"), aligned(4096)))
unsigned char caml_baked_frametable[FRAME_BAKED_RESERVE];
#else
/* Mach-O/PE need different section syntax and frame_descriptors.c never
   references the baked table there; keep this translation unit empty so
   non-ELF builds that link the reserve object still compile. */
typedef int caml_frame_bake_reserve_is_elf_only;
#endif
