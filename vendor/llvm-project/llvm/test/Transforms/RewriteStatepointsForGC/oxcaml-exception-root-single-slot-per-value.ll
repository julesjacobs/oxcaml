; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify < %s | FileCheck %s

; A handler-live value that plays several roles (live into the handler,
; live at the retry rejoin, live at the loop exit) gets exactly one root
; slot, stored once at its definition.  This is the shape of boyer's
; rewrite_with_lemmas, which previously materialized three slots for the
; same term value and re-stored all of them before every protected invoke.

target triple = "arm64-apple-macosx"

declare oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @callee(i64, i64, ptr addrspace(1), ptr addrspace(1))
declare { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare ptr @personality(...)

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @retry_loop_single_slot(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %term,
    ptr addrspace(1) %lemmas)
    gc "oxcaml" personality ptr @personality {
; CHECK-LABEL: define {{.*}} @retry_loop_single_slot(
; The lemma slot's init store is pruned: its defining store dominates every
; load and registered statepoint of the slot.
; CHECK: entry:
; CHECK-NEXT: %lemmas.iter.exnroot = alloca ptr addrspace(1), align 8
; CHECK-NEXT: %term.exnroot = alloca ptr addrspace(1), align 8
; CHECK-NEXT: store ptr addrspace(1) %term, ptr %term.exnroot, align 8
; CHECK-NOT: = alloca
; CHECK-NOT: store ptr addrspace(1) inttoptr (i64 1 to ptr addrspace(1)), ptr %lemmas.iter.exnroot
entry:
  br label %loop

loop:
; The loop-carried lemma list is stored at its definition, once per
; iteration; the term argument's slot was stored once at entry.  No store
; precedes the invoke.
; CHECK: loop:
; CHECK: store ptr addrspace(1) %lemmas.iter, ptr %lemmas.iter.exnroot, align 8
; CHECK-NOT: store volatile
; CHECK: %statepoint_token = invoke {{.*}} [ "gc-live"(ptr %lemmas.iter.exnroot, ptr %term.exnroot) ]
  %lemmas.iter = phi ptr addrspace(1) [ %lemmas, %entry ], [ %next, %retry ]
  %call = invoke oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } }
      @callee(i64 %ds, i64 %alloc, ptr addrspace(1) %term, ptr addrspace(1) %lemmas.iter)
      "statepoint-id"="18"
      to label %normal unwind label %recover

normal:
  ret { { i64, i64 }, { ptr addrspace(1) } } %call

recover:
; Both values are reloaded from their single slots at the recovery entry.
; CHECK: recover:
; CHECK-DAG: %lemmas.iter.exnroot.load = load ptr addrspace(1), ptr %lemmas.iter.exnroot, align 8
; CHECK-DAG: %term.exnroot.load = load ptr addrspace(1), ptr %term.exnroot, align 8
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %tail.addr = getelementptr i8, ptr addrspace(1) %lemmas.iter, i64 8
  %next = load ptr addrspace(1), ptr addrspace(1) %tail.addr, align 8
  %done = icmp eq ptr addrspace(1) %next, inttoptr (i64 1 to ptr addrspace(1))
  br i1 %done, label %exit, label %retry

retry:
  br label %loop

exit:
; The exit path returns the current term through the same slot.
; CHECK: exit:
; CHECK: insertvalue { { i64, i64 }, { ptr addrspace(1) } } %{{.*}}, ptr addrspace(1) %term.exnroot.load, 1, 0
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %ds, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0, i64 %alloc, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1, ptr addrspace(1) %term, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}
