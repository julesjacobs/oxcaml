define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlEnv_find_same_layered_int_key__find_same_without_locks_5_13_code"(i64 %0, i64 %1, ptr addrspace(1) nocapture %2, ptr addrspace(1) nocapture %3) #2 gc "oxcaml" personality ptr @"\01_caml_llvm_eh_personality" {
L1:
  %.0.exnroot = alloca ptr addrspace(1), align 8
  store volatile ptr addrspace(1) inttoptr (i64 1 to ptr addrspace(1)), ptr %.0.exnroot, align 8
  %exnroot = alloca ptr addrspace(1), align 8
  store volatile ptr addrspace(1) inttoptr (i64 1 to ptr addrspace(1)), ptr %exnroot, align 8
  %4 = add i64 %0, 40
  %5 = inttoptr i64 %4 to ptr
  %6 = load i64, ptr %5, align 4
  %7 = add i64 %6, 408
  %8 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #6
  %.not = icmp ult i64 %8, %7
  br i1 %.not, label %L179, label %L147.preheader, !prof !1

L179:                                             ; preds = %L1
  %statepoint_token = tail call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64, i64)) @"\01_caml_llvm_call_realloc_stack", i32 3, i32 0, i64 %0, i64 %1, i64 38, i32 0, i32 0) #7 [ "gc-live"(ptr addrspace(1) %3, ptr addrspace(1) %2, ptr %.0.exnroot, ptr %exnroot) ]
  %9 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token)
  %10 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0) ; (%3, %3)
  %11 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 1, i32 1) ; (%2, %2)
  %12 = extractvalue { { i64, i64 }, {} } %9, 0, 0
  %13 = extractvalue { { i64, i64 }, {} } %9, 0, 1
  br label %L147.preheader

L147.preheader:                                   ; preds = %L179, %L1
  %.068 = phi ptr addrspace(1) [ %11, %L179 ], [ %2, %L1 ]
  %.067 = phi ptr addrspace(1) [ %10, %L179 ], [ %3, %L1 ]
  %alloc.1.ph = phi i64 [ %1, %L1 ], [ %13, %L179 ]
  %ds.1.ph = phi i64 [ %0, %L1 ], [ %12, %L179 ]
  br label %L147

L147:                                             ; preds = %L147.preheader, %L167
  %.045.in = phi ptr addrspace(1) [ %24, %L167 ], [ %.067, %L147.preheader ]
  %alloc.1 = phi i64 [ %gcagg63, %L167 ], [ %alloc.1.ph, %L147.preheader ]
  %ds.1 = phi i64 [ %gcagg64, %L167 ], [ %ds.1.ph, %L147.preheader ]
  %exnroot.select = phi ptr addrspace(1) [ %.068, %L147.preheader ], [ %exnroot.load, %L167 ]
  %.0.in.remat = getelementptr i8, ptr addrspace(1) %.045.in, i64 8
  %.0 = load ptr addrspace(1), ptr addrspace(1) %.0.in.remat, align 8
  %.045 = load ptr addrspace(1), ptr addrspace(1) %.045.in, align 8
  %14 = add i64 %ds.1, 64
  %15 = inttoptr i64 %14 to ptr
  %16 = load i64, ptr %15, align 4
  tail call void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@"\01_camlEnv_find_same_layered_int_key__find_same_without_locks_5_13_code", %L182))
  store volatile ptr addrspace(1) %.0, ptr %.0.exnroot, align 8
  store volatile ptr addrspace(1) %exnroot.select, ptr %exnroot, align 8
  %statepoint_token66 = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 18, i32 0, ptr elementtype({ { i64, i64 }, { i64 } } (i64, i64, ptr addrspace(1), ptr addrspace(1))) @"\01_camlEnv_find_same_layered_int_key__ident_find_same_4_12_code", i32 4, i32 0, i64 %ds.1, i64 %alloc.1, ptr addrspace(1) %exnroot.select, ptr addrspace(1) %.045, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 27, i64 0, i64 6, i64 36, i64 0, i64 36, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 53, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 7235942, i64 7561060, i64 6647137, i64 6911839, i64 7301236, i64 6255733, i64 6516588, i64 29547), "gc-live"(ptr %.0.exnroot, ptr %exnroot) ]
          to label %L183 unwind label %L182

L182:                                             ; preds = %L147
  %17 = landingpad token
          cleanup
  %18 = tail call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %.0.exnroot.load = load volatile ptr addrspace(1), ptr %.0.exnroot, align 8
  %gcagg = extractvalue { ptr addrspace(1), i64, i64, i64 } %18, 0
  %gcagg62 = extractvalue { ptr addrspace(1), i64, i64, i64 } %18, 1
  %gcagg63 = extractvalue { ptr addrspace(1), i64, i64, i64 } %18, 2
  %gcagg64 = extractvalue { ptr addrspace(1), i64, i64, i64 } %18, 3
  %19 = add i64 %gcagg64, 64
  %20 = inttoptr i64 %19 to ptr
  store i64 %16, ptr %20, align 4
  %or.cond.not = icmp eq ptr addrspace(1) %gcagg, inttoptr (i64 ptrtoint (ptr @"\01_camlEnv_find_same_layered_int_key__Not_found_same292" to i64) to ptr addrspace(1))
  br i1 %or.cond.not, label %L163, label %L174

L183:                                             ; preds = %L147
  %21 = call { { i64, i64 }, { i64 } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_i64ss(token %statepoint_token66)
  tail call void @llvm.aarch64.oxcaml.pop.trap()
  ret { { i64, i64 }, { i64 } } %21

L163:                                             ; preds = %L182
  %22 = ptrtoint ptr addrspace(1) %.0.exnroot.load to i64
  %23 = and i64 %22, 1
  %.not47 = icmp eq i64 %23, 0
  br i1 %.not47, label %L167, label %L165

L165:                                             ; preds = %L163
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 ptrtoint (ptr @"\01_camlEnv_find_same_layered_int_key__Not_found_same292" to i64))
  unreachable

L167:                                             ; preds = %L163
  %24 = load ptr addrspace(1), ptr addrspace(1) %.0.exnroot.load, align 8
  %exnroot.load = load volatile ptr addrspace(1), ptr %exnroot, align 8
  br label %L147

L174:                                             ; preds = %L182
  %25 = ptrtoint ptr addrspace(1) %gcagg to i64
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 %25)
  unreachable
}
