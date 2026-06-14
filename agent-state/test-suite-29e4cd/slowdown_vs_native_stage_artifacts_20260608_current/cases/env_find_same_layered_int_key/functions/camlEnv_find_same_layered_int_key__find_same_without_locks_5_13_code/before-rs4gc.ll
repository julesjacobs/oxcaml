define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlEnv_find_same_layered_int_key__find_same_without_locks_5_13_code"(i64 %0, i64 %1, ptr addrspace(1) nocapture readonly %2, ptr addrspace(1) nocapture readonly %3) #3 gc "oxcaml" personality ptr @"\01_caml_llvm_eh_personality" {
L1:
  %4 = add i64 %0, 40
  %5 = inttoptr i64 %4 to ptr
  %6 = load i64, ptr %5, align 4
  %7 = add i64 %6, 408
  %8 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #6
  %.not = icmp ult i64 %8, %7
  br i1 %.not, label %L179, label %L147.preheader, !prof !1

L179:                                             ; preds = %L1
  %9 = tail call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_llvm_call_realloc_stack"(i64 %0, i64 %1, i64 38) #7
  %10 = extractvalue { { i64, i64 }, {} } %9, 0, 0
  %11 = extractvalue { { i64, i64 }, {} } %9, 0, 1
  br label %L147.preheader

L147.preheader:                                   ; preds = %L179, %L1
  %alloc.1.ph = phi i64 [ %1, %L1 ], [ %11, %L179 ]
  %ds.1.ph = phi i64 [ %0, %L1 ], [ %10, %L179 ]
  br label %L147

L147:                                             ; preds = %L147.preheader, %L167
  %.045.in = phi ptr addrspace(1) [ %25, %L167 ], [ %3, %L147.preheader ]
  %alloc.1 = phi i64 [ %19, %L167 ], [ %alloc.1.ph, %L147.preheader ]
  %ds.1 = phi i64 [ %20, %L167 ], [ %ds.1.ph, %L147.preheader ]
  %.0.in = getelementptr i8, ptr addrspace(1) %.045.in, i64 8
  %.0 = load ptr addrspace(1), ptr addrspace(1) %.0.in, align 8
  %.045 = load ptr addrspace(1), ptr addrspace(1) %.045.in, align 8
  %12 = add i64 %ds.1, 64
  %13 = inttoptr i64 %12 to ptr
  %14 = load i64, ptr %13, align 4
  tail call void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@"\01_camlEnv_find_same_layered_int_key__find_same_without_locks_5_13_code", %L182))
  %15 = invoke oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlEnv_find_same_layered_int_key__ident_find_same_4_12_code"(i64 %ds.1, i64 %alloc.1, ptr addrspace(1) %2, ptr addrspace(1) %.045) #9 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 27, i64 0, i64 6, i64 36, i64 0, i64 36, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 53, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 7235942, i64 7561060, i64 6647137, i64 6911839, i64 7301236, i64 6255733, i64 6516588, i64 29547) ]
          to label %L183 unwind label %L182

L182:                                             ; preds = %L147
  %16 = landingpad token
          cleanup
  %17 = tail call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %18 = extractvalue { ptr addrspace(1), i64, i64, i64 } %17, 0
  %19 = extractvalue { ptr addrspace(1), i64, i64, i64 } %17, 2
  %20 = extractvalue { ptr addrspace(1), i64, i64, i64 } %17, 3
  %21 = add i64 %20, 64
  %22 = inttoptr i64 %21 to ptr
  store i64 %14, ptr %22, align 4
  %or.cond.not = icmp eq ptr addrspace(1) %18, inttoptr (i64 ptrtoint (ptr @"\01_camlEnv_find_same_layered_int_key__Not_found_same292" to i64) to ptr addrspace(1))
  br i1 %or.cond.not, label %L163, label %L174

L183:                                             ; preds = %L147
  tail call void @llvm.aarch64.oxcaml.pop.trap()
  ret { { i64, i64 }, { i64 } } %15

L163:                                             ; preds = %L182
  %23 = ptrtoint ptr addrspace(1) %.0 to i64
  %24 = and i64 %23, 1
  %.not47 = icmp eq i64 %24, 0
  br i1 %.not47, label %L167, label %L165

L165:                                             ; preds = %L163
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 ptrtoint (ptr @"\01_camlEnv_find_same_layered_int_key__Not_found_same292" to i64))
  unreachable

L167:                                             ; preds = %L163
  %25 = load ptr addrspace(1), ptr addrspace(1) %.0, align 8
  br label %L147

L174:                                             ; preds = %L182
  %26 = extractvalue { ptr addrspace(1), i64, i64, i64 } %17, 0
  %27 = ptrtoint ptr addrspace(1) %26 to i64
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 %27)
  unreachable
}
