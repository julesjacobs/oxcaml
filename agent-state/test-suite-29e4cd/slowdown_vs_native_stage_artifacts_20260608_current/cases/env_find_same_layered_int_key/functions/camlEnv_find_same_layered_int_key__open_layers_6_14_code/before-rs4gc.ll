define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlEnv_find_same_layered_int_key__open_layers_6_14_code"(i64 %0, i64 %1, i64 %2, ptr addrspace(1) %3) #1 gc "oxcaml" {
L1:
  %or.cond.not = icmp eq i64 %2, 1
  br i1 %or.cond.not, label %common.ret, label %L193

common.ret:                                       ; preds = %L1, %L221
  %.pn76 = phi i64 [ %ds.4, %L221 ], [ %0, %L1 ]
  %.pn74 = phi i64 [ %alloc.4, %L221 ], [ %1, %L1 ]
  %.pn72 = phi ptr addrspace(1) [ %47, %L221 ], [ %3, %L1 ]
  %.pn73 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } undef, i64 %.pn76, 0, 0
  %.pn = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %.pn73, i64 %.pn74, 0, 1
  %common.ret.op = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %.pn, ptr addrspace(1) %.pn72, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %common.ret.op

L193:                                             ; preds = %L1
  %4 = add i64 %0, 40
  %5 = inttoptr i64 %4 to ptr
  %6 = load i64, ptr %5, align 4
  %7 = add i64 %6, 376
  %8 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #6
  %.not = icmp ult i64 %8, %7
  br i1 %.not, label %L213, label %L214, !prof !1

L213:                                             ; preds = %L193
  %9 = tail call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_llvm_call_realloc_stack"(i64 %0, i64 %1, i64 34) #7
  %10 = extractvalue { { i64, i64 }, {} } %9, 0, 0
  %11 = extractvalue { { i64, i64 }, {} } %9, 0, 1
  br label %L214

L214:                                             ; preds = %L213, %L193
  %alloc.0 = phi i64 [ %1, %L193 ], [ %11, %L213 ]
  %ds.0 = phi i64 [ %0, %L193 ], [ %10, %L213 ]
  %12 = and i64 %2, 3
  %or.cond3.not = icmp eq i64 %12, 1
  %13 = add i64 %2, -2
  br i1 %or.cond3.not, label %L196, label %L201

L196:                                             ; preds = %L214
  %14 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlEnv_find_same_layered_int_key__open_layers_6_14_code"(i64 %ds.0, i64 %alloc.0, i64 %13, ptr addrspace(1) %3) #8 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 40, i64 0, i64 18, i64 44, i64 0, i64 44, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 41, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 6647919, i64 7102318, i64 6650209, i64 29554) ]
  %15 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %14, 0, 0
  %16 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %14, 0, 1
  %17 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %14, 1, 0
  %18 = add i64 %16, -16
  %19 = inttoptr i64 %15 to ptr
  %20 = load i64, ptr %19, align 4
  %.not77 = icmp ugt i64 %20, %18
  br i1 %.not77, label %L216, label %L217, !prof !1

L216:                                             ; preds = %L196
  %21 = call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_call_gc"(i64 %15, i64 %18) #10 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 2, i64 1, i64 40, i64 0, i64 13, i64 44, i64 0, i64 44, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 41, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 6647919, i64 7102318, i64 6650209, i64 29554) ]
  %22 = extractvalue { { i64, i64 }, {} } %21, 0, 0
  %23 = extractvalue { { i64, i64 }, {} } %21, 0, 1
  br label %L217

L217:                                             ; preds = %L216, %L196
  %alloc.1 = phi i64 [ %18, %L196 ], [ %23, %L216 ]
  %ds.1 = phi i64 [ %15, %L196 ], [ %22, %L216 ]
  %24 = add i64 %alloc.1, 8
  %25 = inttoptr i64 %24 to ptr addrspace(1)
  %26 = getelementptr i8, ptr addrspace(1) %25, i64 -8
  store volatile i64 1024, ptr addrspace(1) %26, align 4
  store ptr addrspace(1) %17, ptr addrspace(1) %25, align 8
  br label %L207

L201:                                             ; preds = %L214
  %27 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlEnv_find_same_layered_int_key__open_layers_6_14_code"(i64 %ds.0, i64 %alloc.0, i64 %13, ptr addrspace(1) %3) #8 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 41, i64 0, i64 18, i64 44, i64 0, i64 44, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 41, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 6647919, i64 7102318, i64 6650209, i64 29554) ]
  %28 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %27, 0, 0
  %29 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %27, 0, 1
  %30 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %27, 1, 0
  %31 = add i64 %29, -16
  %32 = inttoptr i64 %28 to ptr
  %33 = load i64, ptr %32, align 4
  %.not79 = icmp ugt i64 %33, %31
  br i1 %.not79, label %L218, label %L219, !prof !1

L218:                                             ; preds = %L201
  %34 = call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_call_gc"(i64 %28, i64 %31) #10 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 2, i64 1, i64 41, i64 0, i64 13, i64 44, i64 0, i64 44, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 41, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 6647919, i64 7102318, i64 6650209, i64 29554) ]
  %35 = extractvalue { { i64, i64 }, {} } %34, 0, 0
  %36 = extractvalue { { i64, i64 }, {} } %34, 0, 1
  br label %L219

L219:                                             ; preds = %L218, %L201
  %alloc.2 = phi i64 [ %31, %L201 ], [ %36, %L218 ]
  %ds.2 = phi i64 [ %28, %L201 ], [ %35, %L218 ]
  %37 = add i64 %alloc.2, 8
  %38 = inttoptr i64 %37 to ptr addrspace(1)
  %39 = getelementptr i8, ptr addrspace(1) %38, i64 -8
  store volatile i64 1025, ptr addrspace(1) %39, align 4
  store ptr addrspace(1) %30, ptr addrspace(1) %38, align 8
  br label %L207

L207:                                             ; preds = %L219, %L217
  %.0 = phi ptr addrspace(1) [ %38, %L219 ], [ %25, %L217 ]
  %alloc.3 = phi i64 [ %alloc.2, %L219 ], [ %alloc.1, %L217 ]
  %ds.3 = phi i64 [ %ds.2, %L219 ], [ %ds.1, %L217 ]
  %40 = add i64 %alloc.3, -24
  %41 = inttoptr i64 %ds.3 to ptr
  %42 = load i64, ptr %41, align 4
  %.not78 = icmp ugt i64 %42, %40
  br i1 %.not78, label %L220, label %L221, !prof !1

L220:                                             ; preds = %L207
  %43 = call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_call_gc"(i64 %ds.3, i64 %40) #11 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 1, i64 37, i64 4, i64 4, i64 46, i64 106, i64 152, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 41, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 6647919, i64 7102318, i64 6650209, i64 29554) ]
  %44 = extractvalue { { i64, i64 }, {} } %43, 0, 0
  %45 = extractvalue { { i64, i64 }, {} } %43, 0, 1
  br label %L221

L221:                                             ; preds = %L220, %L207
  %alloc.4 = phi i64 [ %40, %L207 ], [ %45, %L220 ]
  %ds.4 = phi i64 [ %ds.3, %L207 ], [ %44, %L220 ]
  %46 = add i64 %alloc.4, 8
  %47 = inttoptr i64 %46 to ptr addrspace(1)
  %48 = getelementptr i8, ptr addrspace(1) %47, i64 -8
  store volatile i64 2048, ptr addrspace(1) %48, align 4
  store volatile i64 1, ptr addrspace(1) %47, align 4
  %49 = getelementptr i8, ptr addrspace(1) %47, i64 8
  store ptr addrspace(1) %.0, ptr addrspace(1) %49, align 8
  br label %common.ret
}
