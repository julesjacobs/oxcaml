define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlLayered_try_raise_hit_only__open_layers_6_14_code"(i64 %0, i64 %1, i64 %2, ptr addrspace(1) %3) #1 gc "oxcaml" {
L1:
  %or.cond.not = icmp eq i64 %2, 1
  br i1 %or.cond.not, label %common.ret, label %L181

common.ret:                                       ; preds = %L1, %L209
  %.pn76 = phi i64 [ %ds.4, %L209 ], [ %0, %L1 ]
  %.pn74 = phi i64 [ %alloc.4, %L209 ], [ %1, %L1 ]
  %.pn72 = phi ptr addrspace(1) [ %47, %L209 ], [ %3, %L1 ]
  %.pn73 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } undef, i64 %.pn76, 0, 0
  %.pn = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %.pn73, i64 %.pn74, 0, 1
  %common.ret.op = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %.pn, ptr addrspace(1) %.pn72, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %common.ret.op

L181:                                             ; preds = %L1
  %4 = add i64 %0, 40
  %5 = inttoptr i64 %4 to ptr
  %6 = load i64, ptr %5, align 4
  %7 = add i64 %6, 376
  %8 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #6
  %.not = icmp ult i64 %8, %7
  br i1 %.not, label %L201, label %L202, !prof !1

L201:                                             ; preds = %L181
  %9 = tail call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_llvm_call_realloc_stack"(i64 %0, i64 %1, i64 34) #7
  %10 = extractvalue { { i64, i64 }, {} } %9, 0, 0
  %11 = extractvalue { { i64, i64 }, {} } %9, 0, 1
  br label %L202

L202:                                             ; preds = %L201, %L181
  %alloc.0 = phi i64 [ %1, %L181 ], [ %11, %L201 ]
  %ds.0 = phi i64 [ %0, %L181 ], [ %10, %L201 ]
  %12 = and i64 %2, 3
  %or.cond3.not = icmp eq i64 %12, 1
  %13 = add i64 %2, -2
  br i1 %or.cond3.not, label %L184, label %L189

L184:                                             ; preds = %L202
  %14 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlLayered_try_raise_hit_only__open_layers_6_14_code"(i64 %ds.0, i64 %alloc.0, i64 %13, ptr addrspace(1) %3) #8 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 36, i64 0, i64 18, i64 44, i64 0, i64 44, i64 29, i64 7954796, i64 6648421, i64 7626596, i64 6257010, i64 6906226, i64 6251891, i64 7629160, i64 7237471, i64 3045740, i64 27757, i64 38, i64 7954764, i64 6648421, i64 7626596, i64 6257010, i64 6906226, i64 6251891, i64 7629160, i64 7237471, i64 3045740, i64 6647919, i64 7102318, i64 6650209, i64 29554) ]
  %15 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %14, 0, 0
  %16 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %14, 0, 1
  %17 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %14, 1, 0
  %18 = add i64 %16, -16
  %19 = inttoptr i64 %15 to ptr
  %20 = load i64, ptr %19, align 4
  %.not77 = icmp ugt i64 %20, %18
  br i1 %.not77, label %L204, label %L205, !prof !1

L204:                                             ; preds = %L184
  %21 = call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_call_gc"(i64 %15, i64 %18) #10 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 2, i64 1, i64 36, i64 0, i64 13, i64 44, i64 0, i64 44, i64 29, i64 7954796, i64 6648421, i64 7626596, i64 6257010, i64 6906226, i64 6251891, i64 7629160, i64 7237471, i64 3045740, i64 27757, i64 38, i64 7954764, i64 6648421, i64 7626596, i64 6257010, i64 6906226, i64 6251891, i64 7629160, i64 7237471, i64 3045740, i64 6647919, i64 7102318, i64 6650209, i64 29554) ]
  %22 = extractvalue { { i64, i64 }, {} } %21, 0, 0
  %23 = extractvalue { { i64, i64 }, {} } %21, 0, 1
  br label %L205

L205:                                             ; preds = %L204, %L184
  %alloc.1 = phi i64 [ %18, %L184 ], [ %23, %L204 ]
  %ds.1 = phi i64 [ %15, %L184 ], [ %22, %L204 ]
  %24 = add i64 %alloc.1, 8
  %25 = inttoptr i64 %24 to ptr addrspace(1)
  %26 = getelementptr i8, ptr addrspace(1) %25, i64 -8
  store volatile i64 1024, ptr addrspace(1) %26, align 4
  store ptr addrspace(1) %17, ptr addrspace(1) %25, align 8
  br label %L195

L189:                                             ; preds = %L202
  %27 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlLayered_try_raise_hit_only__open_layers_6_14_code"(i64 %ds.0, i64 %alloc.0, i64 %13, ptr addrspace(1) %3) #8 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 37, i64 0, i64 18, i64 44, i64 0, i64 44, i64 29, i64 7954796, i64 6648421, i64 7626596, i64 6257010, i64 6906226, i64 6251891, i64 7629160, i64 7237471, i64 3045740, i64 27757, i64 38, i64 7954764, i64 6648421, i64 7626596, i64 6257010, i64 6906226, i64 6251891, i64 7629160, i64 7237471, i64 3045740, i64 6647919, i64 7102318, i64 6650209, i64 29554) ]
  %28 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %27, 0, 0
  %29 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %27, 0, 1
  %30 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %27, 1, 0
  %31 = add i64 %29, -16
  %32 = inttoptr i64 %28 to ptr
  %33 = load i64, ptr %32, align 4
  %.not79 = icmp ugt i64 %33, %31
  br i1 %.not79, label %L206, label %L207, !prof !1

L206:                                             ; preds = %L189
  %34 = call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_call_gc"(i64 %28, i64 %31) #10 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 2, i64 1, i64 37, i64 0, i64 13, i64 44, i64 0, i64 44, i64 29, i64 7954796, i64 6648421, i64 7626596, i64 6257010, i64 6906226, i64 6251891, i64 7629160, i64 7237471, i64 3045740, i64 27757, i64 38, i64 7954764, i64 6648421, i64 7626596, i64 6257010, i64 6906226, i64 6251891, i64 7629160, i64 7237471, i64 3045740, i64 6647919, i64 7102318, i64 6650209, i64 29554) ]
  %35 = extractvalue { { i64, i64 }, {} } %34, 0, 0
  %36 = extractvalue { { i64, i64 }, {} } %34, 0, 1
  br label %L207

L207:                                             ; preds = %L206, %L189
  %alloc.2 = phi i64 [ %31, %L189 ], [ %36, %L206 ]
  %ds.2 = phi i64 [ %28, %L189 ], [ %35, %L206 ]
  %37 = add i64 %alloc.2, 8
  %38 = inttoptr i64 %37 to ptr addrspace(1)
  %39 = getelementptr i8, ptr addrspace(1) %38, i64 -8
  store volatile i64 1025, ptr addrspace(1) %39, align 4
  store ptr addrspace(1) %30, ptr addrspace(1) %38, align 8
  br label %L195

L195:                                             ; preds = %L207, %L205
  %.0 = phi ptr addrspace(1) [ %38, %L207 ], [ %25, %L205 ]
  %alloc.3 = phi i64 [ %alloc.2, %L207 ], [ %alloc.1, %L205 ]
  %ds.3 = phi i64 [ %ds.2, %L207 ], [ %ds.1, %L205 ]
  %40 = add i64 %alloc.3, -24
  %41 = inttoptr i64 %ds.3 to ptr
  %42 = load i64, ptr %41, align 4
  %.not78 = icmp ugt i64 %42, %40
  br i1 %.not78, label %L208, label %L209, !prof !1

L208:                                             ; preds = %L195
  %43 = call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_call_gc"(i64 %ds.3, i64 %40) #11 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 1, i64 33, i64 4, i64 4, i64 46, i64 102, i64 148, i64 29, i64 7954796, i64 6648421, i64 7626596, i64 6257010, i64 6906226, i64 6251891, i64 7629160, i64 7237471, i64 3045740, i64 27757, i64 38, i64 7954764, i64 6648421, i64 7626596, i64 6257010, i64 6906226, i64 6251891, i64 7629160, i64 7237471, i64 3045740, i64 6647919, i64 7102318, i64 6650209, i64 29554) ]
  %44 = extractvalue { { i64, i64 }, {} } %43, 0, 0
  %45 = extractvalue { { i64, i64 }, {} } %43, 0, 1
  br label %L209

L209:                                             ; preds = %L208, %L195
  %alloc.4 = phi i64 [ %40, %L195 ], [ %45, %L208 ]
  %ds.4 = phi i64 [ %ds.3, %L195 ], [ %44, %L208 ]
  %46 = add i64 %alloc.4, 8
  %47 = inttoptr i64 %46 to ptr addrspace(1)
  %48 = getelementptr i8, ptr addrspace(1) %47, i64 -8
  store volatile i64 2048, ptr addrspace(1) %48, align 4
  store volatile i64 1, ptr addrspace(1) %47, align 4
  %49 = getelementptr i8, ptr addrspace(1) %47, i64 8
  store ptr addrspace(1) %.0, ptr addrspace(1) %49, align 8
  br label %common.ret
}
