define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlHash_lookup_string_equal__find_4_13_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) nocapture %3) #0 gc "oxcaml" {
L1:
  %4 = load ptr addrspace(1), ptr addrspace(1) %3, align 8
  %5 = ptrtoint ptr addrspace(1) %4 to i64
  %6 = and i64 %5, 1
  %.not = icmp eq i64 %6, 0
  br i1 %.not, label %L129, label %common.ret

common.ret.loopexit:                              ; preds = %L134
  %7 = extractvalue { i64, i64, ptr addrspace(1) } %12, 1
  %8 = extractvalue { i64, i64, ptr addrspace(1) } %12, 0
  br label %common.ret

common.ret:                                       ; preds = %common.ret.loopexit, %L1, %L138
  %ds.0.pn = phi i64 [ %ds.245, %L138 ], [ %0, %L1 ], [ %8, %common.ret.loopexit ]
  %alloc.0.pn = phi i64 [ %alloc.244, %L138 ], [ %1, %L1 ], [ %7, %common.ret.loopexit ]
  %.pn37 = phi i64 [ %24, %L138 ], [ -1, %L1 ], [ -1, %common.ret.loopexit ]
  %.pn38 = insertvalue { { i64, i64 }, { i64 } } undef, i64 %ds.0.pn, 0, 0
  %.pn = insertvalue { { i64, i64 }, { i64 } } %.pn38, i64 %alloc.0.pn, 0, 1
  %common.ret.op = insertvalue { { i64, i64 }, { i64 } } %.pn, i64 %.pn37, 1, 0
  ret { { i64, i64 }, { i64 } } %common.ret.op

L129:                                             ; preds = %L1, %L134
  %.035 = phi ptr addrspace(1) [ %17, %L134 ], [ %4, %L1 ]
  %alloc.1 = phi i64 [ %14, %L134 ], [ %1, %L1 ]
  %ds.1 = phi i64 [ %15, %L134 ], [ %0, %L1 ]
  %9 = load ptr addrspace(1), ptr addrspace(1) %.035, align 8
  %10 = load ptr addrspace(1), ptr addrspace(1) %9, align 8
  %11 = icmp eq ptr addrspace(1) %10, %2
  br i1 %11, label %L138, label %L132

L132:                                             ; preds = %L129
  %12 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_string_equal"(i64 %ds.1, i64 %alloc.1, ptr addrspace(1) %2, ptr addrspace(1) %10) #8
  %13 = extractvalue { i64, i64, ptr addrspace(1) } %12, 2
  %or.cond.not = icmp eq ptr addrspace(1) %13, inttoptr (i64 1 to ptr addrspace(1))
  br i1 %or.cond.not, label %L134, label %L138.split.loop.exit49

L134:                                             ; preds = %L132
  %14 = extractvalue { i64, i64, ptr addrspace(1) } %12, 1
  %15 = extractvalue { i64, i64, ptr addrspace(1) } %12, 0
  %16 = getelementptr i8, ptr addrspace(1) %.035, i64 8
  %17 = load ptr addrspace(1), ptr addrspace(1) %16, align 8
  %18 = ptrtoint ptr addrspace(1) %17 to i64
  %19 = and i64 %18, 1
  %.not36 = icmp eq i64 %19, 0
  br i1 %.not36, label %L129, label %common.ret.loopexit

L138.split.loop.exit49:                           ; preds = %L132
  %20 = extractvalue { i64, i64, ptr addrspace(1) } %12, 0
  %21 = extractvalue { i64, i64, ptr addrspace(1) } %12, 1
  br label %L138

L138:                                             ; preds = %L129, %L138.split.loop.exit49
  %ds.245 = phi i64 [ %20, %L138.split.loop.exit49 ], [ %ds.1, %L129 ]
  %alloc.244 = phi i64 [ %21, %L138.split.loop.exit49 ], [ %alloc.1, %L129 ]
  %22 = getelementptr i8, ptr addrspace(1) %9, i64 8
  %23 = load ptr addrspace(1), ptr addrspace(1) %22, align 8
  %24 = ptrtoint ptr addrspace(1) %23 to i64
  br label %common.ret
}
