define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_raise_cross_function_caught__find_5_12_code"(i64 %0, i64 %1, i64 %2) #2 gc "oxcaml" personality ptr @"\01_caml_llvm_eh_personality" {
L1:
  %3 = add i64 %0, 40
  %4 = inttoptr i64 %3 to ptr
  %5 = load i64, ptr %4, align 4
  %6 = add i64 %5, 408
  %7 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #6
  %.not = icmp ult i64 %7, %6
  br i1 %.not, label %L150, label %L151, !prof !1

L150:                                             ; preds = %L1
  %statepoint_token = tail call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64, i64)) @"\01_caml_llvm_call_realloc_stack", i32 3, i32 0, i64 %0, i64 %1, i64 38, i32 0, i32 0) #7
  %8 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token)
  %9 = extractvalue { { i64, i64 }, {} } %8, 0, 0
  %10 = extractvalue { { i64, i64 }, {} } %8, 0, 1
  br label %L151

L151:                                             ; preds = %L150, %L1
  %alloc.0 = phi i64 [ %1, %L1 ], [ %10, %L150 ]
  %ds.0 = phi i64 [ %0, %L1 ], [ %9, %L150 ]
  %11 = add i64 %ds.0, 64
  %12 = inttoptr i64 %11 to ptr
  %13 = load i64, ptr %12, align 4
  tail call void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@"\01_camlTry_raise_cross_function_caught__find_5_12_code", %L153))
  %statepoint_token30 = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 18, i32 0, ptr elementtype({ { i64, i64 }, { i64 } } (i64, i64, i64)) @"\01_camlTry_raise_cross_function_caught__probe_4_11_code", i32 3, i32 0, i64 %ds.0, i64 %alloc.0, i64 %2, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 20, i64 0, i64 6, i64 13, i64 0, i64 13, i64 34, i64 7959156, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7155316, i64 108, i64 36, i64 7959124, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 6696564, i64 6581865) ]
          to label %L154 unwind label %L153

L153:                                             ; preds = %L151
  %14 = landingpad token
          cleanup
  %15 = tail call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %gcagg = extractvalue { ptr addrspace(1), i64, i64, i64 } %15, 0
  %gcagg26 = extractvalue { ptr addrspace(1), i64, i64, i64 } %15, 1
  %gcagg27 = extractvalue { ptr addrspace(1), i64, i64, i64 } %15, 2
  %gcagg28 = extractvalue { ptr addrspace(1), i64, i64, i64 } %15, 3
  %16 = add i64 %gcagg28, 64
  %17 = inttoptr i64 %16 to ptr
  store i64 %13, ptr %17, align 4
  %or.cond.not = icmp eq ptr addrspace(1) %gcagg, inttoptr (i64 ptrtoint (ptr @"\01_camlTry_raise_cross_function_caught__Miss245" to i64) to ptr addrspace(1))
  br i1 %or.cond.not, label %L144, label %L146

L154:                                             ; preds = %L151
  %18 = call { { i64, i64 }, { i64 } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_i64ss(token %statepoint_token30)
  tail call void @llvm.aarch64.oxcaml.pop.trap()
  br label %common.ret

common.ret:                                       ; preds = %L144, %L154
  %common.ret.op = phi { { i64, i64 }, { i64 } } [ %18, %L154 ], [ %21, %L144 ]
  ret { { i64, i64 }, { i64 } } %common.ret.op

L144:                                             ; preds = %L153
  %19 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %gcagg28, 0, 0
  %20 = insertvalue { { i64, i64 }, { i64 } } %19, i64 %gcagg27, 0, 1
  %21 = insertvalue { { i64, i64 }, { i64 } } %20, i64 3, 1, 0
  br label %common.ret

L146:                                             ; preds = %L153
  %22 = ptrtoint ptr addrspace(1) %gcagg to i64
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 %22)
  unreachable
}
