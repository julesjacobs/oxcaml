define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlBoyer__rewrite_with_lemmas_13_33_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3) #3 gc "oxcaml" personality ptr @"\01_caml_llvm_eh_personality" {
L1:
  %4 = add i64 %0, 40
  %5 = inttoptr i64 %4 to ptr
  %6 = load i64, ptr %5, align 4
  %7 = add i64 %6, 408
  %8 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #7
  %.not = icmp ult i64 %8, %7
  br i1 %.not, label %L670, label %L671, !prof !1

L670:                                             ; preds = %L1
  %9 = tail call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_llvm_call_realloc_stack"(i64 %0, i64 %1, i64 38) #8
  %10 = extractvalue { { i64, i64 }, {} } %9, 0, 0
  %11 = extractvalue { { i64, i64 }, {} } %9, 0, 1
  br label %L671

L671:                                             ; preds = %L670, %L1
  %alloc.0 = phi i64 [ %1, %L1 ], [ %11, %L670 ]
  %ds.0 = phi i64 [ %0, %L1 ], [ %10, %L670 ]
  %12 = ptrtoint ptr addrspace(1) %3 to i64
  %13 = and i64 %12, 1
  %.not7381 = icmp eq i64 %13, 0
  br i1 %.not7381, label %L637, label %L635

common.ret:                                       ; preds = %L678, %L635
  %common.ret.op = phi { { i64, i64 }, { ptr addrspace(1) } } [ %18, %L635 ], [ %52, %L678 ]
  ret { { i64, i64 }, { ptr addrspace(1) } } %common.ret.op

L635.loopexit:                                    ; preds = %L660
  %14 = extractvalue { ptr addrspace(1), i64, i64, i64 } %28, 3
  %15 = extractvalue { ptr addrspace(1), i64, i64, i64 } %28, 2
  br label %L635

L635:                                             ; preds = %L635.loopexit, %L671
  %alloc.1.lcssa = phi i64 [ %alloc.0, %L671 ], [ %15, %L635.loopexit ]
  %ds.1.lcssa = phi i64 [ %ds.0, %L671 ], [ %14, %L635.loopexit ]
  %16 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %ds.1.lcssa, 0, 0
  %17 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %16, i64 %alloc.1.lcssa, 0, 1
  %18 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %17, ptr addrspace(1) %2, 1, 0
  br label %common.ret

L637:                                             ; preds = %L671, %L660
  %ds.184 = phi i64 [ %30, %L660 ], [ %ds.0, %L671 ]
  %alloc.183 = phi i64 [ %53, %L660 ], [ %alloc.0, %L671 ]
  %.07182 = phi ptr addrspace(1) [ %55, %L660 ], [ %3, %L671 ]
  %19 = load ptr addrspace(1), ptr addrspace(1) %.07182, align 8
  %20 = add i64 %ds.184, 64
  %21 = inttoptr i64 %20 to ptr
  %22 = load i64, ptr %21, align 4
  tail call void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@"\01_camlBoyer__rewrite_with_lemmas_13_33_code", %L673))
  %23 = getelementptr i8, ptr addrspace(1) %19, i64 8
  %24 = load ptr addrspace(1), ptr addrspace(1) %23, align 8
  %25 = load ptr addrspace(1), ptr addrspace(1) %19, align 8
  %26 = invoke oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlBoyer__unify_9_29_code"(i64 %ds.184, i64 %alloc.183, ptr addrspace(1) %2, ptr addrspace(1) %25) #13 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 107, i64 0, i64 31, i64 46, i64 0, i64 46, i64 8, i64 7958370, i64 3043941, i64 27757, i64 25, i64 7958338, i64 3043941, i64 7824754, i64 7629170, i64 7823205, i64 6845545, i64 6646879, i64 6385005, i64 115) ]
          to label %L674 unwind label %L673

L673:                                             ; preds = %L677, %L676, %L637
  %27 = landingpad token
          cleanup
  %28 = tail call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %29 = extractvalue { ptr addrspace(1), i64, i64, i64 } %28, 0
  %30 = extractvalue { ptr addrspace(1), i64, i64, i64 } %28, 3
  %31 = add i64 %30, 64
  %32 = inttoptr i64 %31 to ptr
  store i64 %22, ptr %32, align 4
  %or.cond.not = icmp eq ptr addrspace(1) %29, inttoptr (i64 ptrtoint (ptr @"\01_camlBoyer__Unify3423" to i64) to ptr addrspace(1))
  br i1 %or.cond.not, label %L660, label %L665

L674:                                             ; preds = %L637
  %33 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %26, 0, 0
  %34 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %26, 0, 1
  %35 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %26, 1, 0
  %36 = add i64 %34, -40
  %37 = inttoptr i64 %33 to ptr
  %38 = load i64, ptr %37, align 4
  %.not74 = icmp ugt i64 %38, %36
  br i1 %.not74, label %L675, label %L676, !prof !1

L675:                                             ; preds = %L674
  %39 = call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_call_gc"(i64 %33, i64 %36) #14 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 5, i64 2, i64 71, i64 2, i64 19, i64 60, i64 93, i64 153, i64 8, i64 7958370, i64 3043941, i64 27757, i64 24, i64 7958338, i64 3043941, i64 7368801, i64 6257004, i64 6452595, i64 3044467, i64 6255457, i64 6514034, i64 107, i64 0, i64 18, i64 50, i64 0, i64 50, i64 8, i64 7958370, i64 3043941, i64 27757, i64 25, i64 7958338, i64 3043941, i64 7824754, i64 7629170, i64 7823205, i64 6845545, i64 6646879, i64 6385005, i64 115) ]
  %40 = extractvalue { { i64, i64 }, {} } %39, 0, 0
  %41 = extractvalue { { i64, i64 }, {} } %39, 0, 1
  br label %L676

L676:                                             ; preds = %L675, %L674
  %alloc.2 = phi i64 [ %36, %L674 ], [ %41, %L675 ]
  %ds.2 = phi i64 [ %33, %L674 ], [ %40, %L675 ]
  %42 = add i64 %alloc.2, 8
  %43 = inttoptr i64 %42 to ptr addrspace(1)
  %44 = getelementptr i8, ptr addrspace(1) %43, i64 -8
  store volatile i64 4343, ptr addrspace(1) %44, align 4
  store volatile i64 ptrtoint (ptr @"\01_camlBoyer__as_rec_8_34_code" to i64), ptr addrspace(1) %43, align 4
  %45 = getelementptr i8, ptr addrspace(1) %43, i64 8
  store volatile i64 108086391056891909, ptr addrspace(1) %45, align 4
  %46 = getelementptr i8, ptr addrspace(1) %43, i64 16
  store ptr addrspace(1) %24, ptr addrspace(1) %46, align 8
  %47 = getelementptr i8, ptr addrspace(1) %43, i64 24
  store ptr addrspace(1) %35, ptr addrspace(1) %47, align 8
  %48 = invoke oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlBoyer__as_rec_8_34_code"(i64 %ds.2, i64 %alloc.2, ptr addrspace(1) %24, ptr addrspace(1) %43) #13 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 2, i64 75, i64 0, i64 2, i64 13, i64 0, i64 13, i64 8, i64 7958370, i64 3043941, i64 27757, i64 17, i64 7958338, i64 3043941, i64 7368801, i64 6257004, i64 6452595, i64 29811, i64 107, i64 0, i64 18, i64 50, i64 0, i64 50, i64 8, i64 7958370, i64 3043941, i64 27757, i64 25, i64 7958338, i64 3043941, i64 7824754, i64 7629170, i64 7823205, i64 6845545, i64 6646879, i64 6385005, i64 115) ]
          to label %L677 unwind label %L673

L677:                                             ; preds = %L676
  %49 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %48, 0, 0
  %50 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %48, 0, 1
  %51 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %48, 1, 0
  %52 = invoke oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlBoyer__rewrite_12_32_code"(i64 %49, i64 %50, ptr addrspace(1) %51) #13 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 107, i64 0, i64 10, i64 50, i64 0, i64 50, i64 8, i64 7958370, i64 3043941, i64 27757, i64 25, i64 7958338, i64 3043941, i64 7824754, i64 7629170, i64 7823205, i64 6845545, i64 6646879, i64 6385005, i64 115) ]
          to label %L678 unwind label %L673

L678:                                             ; preds = %L677
  tail call void @llvm.aarch64.oxcaml.pop.trap()
  br label %common.ret

L660:                                             ; preds = %L673
  %53 = extractvalue { ptr addrspace(1), i64, i64, i64 } %28, 2
  %54 = getelementptr i8, ptr addrspace(1) %.07182, i64 8
  %55 = load ptr addrspace(1), ptr addrspace(1) %54, align 8
  %56 = ptrtoint ptr addrspace(1) %55 to i64
  %57 = and i64 %56, 1
  %.not73 = icmp eq i64 %57, 0
  br i1 %.not73, label %L637, label %L635.loopexit

L665:                                             ; preds = %L673
  %58 = extractvalue { ptr addrspace(1), i64, i64, i64 } %28, 0
  %59 = ptrtoint ptr addrspace(1) %58 to i64
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 %59)
  unreachable
}
