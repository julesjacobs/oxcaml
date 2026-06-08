; ModuleID = '/Users/julesjacobs/git/oxcaml-llvm/agents/test-suite-29e4cd/oxcaml/agent-state/test-suite-29e4cd/slowdown_vs_native_stage_artifacts_20260608_current/cases/try_find_miss_rare/raw/input.ll'
source_filename = "try_find_miss_rare.ml"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx16.0.0"

@"\01_camlTry_find_miss_rare__gc_roots" = local_unnamed_addr global { ptr, ptr, i64 } { ptr @"\01_camlTry_find_miss_rare", ptr @"\01_camlTry_find_miss_rare__Miss281", i64 0 }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare" = local_unnamed_addr global i64 9984, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare" = global { ptr, ptr, ptr, i64, i64, ptr, ptr, ptr, ptr } { ptr @"\01_camlTry_find_miss_rare__black_box_int_7", ptr @"\01_camlTry_find_miss_rare__black_box_string_8", ptr @"\01_camlTry_find_miss_rare__black_box_9", i64 1, i64 1, ptr @"\01_camlTry_find_miss_rare__print_result_10", ptr @"\01_camlTry_find_miss_rare__Miss281", ptr @"\01_camlTry_find_miss_rare__scan_11", ptr @"\01_camlTry_find_miss_rare__run_12" }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__run_12" = local_unnamed_addr global i64 4087, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__run_12" = global { ptr, i64, ptr } { ptr @"\01_caml_curry2", i64 180143985094819847, ptr @"\01_camlTry_find_miss_rare__run_5_12_code" }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__invalid605" = local_unnamed_addr global i64 16380, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__invalid605" = global { [117 x i8], [2 x i8], i8 } { [117 x i8] c"(Defining_expr_of_let (bound_pattern prim/367N)\0A (defining_expr ((Unbox_float apply_result/357N) array.ml:87,5--27)))", [2 x i8] zeroinitializer, i8 2 }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__scan_11" = local_unnamed_addr global i64 4087, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__scan_11" = global { ptr, i64, ptr } { ptr @"\01_caml_curry3", i64 252201579132747783, ptr @"\01_camlTry_find_miss_rare__scan_4_11_code" }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__Miss281" = local_unnamed_addr global i64 3064, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__Miss281" = global { ptr, i64 } { ptr @"\01_camlTry_find_miss_rare__immstring74", i64 1 }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__print_result_10" = local_unnamed_addr global i64 3063, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__print_result_10" = global { ptr, i64 } { ptr @"\01_camlTry_find_miss_rare__print_result_3_10_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__black_box_9" = local_unnamed_addr global i64 3063, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__black_box_9" = global { ptr, i64 } { ptr @"\01_camlTry_find_miss_rare__black_box_2_9_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__black_box_string_8" = local_unnamed_addr global i64 3063, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__black_box_string_8" = global { ptr, i64 } { ptr @"\01_camlTry_find_miss_rare__black_box_string_1_8_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__black_box_int_7" = local_unnamed_addr global i64 3063, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__black_box_int_7" = global { ptr, i64 } { ptr @"\01_camlTry_find_miss_rare__black_box_int_0_7_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__block35" = local_unnamed_addr global i64 2816, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__block35" = global { ptr, ptr } { ptr @"\01_caml_exn_Invalid_argument", ptr @"\01_camlTry_find_miss_rare__string33" }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__string33" = local_unnamed_addr global i64 4092, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__string33" = global { [19 x i8], [4 x i8], i8 } { [19 x i8] c"index out of bounds", [4 x i8] zeroinitializer, i8 4 }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__immstring74" = local_unnamed_addr global i64 4092, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__immstring74" = global { [23 x i8], [0 x i8], i8 } { [23 x i8] c"Try_find_miss_rare.Miss", [0 x i8] zeroinitializer, i8 0 }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__const_block66" = local_unnamed_addr global i64 4868, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__const_block66" = global { i64, i64, i64, ptr } { i64 1, i64 1, i64 1, ptr @"\01_camlTry_find_miss_rare__const_block64" }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__const_block64" = local_unnamed_addr global i64 2828, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__const_block64" = global { i64, ptr } { i64 21, ptr @"\01_camlTry_find_miss_rare__const_block62" }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__const_block62" = local_unnamed_addr global i64 1802, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__const_block62" = global { i64 } { i64 1 }, section "__DATA,__data", align 8
@"\01_camlCamlinternalFormat__make_printf_120_401_code" = external local_unnamed_addr global ptr
@"\01_camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31" = external global ptr
@"\01_caml_array_make" = external global ptr
@"\01_caml_c_call" = external local_unnamed_addr global ptr
@"\01_caml_call_gc" = external local_unnamed_addr global ptr
@"\01_caml_curry2" = external global ptr
@"\01_caml_curry3" = external global ptr
@"\01_caml_equal" = external global ptr
@"\01_caml_exn_Invalid_argument" = external global ptr
@"\01_caml_flambda2_invalid" = external local_unnamed_addr global ptr
@"\01_caml_fresh_oo_id" = external local_unnamed_addr global ptr
@"\01_caml_initialize" = external local_unnamed_addr global ptr
@"\01_caml_int_of_string" = external global ptr
@"\01_caml_llvm_call_realloc_stack" = external local_unnamed_addr global ptr
@"\01_caml_modify" = external local_unnamed_addr global ptr
@"\01_caml_sys_argv" = external global ptr

; Function Attrs: noinline
define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_find_miss_rare__black_box_int_0_7_code"(i64 %0, i64 %1, i64 %2) #0 gc "oxcaml" {
L1:
  %3 = tail call i64 asm "", "=r,0"(i64 %2) #5
  %4 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %0, 0, 0
  %5 = insertvalue { { i64, i64 }, { i64 } } %4, i64 %1, 0, 1
  %6 = insertvalue { { i64, i64 }, { i64 } } %5, i64 %3, 1, 0
  ret { { i64, i64 }, { i64 } } %6
}

; Function Attrs: noinline
define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTry_find_miss_rare__black_box_string_1_8_code"(i64 %0, i64 %1, ptr addrspace(1) %2) #0 gc "oxcaml" {
L1:
  %3 = tail call ptr addrspace(1) asm "", "=r,0"(ptr addrspace(1) %2) #5
  %4 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %0, 0, 0
  %5 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %4, i64 %1, 0, 1
  %6 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %5, ptr addrspace(1) %3, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %6
}

; Function Attrs: noinline
define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTry_find_miss_rare__black_box_2_9_code"(i64 %0, i64 %1, ptr addrspace(1) %2) #0 gc "oxcaml" {
L1:
  %3 = tail call ptr addrspace(1) asm "", "=r,0"(ptr addrspace(1) %2) #5
  %4 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %0, 0, 0
  %5 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %4, i64 %1, 0, 1
  %6 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %5, ptr addrspace(1) %3, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %6
}

; Function Attrs: noinline
define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_find_miss_rare__print_result_3_10_code"(i64 %0, i64 %1, i64 %2) #1 gc "oxcaml" {
L1:
  %3 = add i64 %0, 40
  %4 = inttoptr i64 %3 to ptr
  %5 = load i64, ptr %4, align 4
  %6 = add i64 %5, 376
  %7 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #5
  %.not = icmp ult i64 %7, %6
  br i1 %.not, label %L116, label %L117, !prof !1

L116:                                             ; preds = %L1
  %8 = tail call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_llvm_call_realloc_stack"(i64 %0, i64 %1, i64 34) #6
  %9 = extractvalue { { i64, i64 }, {} } %8, 0, 0
  %10 = extractvalue { { i64, i64 }, {} } %8, 0, 1
  br label %L117

L117:                                             ; preds = %L116, %L1
  %alloc.0 = phi i64 [ %1, %L1 ], [ %10, %L116 ]
  %ds.0 = phi i64 [ %0, %L1 ], [ %9, %L116 ]
  %11 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlCamlinternalFormat__make_printf_120_401_code"(i64 %ds.0, i64 %alloc.0, i64 ptrtoint (ptr @"\01_camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31" to i64), i64 1, i64 ptrtoint (ptr @"\01_camlTry_find_miss_rare__const_block66" to i64)) #7 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 4, i64 27, i64 0, i64 2, i64 63, i64 0, i64 63, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 23, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 7366251, i64 7235954, i64 26228, i64 34, i64 0, i64 21, i64 43, i64 0, i64 43, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 22, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 7499878, i64 7630441, i64 102, i64 38, i64 0, i64 17, i64 35, i64 0, i64 35, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 21, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 6910576, i64 6714478, i64 11, i64 0, i64 21, i64 63, i64 0, i64 63, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 31, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7499822, i64 7630441, i64 6648415, i64 7107955, i64 116) ]
  %12 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %11, 0, 0
  %13 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %11, 0, 1
  %14 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %11, 1, 0
  %15 = and i64 %2, 2147483647
  %16 = load i64, ptr addrspace(1) %14, align 4
  %17 = inttoptr i64 %16 to ptr
  %18 = musttail call oxcaml_nofpcc { { i64, i64 }, { i64 } } %17(i64 %12, i64 %13, i64 %15, ptr addrspace(1) %14) #7
  ret { { i64, i64 }, { i64 } } %18
}

; Function Attrs: noinline
define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_find_miss_rare__scan_4_11_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) nocapture readonly %3, i64 %4) #0 gc "oxcaml" {
L1:
  %5 = getelementptr i8, ptr addrspace(1) %3, i64 -8
  %6 = load i64, ptr addrspace(1) %5, align 4
  %7 = lshr i64 %6, 9
  %8 = and i64 %7, 140737488355326
  %9 = or i64 %8, 1
  %or.cond.not6468 = icmp eq i64 %9, %4
  br i1 %or.cond.not6468, label %L131, label %L136.preheader

L136.preheader:                                   ; preds = %L1
  %10 = getelementptr i8, ptr addrspace(1) %3, i64 -4
  %11 = getelementptr i8, ptr addrspace(1) %3, i64 -4
  br label %L136

L131:                                             ; preds = %L157, %L1
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 ptrtoint (ptr @"\01_camlTry_find_miss_rare__Miss281" to i64))
  unreachable

L136:                                             ; preds = %L136.preheader, %L157
  %.in = phi i64 [ %33, %L157 ], [ %6, %L136.preheader ]
  %ds.071 = phi i64 [ %31, %L157 ], [ %0, %L136.preheader ]
  %alloc.070 = phi i64 [ %30, %L157 ], [ %1, %L136.preheader ]
  %.069 = phi i64 [ %32, %L157 ], [ %4, %L136.preheader ]
  %12 = and i64 %.in, 255
  %or.cond.not = icmp eq i64 %12, 254
  br i1 %or.cond.not, label %L140, label %L147

L140:                                             ; preds = %L136
  %13 = add i64 %alloc.070, -16
  %14 = inttoptr i64 %ds.071 to ptr
  %15 = load i64, ptr %14, align 4
  %.not = icmp ugt i64 %15, %13
  br i1 %.not, label %L168, label %L169, !prof !1

L168:                                             ; preds = %L140
  %16 = call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_call_gc"(i64 %ds.071, i64 %13) #8 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 2, i64 1, i64 18, i64 0, i64 10, i64 30, i64 0, i64 30, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 23, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 6517550, i64 28257) ]
  %17 = extractvalue { { i64, i64 }, {} } %16, 0, 0
  %18 = extractvalue { { i64, i64 }, {} } %16, 0, 1
  br label %L169

L169:                                             ; preds = %L168, %L140
  %alloc.1 = phi i64 [ %13, %L140 ], [ %18, %L168 ]
  %ds.1 = phi i64 [ %ds.071, %L140 ], [ %17, %L168 ]
  %19 = add i64 %alloc.1, 8
  %20 = inttoptr i64 %19 to ptr addrspace(1)
  %21 = getelementptr i8, ptr addrspace(1) %20, i64 -8
  store volatile i64 1277, ptr addrspace(1) %21, align 4
  %22 = shl i64 %.069, 2
  %23 = getelementptr i8, ptr addrspace(1) %11, i64 %22
  %24 = load double, ptr addrspace(1) %23, align 8
  store double %24, ptr addrspace(1) %20, align 8
  br label %L154

L147:                                             ; preds = %L136
  %25 = shl i64 %.069, 2
  %26 = getelementptr i8, ptr addrspace(1) %10, i64 %25
  %27 = load ptr addrspace(1), ptr addrspace(1) %26, align 8
  br label %L154

L154:                                             ; preds = %L147, %L169
  %.061 = phi ptr addrspace(1) [ %27, %L147 ], [ %20, %L169 ]
  %alloc.2 = phi i64 [ %alloc.070, %L147 ], [ %alloc.1, %L169 ]
  %ds.2 = phi i64 [ %ds.071, %L147 ], [ %ds.1, %L169 ]
  %28 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %ds.2, i64 %alloc.2, i64 ptrtoint (ptr @"\01_caml_equal" to i64), ptr addrspace(1) %.061, ptr addrspace(1) %2) #7 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 18, i64 0, i64 10, i64 36, i64 0, i64 36, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 23, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 6517550, i64 28257) ]
  %29 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %28, 1, 0
  %or.cond4.not = icmp eq ptr addrspace(1) %29, inttoptr (i64 1 to ptr addrspace(1))
  br i1 %or.cond4.not, label %L157, label %L160

L157:                                             ; preds = %L154
  %30 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %28, 0, 1
  %31 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %28, 0, 0
  %32 = add i64 %.069, 2
  %33 = load i64, ptr addrspace(1) %5, align 4
  %34 = lshr i64 %33, 9
  %35 = and i64 %34, 140737488355326
  %36 = or i64 %35, 1
  %or.cond.not64 = icmp eq i64 %32, %36
  br i1 %or.cond.not64, label %L131, label %L136

L160:                                             ; preds = %L154
  %37 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %28, 0, 0
  %38 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %28, 0, 1
  %39 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %37, 0, 0
  %40 = insertvalue { { i64, i64 }, { i64 } } %39, i64 %38, 0, 1
  %41 = insertvalue { { i64, i64 }, { i64 } } %40, i64 %.069, 1, 0
  ret { { i64, i64 }, { i64 } } %41
}

; Function Attrs: noinline
define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_find_miss_rare__run_5_12_code"(i64 %0, i64 %1, i64 %2, i64 %3) #2 gc "oxcaml" personality ptr @"\01_caml_llvm_eh_personality" {
L1:
  %4 = add i64 %0, 64
  %5 = inttoptr i64 %4 to ptr
  %6 = load i64, ptr %5, align 4
  %7 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %0, i64 %1, i64 ptrtoint (ptr @"\01_caml_array_make" to i64), ptr addrspace(1) nonnull inttoptr (i64 33 to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1))) #7 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 85, i64 0, i64 13, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 22, i64 0, i64 10, i64 40, i64 0, i64 40, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 22, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7696942, i64 110) ]
  %8 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %7, 0, 0
  %9 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %7, 0, 1
  %10 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %7, 1, 0
  %11 = add i64 %8, 40
  %12 = inttoptr i64 %11 to ptr
  %13 = load i64, ptr %12, align 4
  %14 = add i64 %13, 408
  %15 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #5
  %.not = icmp ult i64 %15, %14
  br i1 %.not, label %L305, label %L306, !prof !1

L305:                                             ; preds = %L1
  %16 = tail call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_llvm_call_realloc_stack"(i64 %8, i64 %9, i64 38) #6
  %17 = extractvalue { { i64, i64 }, {} } %16, 0, 0
  %18 = extractvalue { { i64, i64 }, {} } %16, 0, 1
  br label %L306

L306:                                             ; preds = %L305, %L1
  %alloc.0 = phi i64 [ %9, %L1 ], [ %18, %L305 ]
  %ds.0 = phi i64 [ %8, %L1 ], [ %17, %L305 ]
  %19 = getelementptr i8, ptr addrspace(1) %10, i64 -8
  %20 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not = icmp eq i8 %20, -2
  br i1 %or.cond.not, label %L187, label %L189

L187:                                             ; preds = %L189.13, %L189.12, %L189.11, %L189.10, %L189.9, %L189.8, %L189.7, %L189.6, %L189.5, %L189.4, %L189.3, %L189.2, %L189.1, %L189, %L306
  %alloc.1.lcssa = phi i64 [ %alloc.0, %L306 ], [ %25, %L189 ], [ %30, %L189.1 ], [ %35, %L189.2 ], [ %40, %L189.3 ], [ %45, %L189.4 ], [ %50, %L189.5 ], [ %55, %L189.6 ], [ %60, %L189.7 ], [ %65, %L189.8 ], [ %70, %L189.9 ], [ %75, %L189.10 ], [ %80, %L189.11 ], [ %85, %L189.12 ], [ %90, %L189.13 ]
  %ds.1.lcssa = phi i64 [ %ds.0, %L306 ], [ %24, %L189 ], [ %29, %L189.1 ], [ %34, %L189.2 ], [ %39, %L189.3 ], [ %44, %L189.4 ], [ %49, %L189.5 ], [ %54, %L189.6 ], [ %59, %L189.7 ], [ %64, %L189.8 ], [ %69, %L189.9 ], [ %74, %L189.10 ], [ %79, %L189.11 ], [ %84, %L189.12 ], [ %89, %L189.13 ]
  %21 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_flambda2_invalid"(i64 %ds.1.lcssa, i64 %alloc.1.lcssa, ptr addrspace(1) nonnull inttoptr (i64 ptrtoint (ptr @"\01_camlTry_find_miss_rare__invalid605" to i64) to ptr addrspace(1))) #9
  unreachable

L189:                                             ; preds = %L306
  %22 = getelementptr i8, ptr addrspace(1) %10, i64 8
  %23 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %ds.0, i64 %alloc.0, ptr addrspace(1) %22, ptr addrspace(1) nonnull inttoptr (i64 5 to ptr addrspace(1))) #9
  %24 = extractvalue { i64, i64 } %23, 0
  %25 = extractvalue { i64, i64 } %23, 1
  %26 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.1 = icmp eq i8 %26, -2
  br i1 %or.cond.not.1, label %L187, label %L189.1

L189.1:                                           ; preds = %L189
  %27 = getelementptr i8, ptr addrspace(1) %10, i64 16
  %28 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %24, i64 %25, ptr addrspace(1) %27, ptr addrspace(1) nonnull inttoptr (i64 9 to ptr addrspace(1))) #9
  %29 = extractvalue { i64, i64 } %28, 0
  %30 = extractvalue { i64, i64 } %28, 1
  %31 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.2 = icmp eq i8 %31, -2
  br i1 %or.cond.not.2, label %L187, label %L189.2

L189.2:                                           ; preds = %L189.1
  %32 = getelementptr i8, ptr addrspace(1) %10, i64 24
  %33 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %29, i64 %30, ptr addrspace(1) %32, ptr addrspace(1) nonnull inttoptr (i64 13 to ptr addrspace(1))) #9
  %34 = extractvalue { i64, i64 } %33, 0
  %35 = extractvalue { i64, i64 } %33, 1
  %36 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.3 = icmp eq i8 %36, -2
  br i1 %or.cond.not.3, label %L187, label %L189.3

L189.3:                                           ; preds = %L189.2
  %37 = getelementptr i8, ptr addrspace(1) %10, i64 32
  %38 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %34, i64 %35, ptr addrspace(1) %37, ptr addrspace(1) nonnull inttoptr (i64 17 to ptr addrspace(1))) #9
  %39 = extractvalue { i64, i64 } %38, 0
  %40 = extractvalue { i64, i64 } %38, 1
  %41 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.4 = icmp eq i8 %41, -2
  br i1 %or.cond.not.4, label %L187, label %L189.4

L189.4:                                           ; preds = %L189.3
  %42 = getelementptr i8, ptr addrspace(1) %10, i64 40
  %43 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %39, i64 %40, ptr addrspace(1) %42, ptr addrspace(1) nonnull inttoptr (i64 21 to ptr addrspace(1))) #9
  %44 = extractvalue { i64, i64 } %43, 0
  %45 = extractvalue { i64, i64 } %43, 1
  %46 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.5 = icmp eq i8 %46, -2
  br i1 %or.cond.not.5, label %L187, label %L189.5

L189.5:                                           ; preds = %L189.4
  %47 = getelementptr i8, ptr addrspace(1) %10, i64 48
  %48 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %44, i64 %45, ptr addrspace(1) %47, ptr addrspace(1) nonnull inttoptr (i64 25 to ptr addrspace(1))) #9
  %49 = extractvalue { i64, i64 } %48, 0
  %50 = extractvalue { i64, i64 } %48, 1
  %51 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.6 = icmp eq i8 %51, -2
  br i1 %or.cond.not.6, label %L187, label %L189.6

L189.6:                                           ; preds = %L189.5
  %52 = getelementptr i8, ptr addrspace(1) %10, i64 56
  %53 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %49, i64 %50, ptr addrspace(1) %52, ptr addrspace(1) nonnull inttoptr (i64 29 to ptr addrspace(1))) #9
  %54 = extractvalue { i64, i64 } %53, 0
  %55 = extractvalue { i64, i64 } %53, 1
  %56 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.7 = icmp eq i8 %56, -2
  br i1 %or.cond.not.7, label %L187, label %L189.7

L189.7:                                           ; preds = %L189.6
  %57 = getelementptr i8, ptr addrspace(1) %10, i64 64
  %58 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %54, i64 %55, ptr addrspace(1) %57, ptr addrspace(1) nonnull inttoptr (i64 33 to ptr addrspace(1))) #9
  %59 = extractvalue { i64, i64 } %58, 0
  %60 = extractvalue { i64, i64 } %58, 1
  %61 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.8 = icmp eq i8 %61, -2
  br i1 %or.cond.not.8, label %L187, label %L189.8

L189.8:                                           ; preds = %L189.7
  %62 = getelementptr i8, ptr addrspace(1) %10, i64 72
  %63 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %59, i64 %60, ptr addrspace(1) %62, ptr addrspace(1) nonnull inttoptr (i64 37 to ptr addrspace(1))) #9
  %64 = extractvalue { i64, i64 } %63, 0
  %65 = extractvalue { i64, i64 } %63, 1
  %66 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.9 = icmp eq i8 %66, -2
  br i1 %or.cond.not.9, label %L187, label %L189.9

L189.9:                                           ; preds = %L189.8
  %67 = getelementptr i8, ptr addrspace(1) %10, i64 80
  %68 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %64, i64 %65, ptr addrspace(1) %67, ptr addrspace(1) nonnull inttoptr (i64 41 to ptr addrspace(1))) #9
  %69 = extractvalue { i64, i64 } %68, 0
  %70 = extractvalue { i64, i64 } %68, 1
  %71 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.10 = icmp eq i8 %71, -2
  br i1 %or.cond.not.10, label %L187, label %L189.10

L189.10:                                          ; preds = %L189.9
  %72 = getelementptr i8, ptr addrspace(1) %10, i64 88
  %73 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %69, i64 %70, ptr addrspace(1) %72, ptr addrspace(1) nonnull inttoptr (i64 45 to ptr addrspace(1))) #9
  %74 = extractvalue { i64, i64 } %73, 0
  %75 = extractvalue { i64, i64 } %73, 1
  %76 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.11 = icmp eq i8 %76, -2
  br i1 %or.cond.not.11, label %L187, label %L189.11

L189.11:                                          ; preds = %L189.10
  %77 = getelementptr i8, ptr addrspace(1) %10, i64 96
  %78 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %74, i64 %75, ptr addrspace(1) %77, ptr addrspace(1) nonnull inttoptr (i64 49 to ptr addrspace(1))) #9
  %79 = extractvalue { i64, i64 } %78, 0
  %80 = extractvalue { i64, i64 } %78, 1
  %81 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.12 = icmp eq i8 %81, -2
  br i1 %or.cond.not.12, label %L187, label %L189.12

L189.12:                                          ; preds = %L189.11
  %82 = getelementptr i8, ptr addrspace(1) %10, i64 104
  %83 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %79, i64 %80, ptr addrspace(1) %82, ptr addrspace(1) nonnull inttoptr (i64 53 to ptr addrspace(1))) #9
  %84 = extractvalue { i64, i64 } %83, 0
  %85 = extractvalue { i64, i64 } %83, 1
  %86 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.13 = icmp eq i8 %86, -2
  br i1 %or.cond.not.13, label %L187, label %L189.13

L189.13:                                          ; preds = %L189.12
  %87 = getelementptr i8, ptr addrspace(1) %10, i64 112
  %88 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %84, i64 %85, ptr addrspace(1) %87, ptr addrspace(1) nonnull inttoptr (i64 57 to ptr addrspace(1))) #9
  %89 = extractvalue { i64, i64 } %88, 0
  %90 = extractvalue { i64, i64 } %88, 1
  %91 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.14 = icmp eq i8 %91, -2
  br i1 %or.cond.not.14, label %L187, label %L189.14

L189.14:                                          ; preds = %L189.13
  %92 = getelementptr i8, ptr addrspace(1) %10, i64 120
  %93 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %89, i64 %90, ptr addrspace(1) %92, ptr addrspace(1) nonnull inttoptr (i64 61 to ptr addrspace(1))) #9
  %94 = extractvalue { i64, i64 } %93, 0
  %95 = extractvalue { i64, i64 } %93, 1
  %96 = icmp slt i64 %3, 3
  br i1 %96, label %L300, label %L209

L209:                                             ; preds = %L189.14
  %97 = lshr i64 %3, 1
  %98 = icmp slt i64 %2, 3
  %99 = lshr i64 %2, 1
  br label %L215

L215:                                             ; preds = %L287, %L209
  %.0138 = phi i64 [ 1, %L209 ], [ %.0139, %L287 ]
  %.0137 = phi i64 [ 1, %L209 ], [ %134, %L287 ]
  %alloc.2 = phi i64 [ %95, %L209 ], [ %alloc.5, %L287 ]
  %ds.2 = phi i64 [ %94, %L209 ], [ %ds.5, %L287 ]
  %100 = shl i64 %.0137, 1
  %101 = or i64 %100, 1
  br i1 %98, label %L287, label %L227

L227:                                             ; preds = %L215, %L273
  %.0141 = phi i64 [ %.0143, %L273 ], [ %.0138, %L215 ]
  %.0140 = phi i64 [ %132, %L273 ], [ 1, %L215 ]
  %alloc.3 = phi i64 [ %alloc.4, %L273 ], [ %alloc.2, %L215 ]
  %ds.3 = phi i64 [ %ds.4, %L273 ], [ %ds.2, %L215 ]
  %102 = shl i64 %.0140, 1
  %103 = or i64 %102, 1
  %104 = and i64 %103, 511
  %.not151 = icmp eq i64 %104, 1
  br i1 %.not151, label %L243, label %L236

L236:                                             ; preds = %L227
  %105 = add i64 %101, %103
  %106 = shl i64 %105, 1
  %107 = add i64 %106, 62
  %108 = and i64 %107, 62
  %109 = add nsw i64 %108, -1
  br label %L243

L243:                                             ; preds = %L227, %L236
  %.0142 = phi i64 [ %109, %L236 ], [ -1, %L227 ]
  %110 = add i64 %ds.3, 64
  %111 = inttoptr i64 %110 to ptr
  %112 = load i64, ptr %111, align 4
  tail call void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@"\01_camlTry_find_miss_rare__run_5_12_code", %L313))
  %113 = inttoptr i64 %.0142 to ptr addrspace(1)
  %114 = invoke oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_find_miss_rare__scan_4_11_code"(i64 %ds.3, i64 %alloc.3, ptr addrspace(1) %113, ptr addrspace(1) %10, i64 1) #10 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 27, i64 0, i64 24, i64 36, i64 0, i64 36, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 22, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7696942, i64 110) ]
          to label %L314 unwind label %L313

L313:                                             ; preds = %L243
  %115 = landingpad token
          cleanup
  %116 = tail call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %117 = extractvalue { ptr addrspace(1), i64, i64, i64 } %116, 0
  %118 = extractvalue { ptr addrspace(1), i64, i64, i64 } %116, 3
  %119 = add i64 %118, 64
  %120 = inttoptr i64 %119 to ptr
  store i64 %112, ptr %120, align 4
  %or.cond.not154 = icmp eq ptr addrspace(1) %117, inttoptr (i64 ptrtoint (ptr @"\01_camlTry_find_miss_rare__Miss281" to i64) to ptr addrspace(1))
  br i1 %or.cond.not154, label %L264, label %L269

L314:                                             ; preds = %L243
  %121 = extractvalue { { i64, i64 }, { i64 } } %114, 0, 0
  %122 = extractvalue { { i64, i64 }, { i64 } } %114, 0, 1
  %123 = extractvalue { { i64, i64 }, { i64 } } %114, 1, 0
  %124 = add i64 %.0141, -1
  %125 = add i64 %124, %123
  tail call void @llvm.aarch64.oxcaml.pop.trap()
  br label %L273

L264:                                             ; preds = %L313
  %126 = extractvalue { ptr addrspace(1), i64, i64, i64 } %116, 2
  %127 = add i64 %.0141, 2147483647
  %128 = add i64 %127, %103
  %129 = and i64 %128, 2147483647
  br label %L273

L269:                                             ; preds = %L313
  %130 = extractvalue { ptr addrspace(1), i64, i64, i64 } %116, 0
  %131 = ptrtoint ptr addrspace(1) %130 to i64
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 %131)
  unreachable

L273:                                             ; preds = %L264, %L314
  %.0143 = phi i64 [ %125, %L314 ], [ %129, %L264 ]
  %alloc.4 = phi i64 [ %122, %L314 ], [ %126, %L264 ]
  %ds.4 = phi i64 [ %121, %L314 ], [ %118, %L264 ]
  %132 = add i64 %.0140, 1
  %133 = icmp sgt i64 %132, %99
  br i1 %133, label %L287, label %L227

L287:                                             ; preds = %L273, %L215
  %.0139 = phi i64 [ %.0138, %L215 ], [ %.0143, %L273 ]
  %alloc.5 = phi i64 [ %alloc.2, %L215 ], [ %alloc.4, %L273 ]
  %ds.5 = phi i64 [ %ds.2, %L215 ], [ %ds.4, %L273 ]
  %134 = add i64 %.0137, 1
  %135 = icmp sgt i64 %134, %97
  br i1 %135, label %L300, label %L215

L300:                                             ; preds = %L287, %L189.14
  %.0136 = phi i64 [ 1, %L189.14 ], [ %.0139, %L287 ]
  %alloc.6 = phi i64 [ %95, %L189.14 ], [ %alloc.5, %L287 ]
  %ds.6 = phi i64 [ %94, %L189.14 ], [ %ds.5, %L287 ]
  %136 = add i64 %ds.6, 64
  %137 = inttoptr i64 %136 to ptr
  store i64 %6, ptr %137, align 4
  %138 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %ds.6, 0, 0
  %139 = insertvalue { { i64, i64 }, { i64 } } %138, i64 %alloc.6, 0, 1
  %140 = insertvalue { { i64, i64 }, { i64 } } %139, i64 %.0136, 1, 0
  ret { { i64, i64 }, { i64 } } %140
}

; Function Attrs: noinline
define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTry_find_miss_rare__entry"(i64 %0, i64 %1) local_unnamed_addr #1 gc "oxcaml" {
L1:
  %2 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %0, i64 %1, i64 ptrtoint (ptr @"\01_caml_sys_argv" to i64), ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1))) #7 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 6, i64 0, i64 18, i64 26, i64 0, i64 26, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 20, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 28206) ]
  %3 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %2, 0, 0
  %4 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %2, 0, 1
  %5 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %2, 1, 0
  %6 = getelementptr i8, ptr addrspace(1) %5, i64 -8
  %7 = load i64, ptr addrspace(1) %6, align 4
  %8 = and i64 %7, 72057594037925888
  %.not = icmp eq i64 %8, 0
  br i1 %.not, label %L356, label %L332

L332:                                             ; preds = %L1
  %9 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %3, i64 %4, i64 ptrtoint (ptr @"\01_caml_sys_argv" to i64), ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1))) #7 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 6, i64 0, i64 50, i64 58, i64 0, i64 58, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 20, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 28206) ]
  %10 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %9, 1, 0
  %11 = getelementptr i8, ptr addrspace(1) %10, i64 -8
  %12 = load i64, ptr addrspace(1) %11, align 4
  %13 = and i64 %12, 72057594037925888
  %.not150 = icmp eq i64 %13, 0
  br i1 %.not150, label %L350, label %L342

L342:                                             ; preds = %L332
  %14 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %9, 0, 1
  %15 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %9, 0, 0
  %16 = getelementptr i8, ptr addrspace(1) %10, i64 8
  %17 = load ptr addrspace(1), ptr addrspace(1) %16, align 8
  %18 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %15, i64 %14, i64 ptrtoint (ptr @"\01_caml_int_of_string" to i64), ptr addrspace(1) %17) #7 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 6, i64 0, i64 36, i64 62, i64 0, i64 62, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 20, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 28206) ]
  %19 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %18, 0, 0
  %20 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %18, 0, 1
  %21 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %18, 1, 0
  %22 = ptrtoint ptr addrspace(1) %21 to i64
  br label %L356

L350:                                             ; preds = %L332
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 ptrtoint (ptr @"\01_camlTry_find_miss_rare__block35" to i64))
  unreachable

L356:                                             ; preds = %L1, %L342
  %.0 = phi i64 [ %22, %L342 ], [ 200001, %L1 ]
  %alloc.0 = phi i64 [ %20, %L342 ], [ %4, %L1 ]
  %ds.0 = phi i64 [ %19, %L342 ], [ %3, %L1 ]
  %23 = inttoptr i64 %.0 to ptr addrspace(1)
  %24 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_initialize"(i64 %ds.0, i64 %alloc.0, ptr addrspace(1) inttoptr (i64 add (i64 ptrtoint (ptr @"\01_camlTry_find_miss_rare" to i64), i64 24) to ptr addrspace(1)), ptr addrspace(1) %23) #9
  %25 = extractvalue { i64, i64 } %24, 0
  %26 = extractvalue { i64, i64 } %24, 1
  %27 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %25, i64 %26, i64 ptrtoint (ptr @"\01_caml_sys_argv" to i64), ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1))) #7 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 9, i64 0, i64 18, i64 26, i64 0, i64 26, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 23, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 6648366, i64 29552) ]
  %28 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %27, 0, 0
  %29 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %27, 0, 1
  %30 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %27, 1, 0
  %31 = getelementptr i8, ptr addrspace(1) %30, i64 -8
  %32 = load i64, ptr addrspace(1) %31, align 4
  %33 = and i64 %32, 72057594037926912
  %34 = icmp ugt i64 %33, 2560
  br i1 %34, label %L368, label %L392

L368:                                             ; preds = %L356
  %35 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %28, i64 %29, i64 ptrtoint (ptr @"\01_caml_sys_argv" to i64), ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1))) #7 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 9, i64 0, i64 50, i64 58, i64 0, i64 58, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 23, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 6648366, i64 29552) ]
  %36 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %35, 1, 0
  %37 = getelementptr i8, ptr addrspace(1) %36, i64 -8
  %38 = load i64, ptr addrspace(1) %37, align 4
  %39 = and i64 %38, 72057594037926912
  %40 = icmp ugt i64 %39, 2560
  br i1 %40, label %L378, label %L386

L378:                                             ; preds = %L368
  %41 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %35, 0, 1
  %42 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %35, 0, 0
  %43 = getelementptr i8, ptr addrspace(1) %36, i64 16
  %44 = load ptr addrspace(1), ptr addrspace(1) %43, align 8
  %45 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %42, i64 %41, i64 ptrtoint (ptr @"\01_caml_int_of_string" to i64), ptr addrspace(1) %44) #7 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 9, i64 0, i64 36, i64 62, i64 0, i64 62, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 23, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 6648366, i64 29552) ]
  %46 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %45, 0, 0
  %47 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %45, 0, 1
  %48 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %45, 1, 0
  %49 = ptrtoint ptr addrspace(1) %48 to i64
  br label %L392

L386:                                             ; preds = %L368
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 ptrtoint (ptr @"\01_camlTry_find_miss_rare__block35" to i64))
  unreachable

L392:                                             ; preds = %L356, %L378
  %.0148 = phi i64 [ %49, %L378 ], [ 21, %L356 ]
  %alloc.1 = phi i64 [ %47, %L378 ], [ %29, %L356 ]
  %ds.1 = phi i64 [ %46, %L378 ], [ %28, %L356 ]
  %50 = inttoptr i64 %.0148 to ptr addrspace(1)
  %51 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_initialize"(i64 %ds.1, i64 %alloc.1, ptr addrspace(1) inttoptr (i64 add (i64 ptrtoint (ptr @"\01_camlTry_find_miss_rare" to i64), i64 32) to ptr addrspace(1)), ptr addrspace(1) %50) #9
  %52 = extractvalue { i64, i64 } %51, 0
  %53 = extractvalue { i64, i64 } %51, 1
  %54 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_fresh_oo_id"(i64 %52, i64 %53, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1))) #9
  %55 = extractvalue { i64, i64, ptr addrspace(1) } %54, 0
  %56 = extractvalue { i64, i64, ptr addrspace(1) } %54, 1
  %57 = extractvalue { i64, i64, ptr addrspace(1) } %54, 2
  %58 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_initialize"(i64 %55, i64 %56, ptr addrspace(1) inttoptr (i64 add (i64 ptrtoint (ptr @"\01_camlTry_find_miss_rare__Miss281" to i64), i64 8) to ptr addrspace(1)), ptr addrspace(1) %57) #9
  %59 = extractvalue { i64, i64 } %58, 0
  %60 = extractvalue { i64, i64 } %58, 1
  %61 = add i64 %59, 40
  %62 = inttoptr i64 %61 to ptr
  %63 = load i64, ptr %62, align 4
  %64 = add i64 %63, 376
  %65 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #5
  %.not149 = icmp ult i64 %65, %64
  br i1 %.not149, label %L415, label %L416, !prof !1

L415:                                             ; preds = %L392
  %66 = tail call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_llvm_call_realloc_stack"(i64 %59, i64 %60, i64 34) #6
  %67 = extractvalue { { i64, i64 }, {} } %66, 0, 0
  %68 = extractvalue { { i64, i64 }, {} } %66, 0, 1
  br label %L416

L416:                                             ; preds = %L415, %L392
  %alloc.2 = phi i64 [ %60, %L392 ], [ %68, %L415 ]
  %ds.2 = phi i64 [ %59, %L392 ], [ %67, %L415 ]
  %69 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_find_miss_rare__black_box_int_0_7_code"(i64 %ds.2, i64 %alloc.2, i64 %.0148) #7 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 33, i64 0, i64 45, i64 65, i64 0, i64 65, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 18, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417) ]
  %70 = extractvalue { { i64, i64 }, { i64 } } %69, 0, 0
  %71 = extractvalue { { i64, i64 }, { i64 } } %69, 0, 1
  %72 = extractvalue { { i64, i64 }, { i64 } } %69, 1, 0
  %73 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_find_miss_rare__black_box_int_0_7_code"(i64 %70, i64 %71, i64 %.0) #7 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 33, i64 0, i64 27, i64 44, i64 0, i64 44, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 18, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417) ]
  %74 = extractvalue { { i64, i64 }, { i64 } } %73, 0, 0
  %75 = extractvalue { { i64, i64 }, { i64 } } %73, 0, 1
  %76 = extractvalue { { i64, i64 }, { i64 } } %73, 1, 0
  %77 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_find_miss_rare__run_5_12_code"(i64 %74, i64 %75, i64 %76, i64 %72) #7 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 33, i64 0, i64 22, i64 66, i64 0, i64 66, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 18, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417) ]
  %78 = extractvalue { { i64, i64 }, { i64 } } %77, 0, 0
  %79 = extractvalue { { i64, i64 }, { i64 } } %77, 0, 1
  %80 = extractvalue { { i64, i64 }, { i64 } } %77, 1, 0
  %81 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlCamlinternalFormat__make_printf_120_401_code"(i64 %78, i64 %79, i64 ptrtoint (ptr @"\01_camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31" to i64), i64 1, i64 ptrtoint (ptr @"\01_camlTry_find_miss_rare__const_block66" to i64)) #7 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 5, i64 27, i64 0, i64 2, i64 63, i64 0, i64 63, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 23, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 7366251, i64 7235954, i64 26228, i64 34, i64 0, i64 21, i64 43, i64 0, i64 43, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 22, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 7499878, i64 7630441, i64 102, i64 38, i64 0, i64 17, i64 35, i64 0, i64 35, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 21, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 6910576, i64 6714478, i64 11, i64 0, i64 21, i64 63, i64 0, i64 63, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 31, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7499822, i64 7630441, i64 6648415, i64 7107955, i64 116, i64 33, i64 0, i64 9, i64 66, i64 0, i64 66, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 18, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417) ]
  %82 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %81, 0, 0
  %83 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %81, 0, 1
  %84 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %81, 1, 0
  %85 = and i64 %80, 2147483647
  %86 = load i64, ptr addrspace(1) %84, align 4
  %87 = inttoptr i64 %86 to ptr
  %88 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } %87(i64 %82, i64 %83, i64 %85, ptr addrspace(1) %84) #7 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 2, i64 11, i64 0, i64 21, i64 63, i64 0, i64 63, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 31, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7499822, i64 7630441, i64 6648415, i64 7107955, i64 116, i64 33, i64 0, i64 9, i64 66, i64 0, i64 66, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 18, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417) ]
  %89 = extractvalue { { i64, i64 }, { i64 } } %88, 0, 0
  %90 = extractvalue { { i64, i64 }, { i64 } } %88, 0, 1
  %91 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %89, 0, 0
  %92 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %91, i64 %90, 0, 1
  %93 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %92, ptr addrspace(1) inttoptr (i64 1 to ptr addrspace(1)), 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %93
}

declare i32 @"\01_caml_llvm_eh_personality"(...)

; Function Attrs: convergent mustprogress nocallback noduplicate nofree nomerge nounwind willreturn
declare void @llvm.aarch64.oxcaml.pop.trap() #3

; Function Attrs: convergent mustprogress nocallback noduplicate nofree nomerge nounwind willreturn
declare void @llvm.aarch64.oxcaml.push.trap(ptr) #3

; Function Attrs: convergent nocallback noduplicate nofree nomerge noreturn
declare void @llvm.aarch64.oxcaml.raise.notrace(i64) #4

; Function Attrs: convergent mustprogress nocallback noduplicate nofree nomerge nounwind willreturn
declare { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover() #3

attributes #0 = { noinline "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" }
attributes #1 = { noinline "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" }
attributes #2 = { noinline "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="48" }
attributes #3 = { convergent mustprogress nocallback noduplicate nofree nomerge nounwind willreturn }
attributes #4 = { convergent nocallback noduplicate nofree nomerge noreturn }
attributes #5 = { nounwind "gc-leaf-function"="true" }
attributes #6 = { cold "statepoint-id"="0" }
attributes #7 = { "statepoint-id"="0" }
attributes #8 = { cold "statepoint-id"="131073" }
attributes #9 = { "gc-leaf-function"="true" }
attributes #10 = { "statepoint-id"="18" }

!llvm.module.flags = !{!0}

!0 = !{i32 1, !"oxcaml_module", !"Try_find_miss_rare"}
!1 = !{!"branch_weights", i32 1, i32 2000}
