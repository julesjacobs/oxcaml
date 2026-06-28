source_filename = "try_raise_cross_function_caught.ml"

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_raise_cross_function_caught__black_box_int_0_7_code"(i64 %0, i64 %1, i64 %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %4 = alloca i64 
  store i64 %2, ptr %4
  %5 = alloca ptr addrspace(1) 
  %6 = alloca i64 
  %7 = alloca ptr addrspace(1) 
  br label %L1
L1:
  br label %L101
L101:
  %8 = load i64, ptr %4
  store i64 %8, ptr %6
  %9 = load i64, ptr %6
  %10 = call i64 asm  "", "=r,0"(i64 %9) "gc-leaf-function"="true"
  store i64 %10, ptr %6
  %11 = load i64, ptr %6
  %12 = inttoptr i64 %11 to ptr addrspace(1)
  store ptr addrspace(1) %12, ptr %7
  %13 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %13, ptr %5
  %14 = load ptr addrspace(1), ptr %5
  %15 = ptrtoint ptr addrspace(1) %14 to i64
  %16 = load i64, ptr %ds
  %17 = load i64, ptr %alloc
  %18 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %16, 0, 0
  %19 = insertvalue { { i64, i64 }, { i64 } } %18, i64 %17, 0, 1
  %20 = insertvalue { { i64, i64 }, { i64 } } %19, i64 %15, 1, 0
  ret { { i64, i64 }, { i64 } } %20
}

define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTry_raise_cross_function_caught__black_box_string_1_8_code"(i64 %0, i64 %1, ptr addrspace(1) %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %4 = alloca ptr addrspace(1) 
  store ptr addrspace(1) %2, ptr %4
  %5 = alloca ptr addrspace(1) 
  br label %L1
L1:
  br label %L104
L104:
  %6 = load ptr addrspace(1), ptr %4
  store ptr addrspace(1) %6, ptr %5
  %7 = load ptr addrspace(1), ptr %5
  %8 = call ptr addrspace(1) asm  "", "=r,0"(ptr addrspace(1) %7) "gc-leaf-function"="true"
  store ptr addrspace(1) %8, ptr %5
  %9 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %9, ptr %4
  %10 = load ptr addrspace(1), ptr %4
  %11 = load i64, ptr %ds
  %12 = load i64, ptr %alloc
  %13 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %11, 0, 0
  %14 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %13, i64 %12, 0, 1
  %15 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %14, ptr addrspace(1) %10, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %15
}

define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTry_raise_cross_function_caught__black_box_2_9_code"(i64 %0, i64 %1, ptr addrspace(1) %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %4 = alloca ptr addrspace(1) 
  store ptr addrspace(1) %2, ptr %4
  %5 = alloca ptr addrspace(1) 
  br label %L1
L1:
  br label %L107
L107:
  %6 = load ptr addrspace(1), ptr %4
  store ptr addrspace(1) %6, ptr %5
  %7 = load ptr addrspace(1), ptr %5
  %8 = call ptr addrspace(1) asm  "", "=r,0"(ptr addrspace(1) %7) "gc-leaf-function"="true"
  store ptr addrspace(1) %8, ptr %5
  %9 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %9, ptr %4
  %10 = load ptr addrspace(1), ptr %4
  %11 = load i64, ptr %ds
  %12 = load i64, ptr %alloc
  %13 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %11, 0, 0
  %14 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %13, i64 %12, 0, 1
  %15 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %14, ptr addrspace(1) %10, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %15
}

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_raise_cross_function_caught__print_result_3_10_code"(i64 %0, i64 %1, i64 %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %4 = alloca i64 
  store i64 %2, ptr %4
  %5 = alloca ptr addrspace(1) 
  %6 = alloca ptr addrspace(1) 
  %7 = alloca i64 
  %8 = alloca i64 
  %9 = alloca i64 
  %10 = alloca i64 
  %11 = alloca i64 
  %12 = alloca i64 
  %13 = alloca ptr addrspace(1) 
  %14 = alloca ptr addrspace(1) 
  %15 = alloca i64 
  %16 = alloca i64 
  br label %L1
L1:
  br label %L110
L110:
  %17 = load i64, ptr %ds
  %18 = add i64 %17, 40
  %19 = inttoptr i64 %18 to ptr
  %20 = load i64, ptr %19
  %21 = add i64 %20, 376
  %22 = call i64 asm sideeffect "mov $0, sp", "=r"() "gc-leaf-function"="true"
  %23 = icmp uge i64 %22, %21
  %24 = call  i1 @llvm.expect.i1(i1 %23, i1 1) 
  br i1 %24, label %L117, label %L116
L116:
  %25 = load i64, ptr %ds
  %26 = load i64, ptr %alloc
  %27 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_llvm_call_realloc_stack"(i64 %25, i64 %26, i64 34) "statepoint-id"="0" cold
  %28 = extractvalue { { i64, i64 }, {  } } %27, 0, 0
  %29 = extractvalue { { i64, i64 }, {  } } %27, 0, 1
  store i64 %28, ptr %ds
  store i64 %29, ptr %alloc
  br label %L117
L117:
  %30 = load i64, ptr %4
  store i64 %30, ptr %9
  %31 = ptrtoint ptr @"\01_camlTry_raise_cross_function_caught__const_block66" to i64
  store i64 %31, ptr %10
  %32 = ptrtoint ptr @"\01_camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31" to i64
  store i64 %32, ptr %12
  %33 = load i64, ptr %12
  store i64 %33, ptr %4
  store i64 1, ptr %7
  %34 = load i64, ptr %10
  store i64 %34, ptr %8
  %35 = load i64, ptr %4
  %36 = load i64, ptr %7
  %37 = load i64, ptr %8
  %38 = load i64, ptr %ds
  %39 = load i64, ptr %alloc
  %40 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlCamlinternalFormat__make_printf_120_401_code"(i64 %38, i64 %39, i64 %35, i64 %36, i64 %37) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 4, i64 27, i64 0, i64 2, i64 63, i64 0, i64 63, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 23, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 7366251, i64 7235954, i64 26228, i64 34, i64 0, i64 21, i64 43, i64 0, i64 43, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 22, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 7499878, i64 7630441, i64 102, i64 38, i64 0, i64 17, i64 35, i64 0, i64 35, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 21, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 6910576, i64 6714478, i64 11, i64 0, i64 21, i64 63, i64 0, i64 63, i64 34, i64 7959156, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7155316, i64 108, i64 44, i64 7959124, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7351924, i64 7235954, i64 7495540, i64 7697253, i64 29804) ]
  %41 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %40, 0, 0
  %42 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %40, 0, 1
  store i64 %41, ptr %ds
  store i64 %42, ptr %alloc
  %43 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %40, 1, 0
  store ptr addrspace(1) %43, ptr %5
  br label %L112
L112:
  %44 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %44, ptr %13
  %45 = load ptr addrspace(1), ptr %13
  store ptr addrspace(1) %45, ptr %14
  %46 = load i64, ptr %9
  %47 = and i64 %46, 2147483647
  store i64 %47, ptr %15
  %48 = load ptr addrspace(1), ptr %14
  %49 = load i64, ptr addrspace(1) %48
  store i64 %49, ptr %16
  %50 = load i64, ptr %15
  store i64 %50, ptr %4
  %51 = load ptr addrspace(1), ptr %14
  store ptr addrspace(1) %51, ptr %6
  %52 = load i64, ptr %4
  %53 = load ptr addrspace(1), ptr %6
  %54 = load i64, ptr %ds
  %55 = load i64, ptr %alloc
  %56 = load i64, ptr %16
  %57 = inttoptr i64 %56 to ptr
  %58 = musttail call oxcaml_nofpcc { { i64, i64 }, { i64 } } %57(i64 %54, i64 %55, i64 %52, ptr addrspace(1) %53) "statepoint-id"="0"
  ret { { i64, i64 }, { i64 } } %58
}

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_raise_cross_function_caught__probe_4_11_code"(i64 %0, i64 %1, i64 %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %4 = alloca i64 
  store i64 %2, ptr %4
  %5 = alloca ptr addrspace(1) 
  %6 = alloca i64 
  %7 = alloca i64 
  br label %L1
L1:
  br label %L119
L119:
  %8 = load i64, ptr %4
  store i64 %8, ptr %6
  %9 = load i64, ptr %6
  %10 = icmp slt i64 %9, 1
  br i1 %10, label %L123, label %L126
L126:
  %11 = load i64, ptr %6
  %12 = icmp sgt i64 %11, 1
  br i1 %12, label %L123, label %L121
L121:
  %13 = ptrtoint ptr @"\01_camlTry_raise_cross_function_caught__Miss245" to i64
  store i64 %13, ptr %7
  %14 = load i64, ptr %7
  %15 = inttoptr i64 %14 to ptr addrspace(1)
  store ptr addrspace(1) %15, ptr %5
  %16 = load ptr addrspace(1), ptr %5
  %17 = ptrtoint ptr addrspace(1) %16 to i64
  call  void @llvm.aarch64.oxcaml.raise.notrace(i64 %17) 
  unreachable
L123:
  %18 = load i64, ptr %6
  store i64 %18, ptr %4
  %19 = load i64, ptr %4
  %20 = load i64, ptr %ds
  %21 = load i64, ptr %alloc
  %22 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %20, 0, 0
  %23 = insertvalue { { i64, i64 }, { i64 } } %22, i64 %21, 0, 1
  %24 = insertvalue { { i64, i64 }, { i64 } } %23, i64 %19, 1, 0
  ret { { i64, i64 }, { i64 } } %24
}

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_raise_cross_function_caught__find_5_12_code"(i64 %0, i64 %1, i64 %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="48" noinline gc "oxcaml" personality ptr @"\01_caml_llvm_eh_personality" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %4 = alloca i64 
  store i64 %2, ptr %4
  %5 = alloca ptr addrspace(1) 
  %6 = alloca i64 
  %7 = alloca i64 
  %8 = alloca i64 
  %9 = alloca ptr addrspace(1) 
  %10 = alloca i64 
  %11 = alloca i64 
  %12 = alloca i64 
  %13 = alloca i64 
  %14 = alloca i64 
  %15 = alloca i64 
  %16 = alloca i64
  br label %L1
L1:
  br label %L128
L128:
  %17 = load i64, ptr %ds
  %18 = add i64 %17, 40
  %19 = inttoptr i64 %18 to ptr
  %20 = load i64, ptr %19
  %21 = add i64 %20, 408
  %22 = call i64 asm sideeffect "mov $0, sp", "=r"() "gc-leaf-function"="true"
  %23 = icmp uge i64 %22, %21
  %24 = call  i1 @llvm.expect.i1(i1 %23, i1 1) 
  br i1 %24, label %L151, label %L150
L150:
  %25 = load i64, ptr %ds
  %26 = load i64, ptr %alloc
  %27 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_llvm_call_realloc_stack"(i64 %25, i64 %26, i64 38) "statepoint-id"="0" cold
  %28 = extractvalue { { i64, i64 }, {  } } %27, 0, 0
  %29 = extractvalue { { i64, i64 }, {  } } %27, 0, 1
  store i64 %28, ptr %ds
  store i64 %29, ptr %alloc
  br label %L151
L151:
  %30 = load i64, ptr %4
  store i64 %30, ptr %6
  %31 = load i64, ptr %ds
  %32 = add i64 %31, 64
  %33 = inttoptr i64 %32 to ptr
  %34 = load i64, ptr %33
  store i64 %34, ptr %7
  %35 = load i64, ptr %7
  store i64 %35, ptr %8
  call  void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@"\01_camlTry_raise_cross_function_caught__find_5_12_code", %L153)) 
  br label %L152
L153:
  %36 = landingpad token cleanup
  %37 = call  { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover() 
  %38 = extractvalue { ptr addrspace(1), i64, i64, i64 } %37, 0
  %39 = extractvalue { ptr addrspace(1), i64, i64, i64 } %37, 2
  %40 = extractvalue { ptr addrspace(1), i64, i64, i64 } %37, 3
  store ptr addrspace(1) %38, ptr %16
  store i64 %40, ptr %ds
  store i64 %39, ptr %alloc
  br label %L131
L152:
  %41 = load i64, ptr %6
  store i64 %41, ptr %4
  %42 = load i64, ptr %4
  %43 = load i64, ptr %ds
  %44 = load i64, ptr %alloc
  %45 = invoke oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_raise_cross_function_caught__probe_4_11_code"(i64 %43, i64 %44, i64 %42) "statepoint-id"="18" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 20, i64 0, i64 6, i64 13, i64 0, i64 13, i64 34, i64 7959156, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7155316, i64 108, i64 36, i64 7959124, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 6696564, i64 6581865) ] to label %L154 unwind label %L153
L154:
  %46 = extractvalue { { i64, i64 }, { i64 } } %45, 0, 0
  %47 = extractvalue { { i64, i64 }, { i64 } } %45, 0, 1
  store i64 %46, ptr %ds
  store i64 %47, ptr %alloc
  %48 = extractvalue { { i64, i64 }, { i64 } } %45, 1, 0
  store i64 %48, ptr %4
  br label %L139
L139:
  %49 = load i64, ptr %4
  store i64 %49, ptr %10
  %50 = load i64, ptr %10
  store i64 %50, ptr %11
  call  void @llvm.aarch64.oxcaml.pop.trap() 
  %51 = load i64, ptr %11
  store i64 %51, ptr %4
  %52 = load i64, ptr %4
  %53 = load i64, ptr %ds
  %54 = load i64, ptr %alloc
  %55 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %53, 0, 0
  %56 = insertvalue { { i64, i64 }, { i64 } } %55, i64 %54, 0, 1
  %57 = insertvalue { { i64, i64 }, { i64 } } %56, i64 %52, 1, 0
  ret { { i64, i64 }, { i64 } } %57
L131:
  %58 = load i64, ptr %16
  %59 = inttoptr i64 %58 to ptr addrspace(1)
  store ptr addrspace(1) %59, ptr %5
  %60 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %60, ptr %9
  %61 = load i64, ptr %ds
  %62 = add i64 %61, 64
  %63 = inttoptr i64 %62 to ptr
  %64 = load i64, ptr %8
  store i64 %64, ptr %63
  store i64 1, ptr %13
  %65 = ptrtoint ptr @"\01_camlTry_raise_cross_function_caught__Miss245" to i64
  store i64 %65, ptr %14
  %66 = load ptr addrspace(1), ptr %9
  %67 = load i64, ptr %14
  %68 = inttoptr i64 %67 to ptr addrspace(1)
  %69 = icmp slt ptr addrspace(1) %66, %68
  br i1 %69, label %L146, label %L155
L155:
  %70 = load ptr addrspace(1), ptr %9
  %71 = load i64, ptr %14
  %72 = inttoptr i64 %71 to ptr addrspace(1)
  %73 = icmp sgt ptr addrspace(1) %70, %72
  br i1 %73, label %L146, label %L144
L144:
  store i64 3, ptr %4
  %74 = load i64, ptr %4
  %75 = load i64, ptr %ds
  %76 = load i64, ptr %alloc
  %77 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %75, 0, 0
  %78 = insertvalue { { i64, i64 }, { i64 } } %77, i64 %76, 0, 1
  %79 = insertvalue { { i64, i64 }, { i64 } } %78, i64 %74, 1, 0
  ret { { i64, i64 }, { i64 } } %79
L146:
  %80 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %80, ptr %5
  %81 = load ptr addrspace(1), ptr %5
  %82 = ptrtoint ptr addrspace(1) %81 to i64
  call  void @llvm.aarch64.oxcaml.raise.notrace(i64 %82) 
  unreachable
}

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_raise_cross_function_caught__run_6_13_code"(i64 %0, i64 %1, i64 %2, i64 %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %5 = alloca i64 
  store i64 %2, ptr %5
  %6 = alloca i64 
  store i64 %3, ptr %6
  %7 = alloca i64 
  %8 = alloca i64 
  %9 = alloca i64 
  %10 = alloca i64 
  %11 = alloca i64 
  %12 = alloca i64 
  %13 = alloca i64 
  %14 = alloca i64 
  %15 = alloca i64 
  %16 = alloca i64 
  %17 = alloca i64 
  %18 = alloca i64 
  %19 = alloca i64 
  %20 = alloca i64 
  %21 = alloca i64 
  %22 = alloca i64 
  %23 = alloca i64 
  %24 = alloca i64 
  %25 = alloca i64 
  %26 = alloca i64 
  %27 = alloca i64 
  %28 = alloca i64 
  %29 = alloca i64 
  %30 = alloca i64 
  %31 = alloca i64 
  %32 = alloca i64 
  %33 = alloca i64 
  %34 = alloca i64 
  %35 = alloca i64 
  %36 = alloca i64 
  %37 = alloca i64 
  %38 = alloca i64 
  %39 = alloca i64 
  %40 = alloca i64 
  %41 = alloca i64 
  br label %L1
L1:
  br label %L157
L157:
  %42 = load i64, ptr %5
  store i64 %42, ptr %7
  %43 = load i64, ptr %6
  store i64 %43, ptr %8
  %44 = load i64, ptr %8
  %45 = icmp slt i64 %44, 3
  br i1 %45, label %L201, label %L204
L204:
  %46 = load i64, ptr %8
  %47 = icmp sgt i64 %46, 3
  br i1 %47, label %L159, label %L159
L159:
  %48 = load i64, ptr %ds
  %49 = add i64 %48, 40
  %50 = inttoptr i64 %49 to ptr
  %51 = load i64, ptr %50
  %52 = add i64 %51, 376
  %53 = call i64 asm sideeffect "mov $0, sp", "=r"() "gc-leaf-function"="true"
  %54 = icmp uge i64 %53, %52
  %55 = call  i1 @llvm.expect.i1(i1 %54, i1 1) 
  br i1 %55, label %L206, label %L205
L205:
  %56 = load i64, ptr %ds
  %57 = load i64, ptr %alloc
  %58 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_llvm_call_realloc_stack"(i64 %56, i64 %57, i64 34) "statepoint-id"="0" cold
  %59 = extractvalue { { i64, i64 }, {  } } %58, 0, 0
  %60 = extractvalue { { i64, i64 }, {  } } %58, 0, 1
  store i64 %59, ptr %ds
  store i64 %60, ptr %alloc
  br label %L206
L206:
  %61 = load i64, ptr %8
  %62 = ashr i64 %61, 1
  store i64 %62, ptr %9
  %63 = load i64, ptr %9
  store i64 %63, ptr %10
  store i64 1, ptr %15
  store i64 1, ptr %16
  %64 = load i64, ptr %15
  store i64 %64, ptr %11
  %65 = load i64, ptr %16
  store i64 %65, ptr %12
  %66 = load i64, ptr %7
  %67 = icmp slt i64 %66, 3
  br i1 %67, label %L188, label %L207
L207:
  %68 = load i64, ptr %7
  %69 = icmp sgt i64 %68, 3
  br i1 %69, label %L170, label %L170
L170:
  %70 = load i64, ptr %7
  %71 = ashr i64 %70, 1
  store i64 %71, ptr %18
  %72 = load i64, ptr %18
  store i64 %72, ptr %19
  store i64 1, ptr %23
  %73 = load i64, ptr %12
  store i64 %73, ptr %24
  %74 = load i64, ptr %23
  store i64 %74, ptr %20
  %75 = load i64, ptr %24
  store i64 %75, ptr %21
  br label %L176
L176:
  store i64 1, ptr %5
  %76 = load i64, ptr %5
  %77 = load i64, ptr %ds
  %78 = load i64, ptr %alloc
  %79 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_raise_cross_function_caught__find_5_12_code"(i64 %77, i64 %78, i64 %76) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 26, i64 0, i64 20, i64 26, i64 0, i64 26, i64 34, i64 7959156, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7155316, i64 108, i64 35, i64 7959124, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7482996, i64 28277) ]
  %80 = extractvalue { { i64, i64 }, { i64 } } %79, 0, 0
  %81 = extractvalue { { i64, i64 }, { i64 } } %79, 0, 1
  store i64 %80, ptr %ds
  store i64 %81, ptr %alloc
  %82 = extractvalue { { i64, i64 }, { i64 } } %79, 1, 0
  store i64 %82, ptr %5
  br label %L178
L178:
  %83 = load i64, ptr %5
  store i64 %83, ptr %26
  %84 = load i64, ptr %26
  store i64 %84, ptr %27
  %85 = load i64, ptr %21
  %86 = load i64, ptr %27
  %87 = add i64 %85, %86
  store i64 %87, ptr %28
  %88 = load i64, ptr %28
  %89 = add i64 %88, -1
  store i64 %89, ptr %29
  %90 = load i64, ptr %29
  store i64 %90, ptr %30
  %91 = load i64, ptr %20
  %92 = add i64 %91, 1
  store i64 %92, ptr %31
  %93 = load i64, ptr %31
  store i64 %93, ptr %32
  %94 = load i64, ptr %32
  %95 = load i64, ptr %19
  %96 = icmp slt i64 %94, %95
  br i1 %96, label %L182, label %L208
L208:
  %97 = load i64, ptr %32
  %98 = load i64, ptr %19
  %99 = icmp sgt i64 %97, %98
  br i1 %99, label %L184, label %L182
L182:
  %100 = load i64, ptr %32
  store i64 %100, ptr %33
  %101 = load i64, ptr %30
  store i64 %101, ptr %34
  %102 = load i64, ptr %33
  store i64 %102, ptr %20
  %103 = load i64, ptr %34
  store i64 %103, ptr %21
  br label %L176
L184:
  %104 = load i64, ptr %30
  store i64 %104, ptr %35
  %105 = load i64, ptr %35
  store i64 %105, ptr %17
  br label %L191
L188:
  %106 = load i64, ptr %12
  store i64 %106, ptr %36
  %107 = load i64, ptr %36
  store i64 %107, ptr %17
  br label %L191
L191:
  %108 = load i64, ptr %11
  %109 = add i64 %108, 1
  store i64 %109, ptr %37
  %110 = load i64, ptr %37
  store i64 %110, ptr %38
  %111 = load i64, ptr %38
  %112 = load i64, ptr %10
  %113 = icmp slt i64 %111, %112
  br i1 %113, label %L194, label %L209
L209:
  %114 = load i64, ptr %38
  %115 = load i64, ptr %10
  %116 = icmp sgt i64 %114, %115
  br i1 %116, label %L196, label %L194
L194:
  %117 = load i64, ptr %38
  store i64 %117, ptr %39
  %118 = load i64, ptr %17
  store i64 %118, ptr %40
  %119 = load i64, ptr %39
  store i64 %119, ptr %11
  %120 = load i64, ptr %40
  store i64 %120, ptr %12
  %121 = load i64, ptr %7
  %122 = icmp slt i64 %121, 3
  br i1 %122, label %L188, label %L210
L210:
  %123 = load i64, ptr %7
  %124 = icmp sgt i64 %123, 3
  br i1 %124, label %L170, label %L170
L196:
  %125 = load i64, ptr %17
  store i64 %125, ptr %5
  %126 = load i64, ptr %5
  %127 = load i64, ptr %ds
  %128 = load i64, ptr %alloc
  %129 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %127, 0, 0
  %130 = insertvalue { { i64, i64 }, { i64 } } %129, i64 %128, 0, 1
  %131 = insertvalue { { i64, i64 }, { i64 } } %130, i64 %126, 1, 0
  ret { { i64, i64 }, { i64 } } %131
L201:
  store i64 1, ptr %5
  %132 = load i64, ptr %5
  %133 = load i64, ptr %ds
  %134 = load i64, ptr %alloc
  %135 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %133, 0, 0
  %136 = insertvalue { { i64, i64 }, { i64 } } %135, i64 %134, 0, 1
  %137 = insertvalue { { i64, i64 }, { i64 } } %136, i64 %132, 1, 0
  ret { { i64, i64 }, { i64 } } %137
}

define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTry_raise_cross_function_caught__entry"(i64 %0, i64 %1) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %3 = alloca ptr addrspace(1) 
  %4 = alloca ptr addrspace(1) 
  %5 = alloca i64 
  %6 = alloca ptr addrspace(1) 
  %7 = alloca i64 
  %8 = alloca i64 
  %9 = alloca ptr addrspace(1) 
  %10 = alloca i64 
  %11 = alloca i64 
  %12 = alloca ptr addrspace(1) 
  %13 = alloca ptr addrspace(1) 
  %14 = alloca ptr addrspace(1) 
  %15 = alloca i64 
  %16 = alloca i64 
  %17 = alloca i64 
  %18 = alloca i64 
  %19 = alloca ptr addrspace(1) 
  %20 = alloca ptr addrspace(1) 
  %21 = alloca ptr addrspace(1) 
  %22 = alloca i64 
  %23 = alloca i64 
  %24 = alloca i64 
  %25 = alloca ptr addrspace(1) 
  %26 = alloca ptr addrspace(1) 
  %27 = alloca ptr addrspace(1) 
  %28 = alloca ptr addrspace(1) 
  %29 = alloca ptr addrspace(1) 
  %30 = alloca i64 
  %31 = alloca i64 
  %32 = alloca i64 
  %33 = alloca i64 
  %34 = alloca i64 
  %35 = alloca i64 
  %36 = alloca i64 
  %37 = alloca i64 
  %38 = alloca ptr addrspace(1) 
  %39 = alloca ptr addrspace(1) 
  %40 = alloca ptr addrspace(1) 
  %41 = alloca i64 
  %42 = alloca i64 
  %43 = alloca i64 
  %44 = alloca i64 
  %45 = alloca ptr addrspace(1) 
  %46 = alloca ptr addrspace(1) 
  %47 = alloca ptr addrspace(1) 
  %48 = alloca i64 
  %49 = alloca i64 
  %50 = alloca i64 
  %51 = alloca ptr addrspace(1) 
  %52 = alloca ptr addrspace(1) 
  %53 = alloca ptr addrspace(1) 
  %54 = alloca ptr addrspace(1) 
  %55 = alloca ptr addrspace(1) 
  %56 = alloca i64 
  %57 = alloca i64 
  %58 = alloca i64 
  %59 = alloca i64 
  %60 = alloca i64 
  %61 = alloca i64 
  %62 = alloca i64 
  %63 = alloca ptr addrspace(1) 
  %64 = alloca ptr addrspace(1) 
  %65 = alloca i64 
  %66 = alloca i64 
  %67 = alloca i64 
  %68 = alloca i64 
  %69 = alloca i64 
  %70 = alloca i64 
  %71 = alloca i64 
  %72 = alloca i64 
  %73 = alloca i64 
  %74 = alloca i64 
  %75 = alloca i64 
  %76 = alloca i64 
  %77 = alloca ptr addrspace(1) 
  %78 = alloca ptr addrspace(1) 
  %79 = alloca i64 
  %80 = alloca i64 
  %81 = alloca i64 
  %82 = alloca i64 
  %83 = alloca i64 
  %84 = alloca i64 
  %85 = alloca i64 
  br label %L1
L1:
  br label %L218
L218:
  %86 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %86, ptr %3
  %87 = ptrtoint ptr @"\01_caml_sys_argv" to i64
  %88 = load ptr addrspace(1), ptr %3
  %89 = load i64, ptr %ds
  %90 = load i64, ptr %alloc
  %91 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %89, i64 %90, i64 %87, ptr addrspace(1) %88) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 6, i64 0, i64 18, i64 26, i64 0, i64 26, i64 34, i64 7959156, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7155316, i64 108, i64 33, i64 7959124, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7220852) ]
  %92 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %91, 0, 0
  %93 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %91, 0, 1
  store i64 %92, ptr %ds
  store i64 %93, ptr %alloc
  %94 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %91, 1, 0
  store ptr addrspace(1) %94, ptr %3
  br label %L220
L220:
  %95 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %95, ptr %12
  %96 = load ptr addrspace(1), ptr %12
  store ptr addrspace(1) %96, ptr %13
  %97 = load ptr addrspace(1), ptr %13
  %98 = getelementptr i8, ptr addrspace(1) %97, i64 -8
  store ptr addrspace(1) %98, ptr %14
  %99 = load ptr addrspace(1), ptr %14
  %100 = load i64, ptr addrspace(1) %99
  store i64 %100, ptr %15
  %101 = load i64, ptr %15
  %102 = shl i64 %101, 8
  store i64 %102, ptr %16
  %103 = load i64, ptr %16
  %104 = lshr i64 %103, 17
  store i64 %104, ptr %17
  %105 = load i64, ptr %17
  %106 = icmp slt i64 %105, 3
  br i1 %106, label %L246, label %L304
L304:
  %107 = load i64, ptr %17
  %108 = icmp sgt i64 %107, 3
  br i1 %108, label %L225, label %L246
L225:
  %109 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %109, ptr %3
  %110 = ptrtoint ptr @"\01_caml_sys_argv" to i64
  %111 = load ptr addrspace(1), ptr %3
  %112 = load i64, ptr %ds
  %113 = load i64, ptr %alloc
  %114 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %112, i64 %113, i64 %110, ptr addrspace(1) %111) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 6, i64 0, i64 50, i64 58, i64 0, i64 58, i64 34, i64 7959156, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7155316, i64 108, i64 33, i64 7959124, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7220852) ]
  %115 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %114, 0, 0
  %116 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %114, 0, 1
  store i64 %115, ptr %ds
  store i64 %116, ptr %alloc
  %117 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %114, 1, 0
  store ptr addrspace(1) %117, ptr %3
  br label %L227
L227:
  %118 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %118, ptr %19
  %119 = load ptr addrspace(1), ptr %19
  store ptr addrspace(1) %119, ptr %20
  %120 = load ptr addrspace(1), ptr %20
  %121 = getelementptr i8, ptr addrspace(1) %120, i64 -8
  store ptr addrspace(1) %121, ptr %21
  %122 = load ptr addrspace(1), ptr %21
  %123 = load i64, ptr addrspace(1) %122
  store i64 %123, ptr %22
  %124 = load i64, ptr %22
  %125 = shl i64 %124, 8
  store i64 %125, ptr %23
  %126 = load i64, ptr %23
  %127 = lshr i64 %126, 17
  store i64 %127, ptr %24
  %128 = load i64, ptr %24
  %129 = icmp ult i64 %128, 3
  br i1 %129, label %L243, label %L305
L305:
  %130 = load i64, ptr %24
  %131 = icmp ugt i64 %130, 3
  br i1 %131, label %L235, label %L243
L235:
  %132 = load ptr addrspace(1), ptr %20
  %133 = getelementptr i8, ptr addrspace(1) %132, i64 8
  store ptr addrspace(1) %133, ptr %25
  %134 = load ptr addrspace(1), ptr %25
  %135 = load ptr addrspace(1), ptr addrspace(1) %134
  store ptr addrspace(1) %135, ptr %26
  %136 = load ptr addrspace(1), ptr %26
  store ptr addrspace(1) %136, ptr %3
  %137 = ptrtoint ptr @"\01_caml_int_of_string" to i64
  %138 = load ptr addrspace(1), ptr %3
  %139 = load i64, ptr %ds
  %140 = load i64, ptr %alloc
  %141 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %139, i64 %140, i64 %137, ptr addrspace(1) %138) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 6, i64 0, i64 36, i64 62, i64 0, i64 62, i64 34, i64 7959156, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7155316, i64 108, i64 33, i64 7959124, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7220852) ]
  %142 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %141, 0, 0
  %143 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %141, 0, 1
  store i64 %142, ptr %ds
  store i64 %143, ptr %alloc
  %144 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %141, 1, 0
  store ptr addrspace(1) %144, ptr %3
  br label %L237
L237:
  %145 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %145, ptr %27
  %146 = load ptr addrspace(1), ptr %27
  store ptr addrspace(1) %146, ptr %28
  %147 = load ptr addrspace(1), ptr %28
  store ptr addrspace(1) %147, ptr %29
  %148 = load ptr addrspace(1), ptr %29
  %149 = ptrtoint ptr addrspace(1) %148 to i64
  store i64 %149, ptr %10
  br label %L249
L243:
  %150 = ptrtoint ptr @"\01_camlTry_raise_cross_function_caught__block35" to i64
  store i64 %150, ptr %30
  %151 = load i64, ptr %30
  %152 = inttoptr i64 %151 to ptr addrspace(1)
  store ptr addrspace(1) %152, ptr %3
  %153 = load ptr addrspace(1), ptr %3
  %154 = ptrtoint ptr addrspace(1) %153 to i64
  call  void @llvm.aarch64.oxcaml.raise.notrace(i64 %154) 
  unreachable
L246:
  store i64 200001, ptr %32
  %155 = load i64, ptr %32
  store i64 %155, ptr %10
  br label %L249
L249:
  %156 = ptrtoint ptr @"\01_camlTry_raise_cross_function_caught" to i64
  store i64 %156, ptr %33
  %157 = load i64, ptr %33
  %158 = add i64 %157, 24
  store i64 %158, ptr %34
  %159 = load i64, ptr %34
  %160 = inttoptr i64 %159 to ptr addrspace(1)
  store ptr addrspace(1) %160, ptr %4
  %161 = load i64, ptr %10
  %162 = inttoptr i64 %161 to ptr addrspace(1)
  store ptr addrspace(1) %162, ptr %6
  %163 = ptrtoint ptr @"\01_caml_initialize" to i64
  %164 = load ptr addrspace(1), ptr %4
  %165 = load ptr addrspace(1), ptr %6
  %166 = load i64, ptr %ds
  %167 = load i64, ptr %alloc
  %168 = call oxcaml_c_directcc { i64, i64 } @"\01_caml_initialize"(i64 %166, i64 %167, ptr addrspace(1) %164, ptr addrspace(1) %165) "gc-leaf-function"="true"
  %169 = extractvalue { i64, i64 } %168, 0
  %170 = extractvalue { i64, i64 } %168, 1
  store i64 %169, ptr %ds
  store i64 %170, ptr %alloc
  br label %L251
L251:
  %171 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %171, ptr %3
  %172 = ptrtoint ptr @"\01_caml_sys_argv" to i64
  %173 = load ptr addrspace(1), ptr %3
  %174 = load i64, ptr %ds
  %175 = load i64, ptr %alloc
  %176 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %174, i64 %175, i64 %172, ptr addrspace(1) %173) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 9, i64 0, i64 18, i64 26, i64 0, i64 26, i64 34, i64 7959156, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7155316, i64 108, i64 36, i64 7959124, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7482996, i64 7565413) ]
  %177 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %176, 0, 0
  %178 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %176, 0, 1
  store i64 %177, ptr %ds
  store i64 %178, ptr %alloc
  %179 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %176, 1, 0
  store ptr addrspace(1) %179, ptr %3
  br label %L256
L256:
  %180 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %180, ptr %38
  %181 = load ptr addrspace(1), ptr %38
  store ptr addrspace(1) %181, ptr %39
  %182 = load ptr addrspace(1), ptr %39
  %183 = getelementptr i8, ptr addrspace(1) %182, i64 -8
  store ptr addrspace(1) %183, ptr %40
  %184 = load ptr addrspace(1), ptr %40
  %185 = load i64, ptr addrspace(1) %184
  store i64 %185, ptr %41
  %186 = load i64, ptr %41
  %187 = shl i64 %186, 8
  store i64 %187, ptr %42
  %188 = load i64, ptr %42
  %189 = lshr i64 %188, 17
  store i64 %189, ptr %43
  %190 = load i64, ptr %43
  %191 = icmp slt i64 %190, 5
  br i1 %191, label %L282, label %L306
L306:
  %192 = load i64, ptr %43
  %193 = icmp sgt i64 %192, 5
  br i1 %193, label %L261, label %L282
L261:
  %194 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %194, ptr %3
  %195 = ptrtoint ptr @"\01_caml_sys_argv" to i64
  %196 = load ptr addrspace(1), ptr %3
  %197 = load i64, ptr %ds
  %198 = load i64, ptr %alloc
  %199 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %197, i64 %198, i64 %195, ptr addrspace(1) %196) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 9, i64 0, i64 50, i64 58, i64 0, i64 58, i64 34, i64 7959156, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7155316, i64 108, i64 36, i64 7959124, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7482996, i64 7565413) ]
  %200 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %199, 0, 0
  %201 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %199, 0, 1
  store i64 %200, ptr %ds
  store i64 %201, ptr %alloc
  %202 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %199, 1, 0
  store ptr addrspace(1) %202, ptr %3
  br label %L263
L263:
  %203 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %203, ptr %45
  %204 = load ptr addrspace(1), ptr %45
  store ptr addrspace(1) %204, ptr %46
  %205 = load ptr addrspace(1), ptr %46
  %206 = getelementptr i8, ptr addrspace(1) %205, i64 -8
  store ptr addrspace(1) %206, ptr %47
  %207 = load ptr addrspace(1), ptr %47
  %208 = load i64, ptr addrspace(1) %207
  store i64 %208, ptr %48
  %209 = load i64, ptr %48
  %210 = shl i64 %209, 8
  store i64 %210, ptr %49
  %211 = load i64, ptr %49
  %212 = lshr i64 %211, 17
  store i64 %212, ptr %50
  %213 = load i64, ptr %50
  %214 = icmp ult i64 %213, 5
  br i1 %214, label %L279, label %L307
L307:
  %215 = load i64, ptr %50
  %216 = icmp ugt i64 %215, 5
  br i1 %216, label %L271, label %L279
L271:
  %217 = load ptr addrspace(1), ptr %46
  %218 = getelementptr i8, ptr addrspace(1) %217, i64 16
  store ptr addrspace(1) %218, ptr %51
  %219 = load ptr addrspace(1), ptr %51
  %220 = load ptr addrspace(1), ptr addrspace(1) %219
  store ptr addrspace(1) %220, ptr %52
  %221 = load ptr addrspace(1), ptr %52
  store ptr addrspace(1) %221, ptr %3
  %222 = ptrtoint ptr @"\01_caml_int_of_string" to i64
  %223 = load ptr addrspace(1), ptr %3
  %224 = load i64, ptr %ds
  %225 = load i64, ptr %alloc
  %226 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %224, i64 %225, i64 %222, ptr addrspace(1) %223) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 9, i64 0, i64 36, i64 62, i64 0, i64 62, i64 34, i64 7959156, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7155316, i64 108, i64 36, i64 7959124, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7482996, i64 7565413) ]
  %227 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %226, 0, 0
  %228 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %226, 0, 1
  store i64 %227, ptr %ds
  store i64 %228, ptr %alloc
  %229 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %226, 1, 0
  store ptr addrspace(1) %229, ptr %3
  br label %L273
L273:
  %230 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %230, ptr %53
  %231 = load ptr addrspace(1), ptr %53
  store ptr addrspace(1) %231, ptr %54
  %232 = load ptr addrspace(1), ptr %54
  store ptr addrspace(1) %232, ptr %55
  %233 = load ptr addrspace(1), ptr %55
  %234 = ptrtoint ptr addrspace(1) %233 to i64
  store i64 %234, ptr %36
  br label %L285
L279:
  %235 = ptrtoint ptr @"\01_camlTry_raise_cross_function_caught__block35" to i64
  store i64 %235, ptr %56
  %236 = load i64, ptr %56
  %237 = inttoptr i64 %236 to ptr addrspace(1)
  store ptr addrspace(1) %237, ptr %3
  %238 = load ptr addrspace(1), ptr %3
  %239 = ptrtoint ptr addrspace(1) %238 to i64
  call  void @llvm.aarch64.oxcaml.raise.notrace(i64 %239) 
  unreachable
L282:
  store i64 21, ptr %58
  %240 = load i64, ptr %58
  store i64 %240, ptr %36
  br label %L285
L285:
  %241 = ptrtoint ptr @"\01_camlTry_raise_cross_function_caught" to i64
  store i64 %241, ptr %59
  %242 = load i64, ptr %59
  %243 = add i64 %242, 32
  store i64 %243, ptr %60
  %244 = load i64, ptr %60
  %245 = inttoptr i64 %244 to ptr addrspace(1)
  store ptr addrspace(1) %245, ptr %4
  %246 = load i64, ptr %36
  %247 = inttoptr i64 %246 to ptr addrspace(1)
  store ptr addrspace(1) %247, ptr %6
  %248 = ptrtoint ptr @"\01_caml_initialize" to i64
  %249 = load ptr addrspace(1), ptr %4
  %250 = load ptr addrspace(1), ptr %6
  %251 = load i64, ptr %ds
  %252 = load i64, ptr %alloc
  %253 = call oxcaml_c_directcc { i64, i64 } @"\01_caml_initialize"(i64 %251, i64 %252, ptr addrspace(1) %249, ptr addrspace(1) %250) "gc-leaf-function"="true"
  %254 = extractvalue { i64, i64 } %253, 0
  %255 = extractvalue { i64, i64 } %253, 1
  store i64 %254, ptr %ds
  store i64 %255, ptr %alloc
  br label %L287
L287:
  %256 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %256, ptr %3
  %257 = ptrtoint ptr @"\01_caml_fresh_oo_id" to i64
  %258 = load ptr addrspace(1), ptr %3
  %259 = load i64, ptr %ds
  %260 = load i64, ptr %alloc
  %261 = call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_fresh_oo_id"(i64 %259, i64 %260, ptr addrspace(1) %258) "gc-leaf-function"="true"
  %262 = extractvalue { i64, i64, ptr addrspace(1) } %261, 0
  %263 = extractvalue { i64, i64, ptr addrspace(1) } %261, 1
  store i64 %262, ptr %ds
  store i64 %263, ptr %alloc
  %264 = extractvalue { i64, i64, ptr addrspace(1) } %261, 2
  store ptr addrspace(1) %264, ptr %3
  br label %L289
L289:
  %265 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %265, ptr %63
  %266 = load ptr addrspace(1), ptr %63
  store ptr addrspace(1) %266, ptr %64
  %267 = ptrtoint ptr @"\01_camlTry_raise_cross_function_caught__Miss245" to i64
  store i64 %267, ptr %65
  %268 = load i64, ptr %65
  %269 = add i64 %268, 8
  store i64 %269, ptr %66
  %270 = load i64, ptr %66
  %271 = inttoptr i64 %270 to ptr addrspace(1)
  store ptr addrspace(1) %271, ptr %4
  %272 = load ptr addrspace(1), ptr %64
  store ptr addrspace(1) %272, ptr %6
  %273 = ptrtoint ptr @"\01_caml_initialize" to i64
  %274 = load ptr addrspace(1), ptr %4
  %275 = load ptr addrspace(1), ptr %6
  %276 = load i64, ptr %ds
  %277 = load i64, ptr %alloc
  %278 = call oxcaml_c_directcc { i64, i64 } @"\01_caml_initialize"(i64 %276, i64 %277, ptr addrspace(1) %274, ptr addrspace(1) %275) "gc-leaf-function"="true"
  %279 = extractvalue { i64, i64 } %278, 0
  %280 = extractvalue { i64, i64 } %278, 1
  store i64 %279, ptr %ds
  store i64 %280, ptr %alloc
  br label %L290
L290:
  %281 = load i64, ptr %ds
  %282 = add i64 %281, 40
  %283 = inttoptr i64 %282 to ptr
  %284 = load i64, ptr %283
  %285 = add i64 %284, 376
  %286 = call i64 asm sideeffect "mov $0, sp", "=r"() "gc-leaf-function"="true"
  %287 = icmp uge i64 %286, %285
  %288 = call  i1 @llvm.expect.i1(i1 %287, i1 1) 
  br i1 %288, label %L309, label %L308
L308:
  %289 = load i64, ptr %ds
  %290 = load i64, ptr %alloc
  %291 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_llvm_call_realloc_stack"(i64 %289, i64 %290, i64 34) "statepoint-id"="0" cold
  %292 = extractvalue { { i64, i64 }, {  } } %291, 0, 0
  %293 = extractvalue { { i64, i64 }, {  } } %291, 0, 1
  store i64 %292, ptr %ds
  store i64 %293, ptr %alloc
  br label %L309
L309:
  %294 = load i64, ptr %36
  store i64 %294, ptr %5
  %295 = load i64, ptr %5
  %296 = load i64, ptr %ds
  %297 = load i64, ptr %alloc
  %298 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_raise_cross_function_caught__black_box_int_0_7_code"(i64 %296, i64 %297, i64 %295) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 31, i64 0, i64 45, i64 65, i64 0, i64 65, i64 34, i64 7959156, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7155316, i64 108, i64 31, i64 7959124, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 116) ]
  %299 = extractvalue { { i64, i64 }, { i64 } } %298, 0, 0
  %300 = extractvalue { { i64, i64 }, { i64 } } %298, 0, 1
  store i64 %299, ptr %ds
  store i64 %300, ptr %alloc
  %301 = extractvalue { { i64, i64 }, { i64 } } %298, 1, 0
  store i64 %301, ptr %5
  br label %L292
L292:
  %302 = load i64, ptr %5
  store i64 %302, ptr %68
  %303 = load i64, ptr %68
  store i64 %303, ptr %69
  %304 = load i64, ptr %10
  store i64 %304, ptr %5
  %305 = load i64, ptr %5
  %306 = load i64, ptr %ds
  %307 = load i64, ptr %alloc
  %308 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_raise_cross_function_caught__black_box_int_0_7_code"(i64 %306, i64 %307, i64 %305) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 31, i64 0, i64 27, i64 44, i64 0, i64 44, i64 34, i64 7959156, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7155316, i64 108, i64 31, i64 7959124, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 116) ]
  %309 = extractvalue { { i64, i64 }, { i64 } } %308, 0, 0
  %310 = extractvalue { { i64, i64 }, { i64 } } %308, 0, 1
  store i64 %309, ptr %ds
  store i64 %310, ptr %alloc
  %311 = extractvalue { { i64, i64 }, { i64 } } %308, 1, 0
  store i64 %311, ptr %5
  br label %L293
L293:
  %312 = load i64, ptr %5
  store i64 %312, ptr %70
  %313 = load i64, ptr %70
  store i64 %313, ptr %71
  %314 = load i64, ptr %71
  store i64 %314, ptr %5
  %315 = load i64, ptr %69
  store i64 %315, ptr %7
  %316 = load i64, ptr %5
  %317 = load i64, ptr %7
  %318 = load i64, ptr %ds
  %319 = load i64, ptr %alloc
  %320 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_raise_cross_function_caught__run_6_13_code"(i64 %318, i64 %319, i64 %316, i64 %317) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 31, i64 0, i64 22, i64 66, i64 0, i64 66, i64 34, i64 7959156, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7155316, i64 108, i64 31, i64 7959124, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 116) ]
  %321 = extractvalue { { i64, i64 }, { i64 } } %320, 0, 0
  %322 = extractvalue { { i64, i64 }, { i64 } } %320, 0, 1
  store i64 %321, ptr %ds
  store i64 %322, ptr %alloc
  %323 = extractvalue { { i64, i64 }, { i64 } } %320, 1, 0
  store i64 %323, ptr %5
  br label %L294
L294:
  %324 = load i64, ptr %5
  store i64 %324, ptr %72
  %325 = load i64, ptr %72
  store i64 %325, ptr %73
  %326 = ptrtoint ptr @"\01_camlTry_raise_cross_function_caught__const_block66" to i64
  store i64 %326, ptr %74
  %327 = ptrtoint ptr @"\01_camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31" to i64
  store i64 %327, ptr %76
  %328 = load i64, ptr %76
  store i64 %328, ptr %5
  store i64 1, ptr %7
  %329 = load i64, ptr %74
  store i64 %329, ptr %8
  %330 = load i64, ptr %5
  %331 = load i64, ptr %7
  %332 = load i64, ptr %8
  %333 = load i64, ptr %ds
  %334 = load i64, ptr %alloc
  %335 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlCamlinternalFormat__make_printf_120_401_code"(i64 %333, i64 %334, i64 %330, i64 %331, i64 %332) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 5, i64 27, i64 0, i64 2, i64 63, i64 0, i64 63, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 23, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 7366251, i64 7235954, i64 26228, i64 34, i64 0, i64 21, i64 43, i64 0, i64 43, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 22, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 7499878, i64 7630441, i64 102, i64 38, i64 0, i64 17, i64 35, i64 0, i64 35, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 21, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 6910576, i64 6714478, i64 11, i64 0, i64 21, i64 63, i64 0, i64 63, i64 34, i64 7959156, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7155316, i64 108, i64 44, i64 7959124, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7351924, i64 7235954, i64 7495540, i64 7697253, i64 29804, i64 31, i64 0, i64 9, i64 66, i64 0, i64 66, i64 34, i64 7959156, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7155316, i64 108, i64 31, i64 7959124, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 116) ]
  %336 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %335, 0, 0
  %337 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %335, 0, 1
  store i64 %336, ptr %ds
  store i64 %337, ptr %alloc
  %338 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %335, 1, 0
  store ptr addrspace(1) %338, ptr %3
  br label %L295
L295:
  %339 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %339, ptr %77
  %340 = load ptr addrspace(1), ptr %77
  store ptr addrspace(1) %340, ptr %78
  %341 = load i64, ptr %73
  %342 = and i64 %341, 2147483647
  store i64 %342, ptr %79
  %343 = load ptr addrspace(1), ptr %78
  %344 = load i64, ptr addrspace(1) %343
  store i64 %344, ptr %80
  %345 = load i64, ptr %79
  store i64 %345, ptr %5
  %346 = load ptr addrspace(1), ptr %78
  store ptr addrspace(1) %346, ptr %6
  %347 = load i64, ptr %5
  %348 = load ptr addrspace(1), ptr %6
  %349 = load i64, ptr %ds
  %350 = load i64, ptr %alloc
  %351 = load i64, ptr %80
  %352 = inttoptr i64 %351 to ptr
  %353 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } %352(i64 %349, i64 %350, i64 %347, ptr addrspace(1) %348) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 2, i64 11, i64 0, i64 21, i64 63, i64 0, i64 63, i64 34, i64 7959156, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7155316, i64 108, i64 44, i64 7959124, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7351924, i64 7235954, i64 7495540, i64 7697253, i64 29804, i64 31, i64 0, i64 9, i64 66, i64 0, i64 66, i64 34, i64 7959156, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7155316, i64 108, i64 31, i64 7959124, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 116) ]
  %354 = extractvalue { { i64, i64 }, { i64 } } %353, 0, 0
  %355 = extractvalue { { i64, i64 }, { i64 } } %353, 0, 1
  store i64 %354, ptr %ds
  store i64 %355, ptr %alloc
  %356 = extractvalue { { i64, i64 }, { i64 } } %353, 1, 0
  store i64 %356, ptr %5
  br label %L296
L296:
  %357 = load i64, ptr %5
  store i64 %357, ptr %81
  %358 = load i64, ptr %81
  store i64 %358, ptr %82
  %359 = ptrtoint ptr @"\01_camlTry_raise_cross_function_caught" to i64
  store i64 %359, ptr %83
  %360 = load i64, ptr %83
  store i64 %360, ptr %84
  %361 = load i64, ptr %84
  %362 = inttoptr i64 %361 to ptr addrspace(1)
  store ptr addrspace(1) %362, ptr %9
  store i64 1, ptr %5
  %363 = load i64, ptr %5
  %364 = inttoptr i64 %363 to ptr addrspace(1)
  %365 = load i64, ptr %ds
  %366 = load i64, ptr %alloc
  %367 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %365, 0, 0
  %368 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %367, i64 %366, 0, 1
  %369 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %368, ptr addrspace(1) %364, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %369
}

@"\01_camlTry_raise_cross_function_caught__gc_roots" = global { ptr, ptr, i64 } { ptr @"\01_camlTry_raise_cross_function_caught", ptr @"\01_camlTry_raise_cross_function_caught__Miss245", i64 0 }, section "__DATA,__data", align 8
@"\01_header.camlTry_raise_cross_function_caught" = global i64 11008, section "__DATA,__data", align 8
@"\01_camlTry_raise_cross_function_caught" = global { ptr, ptr, ptr, i64, i64, ptr, ptr, ptr, ptr, ptr } { ptr @"\01_camlTry_raise_cross_function_caught__black_box_int_7", ptr @"\01_camlTry_raise_cross_function_caught__black_box_string_8", ptr @"\01_camlTry_raise_cross_function_caught__black_box_9", i64 1, i64 1, ptr @"\01_camlTry_raise_cross_function_caught__print_result_10", ptr @"\01_camlTry_raise_cross_function_caught__Miss245", ptr @"\01_camlTry_raise_cross_function_caught__probe_11", ptr @"\01_camlTry_raise_cross_function_caught__find_12", ptr @"\01_camlTry_raise_cross_function_caught__run_13" }, section "__DATA,__data", align 8
@"\01_header.camlTry_raise_cross_function_caught__run_13" = global i64 4087, section "__DATA,__data", align 8
@"\01_camlTry_raise_cross_function_caught__run_13" = global { ptr, i64, ptr } { ptr @"\01_caml_curry2", i64 180143985094819847, ptr @"\01_camlTry_raise_cross_function_caught__run_6_13_code" }, section "__DATA,__data", align 8
@"\01_header.camlTry_raise_cross_function_caught__find_12" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlTry_raise_cross_function_caught__find_12" = global { ptr, i64 } { ptr @"\01_camlTry_raise_cross_function_caught__find_5_12_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlTry_raise_cross_function_caught__probe_11" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlTry_raise_cross_function_caught__probe_11" = global { ptr, i64 } { ptr @"\01_camlTry_raise_cross_function_caught__probe_4_11_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlTry_raise_cross_function_caught__Miss245" = global i64 3064, section "__DATA,__data", align 8
@"\01_camlTry_raise_cross_function_caught__Miss245" = global { ptr, i64 } { ptr @"\01_camlTry_raise_cross_function_caught__immstring74", i64 1 }, section "__DATA,__data", align 8
@"\01_header.camlTry_raise_cross_function_caught__print_result_10" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlTry_raise_cross_function_caught__print_result_10" = global { ptr, i64 } { ptr @"\01_camlTry_raise_cross_function_caught__print_result_3_10_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlTry_raise_cross_function_caught__black_box_9" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlTry_raise_cross_function_caught__black_box_9" = global { ptr, i64 } { ptr @"\01_camlTry_raise_cross_function_caught__black_box_2_9_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlTry_raise_cross_function_caught__black_box_string_8" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlTry_raise_cross_function_caught__black_box_string_8" = global { ptr, i64 } { ptr @"\01_camlTry_raise_cross_function_caught__black_box_string_1_8_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlTry_raise_cross_function_caught__black_box_int_7" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlTry_raise_cross_function_caught__black_box_int_7" = global { ptr, i64 } { ptr @"\01_camlTry_raise_cross_function_caught__black_box_int_0_7_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlTry_raise_cross_function_caught__block35" = global i64 2816, section "__DATA,__data", align 8
@"\01_camlTry_raise_cross_function_caught__block35" = global { ptr, ptr } { ptr @"\01_caml_exn_Invalid_argument", ptr @"\01_camlTry_raise_cross_function_caught__string33" }, section "__DATA,__data", align 8
@"\01_header.camlTry_raise_cross_function_caught__string33" = global i64 4092, section "__DATA,__data", align 8
@"\01_camlTry_raise_cross_function_caught__string33" = global { [ 19 x i8 ], [ 4 x i8 ], i8 } { [ 19 x i8 ] c"\69\6e\64\65\78\20\6f\75\74\20\6f\66\20\62\6f\75\6e\64\73", [ 4 x i8 ] zeroinitializer, i8 4 }, section "__DATA,__data", align 8
@"\01_header.camlTry_raise_cross_function_caught__immstring74" = global i64 6140, section "__DATA,__data", align 8
@"\01_camlTry_raise_cross_function_caught__immstring74" = global { [ 36 x i8 ], [ 3 x i8 ], i8 } { [ 36 x i8 ] c"\54\72\79\5f\72\61\69\73\65\5f\63\72\6f\73\73\5f\66\75\6e\63\74\69\6f\6e\5f\63\61\75\67\68\74\2e\4d\69\73\73", [ 3 x i8 ] zeroinitializer, i8 3 }, section "__DATA,__data", align 8
@"\01_header.camlTry_raise_cross_function_caught__const_block66" = global i64 4868, section "__DATA,__data", align 8
@"\01_camlTry_raise_cross_function_caught__const_block66" = global { i64, i64, i64, ptr } { i64 1, i64 1, i64 1, ptr @"\01_camlTry_raise_cross_function_caught__const_block64" }, section "__DATA,__data", align 8
@"\01_header.camlTry_raise_cross_function_caught__const_block64" = global i64 2828, section "__DATA,__data", align 8
@"\01_camlTry_raise_cross_function_caught__const_block64" = global { i64, ptr } { i64 21, ptr @"\01_camlTry_raise_cross_function_caught__const_block62" }, section "__DATA,__data", align 8
@"\01_header.camlTry_raise_cross_function_caught__const_block62" = global i64 1802, section "__DATA,__data", align 8
@"\01_camlTry_raise_cross_function_caught__const_block62" = global { i64 } { i64 1 }, section "__DATA,__data", align 8
@"\01_camlCamlinternalFormat__make_printf_120_401_code" = external global ptr
@"\01_camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31" = external global ptr
@"\01_caml_c_call" = external global ptr
@"\01_caml_curry2" = external global ptr
@"\01_caml_exn_Invalid_argument" = external global ptr
@"\01_caml_fresh_oo_id" = external global ptr
@"\01_caml_initialize" = external global ptr
@"\01_caml_int_of_string" = external global ptr
@"\01_caml_llvm_call_realloc_stack" = external global ptr
@"\01_caml_sys_argv" = external global ptr

declare i32 @"\01_caml_llvm_eh_personality"(...)
declare void @llvm.aarch64.oxcaml.pop.trap()
declare void @llvm.aarch64.oxcaml.push.trap(ptr)
declare void @llvm.aarch64.oxcaml.raise.notrace(i64)
declare { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare i1 @llvm.expect.i1(i1, i1)


!0 = !{ i32 1, !"oxcaml_module", !"Try_raise_cross_function_caught" }
!llvm.module.flags = !{ !0 }
