source_filename = "env_find_same_layered_int_key.ml"

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlEnv_find_same_layered_int_key__black_box_int_0_8_code"(i64 %0, i64 %1, i64 %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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

define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlEnv_find_same_layered_int_key__black_box_string_1_9_code"(i64 %0, i64 %1, ptr addrspace(1) %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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

define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlEnv_find_same_layered_int_key__black_box_2_10_code"(i64 %0, i64 %1, ptr addrspace(1) %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlEnv_find_same_layered_int_key__print_result_3_11_code"(i64 %0, i64 %1, i64 %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" noinline gc "oxcaml" {
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
  %31 = ptrtoint ptr @"\01_camlEnv_find_same_layered_int_key__const_block66" to i64
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
  %40 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlCamlinternalFormat__make_printf_120_401_code"(i64 %38, i64 %39, i64 %35, i64 %36, i64 %37) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 4, i64 27, i64 0, i64 2, i64 63, i64 0, i64 63, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 23, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 7366251, i64 7235954, i64 26228, i64 34, i64 0, i64 21, i64 43, i64 0, i64 43, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 22, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 7499878, i64 7630441, i64 102, i64 38, i64 0, i64 17, i64 35, i64 0, i64 35, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 21, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 6910576, i64 6714478, i64 11, i64 0, i64 21, i64 63, i64 0, i64 63, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 42, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 6910576, i64 6255726, i64 7562610, i64 7629941) ]
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

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlEnv_find_same_layered_int_key__ident_find_same_4_12_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %5 = alloca ptr addrspace(1) 
  store ptr addrspace(1) %2, ptr %5
  %6 = alloca ptr addrspace(1) 
  store ptr addrspace(1) %3, ptr %6
  %7 = alloca ptr addrspace(1) 
  %8 = alloca ptr addrspace(1) 
  %9 = alloca i64 
  %10 = alloca ptr addrspace(1) 
  %11 = alloca ptr addrspace(1) 
  %12 = alloca ptr addrspace(1) 
  %13 = alloca ptr addrspace(1) 
  %14 = alloca ptr addrspace(1) 
  %15 = alloca ptr addrspace(1) 
  %16 = alloca i64 
  br label %L1
L1:
  br label %L119
L119:
  %17 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %17, ptr %7
  %18 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %18, ptr %8
  %19 = load ptr addrspace(1), ptr %8
  %20 = ptrtoint ptr addrspace(1) %19 to i64
  %21 = trunc i64 %20 to i1
  br i1 %21, label %L121, label %L123
L121:
  %22 = ptrtoint ptr @"\01_camlEnv_find_same_layered_int_key__Not_found_same292" to i64
  store i64 %22, ptr %9
  %23 = load i64, ptr %9
  %24 = inttoptr i64 %23 to ptr addrspace(1)
  store ptr addrspace(1) %24, ptr %5
  %25 = load ptr addrspace(1), ptr %5
  %26 = ptrtoint ptr addrspace(1) %25 to i64
  call  void @llvm.aarch64.oxcaml.raise.notrace(i64 %26) 
  unreachable
L123:
  %27 = load ptr addrspace(1), ptr %8
  %28 = getelementptr i8, ptr addrspace(1) %27, i64 8
  store ptr addrspace(1) %28, ptr %10
  %29 = load ptr addrspace(1), ptr %10
  %30 = load ptr addrspace(1), ptr addrspace(1) %29
  store ptr addrspace(1) %30, ptr %11
  %31 = load ptr addrspace(1), ptr %11
  %32 = load ptr addrspace(1), ptr addrspace(1) %31
  store ptr addrspace(1) %32, ptr %12
  %33 = load ptr addrspace(1), ptr %7
  %34 = load ptr addrspace(1), ptr addrspace(1) %33
  store ptr addrspace(1) %34, ptr %13
  %35 = load ptr addrspace(1), ptr %13
  %36 = load ptr addrspace(1), ptr %12
  %37 = icmp slt ptr addrspace(1) %35, %36
  br i1 %37, label %L133, label %L137
L137:
  %38 = load ptr addrspace(1), ptr %13
  %39 = load ptr addrspace(1), ptr %12
  %40 = icmp sgt ptr addrspace(1) %38, %39
  br i1 %40, label %L133, label %L129
L129:
  %41 = load ptr addrspace(1), ptr %8
  %42 = getelementptr i8, ptr addrspace(1) %41, i64 16
  store ptr addrspace(1) %42, ptr %14
  %43 = load ptr addrspace(1), ptr %14
  %44 = load ptr addrspace(1), ptr addrspace(1) %43
  store ptr addrspace(1) %44, ptr %15
  %45 = load ptr addrspace(1), ptr %15
  store ptr addrspace(1) %45, ptr %5
  %46 = load ptr addrspace(1), ptr %5
  %47 = ptrtoint ptr addrspace(1) %46 to i64
  %48 = load i64, ptr %ds
  %49 = load i64, ptr %alloc
  %50 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %48, 0, 0
  %51 = insertvalue { { i64, i64 }, { i64 } } %50, i64 %49, 0, 1
  %52 = insertvalue { { i64, i64 }, { i64 } } %51, i64 %47, 1, 0
  ret { { i64, i64 }, { i64 } } %52
L133:
  %53 = ptrtoint ptr @"\01_camlEnv_find_same_layered_int_key__Not_found_same292" to i64
  store i64 %53, ptr %16
  %54 = load i64, ptr %16
  %55 = inttoptr i64 %54 to ptr addrspace(1)
  store ptr addrspace(1) %55, ptr %5
  %56 = load ptr addrspace(1), ptr %5
  %57 = ptrtoint ptr addrspace(1) %56 to i64
  call  void @llvm.aarch64.oxcaml.raise.notrace(i64 %57) 
  unreachable
}

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlEnv_find_same_layered_int_key__find_same_without_locks_5_13_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="48" noinline gc "oxcaml" personality ptr @"\01_caml_llvm_eh_personality" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %5 = alloca ptr addrspace(1) 
  store ptr addrspace(1) %2, ptr %5
  %6 = alloca ptr addrspace(1) 
  store ptr addrspace(1) %3, ptr %6
  %7 = alloca i64 
  %8 = alloca ptr addrspace(1) 
  %9 = alloca ptr addrspace(1) 
  %10 = alloca ptr addrspace(1) 
  %11 = alloca ptr addrspace(1) 
  %12 = alloca ptr addrspace(1) 
  %13 = alloca ptr addrspace(1) 
  %14 = alloca ptr addrspace(1) 
  %15 = alloca ptr addrspace(1) 
  %16 = alloca ptr addrspace(1) 
  %17 = alloca i64 
  %18 = alloca i64 
  %19 = alloca ptr addrspace(1) 
  %20 = alloca i64 
  %21 = alloca i64 
  %22 = alloca i64 
  %23 = alloca i64 
  %24 = alloca i64 
  %25 = alloca i64 
  %26 = alloca ptr addrspace(1) 
  %27 = alloca ptr addrspace(1) 
  %28 = alloca ptr addrspace(1) 
  %29 = alloca ptr addrspace(1) 
  %30 = alloca ptr addrspace(1) 
  %31 = alloca ptr addrspace(1) 
  %32 = alloca ptr addrspace(1) 
  %33 = alloca i64
  br label %L1
L1:
  br label %L139
L139:
  %34 = load i64, ptr %ds
  %35 = add i64 %34, 40
  %36 = inttoptr i64 %35 to ptr
  %37 = load i64, ptr %36
  %38 = add i64 %37, 408
  %39 = call i64 asm sideeffect "mov $0, sp", "=r"() "gc-leaf-function"="true"
  %40 = icmp uge i64 %39, %38
  %41 = call  i1 @llvm.expect.i1(i1 %40, i1 1) 
  br i1 %41, label %L180, label %L179
L179:
  %42 = load i64, ptr %ds
  %43 = load i64, ptr %alloc
  %44 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_llvm_call_realloc_stack"(i64 %42, i64 %43, i64 38) "statepoint-id"="0" cold
  %45 = extractvalue { { i64, i64 }, {  } } %44, 0, 0
  %46 = extractvalue { { i64, i64 }, {  } } %44, 0, 1
  store i64 %45, ptr %ds
  store i64 %46, ptr %alloc
  br label %L180
L180:
  %47 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %47, ptr %8
  %48 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %48, ptr %9
  %49 = load ptr addrspace(1), ptr %9
  %50 = load ptr addrspace(1), ptr addrspace(1) %49
  store ptr addrspace(1) %50, ptr %12
  %51 = load ptr addrspace(1), ptr %9
  %52 = getelementptr i8, ptr addrspace(1) %51, i64 8
  store ptr addrspace(1) %52, ptr %13
  %53 = load ptr addrspace(1), ptr %13
  %54 = load ptr addrspace(1), ptr addrspace(1) %53
  store ptr addrspace(1) %54, ptr %14
  %55 = load ptr addrspace(1), ptr %14
  store ptr addrspace(1) %55, ptr %15
  %56 = load ptr addrspace(1), ptr %12
  store ptr addrspace(1) %56, ptr %16
  %57 = load ptr addrspace(1), ptr %15
  store ptr addrspace(1) %57, ptr %10
  %58 = load ptr addrspace(1), ptr %16
  store ptr addrspace(1) %58, ptr %11
  br label %L147
L147:
  %59 = load i64, ptr %ds
  %60 = add i64 %59, 64
  %61 = inttoptr i64 %60 to ptr
  %62 = load i64, ptr %61
  store i64 %62, ptr %17
  %63 = load i64, ptr %17
  store i64 %63, ptr %18
  call  void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@"\01_camlEnv_find_same_layered_int_key__find_same_without_locks_5_13_code", %L182)) 
  br label %L181
L182:
  %64 = landingpad token cleanup
  %65 = call  { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover() 
  %66 = extractvalue { ptr addrspace(1), i64, i64, i64 } %65, 0
  %67 = extractvalue { ptr addrspace(1), i64, i64, i64 } %65, 2
  %68 = extractvalue { ptr addrspace(1), i64, i64, i64 } %65, 3
  store ptr addrspace(1) %66, ptr %33
  store i64 %68, ptr %ds
  store i64 %67, ptr %alloc
  br label %L150
L181:
  %69 = load ptr addrspace(1), ptr %8
  store ptr addrspace(1) %69, ptr %5
  %70 = load ptr addrspace(1), ptr %11
  store ptr addrspace(1) %70, ptr %6
  %71 = load ptr addrspace(1), ptr %5
  %72 = load ptr addrspace(1), ptr %6
  %73 = load i64, ptr %ds
  %74 = load i64, ptr %alloc
  %75 = invoke oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlEnv_find_same_layered_int_key__ident_find_same_4_12_code"(i64 %73, i64 %74, ptr addrspace(1) %71, ptr addrspace(1) %72) "statepoint-id"="18" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 27, i64 0, i64 6, i64 36, i64 0, i64 36, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 53, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 7235942, i64 7561060, i64 6647137, i64 6911839, i64 7301236, i64 6255733, i64 6516588, i64 29547) ] to label %L183 unwind label %L182
L183:
  %76 = extractvalue { { i64, i64 }, { i64 } } %75, 0, 0
  %77 = extractvalue { { i64, i64 }, { i64 } } %75, 0, 1
  store i64 %76, ptr %ds
  store i64 %77, ptr %alloc
  %78 = extractvalue { { i64, i64 }, { i64 } } %75, 1, 0
  store i64 %78, ptr %7
  br label %L158
L158:
  %79 = load i64, ptr %7
  store i64 %79, ptr %20
  %80 = load i64, ptr %20
  store i64 %80, ptr %21
  call  void @llvm.aarch64.oxcaml.pop.trap() 
  %81 = load i64, ptr %21
  store i64 %81, ptr %7
  %82 = load i64, ptr %7
  %83 = load i64, ptr %ds
  %84 = load i64, ptr %alloc
  %85 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %83, 0, 0
  %86 = insertvalue { { i64, i64 }, { i64 } } %85, i64 %84, 0, 1
  %87 = insertvalue { { i64, i64 }, { i64 } } %86, i64 %82, 1, 0
  ret { { i64, i64 }, { i64 } } %87
L150:
  %88 = load i64, ptr %33
  %89 = inttoptr i64 %88 to ptr addrspace(1)
  store ptr addrspace(1) %89, ptr %5
  %90 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %90, ptr %19
  %91 = load i64, ptr %ds
  %92 = add i64 %91, 64
  %93 = inttoptr i64 %92 to ptr
  %94 = load i64, ptr %18
  store i64 %94, ptr %93
  store i64 1, ptr %23
  %95 = ptrtoint ptr @"\01_camlEnv_find_same_layered_int_key__Not_found_same292" to i64
  store i64 %95, ptr %24
  %96 = load ptr addrspace(1), ptr %19
  %97 = load i64, ptr %24
  %98 = inttoptr i64 %97 to ptr addrspace(1)
  %99 = icmp slt ptr addrspace(1) %96, %98
  br i1 %99, label %L174, label %L184
L184:
  %100 = load ptr addrspace(1), ptr %19
  %101 = load i64, ptr %24
  %102 = inttoptr i64 %101 to ptr addrspace(1)
  %103 = icmp sgt ptr addrspace(1) %100, %102
  br i1 %103, label %L174, label %L163
L163:
  %104 = load ptr addrspace(1), ptr %10
  %105 = ptrtoint ptr addrspace(1) %104 to i64
  %106 = trunc i64 %105 to i1
  br i1 %106, label %L165, label %L167
L165:
  %107 = ptrtoint ptr @"\01_camlEnv_find_same_layered_int_key__Not_found_same292" to i64
  store i64 %107, ptr %25
  %108 = load i64, ptr %25
  %109 = inttoptr i64 %108 to ptr addrspace(1)
  store ptr addrspace(1) %109, ptr %5
  %110 = load ptr addrspace(1), ptr %5
  %111 = ptrtoint ptr addrspace(1) %110 to i64
  call  void @llvm.aarch64.oxcaml.raise.notrace(i64 %111) 
  unreachable
L167:
  %112 = load ptr addrspace(1), ptr %10
  %113 = load ptr addrspace(1), ptr addrspace(1) %112
  store ptr addrspace(1) %113, ptr %26
  %114 = load ptr addrspace(1), ptr %26
  store ptr addrspace(1) %114, ptr %27
  %115 = load ptr addrspace(1), ptr %27
  %116 = load ptr addrspace(1), ptr addrspace(1) %115
  store ptr addrspace(1) %116, ptr %28
  %117 = load ptr addrspace(1), ptr %27
  %118 = getelementptr i8, ptr addrspace(1) %117, i64 8
  store ptr addrspace(1) %118, ptr %29
  %119 = load ptr addrspace(1), ptr %29
  %120 = load ptr addrspace(1), ptr addrspace(1) %119
  store ptr addrspace(1) %120, ptr %30
  %121 = load ptr addrspace(1), ptr %30
  store ptr addrspace(1) %121, ptr %31
  %122 = load ptr addrspace(1), ptr %28
  store ptr addrspace(1) %122, ptr %32
  %123 = load ptr addrspace(1), ptr %31
  store ptr addrspace(1) %123, ptr %10
  %124 = load ptr addrspace(1), ptr %32
  store ptr addrspace(1) %124, ptr %11
  br label %L147
L174:
  %125 = load ptr addrspace(1), ptr %19
  store ptr addrspace(1) %125, ptr %5
  %126 = load ptr addrspace(1), ptr %5
  %127 = ptrtoint ptr addrspace(1) %126 to i64
  call  void @llvm.aarch64.oxcaml.raise.notrace(i64 %127) 
  unreachable
}

define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlEnv_find_same_layered_int_key__open_layers_6_14_code"(i64 %0, i64 %1, i64 %2, ptr addrspace(1) %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %5 = alloca i64 
  store i64 %2, ptr %5
  %6 = alloca ptr addrspace(1) 
  store ptr addrspace(1) %3, ptr %6
  %7 = alloca ptr addrspace(1) 
  %8 = alloca i64 
  %9 = alloca ptr addrspace(1) 
  %10 = alloca ptr addrspace(1) 
  %11 = alloca i64 
  %12 = alloca i64 
  %13 = alloca ptr addrspace(1) 
  %14 = alloca ptr addrspace(1) 
  %15 = alloca ptr addrspace(1) 
  %16 = alloca i64 
  %17 = alloca ptr addrspace(1) 
  %18 = alloca ptr addrspace(1) 
  %19 = alloca i64 
  %20 = alloca ptr addrspace(1) 
  %21 = alloca ptr addrspace(1) 
  %22 = alloca ptr addrspace(1) 
  %23 = alloca i64 
  %24 = alloca ptr addrspace(1) 
  %25 = alloca ptr addrspace(1) 
  %26 = alloca ptr addrspace(1) 
  %27 = alloca i64 
  %28 = alloca i64 
  br label %L1
L1:
  br label %L186
L186:
  %29 = load i64, ptr %5
  store i64 %29, ptr %8
  %30 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %30, ptr %9
  %31 = load i64, ptr %8
  %32 = icmp slt i64 %31, 1
  br i1 %32, label %L193, label %L212
L212:
  %33 = load i64, ptr %8
  %34 = icmp sgt i64 %33, 1
  br i1 %34, label %L193, label %L188
L188:
  %35 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %35, ptr %7
  %36 = load ptr addrspace(1), ptr %7
  %37 = load i64, ptr %ds
  %38 = load i64, ptr %alloc
  %39 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %37, 0, 0
  %40 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %39, i64 %38, 0, 1
  %41 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %40, ptr addrspace(1) %36, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %41
L193:
  %42 = load i64, ptr %ds
  %43 = add i64 %42, 40
  %44 = inttoptr i64 %43 to ptr
  %45 = load i64, ptr %44
  %46 = add i64 %45, 376
  %47 = call i64 asm sideeffect "mov $0, sp", "=r"() "gc-leaf-function"="true"
  %48 = icmp uge i64 %47, %46
  %49 = call  i1 @llvm.expect.i1(i1 %48, i1 1) 
  br i1 %49, label %L214, label %L213
L213:
  %50 = load i64, ptr %ds
  %51 = load i64, ptr %alloc
  %52 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_llvm_call_realloc_stack"(i64 %50, i64 %51, i64 34) "statepoint-id"="0" cold
  %53 = extractvalue { { i64, i64 }, {  } } %52, 0, 0
  %54 = extractvalue { { i64, i64 }, {  } } %52, 0, 1
  store i64 %53, ptr %ds
  store i64 %54, ptr %alloc
  br label %L214
L214:
  %55 = load i64, ptr %8
  %56 = and i64 %55, 3
  store i64 %56, ptr %11
  %57 = load i64, ptr %11
  %58 = icmp slt i64 %57, 1
  br i1 %58, label %L201, label %L215
L215:
  %59 = load i64, ptr %11
  %60 = icmp sgt i64 %59, 1
  br i1 %60, label %L201, label %L196
L196:
  %61 = load i64, ptr %8
  %62 = add i64 %61, -2
  store i64 %62, ptr %12
  %63 = load i64, ptr %12
  store i64 %63, ptr %5
  %64 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %64, ptr %6
  %65 = load i64, ptr %5
  %66 = load ptr addrspace(1), ptr %6
  %67 = load i64, ptr %ds
  %68 = load i64, ptr %alloc
  %69 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlEnv_find_same_layered_int_key__open_layers_6_14_code"(i64 %67, i64 %68, i64 %65, ptr addrspace(1) %66) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 40, i64 0, i64 18, i64 44, i64 0, i64 44, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 41, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 6647919, i64 7102318, i64 6650209, i64 29554) ]
  %70 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %69, 0, 0
  %71 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %69, 0, 1
  store i64 %70, ptr %ds
  store i64 %71, ptr %alloc
  %72 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %69, 1, 0
  store ptr addrspace(1) %72, ptr %7
  br label %L198
L198:
  %73 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %73, ptr %13
  %74 = load ptr addrspace(1), ptr %13
  store ptr addrspace(1) %74, ptr %14
  %75 = load i64, ptr %alloc
  %76 = sub i64 %75, 16
  store i64 %76, ptr %alloc
  %77 = load i64, ptr %ds
  %78 = inttoptr i64 %77 to ptr
  %79 = load i64, ptr %78
  %80 = icmp ule i64 %79, %76
  %81 = call  i1 @llvm.expect.i1(i1 %80, i1 1) 
  br i1 %81, label %L217, label %L216
L216:
  %82 = load i64, ptr %ds
  %83 = load i64, ptr %alloc
  %84 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %82, i64 %83) "statepoint-id"="131073" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 2, i64 1, i64 40, i64 0, i64 13, i64 44, i64 0, i64 44, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 41, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 6647919, i64 7102318, i64 6650209, i64 29554) ]
  %85 = extractvalue { { i64, i64 }, {  } } %84, 0, 0
  %86 = extractvalue { { i64, i64 }, {  } } %84, 0, 1
  store i64 %85, ptr %ds
  store i64 %86, ptr %alloc
  br label %L217
L217:
  %87 = load i64, ptr %alloc
  %88 = add i64 %87, 8
  %89 = inttoptr i64 %88 to ptr addrspace(1)
  store ptr addrspace(1) %89, ptr %15
  %90 = load ptr addrspace(1), ptr %15
  %91 = getelementptr i8, ptr addrspace(1) %90, i64 -8
  store volatile i64 1024, ptr addrspace(1) %91
  %92 = load ptr addrspace(1), ptr %15
  %93 = load ptr addrspace(1), ptr %14
  store ptr addrspace(1) %93, ptr addrspace(1) %92
  %94 = load ptr addrspace(1), ptr %15
  store ptr addrspace(1) %94, ptr %17
  %95 = load ptr addrspace(1), ptr %17
  store ptr addrspace(1) %95, ptr %18
  %96 = load ptr addrspace(1), ptr %18
  store ptr addrspace(1) %96, ptr %10
  br label %L207
L201:
  %97 = load i64, ptr %8
  %98 = add i64 %97, -2
  store i64 %98, ptr %19
  %99 = load i64, ptr %19
  store i64 %99, ptr %5
  %100 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %100, ptr %6
  %101 = load i64, ptr %5
  %102 = load ptr addrspace(1), ptr %6
  %103 = load i64, ptr %ds
  %104 = load i64, ptr %alloc
  %105 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlEnv_find_same_layered_int_key__open_layers_6_14_code"(i64 %103, i64 %104, i64 %101, ptr addrspace(1) %102) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 41, i64 0, i64 18, i64 44, i64 0, i64 44, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 41, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 6647919, i64 7102318, i64 6650209, i64 29554) ]
  %106 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %105, 0, 0
  %107 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %105, 0, 1
  store i64 %106, ptr %ds
  store i64 %107, ptr %alloc
  %108 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %105, 1, 0
  store ptr addrspace(1) %108, ptr %7
  br label %L203
L203:
  %109 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %109, ptr %20
  %110 = load ptr addrspace(1), ptr %20
  store ptr addrspace(1) %110, ptr %21
  %111 = load i64, ptr %alloc
  %112 = sub i64 %111, 16
  store i64 %112, ptr %alloc
  %113 = load i64, ptr %ds
  %114 = inttoptr i64 %113 to ptr
  %115 = load i64, ptr %114
  %116 = icmp ule i64 %115, %112
  %117 = call  i1 @llvm.expect.i1(i1 %116, i1 1) 
  br i1 %117, label %L219, label %L218
L218:
  %118 = load i64, ptr %ds
  %119 = load i64, ptr %alloc
  %120 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %118, i64 %119) "statepoint-id"="131073" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 2, i64 1, i64 41, i64 0, i64 13, i64 44, i64 0, i64 44, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 41, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 6647919, i64 7102318, i64 6650209, i64 29554) ]
  %121 = extractvalue { { i64, i64 }, {  } } %120, 0, 0
  %122 = extractvalue { { i64, i64 }, {  } } %120, 0, 1
  store i64 %121, ptr %ds
  store i64 %122, ptr %alloc
  br label %L219
L219:
  %123 = load i64, ptr %alloc
  %124 = add i64 %123, 8
  %125 = inttoptr i64 %124 to ptr addrspace(1)
  store ptr addrspace(1) %125, ptr %22
  %126 = load ptr addrspace(1), ptr %22
  %127 = getelementptr i8, ptr addrspace(1) %126, i64 -8
  store volatile i64 1025, ptr addrspace(1) %127
  %128 = load ptr addrspace(1), ptr %22
  %129 = load ptr addrspace(1), ptr %21
  store ptr addrspace(1) %129, ptr addrspace(1) %128
  %130 = load ptr addrspace(1), ptr %22
  store ptr addrspace(1) %130, ptr %24
  %131 = load ptr addrspace(1), ptr %24
  store ptr addrspace(1) %131, ptr %25
  %132 = load ptr addrspace(1), ptr %25
  store ptr addrspace(1) %132, ptr %10
  br label %L207
L207:
  %133 = load i64, ptr %alloc
  %134 = sub i64 %133, 24
  store i64 %134, ptr %alloc
  %135 = load i64, ptr %ds
  %136 = inttoptr i64 %135 to ptr
  %137 = load i64, ptr %136
  %138 = icmp ule i64 %137, %134
  %139 = call  i1 @llvm.expect.i1(i1 %138, i1 1) 
  br i1 %139, label %L221, label %L220
L220:
  %140 = load i64, ptr %ds
  %141 = load i64, ptr %alloc
  %142 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %140, i64 %141) "statepoint-id"="196609" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 1, i64 37, i64 4, i64 4, i64 46, i64 106, i64 152, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 41, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 6647919, i64 7102318, i64 6650209, i64 29554) ]
  %143 = extractvalue { { i64, i64 }, {  } } %142, 0, 0
  %144 = extractvalue { { i64, i64 }, {  } } %142, 0, 1
  store i64 %143, ptr %ds
  store i64 %144, ptr %alloc
  br label %L221
L221:
  %145 = load i64, ptr %alloc
  %146 = add i64 %145, 8
  %147 = inttoptr i64 %146 to ptr addrspace(1)
  store ptr addrspace(1) %147, ptr %26
  %148 = load ptr addrspace(1), ptr %26
  %149 = getelementptr i8, ptr addrspace(1) %148, i64 -8
  store volatile i64 2048, ptr addrspace(1) %149
  %150 = load ptr addrspace(1), ptr %26
  store volatile i64 1, ptr addrspace(1) %150
  %151 = load ptr addrspace(1), ptr %26
  %152 = getelementptr i8, ptr addrspace(1) %151, i64 8
  %153 = load ptr addrspace(1), ptr %10
  store ptr addrspace(1) %153, ptr addrspace(1) %152
  %154 = load ptr addrspace(1), ptr %26
  store ptr addrspace(1) %154, ptr %7
  %155 = load ptr addrspace(1), ptr %7
  %156 = load i64, ptr %ds
  %157 = load i64, ptr %alloc
  %158 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %156, 0, 0
  %159 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %158, i64 %157, 0, 1
  %160 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %159, ptr addrspace(1) %155, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %160
}

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlEnv_find_same_layered_int_key__run_7_15_code"(i64 %0, i64 %1, i64 %2, i64 %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %5 = alloca i64 
  store i64 %2, ptr %5
  %6 = alloca i64 
  store i64 %3, ptr %6
  %7 = alloca ptr addrspace(1) 
  %8 = alloca ptr addrspace(1) 
  %9 = alloca i64 
  %10 = alloca i64 
  %11 = alloca i64 
  %12 = alloca i64 
  %13 = alloca ptr addrspace(1) 
  %14 = alloca ptr addrspace(1) 
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
  %42 = alloca i64 
  %43 = alloca i64 
  %44 = alloca i64 
  %45 = alloca i64 
  %46 = alloca i64 
  %47 = alloca i64 
  br label %L1
L1:
  br label %L223
L223:
  %48 = load i64, ptr %ds
  %49 = add i64 %48, 40
  %50 = inttoptr i64 %49 to ptr
  %51 = load i64, ptr %50
  %52 = add i64 %51, 376
  %53 = call i64 asm sideeffect "mov $0, sp", "=r"() "gc-leaf-function"="true"
  %54 = icmp uge i64 %53, %52
  %55 = call  i1 @llvm.expect.i1(i1 %54, i1 1) 
  br i1 %55, label %L272, label %L271
L271:
  %56 = load i64, ptr %ds
  %57 = load i64, ptr %alloc
  %58 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_llvm_call_realloc_stack"(i64 %56, i64 %57, i64 34) "statepoint-id"="0" cold
  %59 = extractvalue { { i64, i64 }, {  } } %58, 0, 0
  %60 = extractvalue { { i64, i64 }, {  } } %58, 0, 1
  store i64 %59, ptr %ds
  store i64 %60, ptr %alloc
  br label %L272
L272:
  %61 = load i64, ptr %5
  store i64 %61, ptr %9
  %62 = load i64, ptr %6
  store i64 %62, ptr %10
  %63 = ptrtoint ptr @"\01_camlEnv_find_same_layered_int_key__const_block153" to i64
  store i64 %63, ptr %11
  store i64 13, ptr %5
  %64 = load i64, ptr %11
  store i64 %64, ptr %6
  %65 = load i64, ptr %5
  %66 = load i64, ptr %6
  %67 = inttoptr i64 %66 to ptr addrspace(1)
  %68 = load i64, ptr %ds
  %69 = load i64, ptr %alloc
  %70 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlEnv_find_same_layered_int_key__open_layers_6_14_code"(i64 %68, i64 %69, i64 %65, ptr addrspace(1) %67) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 47, i64 0, i64 12, i64 32, i64 0, i64 32, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 33, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 7239026) ]
  %71 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %70, 0, 0
  %72 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %70, 0, 1
  store i64 %71, ptr %ds
  store i64 %72, ptr %alloc
  %73 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %70, 1, 0
  store ptr addrspace(1) %73, ptr %7
  br label %L225
L225:
  %74 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %74, ptr %13
  %75 = load ptr addrspace(1), ptr %13
  store ptr addrspace(1) %75, ptr %14
  %76 = load i64, ptr %10
  %77 = icmp slt i64 %76, 3
  br i1 %77, label %L268, label %L273
L273:
  %78 = load i64, ptr %10
  %79 = icmp sgt i64 %78, 3
  br i1 %79, label %L226, label %L226
L226:
  %80 = load i64, ptr %10
  %81 = ashr i64 %80, 1
  store i64 %81, ptr %15
  %82 = load i64, ptr %15
  store i64 %82, ptr %16
  store i64 1, ptr %21
  store i64 1, ptr %22
  %83 = load i64, ptr %21
  store i64 %83, ptr %17
  %84 = load i64, ptr %22
  store i64 %84, ptr %18
  %85 = load i64, ptr %9
  %86 = icmp slt i64 %85, 3
  br i1 %86, label %L255, label %L274
L274:
  %87 = load i64, ptr %9
  %88 = icmp sgt i64 %87, 3
  br i1 %88, label %L237, label %L237
L237:
  %89 = load i64, ptr %9
  %90 = ashr i64 %89, 1
  store i64 %90, ptr %24
  %91 = load i64, ptr %24
  store i64 %91, ptr %25
  store i64 1, ptr %29
  %92 = load i64, ptr %18
  store i64 %92, ptr %30
  %93 = load i64, ptr %29
  store i64 %93, ptr %26
  %94 = load i64, ptr %30
  store i64 %94, ptr %27
  br label %L243
L243:
  %95 = ptrtoint ptr @"\01_camlEnv_find_same_layered_int_key__const_block149" to i64
  store i64 %95, ptr %31
  %96 = load i64, ptr %31
  store i64 %96, ptr %5
  %97 = load ptr addrspace(1), ptr %14
  store ptr addrspace(1) %97, ptr %8
  %98 = load i64, ptr %5
  %99 = inttoptr i64 %98 to ptr addrspace(1)
  %100 = load ptr addrspace(1), ptr %8
  %101 = load i64, ptr %ds
  %102 = load i64, ptr %alloc
  %103 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlEnv_find_same_layered_int_key__find_same_without_locks_5_13_code"(i64 %101, i64 %102, ptr addrspace(1) %99, ptr addrspace(1) %100) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 52, i64 0, i64 20, i64 50, i64 0, i64 50, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 33, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 7239026) ]
  %104 = extractvalue { { i64, i64 }, { i64 } } %103, 0, 0
  %105 = extractvalue { { i64, i64 }, { i64 } } %103, 0, 1
  store i64 %104, ptr %ds
  store i64 %105, ptr %alloc
  %106 = extractvalue { { i64, i64 }, { i64 } } %103, 1, 0
  store i64 %106, ptr %5
  br label %L245
L245:
  %107 = load i64, ptr %5
  store i64 %107, ptr %32
  %108 = load i64, ptr %32
  store i64 %108, ptr %33
  %109 = load i64, ptr %27
  %110 = load i64, ptr %33
  %111 = add i64 %109, %110
  store i64 %111, ptr %34
  %112 = load i64, ptr %34
  %113 = add i64 %112, -1
  store i64 %113, ptr %35
  %114 = load i64, ptr %35
  store i64 %114, ptr %36
  %115 = load i64, ptr %26
  %116 = add i64 %115, 1
  store i64 %116, ptr %37
  %117 = load i64, ptr %37
  store i64 %117, ptr %38
  %118 = load i64, ptr %38
  %119 = load i64, ptr %25
  %120 = icmp slt i64 %118, %119
  br i1 %120, label %L249, label %L275
L275:
  %121 = load i64, ptr %38
  %122 = load i64, ptr %25
  %123 = icmp sgt i64 %121, %122
  br i1 %123, label %L251, label %L249
L249:
  %124 = load i64, ptr %38
  store i64 %124, ptr %39
  %125 = load i64, ptr %36
  store i64 %125, ptr %40
  %126 = load i64, ptr %39
  store i64 %126, ptr %26
  %127 = load i64, ptr %40
  store i64 %127, ptr %27
  br label %L243
L251:
  %128 = load i64, ptr %36
  store i64 %128, ptr %41
  %129 = load i64, ptr %41
  store i64 %129, ptr %23
  br label %L258
L255:
  %130 = load i64, ptr %18
  store i64 %130, ptr %42
  %131 = load i64, ptr %42
  store i64 %131, ptr %23
  br label %L258
L258:
  %132 = load i64, ptr %17
  %133 = add i64 %132, 1
  store i64 %133, ptr %43
  %134 = load i64, ptr %43
  store i64 %134, ptr %44
  %135 = load i64, ptr %44
  %136 = load i64, ptr %16
  %137 = icmp slt i64 %135, %136
  br i1 %137, label %L261, label %L276
L276:
  %138 = load i64, ptr %44
  %139 = load i64, ptr %16
  %140 = icmp sgt i64 %138, %139
  br i1 %140, label %L263, label %L261
L261:
  %141 = load i64, ptr %44
  store i64 %141, ptr %45
  %142 = load i64, ptr %23
  store i64 %142, ptr %46
  %143 = load i64, ptr %45
  store i64 %143, ptr %17
  %144 = load i64, ptr %46
  store i64 %144, ptr %18
  %145 = load i64, ptr %9
  %146 = icmp slt i64 %145, 3
  br i1 %146, label %L255, label %L277
L277:
  %147 = load i64, ptr %9
  %148 = icmp sgt i64 %147, 3
  br i1 %148, label %L237, label %L237
L263:
  %149 = load i64, ptr %23
  store i64 %149, ptr %5
  %150 = load i64, ptr %5
  %151 = load i64, ptr %ds
  %152 = load i64, ptr %alloc
  %153 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %151, 0, 0
  %154 = insertvalue { { i64, i64 }, { i64 } } %153, i64 %152, 0, 1
  %155 = insertvalue { { i64, i64 }, { i64 } } %154, i64 %150, 1, 0
  ret { { i64, i64 }, { i64 } } %155
L268:
  store i64 1, ptr %5
  %156 = load i64, ptr %5
  %157 = load i64, ptr %ds
  %158 = load i64, ptr %alloc
  %159 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %157, 0, 0
  %160 = insertvalue { { i64, i64 }, { i64 } } %159, i64 %158, 0, 1
  %161 = insertvalue { { i64, i64 }, { i64 } } %160, i64 %156, 1, 0
  ret { { i64, i64 }, { i64 } } %161
}

define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlEnv_find_same_layered_int_key__entry"(i64 %0, i64 %1) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" noinline gc "oxcaml" {
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
  br label %L285
L285:
  %86 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %86, ptr %3
  %87 = ptrtoint ptr @"\01_caml_sys_argv" to i64
  %88 = load ptr addrspace(1), ptr %3
  %89 = load i64, ptr %ds
  %90 = load i64, ptr %alloc
  %91 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %89, i64 %90, i64 %87, ptr addrspace(1) %88) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 6, i64 0, i64 18, i64 26, i64 0, i64 26, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 31, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 110) ]
  %92 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %91, 0, 0
  %93 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %91, 0, 1
  store i64 %92, ptr %ds
  store i64 %93, ptr %alloc
  %94 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %91, 1, 0
  store ptr addrspace(1) %94, ptr %3
  br label %L287
L287:
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
  br i1 %106, label %L313, label %L371
L371:
  %107 = load i64, ptr %17
  %108 = icmp sgt i64 %107, 3
  br i1 %108, label %L292, label %L313
L292:
  %109 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %109, ptr %3
  %110 = ptrtoint ptr @"\01_caml_sys_argv" to i64
  %111 = load ptr addrspace(1), ptr %3
  %112 = load i64, ptr %ds
  %113 = load i64, ptr %alloc
  %114 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %112, i64 %113, i64 %110, ptr addrspace(1) %111) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 6, i64 0, i64 50, i64 58, i64 0, i64 58, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 31, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 110) ]
  %115 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %114, 0, 0
  %116 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %114, 0, 1
  store i64 %115, ptr %ds
  store i64 %116, ptr %alloc
  %117 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %114, 1, 0
  store ptr addrspace(1) %117, ptr %3
  br label %L294
L294:
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
  br i1 %129, label %L310, label %L372
L372:
  %130 = load i64, ptr %24
  %131 = icmp ugt i64 %130, 3
  br i1 %131, label %L302, label %L310
L302:
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
  %141 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %139, i64 %140, i64 %137, ptr addrspace(1) %138) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 6, i64 0, i64 36, i64 62, i64 0, i64 62, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 31, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 110) ]
  %142 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %141, 0, 0
  %143 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %141, 0, 1
  store i64 %142, ptr %ds
  store i64 %143, ptr %alloc
  %144 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %141, 1, 0
  store ptr addrspace(1) %144, ptr %3
  br label %L304
L304:
  %145 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %145, ptr %27
  %146 = load ptr addrspace(1), ptr %27
  store ptr addrspace(1) %146, ptr %28
  %147 = load ptr addrspace(1), ptr %28
  store ptr addrspace(1) %147, ptr %29
  %148 = load ptr addrspace(1), ptr %29
  %149 = ptrtoint ptr addrspace(1) %148 to i64
  store i64 %149, ptr %10
  br label %L316
L310:
  %150 = ptrtoint ptr @"\01_camlEnv_find_same_layered_int_key__block35" to i64
  store i64 %150, ptr %30
  %151 = load i64, ptr %30
  %152 = inttoptr i64 %151 to ptr addrspace(1)
  store ptr addrspace(1) %152, ptr %3
  %153 = load ptr addrspace(1), ptr %3
  %154 = ptrtoint ptr addrspace(1) %153 to i64
  call  void @llvm.aarch64.oxcaml.raise.notrace(i64 %154) 
  unreachable
L313:
  store i64 200001, ptr %32
  %155 = load i64, ptr %32
  store i64 %155, ptr %10
  br label %L316
L316:
  %156 = ptrtoint ptr @"\01_camlEnv_find_same_layered_int_key" to i64
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
  br label %L318
L318:
  %171 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %171, ptr %3
  %172 = ptrtoint ptr @"\01_caml_sys_argv" to i64
  %173 = load ptr addrspace(1), ptr %3
  %174 = load i64, ptr %ds
  %175 = load i64, ptr %alloc
  %176 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %174, i64 %175, i64 %172, ptr addrspace(1) %173) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 9, i64 0, i64 18, i64 26, i64 0, i64 26, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 34, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 7366002, i64 115) ]
  %177 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %176, 0, 0
  %178 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %176, 0, 1
  store i64 %177, ptr %ds
  store i64 %178, ptr %alloc
  %179 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %176, 1, 0
  store ptr addrspace(1) %179, ptr %3
  br label %L323
L323:
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
  br i1 %191, label %L349, label %L373
L373:
  %192 = load i64, ptr %43
  %193 = icmp sgt i64 %192, 5
  br i1 %193, label %L328, label %L349
L328:
  %194 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %194, ptr %3
  %195 = ptrtoint ptr @"\01_caml_sys_argv" to i64
  %196 = load ptr addrspace(1), ptr %3
  %197 = load i64, ptr %ds
  %198 = load i64, ptr %alloc
  %199 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %197, i64 %198, i64 %195, ptr addrspace(1) %196) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 9, i64 0, i64 50, i64 58, i64 0, i64 58, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 34, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 7366002, i64 115) ]
  %200 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %199, 0, 0
  %201 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %199, 0, 1
  store i64 %200, ptr %ds
  store i64 %201, ptr %alloc
  %202 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %199, 1, 0
  store ptr addrspace(1) %202, ptr %3
  br label %L330
L330:
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
  br i1 %214, label %L346, label %L374
L374:
  %215 = load i64, ptr %50
  %216 = icmp ugt i64 %215, 5
  br i1 %216, label %L338, label %L346
L338:
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
  %226 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %224, i64 %225, i64 %222, ptr addrspace(1) %223) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 9, i64 0, i64 36, i64 62, i64 0, i64 62, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 34, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 7366002, i64 115) ]
  %227 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %226, 0, 0
  %228 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %226, 0, 1
  store i64 %227, ptr %ds
  store i64 %228, ptr %alloc
  %229 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %226, 1, 0
  store ptr addrspace(1) %229, ptr %3
  br label %L340
L340:
  %230 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %230, ptr %53
  %231 = load ptr addrspace(1), ptr %53
  store ptr addrspace(1) %231, ptr %54
  %232 = load ptr addrspace(1), ptr %54
  store ptr addrspace(1) %232, ptr %55
  %233 = load ptr addrspace(1), ptr %55
  %234 = ptrtoint ptr addrspace(1) %233 to i64
  store i64 %234, ptr %36
  br label %L352
L346:
  %235 = ptrtoint ptr @"\01_camlEnv_find_same_layered_int_key__block35" to i64
  store i64 %235, ptr %56
  %236 = load i64, ptr %56
  %237 = inttoptr i64 %236 to ptr addrspace(1)
  store ptr addrspace(1) %237, ptr %3
  %238 = load ptr addrspace(1), ptr %3
  %239 = ptrtoint ptr addrspace(1) %238 to i64
  call  void @llvm.aarch64.oxcaml.raise.notrace(i64 %239) 
  unreachable
L349:
  store i64 21, ptr %58
  %240 = load i64, ptr %58
  store i64 %240, ptr %36
  br label %L352
L352:
  %241 = ptrtoint ptr @"\01_camlEnv_find_same_layered_int_key" to i64
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
  br label %L354
L354:
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
  br label %L356
L356:
  %265 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %265, ptr %63
  %266 = load ptr addrspace(1), ptr %63
  store ptr addrspace(1) %266, ptr %64
  %267 = ptrtoint ptr @"\01_camlEnv_find_same_layered_int_key__Not_found_same292" to i64
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
  br label %L357
L357:
  %281 = load i64, ptr %ds
  %282 = add i64 %281, 40
  %283 = inttoptr i64 %282 to ptr
  %284 = load i64, ptr %283
  %285 = add i64 %284, 376
  %286 = call i64 asm sideeffect "mov $0, sp", "=r"() "gc-leaf-function"="true"
  %287 = icmp uge i64 %286, %285
  %288 = call  i1 @llvm.expect.i1(i1 %287, i1 1) 
  br i1 %288, label %L376, label %L375
L375:
  %289 = load i64, ptr %ds
  %290 = load i64, ptr %alloc
  %291 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_llvm_call_realloc_stack"(i64 %289, i64 %290, i64 34) "statepoint-id"="0" cold
  %292 = extractvalue { { i64, i64 }, {  } } %291, 0, 0
  %293 = extractvalue { { i64, i64 }, {  } } %291, 0, 1
  store i64 %292, ptr %ds
  store i64 %293, ptr %alloc
  br label %L376
L376:
  %294 = load i64, ptr %36
  store i64 %294, ptr %5
  %295 = load i64, ptr %5
  %296 = load i64, ptr %ds
  %297 = load i64, ptr %alloc
  %298 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlEnv_find_same_layered_int_key__black_box_int_0_8_code"(i64 %296, i64 %297, i64 %295) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 57, i64 0, i64 45, i64 65, i64 0, i64 65, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 29, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 31077) ]
  %299 = extractvalue { { i64, i64 }, { i64 } } %298, 0, 0
  %300 = extractvalue { { i64, i64 }, { i64 } } %298, 0, 1
  store i64 %299, ptr %ds
  store i64 %300, ptr %alloc
  %301 = extractvalue { { i64, i64 }, { i64 } } %298, 1, 0
  store i64 %301, ptr %5
  br label %L359
L359:
  %302 = load i64, ptr %5
  store i64 %302, ptr %68
  %303 = load i64, ptr %68
  store i64 %303, ptr %69
  %304 = load i64, ptr %10
  store i64 %304, ptr %5
  %305 = load i64, ptr %5
  %306 = load i64, ptr %ds
  %307 = load i64, ptr %alloc
  %308 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlEnv_find_same_layered_int_key__black_box_int_0_8_code"(i64 %306, i64 %307, i64 %305) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 57, i64 0, i64 27, i64 44, i64 0, i64 44, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 29, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 31077) ]
  %309 = extractvalue { { i64, i64 }, { i64 } } %308, 0, 0
  %310 = extractvalue { { i64, i64 }, { i64 } } %308, 0, 1
  store i64 %309, ptr %ds
  store i64 %310, ptr %alloc
  %311 = extractvalue { { i64, i64 }, { i64 } } %308, 1, 0
  store i64 %311, ptr %5
  br label %L360
L360:
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
  %320 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlEnv_find_same_layered_int_key__run_7_15_code"(i64 %318, i64 %319, i64 %316, i64 %317) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 57, i64 0, i64 22, i64 66, i64 0, i64 66, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 29, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 31077) ]
  %321 = extractvalue { { i64, i64 }, { i64 } } %320, 0, 0
  %322 = extractvalue { { i64, i64 }, { i64 } } %320, 0, 1
  store i64 %321, ptr %ds
  store i64 %322, ptr %alloc
  %323 = extractvalue { { i64, i64 }, { i64 } } %320, 1, 0
  store i64 %323, ptr %5
  br label %L361
L361:
  %324 = load i64, ptr %5
  store i64 %324, ptr %72
  %325 = load i64, ptr %72
  store i64 %325, ptr %73
  %326 = ptrtoint ptr @"\01_camlEnv_find_same_layered_int_key__const_block66" to i64
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
  %335 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlCamlinternalFormat__make_printf_120_401_code"(i64 %333, i64 %334, i64 %330, i64 %331, i64 %332) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 5, i64 27, i64 0, i64 2, i64 63, i64 0, i64 63, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 23, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 7366251, i64 7235954, i64 26228, i64 34, i64 0, i64 21, i64 43, i64 0, i64 43, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 22, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 7499878, i64 7630441, i64 102, i64 38, i64 0, i64 17, i64 35, i64 0, i64 35, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 21, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 6910576, i64 6714478, i64 11, i64 0, i64 21, i64 63, i64 0, i64 63, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 42, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 6910576, i64 6255726, i64 7562610, i64 7629941, i64 57, i64 0, i64 9, i64 66, i64 0, i64 66, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 29, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 31077) ]
  %336 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %335, 0, 0
  %337 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %335, 0, 1
  store i64 %336, ptr %ds
  store i64 %337, ptr %alloc
  %338 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %335, 1, 0
  store ptr addrspace(1) %338, ptr %3
  br label %L362
L362:
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
  %353 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } %352(i64 %349, i64 %350, i64 %347, ptr addrspace(1) %348) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 2, i64 11, i64 0, i64 21, i64 63, i64 0, i64 63, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 42, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 6910576, i64 6255726, i64 7562610, i64 7629941, i64 57, i64 0, i64 9, i64 66, i64 0, i64 66, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 29, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 31077) ]
  %354 = extractvalue { { i64, i64 }, { i64 } } %353, 0, 0
  %355 = extractvalue { { i64, i64 }, { i64 } } %353, 0, 1
  store i64 %354, ptr %ds
  store i64 %355, ptr %alloc
  %356 = extractvalue { { i64, i64 }, { i64 } } %353, 1, 0
  store i64 %356, ptr %5
  br label %L363
L363:
  %357 = load i64, ptr %5
  store i64 %357, ptr %81
  %358 = load i64, ptr %81
  store i64 %358, ptr %82
  %359 = ptrtoint ptr @"\01_camlEnv_find_same_layered_int_key" to i64
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

@"\01_camlEnv_find_same_layered_int_key__gc_roots" = global { ptr, ptr, i64 } { ptr @"\01_camlEnv_find_same_layered_int_key", ptr @"\01_camlEnv_find_same_layered_int_key__Not_found_same292", i64 0 }, section "__DATA,__data", align 8
@"\01_header.camlEnv_find_same_layered_int_key" = global i64 12032, section "__DATA,__data", align 8
@"\01_camlEnv_find_same_layered_int_key" = global { ptr, ptr, ptr, i64, i64, ptr, ptr, ptr, ptr, ptr, ptr } { ptr @"\01_camlEnv_find_same_layered_int_key__black_box_int_8", ptr @"\01_camlEnv_find_same_layered_int_key__black_box_string_9", ptr @"\01_camlEnv_find_same_layered_int_key__black_box_10", i64 1, i64 1, ptr @"\01_camlEnv_find_same_layered_int_key__print_result_11", ptr @"\01_camlEnv_find_same_layered_int_key__Not_found_same292", ptr @"\01_camlEnv_find_same_layered_int_key__ident_find_same_12", ptr @"\01_camlEnv_find_same_layered_int_key__find_same_without_locks_13", ptr @"\01_camlEnv_find_same_layered_int_key__open_layers_14", ptr @"\01_camlEnv_find_same_layered_int_key__run_15" }, section "__DATA,__data", align 8
@"\01_header.camlEnv_find_same_layered_int_key__run_15" = global i64 4087, section "__DATA,__data", align 8
@"\01_camlEnv_find_same_layered_int_key__run_15" = global { ptr, i64, ptr } { ptr @"\01_caml_curry2", i64 180143985094819847, ptr @"\01_camlEnv_find_same_layered_int_key__run_7_15_code" }, section "__DATA,__data", align 8
@"\01_header.camlEnv_find_same_layered_int_key__open_layers_14" = global i64 4087, section "__DATA,__data", align 8
@"\01_camlEnv_find_same_layered_int_key__open_layers_14" = global { ptr, i64, ptr } { ptr @"\01_caml_curry2", i64 180143985094819847, ptr @"\01_camlEnv_find_same_layered_int_key__open_layers_6_14_code" }, section "__DATA,__data", align 8
@"\01_header.camlEnv_find_same_layered_int_key__find_same_without_locks_13" = global i64 4087, section "__DATA,__data", align 8
@"\01_camlEnv_find_same_layered_int_key__find_same_without_locks_13" = global { ptr, i64, ptr } { ptr @"\01_caml_curry2", i64 180143985094819847, ptr @"\01_camlEnv_find_same_layered_int_key__find_same_without_locks_5_13_code" }, section "__DATA,__data", align 8
@"\01_header.camlEnv_find_same_layered_int_key__ident_find_same_12" = global i64 4087, section "__DATA,__data", align 8
@"\01_camlEnv_find_same_layered_int_key__ident_find_same_12" = global { ptr, i64, ptr } { ptr @"\01_caml_curry2", i64 180143985094819847, ptr @"\01_camlEnv_find_same_layered_int_key__ident_find_same_4_12_code" }, section "__DATA,__data", align 8
@"\01_header.camlEnv_find_same_layered_int_key__Not_found_same292" = global i64 3064, section "__DATA,__data", align 8
@"\01_camlEnv_find_same_layered_int_key__Not_found_same292" = global { ptr, i64 } { ptr @"\01_camlEnv_find_same_layered_int_key__immstring74", i64 1 }, section "__DATA,__data", align 8
@"\01_header.camlEnv_find_same_layered_int_key__print_result_11" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlEnv_find_same_layered_int_key__print_result_11" = global { ptr, i64 } { ptr @"\01_camlEnv_find_same_layered_int_key__print_result_3_11_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlEnv_find_same_layered_int_key__black_box_10" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlEnv_find_same_layered_int_key__black_box_10" = global { ptr, i64 } { ptr @"\01_camlEnv_find_same_layered_int_key__black_box_2_10_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlEnv_find_same_layered_int_key__black_box_string_9" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlEnv_find_same_layered_int_key__black_box_string_9" = global { ptr, i64 } { ptr @"\01_camlEnv_find_same_layered_int_key__black_box_string_1_9_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlEnv_find_same_layered_int_key__black_box_int_8" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlEnv_find_same_layered_int_key__black_box_int_8" = global { ptr, i64 } { ptr @"\01_camlEnv_find_same_layered_int_key__black_box_int_0_8_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlEnv_find_same_layered_int_key__block35" = global i64 2816, section "__DATA,__data", align 8
@"\01_camlEnv_find_same_layered_int_key__block35" = global { ptr, ptr } { ptr @"\01_caml_exn_Invalid_argument", ptr @"\01_camlEnv_find_same_layered_int_key__string33" }, section "__DATA,__data", align 8
@"\01_header.camlEnv_find_same_layered_int_key__string33" = global i64 4092, section "__DATA,__data", align 8
@"\01_camlEnv_find_same_layered_int_key__string33" = global { [ 19 x i8 ], [ 4 x i8 ], i8 } { [ 19 x i8 ] c"\69\6e\64\65\78\20\6f\75\74\20\6f\66\20\62\6f\75\6e\64\73", [ 4 x i8 ] zeroinitializer, i8 4 }, section "__DATA,__data", align 8
@"\01_header.camlEnv_find_same_layered_int_key__immstring74" = global i64 7164, section "__DATA,__data", align 8
@"\01_camlEnv_find_same_layered_int_key__immstring74" = global { [ 44 x i8 ], [ 3 x i8 ], i8 } { [ 44 x i8 ] c"\45\6e\76\5f\66\69\6e\64\5f\73\61\6d\65\5f\6c\61\79\65\72\65\64\5f\69\6e\74\5f\6b\65\79\2e\4e\6f\74\5f\66\6f\75\6e\64\5f\73\61\6d\65", [ 3 x i8 ] zeroinitializer, i8 3 }, section "__DATA,__data", align 8
@"\01_header.camlEnv_find_same_layered_int_key__const_block153" = global i64 2816, section "__DATA,__data", align 8
@"\01_camlEnv_find_same_layered_int_key__const_block153" = global { ptr, i64 } { ptr @"\01_camlEnv_find_same_layered_int_key__const_block151", i64 1 }, section "__DATA,__data", align 8
@"\01_header.camlEnv_find_same_layered_int_key__const_block151" = global i64 4864, section "__DATA,__data", align 8
@"\01_camlEnv_find_same_layered_int_key__const_block151" = global { i64, ptr, i64, i64 } { i64 1, ptr @"\01_camlEnv_find_same_layered_int_key__const_block149", i64 3, i64 1 }, section "__DATA,__data", align 8
@"\01_header.camlEnv_find_same_layered_int_key__const_block149" = global i64 1792, section "__DATA,__data", align 8
@"\01_camlEnv_find_same_layered_int_key__const_block149" = global { i64 } { i64 1 }, section "__DATA,__data", align 8
@"\01_header.camlEnv_find_same_layered_int_key__const_block66" = global i64 4868, section "__DATA,__data", align 8
@"\01_camlEnv_find_same_layered_int_key__const_block66" = global { i64, i64, i64, ptr } { i64 1, i64 1, i64 1, ptr @"\01_camlEnv_find_same_layered_int_key__const_block64" }, section "__DATA,__data", align 8
@"\01_header.camlEnv_find_same_layered_int_key__const_block64" = global i64 2828, section "__DATA,__data", align 8
@"\01_camlEnv_find_same_layered_int_key__const_block64" = global { i64, ptr } { i64 21, ptr @"\01_camlEnv_find_same_layered_int_key__const_block62" }, section "__DATA,__data", align 8
@"\01_header.camlEnv_find_same_layered_int_key__const_block62" = global i64 1802, section "__DATA,__data", align 8
@"\01_camlEnv_find_same_layered_int_key__const_block62" = global { i64 } { i64 1 }, section "__DATA,__data", align 8
@"\01_camlCamlinternalFormat__make_printf_120_401_code" = external global ptr
@"\01_camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31" = external global ptr
@"\01_caml_c_call" = external global ptr
@"\01_caml_call_gc" = external global ptr
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


!0 = !{ i32 1, !"oxcaml_module", !"Env_find_same_layered_int_key" }
!llvm.module.flags = !{ !0 }
