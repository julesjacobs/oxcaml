source_filename = "try_find_miss_rare.ml"

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_find_miss_rare__black_box_int_0_7_code"(i64 %0, i64 %1, i64 %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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

define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTry_find_miss_rare__black_box_string_1_8_code"(i64 %0, i64 %1, ptr addrspace(1) %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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

define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTry_find_miss_rare__black_box_2_9_code"(i64 %0, i64 %1, ptr addrspace(1) %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_find_miss_rare__print_result_3_10_code"(i64 %0, i64 %1, i64 %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" noinline gc "oxcaml" {
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
  %31 = ptrtoint ptr @"\01_camlTry_find_miss_rare__const_block66" to i64
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
  %40 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlCamlinternalFormat__make_printf_120_401_code"(i64 %38, i64 %39, i64 %35, i64 %36, i64 %37) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 4, i64 27, i64 0, i64 2, i64 63, i64 0, i64 63, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 23, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 7366251, i64 7235954, i64 26228, i64 34, i64 0, i64 21, i64 43, i64 0, i64 43, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 22, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 7499878, i64 7630441, i64 102, i64 38, i64 0, i64 17, i64 35, i64 0, i64 35, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 21, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 6910576, i64 6714478, i64 11, i64 0, i64 21, i64 63, i64 0, i64 63, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 31, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7499822, i64 7630441, i64 6648415, i64 7107955, i64 116) ]
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

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_find_miss_rare__scan_4_11_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3, i64 %4) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %6 = alloca ptr addrspace(1) 
  store ptr addrspace(1) %2, ptr %6
  %7 = alloca ptr addrspace(1) 
  store ptr addrspace(1) %3, ptr %7
  %8 = alloca i64 
  store i64 %4, ptr %8
  %9 = alloca i64 
  %10 = alloca ptr addrspace(1) 
  %11 = alloca ptr addrspace(1) 
  %12 = alloca i64 
  %13 = alloca i64 
  %14 = alloca i64 
  %15 = alloca ptr addrspace(1) 
  %16 = alloca i64 
  %17 = alloca i64 
  %18 = alloca i64 
  %19 = alloca i64 
  %20 = alloca i64 
  %21 = alloca ptr addrspace(1) 
  %22 = alloca ptr addrspace(1) 
  %23 = alloca i64 
  %24 = alloca ptr addrspace(1) 
  %25 = alloca i64 
  %26 = alloca i64 
  %27 = alloca ptr addrspace(1) 
  %28 = alloca ptr addrspace(1) 
  %29 = alloca double 
  %30 = alloca ptr addrspace(1) 
  %31 = alloca ptr addrspace(1) 
  %32 = alloca i64 
  %33 = alloca ptr addrspace(1) 
  %34 = alloca ptr addrspace(1) 
  %35 = alloca ptr addrspace(1) 
  %36 = alloca ptr addrspace(1) 
  %37 = alloca ptr addrspace(1) 
  %38 = alloca ptr addrspace(1) 
  %39 = alloca i64 
  %40 = alloca i64 
  br label %L1
L1:
  br label %L119
L119:
  %41 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %41, ptr %10
  %42 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %42, ptr %11
  %43 = load i64, ptr %8
  store i64 %43, ptr %12
  %44 = load i64, ptr %12
  store i64 %44, ptr %14
  %45 = load i64, ptr %14
  store i64 %45, ptr %13
  br label %L124
L124:
  %46 = load ptr addrspace(1), ptr %11
  %47 = getelementptr i8, ptr addrspace(1) %46, i64 -8
  store ptr addrspace(1) %47, ptr %15
  %48 = load ptr addrspace(1), ptr %15
  %49 = load i64, ptr addrspace(1) %48
  store i64 %49, ptr %16
  %50 = load i64, ptr %16
  %51 = shl i64 %50, 8
  store i64 %51, ptr %17
  %52 = load i64, ptr %17
  %53 = lshr i64 %52, 17
  store i64 %53, ptr %18
  %54 = load i64, ptr %18
  %55 = or i64 %54, 1
  store i64 %55, ptr %19
  %56 = load i64, ptr %13
  %57 = load i64, ptr %19
  %58 = icmp slt i64 %56, %57
  br i1 %58, label %L136, label %L166
L166:
  %59 = load i64, ptr %13
  %60 = load i64, ptr %19
  %61 = icmp sgt i64 %59, %60
  br i1 %61, label %L136, label %L131
L131:
  %62 = ptrtoint ptr @"\01_camlTry_find_miss_rare__Miss281" to i64
  store i64 %62, ptr %20
  %63 = load i64, ptr %20
  %64 = inttoptr i64 %63 to ptr addrspace(1)
  store ptr addrspace(1) %64, ptr %6
  %65 = load ptr addrspace(1), ptr %6
  %66 = ptrtoint ptr addrspace(1) %65 to i64
  call  void @llvm.aarch64.oxcaml.raise.notrace(i64 %66) 
  unreachable
L136:
  %67 = load ptr addrspace(1), ptr %11
  %68 = getelementptr i8, ptr addrspace(1) %67, i64 -8
  store ptr addrspace(1) %68, ptr %22
  %69 = load ptr addrspace(1), ptr %22
  %70 = load i8, ptr addrspace(1) %69
  %71 = zext i8 %70 to i64
  store i64 %71, ptr %23
  %72 = load i64, ptr %23
  %73 = icmp slt i64 %72, 254
  br i1 %73, label %L147, label %L167
L167:
  %74 = load i64, ptr %23
  %75 = icmp sgt i64 %74, 254
  br i1 %75, label %L147, label %L140
L140:
  %76 = load i64, ptr %alloc
  %77 = sub i64 %76, 16
  store i64 %77, ptr %alloc
  %78 = load i64, ptr %ds
  %79 = inttoptr i64 %78 to ptr
  %80 = load i64, ptr %79
  %81 = icmp ule i64 %80, %77
  %82 = call  i1 @llvm.expect.i1(i1 %81, i1 1) 
  br i1 %82, label %L169, label %L168
L168:
  %83 = load i64, ptr %ds
  %84 = load i64, ptr %alloc
  %85 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %83, i64 %84) "statepoint-id"="131073" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 2, i64 1, i64 18, i64 0, i64 10, i64 30, i64 0, i64 30, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 23, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 6517550, i64 28257) ]
  %86 = extractvalue { { i64, i64 }, {  } } %85, 0, 0
  %87 = extractvalue { { i64, i64 }, {  } } %85, 0, 1
  store i64 %86, ptr %ds
  store i64 %87, ptr %alloc
  br label %L169
L169:
  %88 = load i64, ptr %alloc
  %89 = add i64 %88, 8
  %90 = inttoptr i64 %89 to ptr addrspace(1)
  store ptr addrspace(1) %90, ptr %24
  %91 = load ptr addrspace(1), ptr %24
  %92 = getelementptr i8, ptr addrspace(1) %91, i64 -8
  store volatile i64 1277, ptr addrspace(1) %92
  %93 = load i64, ptr %13
  %94 = shl i64 %93, 2
  store i64 %94, ptr %26
  %95 = load ptr addrspace(1), ptr %11
  %96 = load i64, ptr %26
  %97 = getelementptr i8, ptr addrspace(1) %95, i64 %96
  store ptr addrspace(1) %97, ptr %27
  %98 = load ptr addrspace(1), ptr %27
  %99 = getelementptr i8, ptr addrspace(1) %98, i64 -4
  store ptr addrspace(1) %99, ptr %28
  %100 = load ptr addrspace(1), ptr %28
  %101 = load double, ptr addrspace(1) %100
  store double %101, ptr %29
  %102 = load ptr addrspace(1), ptr %24
  %103 = load double, ptr %29
  store double %103, ptr addrspace(1) %102
  %104 = load ptr addrspace(1), ptr %24
  store ptr addrspace(1) %104, ptr %30
  %105 = load ptr addrspace(1), ptr %30
  store ptr addrspace(1) %105, ptr %31
  %106 = load ptr addrspace(1), ptr %31
  store ptr addrspace(1) %106, ptr %21
  br label %L154
L147:
  %107 = load i64, ptr %13
  %108 = shl i64 %107, 2
  store i64 %108, ptr %32
  %109 = load ptr addrspace(1), ptr %11
  %110 = load i64, ptr %32
  %111 = getelementptr i8, ptr addrspace(1) %109, i64 %110
  store ptr addrspace(1) %111, ptr %33
  %112 = load ptr addrspace(1), ptr %33
  %113 = getelementptr i8, ptr addrspace(1) %112, i64 -4
  store ptr addrspace(1) %113, ptr %34
  %114 = load ptr addrspace(1), ptr %34
  %115 = load ptr addrspace(1), ptr addrspace(1) %114
  store ptr addrspace(1) %115, ptr %35
  %116 = load ptr addrspace(1), ptr %35
  store ptr addrspace(1) %116, ptr %36
  %117 = load ptr addrspace(1), ptr %36
  store ptr addrspace(1) %117, ptr %21
  br label %L154
L154:
  %118 = load ptr addrspace(1), ptr %21
  store ptr addrspace(1) %118, ptr %6
  %119 = load ptr addrspace(1), ptr %10
  store ptr addrspace(1) %119, ptr %7
  %120 = ptrtoint ptr @"\01_caml_equal" to i64
  %121 = load ptr addrspace(1), ptr %6
  %122 = load ptr addrspace(1), ptr %7
  %123 = load i64, ptr %ds
  %124 = load i64, ptr %alloc
  %125 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %123, i64 %124, i64 %120, ptr addrspace(1) %121, ptr addrspace(1) %122) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 18, i64 0, i64 10, i64 36, i64 0, i64 36, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 23, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 6517550, i64 28257) ]
  %126 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %125, 0, 0
  %127 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %125, 0, 1
  store i64 %126, ptr %ds
  store i64 %127, ptr %alloc
  %128 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %125, 1, 0
  store ptr addrspace(1) %128, ptr %6
  br label %L156
L156:
  %129 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %129, ptr %37
  %130 = load ptr addrspace(1), ptr %37
  store ptr addrspace(1) %130, ptr %38
  %131 = load ptr addrspace(1), ptr %38
  %132 = inttoptr i64 1 to ptr addrspace(1)
  %133 = icmp slt ptr addrspace(1) %131, %132
  br i1 %133, label %L160, label %L170
L170:
  %134 = load ptr addrspace(1), ptr %38
  %135 = inttoptr i64 1 to ptr addrspace(1)
  %136 = icmp sgt ptr addrspace(1) %134, %135
  br i1 %136, label %L160, label %L157
L157:
  %137 = load i64, ptr %13
  %138 = add i64 %137, 2
  store i64 %138, ptr %39
  %139 = load i64, ptr %39
  store i64 %139, ptr %40
  %140 = load i64, ptr %40
  store i64 %140, ptr %13
  br label %L124
L160:
  %141 = load i64, ptr %13
  store i64 %141, ptr %9
  %142 = load i64, ptr %9
  %143 = load i64, ptr %ds
  %144 = load i64, ptr %alloc
  %145 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %143, 0, 0
  %146 = insertvalue { { i64, i64 }, { i64 } } %145, i64 %144, 0, 1
  %147 = insertvalue { { i64, i64 }, { i64 } } %146, i64 %142, 1, 0
  ret { { i64, i64 }, { i64 } } %147
}

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_find_miss_rare__run_5_12_code"(i64 %0, i64 %1, i64 %2, i64 %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="48" noinline gc "oxcaml" personality ptr @"\01_caml_llvm_eh_personality" {
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
  %9 = alloca ptr addrspace(1) 
  %10 = alloca i64 
  %11 = alloca i64 
  %12 = alloca i64 
  %13 = alloca i64 
  %14 = alloca i64 
  %15 = alloca ptr addrspace(1) 
  %16 = alloca i64 
  %17 = alloca i64 
  %18 = alloca ptr addrspace(1) 
  %19 = alloca ptr addrspace(1) 
  %20 = alloca i64 
  %21 = alloca i64 
  %22 = alloca i64 
  %23 = alloca i64 
  %24 = alloca i64 
  %25 = alloca i64 
  %26 = alloca ptr addrspace(1) 
  %27 = alloca i64 
  %28 = alloca i64 
  %29 = alloca i64 
  %30 = alloca i64 
  %31 = alloca i64 
  %32 = alloca ptr addrspace(1) 
  %33 = alloca ptr addrspace(1) 
  %34 = alloca i64 
  %35 = alloca i64 
  %36 = alloca i64 
  %37 = alloca i64 
  %38 = alloca i64 
  %39 = alloca ptr addrspace(1) 
  %40 = alloca i64 
  %41 = alloca i64 
  %42 = alloca i64 
  %43 = alloca i64 
  %44 = alloca i64 
  %45 = alloca i64 
  %46 = alloca i64 
  %47 = alloca i64 
  %48 = alloca i64 
  %49 = alloca i64 
  %50 = alloca i64 
  %51 = alloca i64 
  %52 = alloca i64 
  %53 = alloca i64 
  %54 = alloca i64 
  %55 = alloca i64 
  %56 = alloca i64 
  %57 = alloca i64 
  %58 = alloca i64 
  %59 = alloca i64 
  %60 = alloca i64 
  %61 = alloca i64 
  %62 = alloca i64 
  %63 = alloca i64 
  %64 = alloca i64 
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
  %76 = alloca ptr addrspace(1) 
  %77 = alloca i64 
  %78 = alloca i64 
  %79 = alloca i64 
  %80 = alloca i64 
  %81 = alloca i64 
  %82 = alloca i64 
  %83 = alloca i64 
  %84 = alloca i64 
  %85 = alloca i64 
  %86 = alloca i64 
  %87 = alloca i64 
  %88 = alloca i64 
  %89 = alloca i64 
  %90 = alloca i64 
  %91 = alloca i64 
  %92 = alloca i64 
  %93 = alloca i64 
  %94 = alloca i64 
  %95 = alloca i64 
  %96 = alloca i64 
  %97 = alloca i64 
  %98 = alloca i64 
  %99 = alloca i64 
  %100 = alloca i64 
  %101 = alloca i64 
  %102 = alloca i64 
  %103 = alloca i64 
  %104 = alloca i64 
  %105 = alloca i64
  br label %L1
L1:
  br label %L172
L172:
  %106 = load i64, ptr %5
  store i64 %106, ptr %11
  %107 = load i64, ptr %6
  store i64 %107, ptr %12
  %108 = load i64, ptr %ds
  %109 = add i64 %108, 64
  %110 = inttoptr i64 %109 to ptr
  %111 = load i64, ptr %110
  store i64 %111, ptr %13
  %112 = load i64, ptr %13
  store i64 %112, ptr %14
  %113 = inttoptr i64 33 to ptr addrspace(1)
  store ptr addrspace(1) %113, ptr %7
  %114 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %114, ptr %9
  %115 = ptrtoint ptr @"\01_caml_array_make" to i64
  %116 = load ptr addrspace(1), ptr %7
  %117 = load ptr addrspace(1), ptr %9
  %118 = load i64, ptr %ds
  %119 = load i64, ptr %alloc
  %120 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %118, i64 %119, i64 %115, ptr addrspace(1) %116, ptr addrspace(1) %117) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 85, i64 0, i64 13, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 22, i64 0, i64 10, i64 40, i64 0, i64 40, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 22, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7696942, i64 110) ]
  %121 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %120, 0, 0
  %122 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %120, 0, 1
  store i64 %121, ptr %ds
  store i64 %122, ptr %alloc
  %123 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %120, 1, 0
  store ptr addrspace(1) %123, ptr %7
  br label %L178
L178:
  %124 = load i64, ptr %ds
  %125 = add i64 %124, 40
  %126 = inttoptr i64 %125 to ptr
  %127 = load i64, ptr %126
  %128 = add i64 %127, 408
  %129 = call i64 asm sideeffect "mov $0, sp", "=r"() "gc-leaf-function"="true"
  %130 = icmp uge i64 %129, %128
  %131 = call  i1 @llvm.expect.i1(i1 %130, i1 1) 
  br i1 %131, label %L306, label %L305
L305:
  %132 = load i64, ptr %ds
  %133 = load i64, ptr %alloc
  %134 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_llvm_call_realloc_stack"(i64 %132, i64 %133, i64 38) "statepoint-id"="0" cold
  %135 = extractvalue { { i64, i64 }, {  } } %134, 0, 0
  %136 = extractvalue { { i64, i64 }, {  } } %134, 0, 1
  store i64 %135, ptr %ds
  store i64 %136, ptr %alloc
  br label %L306
L306:
  %137 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %137, ptr %18
  %138 = load ptr addrspace(1), ptr %18
  store ptr addrspace(1) %138, ptr %19
  store i64 1, ptr %22
  %139 = load i64, ptr %22
  store i64 %139, ptr %20
  br label %L182
L182:
  %140 = load i64, ptr %20
  %141 = shl i64 %140, 1
  %142 = add i64 1, %141
  store i64 %142, ptr %24
  %143 = load i64, ptr %24
  store i64 %143, ptr %25
  %144 = load ptr addrspace(1), ptr %19
  %145 = getelementptr i8, ptr addrspace(1) %144, i64 -8
  store ptr addrspace(1) %145, ptr %26
  %146 = load ptr addrspace(1), ptr %26
  %147 = load i8, ptr addrspace(1) %146
  %148 = zext i8 %147 to i64
  store i64 %148, ptr %27
  %149 = load i64, ptr %27
  %150 = icmp slt i64 %149, 254
  br i1 %150, label %L189, label %L307
L307:
  %151 = load i64, ptr %27
  %152 = icmp sgt i64 %151, 254
  br i1 %152, label %L189, label %L187
L187:
  %153 = ptrtoint ptr @"\01_camlTry_find_miss_rare__invalid605" to i64
  store i64 %153, ptr %28
  %154 = load i64, ptr %28
  %155 = inttoptr i64 %154 to ptr addrspace(1)
  store ptr addrspace(1) %155, ptr %7
  %156 = ptrtoint ptr @"\01_caml_flambda2_invalid" to i64
  %157 = load ptr addrspace(1), ptr %7
  %158 = load i64, ptr %ds
  %159 = load i64, ptr %alloc
  %160 = call oxcaml_c_directcc { i64, i64 } @"\01_caml_flambda2_invalid"(i64 %158, i64 %159, ptr addrspace(1) %157) "gc-leaf-function"="true"
  %161 = extractvalue { i64, i64 } %160, 0
  %162 = extractvalue { i64, i64 } %160, 1
  store i64 %161, ptr %ds
  store i64 %162, ptr %alloc
  unreachable
L189:
  %163 = load i64, ptr %25
  %164 = shl i64 %163, 1
  %165 = add i64 -1, %164
  store i64 %165, ptr %30
  %166 = load i64, ptr %25
  %167 = shl i64 %166, 2
  store i64 %167, ptr %31
  %168 = load ptr addrspace(1), ptr %19
  %169 = load i64, ptr %31
  %170 = getelementptr i8, ptr addrspace(1) %168, i64 %169
  store ptr addrspace(1) %170, ptr %32
  %171 = load ptr addrspace(1), ptr %32
  %172 = getelementptr i8, ptr addrspace(1) %171, i64 -4
  store ptr addrspace(1) %172, ptr %33
  %173 = load ptr addrspace(1), ptr %33
  store ptr addrspace(1) %173, ptr %8
  %174 = load i64, ptr %30
  %175 = inttoptr i64 %174 to ptr addrspace(1)
  store ptr addrspace(1) %175, ptr %9
  %176 = ptrtoint ptr @"\01_caml_modify" to i64
  %177 = load ptr addrspace(1), ptr %8
  %178 = load ptr addrspace(1), ptr %9
  %179 = load i64, ptr %ds
  %180 = load i64, ptr %alloc
  %181 = call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %179, i64 %180, ptr addrspace(1) %177, ptr addrspace(1) %178) "gc-leaf-function"="true"
  %182 = extractvalue { i64, i64 } %181, 0
  %183 = extractvalue { i64, i64 } %181, 1
  store i64 %182, ptr %ds
  store i64 %183, ptr %alloc
  br label %L191
L191:
  store i64 1, ptr %35
  %184 = load i64, ptr %20
  %185 = add i64 %184, 1
  store i64 %185, ptr %36
  %186 = load i64, ptr %36
  store i64 %186, ptr %37
  %187 = load i64, ptr %37
  %188 = icmp slt i64 %187, 15
  br i1 %188, label %L197, label %L308
L308:
  %189 = load i64, ptr %37
  %190 = icmp sgt i64 %189, 15
  br i1 %190, label %L199, label %L197
L197:
  %191 = load i64, ptr %37
  store i64 %191, ptr %38
  %192 = load i64, ptr %38
  store i64 %192, ptr %20
  br label %L182
L199:
  %193 = load ptr addrspace(1), ptr %19
  store ptr addrspace(1) %193, ptr %39
  %194 = load ptr addrspace(1), ptr %39
  store ptr addrspace(1) %194, ptr %15
  %195 = load i64, ptr %12
  %196 = icmp slt i64 %195, 3
  br i1 %196, label %L297, label %L309
L309:
  %197 = load i64, ptr %12
  %198 = icmp sgt i64 %197, 3
  br i1 %198, label %L209, label %L209
L209:
  %199 = load i64, ptr %12
  %200 = ashr i64 %199, 1
  store i64 %200, ptr %41
  %201 = load i64, ptr %41
  store i64 %201, ptr %42
  store i64 1, ptr %47
  store i64 1, ptr %48
  %202 = load i64, ptr %47
  store i64 %202, ptr %43
  %203 = load i64, ptr %48
  store i64 %203, ptr %44
  br label %L215
L215:
  %204 = load i64, ptr %43
  %205 = shl i64 %204, 1
  %206 = add i64 1, %205
  store i64 %206, ptr %50
  %207 = load i64, ptr %50
  store i64 %207, ptr %51
  %208 = load i64, ptr %11
  %209 = icmp slt i64 %208, 3
  br i1 %209, label %L284, label %L310
L310:
  %210 = load i64, ptr %11
  %211 = icmp sgt i64 %210, 3
  br i1 %211, label %L221, label %L221
L221:
  %212 = load i64, ptr %11
  %213 = ashr i64 %212, 1
  store i64 %213, ptr %53
  %214 = load i64, ptr %53
  store i64 %214, ptr %54
  store i64 1, ptr %58
  %215 = load i64, ptr %44
  store i64 %215, ptr %59
  %216 = load i64, ptr %58
  store i64 %216, ptr %55
  %217 = load i64, ptr %59
  store i64 %217, ptr %56
  br label %L227
L227:
  %218 = load i64, ptr %55
  %219 = shl i64 %218, 1
  %220 = add i64 1, %219
  store i64 %220, ptr %61
  %221 = load i64, ptr %61
  store i64 %221, ptr %62
  %222 = load i64, ptr %62
  %223 = and i64 %222, 511
  store i64 %223, ptr %64
  %224 = load i64, ptr %64
  %225 = icmp slt i64 %224, 1
  br i1 %225, label %L236, label %L311
L311:
  %226 = load i64, ptr %64
  %227 = icmp sgt i64 %226, 1
  br i1 %227, label %L236, label %L234
L234:
  store i64 -1, ptr %66
  %228 = load i64, ptr %66
  store i64 %228, ptr %63
  br label %L243
L236:
  %229 = load i64, ptr %62
  %230 = load i64, ptr %51
  %231 = add i64 %229, %230
  store i64 %231, ptr %67
  %232 = load i64, ptr %67
  %233 = add i64 %232, -1
  store i64 %233, ptr %68
  %234 = load i64, ptr %68
  %235 = and i64 %234, 31
  store i64 %235, ptr %69
  %236 = load i64, ptr %69
  %237 = shl i64 %236, 1
  %238 = add i64 -1, %237
  store i64 %238, ptr %71
  %239 = load i64, ptr %71
  store i64 %239, ptr %72
  %240 = load i64, ptr %72
  store i64 %240, ptr %63
  br label %L243
L243:
  %241 = load i64, ptr %ds
  %242 = add i64 %241, 64
  %243 = inttoptr i64 %242 to ptr
  %244 = load i64, ptr %243
  store i64 %244, ptr %73
  %245 = load i64, ptr %73
  store i64 %245, ptr %74
  call  void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@"\01_camlTry_find_miss_rare__run_5_12_code", %L313)) 
  br label %L312
L313:
  %246 = landingpad token cleanup
  %247 = call  { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover() 
  %248 = extractvalue { ptr addrspace(1), i64, i64, i64 } %247, 0
  %249 = extractvalue { ptr addrspace(1), i64, i64, i64 } %247, 2
  %250 = extractvalue { ptr addrspace(1), i64, i64, i64 } %247, 3
  store ptr addrspace(1) %248, ptr %105
  store i64 %250, ptr %ds
  store i64 %249, ptr %alloc
  br label %L249
L312:
  %251 = load i64, ptr %63
  store i64 %251, ptr %5
  %252 = load ptr addrspace(1), ptr %15
  store ptr addrspace(1) %252, ptr %9
  store i64 1, ptr %10
  %253 = load i64, ptr %5
  %254 = inttoptr i64 %253 to ptr addrspace(1)
  %255 = load ptr addrspace(1), ptr %9
  %256 = load i64, ptr %10
  %257 = load i64, ptr %ds
  %258 = load i64, ptr %alloc
  %259 = invoke oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_find_miss_rare__scan_4_11_code"(i64 %257, i64 %258, ptr addrspace(1) %254, ptr addrspace(1) %255, i64 %256) "statepoint-id"="18" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 27, i64 0, i64 24, i64 36, i64 0, i64 36, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 22, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7696942, i64 110) ] to label %L314 unwind label %L313
L314:
  %260 = extractvalue { { i64, i64 }, { i64 } } %259, 0, 0
  %261 = extractvalue { { i64, i64 }, { i64 } } %259, 0, 1
  store i64 %260, ptr %ds
  store i64 %261, ptr %alloc
  %262 = extractvalue { { i64, i64 }, { i64 } } %259, 1, 0
  store i64 %262, ptr %5
  br label %L257
L257:
  %263 = load i64, ptr %5
  store i64 %263, ptr %78
  %264 = load i64, ptr %78
  store i64 %264, ptr %79
  %265 = load i64, ptr %56
  %266 = load i64, ptr %79
  %267 = add i64 %265, %266
  store i64 %267, ptr %80
  %268 = load i64, ptr %80
  %269 = add i64 %268, -1
  store i64 %269, ptr %81
  %270 = load i64, ptr %81
  store i64 %270, ptr %82
  %271 = load i64, ptr %82
  store i64 %271, ptr %75
  call  void @llvm.aarch64.oxcaml.pop.trap() 
  br label %L273
L249:
  %272 = load i64, ptr %105
  %273 = inttoptr i64 %272 to ptr addrspace(1)
  store ptr addrspace(1) %273, ptr %7
  %274 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %274, ptr %76
  %275 = load i64, ptr %ds
  %276 = add i64 %275, 64
  %277 = inttoptr i64 %276 to ptr
  %278 = load i64, ptr %74
  store i64 %278, ptr %277
  store i64 1, ptr %84
  %279 = ptrtoint ptr @"\01_camlTry_find_miss_rare__Miss281" to i64
  store i64 %279, ptr %85
  %280 = load ptr addrspace(1), ptr %76
  %281 = load i64, ptr %85
  %282 = inttoptr i64 %281 to ptr addrspace(1)
  %283 = icmp slt ptr addrspace(1) %280, %282
  br i1 %283, label %L269, label %L315
L315:
  %284 = load ptr addrspace(1), ptr %76
  %285 = load i64, ptr %85
  %286 = inttoptr i64 %285 to ptr addrspace(1)
  %287 = icmp sgt ptr addrspace(1) %284, %286
  br i1 %287, label %L269, label %L264
L264:
  %288 = load i64, ptr %56
  %289 = load i64, ptr %62
  %290 = add i64 %288, %289
  store i64 %290, ptr %86
  %291 = load i64, ptr %86
  %292 = add i64 %291, -1
  store i64 %292, ptr %87
  %293 = load i64, ptr %87
  %294 = and i64 %293, 2147483647
  store i64 %294, ptr %88
  %295 = load i64, ptr %88
  store i64 %295, ptr %89
  %296 = load i64, ptr %89
  store i64 %296, ptr %75
  br label %L273
L269:
  %297 = load ptr addrspace(1), ptr %76
  store ptr addrspace(1) %297, ptr %7
  %298 = load ptr addrspace(1), ptr %7
  %299 = ptrtoint ptr addrspace(1) %298 to i64
  call  void @llvm.aarch64.oxcaml.raise.notrace(i64 %299) 
  unreachable
L273:
  %300 = load i64, ptr %55
  %301 = add i64 %300, 1
  store i64 %301, ptr %90
  %302 = load i64, ptr %90
  store i64 %302, ptr %91
  %303 = load i64, ptr %91
  %304 = load i64, ptr %54
  %305 = icmp slt i64 %303, %304
  br i1 %305, label %L276, label %L316
L316:
  %306 = load i64, ptr %91
  %307 = load i64, ptr %54
  %308 = icmp sgt i64 %306, %307
  br i1 %308, label %L278, label %L276
L276:
  %309 = load i64, ptr %91
  store i64 %309, ptr %92
  %310 = load i64, ptr %75
  store i64 %310, ptr %93
  %311 = load i64, ptr %92
  store i64 %311, ptr %55
  %312 = load i64, ptr %93
  store i64 %312, ptr %56
  br label %L227
L278:
  %313 = load i64, ptr %75
  store i64 %313, ptr %94
  %314 = load i64, ptr %94
  store i64 %314, ptr %52
  br label %L287
L284:
  %315 = load i64, ptr %44
  store i64 %315, ptr %95
  %316 = load i64, ptr %95
  store i64 %316, ptr %52
  br label %L287
L287:
  %317 = load i64, ptr %43
  %318 = add i64 %317, 1
  store i64 %318, ptr %96
  %319 = load i64, ptr %96
  store i64 %319, ptr %97
  %320 = load i64, ptr %97
  %321 = load i64, ptr %42
  %322 = icmp slt i64 %320, %321
  br i1 %322, label %L290, label %L317
L317:
  %323 = load i64, ptr %97
  %324 = load i64, ptr %42
  %325 = icmp sgt i64 %323, %324
  br i1 %325, label %L292, label %L290
L290:
  %326 = load i64, ptr %97
  store i64 %326, ptr %98
  %327 = load i64, ptr %52
  store i64 %327, ptr %99
  %328 = load i64, ptr %98
  store i64 %328, ptr %43
  %329 = load i64, ptr %99
  store i64 %329, ptr %44
  br label %L215
L292:
  %330 = load i64, ptr %52
  store i64 %330, ptr %100
  %331 = load i64, ptr %100
  store i64 %331, ptr %40
  br label %L300
L297:
  store i64 1, ptr %102
  %332 = load i64, ptr %102
  store i64 %332, ptr %40
  br label %L300
L300:
  %333 = load i64, ptr %ds
  %334 = add i64 %333, 64
  %335 = inttoptr i64 %334 to ptr
  %336 = load i64, ptr %14
  store i64 %336, ptr %335
  store i64 1, ptr %104
  %337 = load i64, ptr %40
  store i64 %337, ptr %5
  %338 = load i64, ptr %5
  %339 = load i64, ptr %ds
  %340 = load i64, ptr %alloc
  %341 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %339, 0, 0
  %342 = insertvalue { { i64, i64 }, { i64 } } %341, i64 %340, 0, 1
  %343 = insertvalue { { i64, i64 }, { i64 } } %342, i64 %338, 1, 0
  ret { { i64, i64 }, { i64 } } %343
}

define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTry_find_miss_rare__entry"(i64 %0, i64 %1) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" noinline gc "oxcaml" {
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
  br label %L325
L325:
  %86 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %86, ptr %3
  %87 = ptrtoint ptr @"\01_caml_sys_argv" to i64
  %88 = load ptr addrspace(1), ptr %3
  %89 = load i64, ptr %ds
  %90 = load i64, ptr %alloc
  %91 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %89, i64 %90, i64 %87, ptr addrspace(1) %88) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 6, i64 0, i64 18, i64 26, i64 0, i64 26, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 20, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 28206) ]
  %92 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %91, 0, 0
  %93 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %91, 0, 1
  store i64 %92, ptr %ds
  store i64 %93, ptr %alloc
  %94 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %91, 1, 0
  store ptr addrspace(1) %94, ptr %3
  br label %L327
L327:
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
  br i1 %106, label %L353, label %L411
L411:
  %107 = load i64, ptr %17
  %108 = icmp sgt i64 %107, 3
  br i1 %108, label %L332, label %L353
L332:
  %109 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %109, ptr %3
  %110 = ptrtoint ptr @"\01_caml_sys_argv" to i64
  %111 = load ptr addrspace(1), ptr %3
  %112 = load i64, ptr %ds
  %113 = load i64, ptr %alloc
  %114 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %112, i64 %113, i64 %110, ptr addrspace(1) %111) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 6, i64 0, i64 50, i64 58, i64 0, i64 58, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 20, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 28206) ]
  %115 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %114, 0, 0
  %116 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %114, 0, 1
  store i64 %115, ptr %ds
  store i64 %116, ptr %alloc
  %117 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %114, 1, 0
  store ptr addrspace(1) %117, ptr %3
  br label %L334
L334:
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
  br i1 %129, label %L350, label %L412
L412:
  %130 = load i64, ptr %24
  %131 = icmp ugt i64 %130, 3
  br i1 %131, label %L342, label %L350
L342:
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
  %141 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %139, i64 %140, i64 %137, ptr addrspace(1) %138) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 6, i64 0, i64 36, i64 62, i64 0, i64 62, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 20, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 28206) ]
  %142 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %141, 0, 0
  %143 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %141, 0, 1
  store i64 %142, ptr %ds
  store i64 %143, ptr %alloc
  %144 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %141, 1, 0
  store ptr addrspace(1) %144, ptr %3
  br label %L344
L344:
  %145 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %145, ptr %27
  %146 = load ptr addrspace(1), ptr %27
  store ptr addrspace(1) %146, ptr %28
  %147 = load ptr addrspace(1), ptr %28
  store ptr addrspace(1) %147, ptr %29
  %148 = load ptr addrspace(1), ptr %29
  %149 = ptrtoint ptr addrspace(1) %148 to i64
  store i64 %149, ptr %10
  br label %L356
L350:
  %150 = ptrtoint ptr @"\01_camlTry_find_miss_rare__block35" to i64
  store i64 %150, ptr %30
  %151 = load i64, ptr %30
  %152 = inttoptr i64 %151 to ptr addrspace(1)
  store ptr addrspace(1) %152, ptr %3
  %153 = load ptr addrspace(1), ptr %3
  %154 = ptrtoint ptr addrspace(1) %153 to i64
  call  void @llvm.aarch64.oxcaml.raise.notrace(i64 %154) 
  unreachable
L353:
  store i64 200001, ptr %32
  %155 = load i64, ptr %32
  store i64 %155, ptr %10
  br label %L356
L356:
  %156 = ptrtoint ptr @"\01_camlTry_find_miss_rare" to i64
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
  br label %L358
L358:
  %171 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %171, ptr %3
  %172 = ptrtoint ptr @"\01_caml_sys_argv" to i64
  %173 = load ptr addrspace(1), ptr %3
  %174 = load i64, ptr %ds
  %175 = load i64, ptr %alloc
  %176 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %174, i64 %175, i64 %172, ptr addrspace(1) %173) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 9, i64 0, i64 18, i64 26, i64 0, i64 26, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 23, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 6648366, i64 29552) ]
  %177 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %176, 0, 0
  %178 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %176, 0, 1
  store i64 %177, ptr %ds
  store i64 %178, ptr %alloc
  %179 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %176, 1, 0
  store ptr addrspace(1) %179, ptr %3
  br label %L363
L363:
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
  br i1 %191, label %L389, label %L413
L413:
  %192 = load i64, ptr %43
  %193 = icmp sgt i64 %192, 5
  br i1 %193, label %L368, label %L389
L368:
  %194 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %194, ptr %3
  %195 = ptrtoint ptr @"\01_caml_sys_argv" to i64
  %196 = load ptr addrspace(1), ptr %3
  %197 = load i64, ptr %ds
  %198 = load i64, ptr %alloc
  %199 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %197, i64 %198, i64 %195, ptr addrspace(1) %196) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 9, i64 0, i64 50, i64 58, i64 0, i64 58, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 23, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 6648366, i64 29552) ]
  %200 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %199, 0, 0
  %201 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %199, 0, 1
  store i64 %200, ptr %ds
  store i64 %201, ptr %alloc
  %202 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %199, 1, 0
  store ptr addrspace(1) %202, ptr %3
  br label %L370
L370:
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
  br i1 %214, label %L386, label %L414
L414:
  %215 = load i64, ptr %50
  %216 = icmp ugt i64 %215, 5
  br i1 %216, label %L378, label %L386
L378:
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
  %226 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %224, i64 %225, i64 %222, ptr addrspace(1) %223) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 9, i64 0, i64 36, i64 62, i64 0, i64 62, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 23, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 6648366, i64 29552) ]
  %227 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %226, 0, 0
  %228 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %226, 0, 1
  store i64 %227, ptr %ds
  store i64 %228, ptr %alloc
  %229 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %226, 1, 0
  store ptr addrspace(1) %229, ptr %3
  br label %L380
L380:
  %230 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %230, ptr %53
  %231 = load ptr addrspace(1), ptr %53
  store ptr addrspace(1) %231, ptr %54
  %232 = load ptr addrspace(1), ptr %54
  store ptr addrspace(1) %232, ptr %55
  %233 = load ptr addrspace(1), ptr %55
  %234 = ptrtoint ptr addrspace(1) %233 to i64
  store i64 %234, ptr %36
  br label %L392
L386:
  %235 = ptrtoint ptr @"\01_camlTry_find_miss_rare__block35" to i64
  store i64 %235, ptr %56
  %236 = load i64, ptr %56
  %237 = inttoptr i64 %236 to ptr addrspace(1)
  store ptr addrspace(1) %237, ptr %3
  %238 = load ptr addrspace(1), ptr %3
  %239 = ptrtoint ptr addrspace(1) %238 to i64
  call  void @llvm.aarch64.oxcaml.raise.notrace(i64 %239) 
  unreachable
L389:
  store i64 21, ptr %58
  %240 = load i64, ptr %58
  store i64 %240, ptr %36
  br label %L392
L392:
  %241 = ptrtoint ptr @"\01_camlTry_find_miss_rare" to i64
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
  br label %L394
L394:
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
  br label %L396
L396:
  %265 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %265, ptr %63
  %266 = load ptr addrspace(1), ptr %63
  store ptr addrspace(1) %266, ptr %64
  %267 = ptrtoint ptr @"\01_camlTry_find_miss_rare__Miss281" to i64
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
  br label %L397
L397:
  %281 = load i64, ptr %ds
  %282 = add i64 %281, 40
  %283 = inttoptr i64 %282 to ptr
  %284 = load i64, ptr %283
  %285 = add i64 %284, 376
  %286 = call i64 asm sideeffect "mov $0, sp", "=r"() "gc-leaf-function"="true"
  %287 = icmp uge i64 %286, %285
  %288 = call  i1 @llvm.expect.i1(i1 %287, i1 1) 
  br i1 %288, label %L416, label %L415
L415:
  %289 = load i64, ptr %ds
  %290 = load i64, ptr %alloc
  %291 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_llvm_call_realloc_stack"(i64 %289, i64 %290, i64 34) "statepoint-id"="0" cold
  %292 = extractvalue { { i64, i64 }, {  } } %291, 0, 0
  %293 = extractvalue { { i64, i64 }, {  } } %291, 0, 1
  store i64 %292, ptr %ds
  store i64 %293, ptr %alloc
  br label %L416
L416:
  %294 = load i64, ptr %36
  store i64 %294, ptr %5
  %295 = load i64, ptr %5
  %296 = load i64, ptr %ds
  %297 = load i64, ptr %alloc
  %298 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_find_miss_rare__black_box_int_0_7_code"(i64 %296, i64 %297, i64 %295) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 33, i64 0, i64 45, i64 65, i64 0, i64 65, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 18, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417) ]
  %299 = extractvalue { { i64, i64 }, { i64 } } %298, 0, 0
  %300 = extractvalue { { i64, i64 }, { i64 } } %298, 0, 1
  store i64 %299, ptr %ds
  store i64 %300, ptr %alloc
  %301 = extractvalue { { i64, i64 }, { i64 } } %298, 1, 0
  store i64 %301, ptr %5
  br label %L399
L399:
  %302 = load i64, ptr %5
  store i64 %302, ptr %68
  %303 = load i64, ptr %68
  store i64 %303, ptr %69
  %304 = load i64, ptr %10
  store i64 %304, ptr %5
  %305 = load i64, ptr %5
  %306 = load i64, ptr %ds
  %307 = load i64, ptr %alloc
  %308 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_find_miss_rare__black_box_int_0_7_code"(i64 %306, i64 %307, i64 %305) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 33, i64 0, i64 27, i64 44, i64 0, i64 44, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 18, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417) ]
  %309 = extractvalue { { i64, i64 }, { i64 } } %308, 0, 0
  %310 = extractvalue { { i64, i64 }, { i64 } } %308, 0, 1
  store i64 %309, ptr %ds
  store i64 %310, ptr %alloc
  %311 = extractvalue { { i64, i64 }, { i64 } } %308, 1, 0
  store i64 %311, ptr %5
  br label %L400
L400:
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
  %320 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_find_miss_rare__run_5_12_code"(i64 %318, i64 %319, i64 %316, i64 %317) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 33, i64 0, i64 22, i64 66, i64 0, i64 66, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 18, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417) ]
  %321 = extractvalue { { i64, i64 }, { i64 } } %320, 0, 0
  %322 = extractvalue { { i64, i64 }, { i64 } } %320, 0, 1
  store i64 %321, ptr %ds
  store i64 %322, ptr %alloc
  %323 = extractvalue { { i64, i64 }, { i64 } } %320, 1, 0
  store i64 %323, ptr %5
  br label %L401
L401:
  %324 = load i64, ptr %5
  store i64 %324, ptr %72
  %325 = load i64, ptr %72
  store i64 %325, ptr %73
  %326 = ptrtoint ptr @"\01_camlTry_find_miss_rare__const_block66" to i64
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
  %335 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlCamlinternalFormat__make_printf_120_401_code"(i64 %333, i64 %334, i64 %330, i64 %331, i64 %332) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 5, i64 27, i64 0, i64 2, i64 63, i64 0, i64 63, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 23, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 7366251, i64 7235954, i64 26228, i64 34, i64 0, i64 21, i64 43, i64 0, i64 43, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 22, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 7499878, i64 7630441, i64 102, i64 38, i64 0, i64 17, i64 35, i64 0, i64 35, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 21, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 6910576, i64 6714478, i64 11, i64 0, i64 21, i64 63, i64 0, i64 63, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 31, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7499822, i64 7630441, i64 6648415, i64 7107955, i64 116, i64 33, i64 0, i64 9, i64 66, i64 0, i64 66, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 18, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417) ]
  %336 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %335, 0, 0
  %337 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %335, 0, 1
  store i64 %336, ptr %ds
  store i64 %337, ptr %alloc
  %338 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %335, 1, 0
  store ptr addrspace(1) %338, ptr %3
  br label %L402
L402:
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
  %353 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } %352(i64 %349, i64 %350, i64 %347, ptr addrspace(1) %348) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 2, i64 11, i64 0, i64 21, i64 63, i64 0, i64 63, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 31, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7499822, i64 7630441, i64 6648415, i64 7107955, i64 116, i64 33, i64 0, i64 9, i64 66, i64 0, i64 66, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 18, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417) ]
  %354 = extractvalue { { i64, i64 }, { i64 } } %353, 0, 0
  %355 = extractvalue { { i64, i64 }, { i64 } } %353, 0, 1
  store i64 %354, ptr %ds
  store i64 %355, ptr %alloc
  %356 = extractvalue { { i64, i64 }, { i64 } } %353, 1, 0
  store i64 %356, ptr %5
  br label %L403
L403:
  %357 = load i64, ptr %5
  store i64 %357, ptr %81
  %358 = load i64, ptr %81
  store i64 %358, ptr %82
  %359 = ptrtoint ptr @"\01_camlTry_find_miss_rare" to i64
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

@"\01_camlTry_find_miss_rare__gc_roots" = global { ptr, ptr, i64 } { ptr @"\01_camlTry_find_miss_rare", ptr @"\01_camlTry_find_miss_rare__Miss281", i64 0 }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare" = global i64 9984, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare" = global { ptr, ptr, ptr, i64, i64, ptr, ptr, ptr, ptr } { ptr @"\01_camlTry_find_miss_rare__black_box_int_7", ptr @"\01_camlTry_find_miss_rare__black_box_string_8", ptr @"\01_camlTry_find_miss_rare__black_box_9", i64 1, i64 1, ptr @"\01_camlTry_find_miss_rare__print_result_10", ptr @"\01_camlTry_find_miss_rare__Miss281", ptr @"\01_camlTry_find_miss_rare__scan_11", ptr @"\01_camlTry_find_miss_rare__run_12" }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__run_12" = global i64 4087, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__run_12" = global { ptr, i64, ptr } { ptr @"\01_caml_curry2", i64 180143985094819847, ptr @"\01_camlTry_find_miss_rare__run_5_12_code" }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__invalid605" = global i64 16380, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__invalid605" = global { [ 117 x i8 ], [ 2 x i8 ], i8 } { [ 117 x i8 ] c"\28\44\65\66\69\6e\69\6e\67\5f\65\78\70\72\5f\6f\66\5f\6c\65\74\20\28\62\6f\75\6e\64\5f\70\61\74\74\65\72\6e\20\70\72\69\6d\2f\33\36\37\4e\29\0a\20\28\64\65\66\69\6e\69\6e\67\5f\65\78\70\72\20\28\28\55\6e\62\6f\78\5f\66\6c\6f\61\74\20\61\70\70\6c\79\5f\72\65\73\75\6c\74\2f\33\35\37\4e\29\20\61\72\72\61\79\2e\6d\6c\3a\38\37\2c\35\2d\2d\32\37\29\29\29", [ 2 x i8 ] zeroinitializer, i8 2 }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__scan_11" = global i64 4087, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__scan_11" = global { ptr, i64, ptr } { ptr @"\01_caml_curry3", i64 252201579132747783, ptr @"\01_camlTry_find_miss_rare__scan_4_11_code" }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__Miss281" = global i64 3064, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__Miss281" = global { ptr, i64 } { ptr @"\01_camlTry_find_miss_rare__immstring74", i64 1 }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__print_result_10" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__print_result_10" = global { ptr, i64 } { ptr @"\01_camlTry_find_miss_rare__print_result_3_10_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__black_box_9" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__black_box_9" = global { ptr, i64 } { ptr @"\01_camlTry_find_miss_rare__black_box_2_9_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__black_box_string_8" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__black_box_string_8" = global { ptr, i64 } { ptr @"\01_camlTry_find_miss_rare__black_box_string_1_8_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__black_box_int_7" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__black_box_int_7" = global { ptr, i64 } { ptr @"\01_camlTry_find_miss_rare__black_box_int_0_7_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__block35" = global i64 2816, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__block35" = global { ptr, ptr } { ptr @"\01_caml_exn_Invalid_argument", ptr @"\01_camlTry_find_miss_rare__string33" }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__string33" = global i64 4092, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__string33" = global { [ 19 x i8 ], [ 4 x i8 ], i8 } { [ 19 x i8 ] c"\69\6e\64\65\78\20\6f\75\74\20\6f\66\20\62\6f\75\6e\64\73", [ 4 x i8 ] zeroinitializer, i8 4 }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__immstring74" = global i64 4092, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__immstring74" = global { [ 23 x i8 ], [ 0 x i8 ], i8 } { [ 23 x i8 ] c"\54\72\79\5f\66\69\6e\64\5f\6d\69\73\73\5f\72\61\72\65\2e\4d\69\73\73", [ 0 x i8 ] zeroinitializer, i8 0 }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__const_block66" = global i64 4868, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__const_block66" = global { i64, i64, i64, ptr } { i64 1, i64 1, i64 1, ptr @"\01_camlTry_find_miss_rare__const_block64" }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__const_block64" = global i64 2828, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__const_block64" = global { i64, ptr } { i64 21, ptr @"\01_camlTry_find_miss_rare__const_block62" }, section "__DATA,__data", align 8
@"\01_header.camlTry_find_miss_rare__const_block62" = global i64 1802, section "__DATA,__data", align 8
@"\01_camlTry_find_miss_rare__const_block62" = global { i64 } { i64 1 }, section "__DATA,__data", align 8
@"\01_camlCamlinternalFormat__make_printf_120_401_code" = external global ptr
@"\01_camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31" = external global ptr
@"\01_caml_array_make" = external global ptr
@"\01_caml_c_call" = external global ptr
@"\01_caml_call_gc" = external global ptr
@"\01_caml_curry2" = external global ptr
@"\01_caml_curry3" = external global ptr
@"\01_caml_equal" = external global ptr
@"\01_caml_exn_Invalid_argument" = external global ptr
@"\01_caml_flambda2_invalid" = external global ptr
@"\01_caml_fresh_oo_id" = external global ptr
@"\01_caml_initialize" = external global ptr
@"\01_caml_int_of_string" = external global ptr
@"\01_caml_llvm_call_realloc_stack" = external global ptr
@"\01_caml_modify" = external global ptr
@"\01_caml_sys_argv" = external global ptr

declare i32 @"\01_caml_llvm_eh_personality"(...)
declare void @llvm.aarch64.oxcaml.pop.trap()
declare void @llvm.aarch64.oxcaml.push.trap(ptr)
declare void @llvm.aarch64.oxcaml.raise.notrace(i64)
declare { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare i1 @llvm.expect.i1(i1, i1)


!0 = !{ i32 1, !"oxcaml_module", !"Try_find_miss_rare" }
!llvm.module.flags = !{ !0 }
