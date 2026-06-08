source_filename = "string_tree_first_byte_diff.ml"

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlString_tree_first_byte_diff__black_box_int_0_8_code"(i64 %0, i64 %1, i64 %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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

define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlString_tree_first_byte_diff__black_box_string_1_9_code"(i64 %0, i64 %1, ptr addrspace(1) %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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

define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlString_tree_first_byte_diff__black_box_2_10_code"(i64 %0, i64 %1, ptr addrspace(1) %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlString_tree_first_byte_diff__print_result_3_11_code"(i64 %0, i64 %1, i64 %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" noinline gc "oxcaml" {
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
  %31 = ptrtoint ptr @"\01_camlString_tree_first_byte_diff__const_block66" to i64
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
  %40 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlCamlinternalFormat__make_printf_120_401_code"(i64 %38, i64 %39, i64 %35, i64 %36, i64 %37) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 4, i64 27, i64 0, i64 2, i64 63, i64 0, i64 63, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 23, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 7366251, i64 7235954, i64 26228, i64 34, i64 0, i64 21, i64 43, i64 0, i64 43, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 22, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 7499878, i64 7630441, i64 102, i64 38, i64 0, i64 17, i64 35, i64 0, i64 35, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 21, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 6910576, i64 6714478, i64 11, i64 0, i64 21, i64 63, i64 0, i64 63, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 40, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7499822, i64 7630441, i64 6648415, i64 7107955, i64 116) ]
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

define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlString_tree_first_byte_diff__key_4_12_code"(i64 %0, i64 %1, i64 %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %4 = alloca i64 
  store i64 %2, ptr %4
  %5 = alloca ptr addrspace(1) 
  %6 = alloca ptr addrspace(1) 
  %7 = alloca ptr addrspace(1) 
  %8 = alloca ptr addrspace(1) 
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
  %19 = alloca i64 
  %20 = alloca i64 
  %21 = alloca ptr addrspace(1) 
  %22 = alloca i64 
  %23 = alloca i64 
  %24 = alloca i64 
  %25 = alloca i64 
  %26 = alloca i64 
  %27 = alloca i64 
  %28 = alloca ptr addrspace(1) 
  %29 = alloca ptr addrspace(1) 
  %30 = alloca i64 
  %31 = alloca i64 
  %32 = alloca i64 
  %33 = alloca i64 
  %34 = alloca ptr addrspace(1) 
  %35 = alloca ptr addrspace(1) 
  %36 = alloca i64 
  %37 = alloca i64 
  %38 = alloca ptr addrspace(1) 
  %39 = alloca ptr addrspace(1) 
  %40 = alloca i64 
  %41 = alloca i64 
  %42 = alloca i64 
  %43 = alloca i64 
  %44 = alloca i64 
  %45 = alloca i64 
  %46 = alloca i64 
  %47 = alloca ptr addrspace(1) 
  %48 = alloca ptr addrspace(1) 
  %49 = alloca i64 
  %50 = alloca i64 
  %51 = alloca ptr addrspace(1) 
  %52 = alloca ptr addrspace(1) 
  %53 = alloca ptr addrspace(1) 
  %54 = alloca i64 
  %55 = alloca i64 
  %56 = alloca i64 
  %57 = alloca i64 
  %58 = alloca i64 
  %59 = alloca i64 
  %60 = alloca ptr addrspace(1) 
  %61 = alloca i64 
  %62 = alloca i64 
  %63 = alloca i64 
  %64 = alloca i64 
  %65 = alloca i64 
  %66 = alloca ptr addrspace(1) 
  %67 = alloca i64 
  %68 = alloca i64 
  %69 = alloca i64 
  %70 = alloca i64 
  %71 = alloca i64 
  %72 = alloca i64 
  %73 = alloca ptr addrspace(1) 
  %74 = alloca i64 
  %75 = alloca i64 
  %76 = alloca i64 
  %77 = alloca i64 
  %78 = alloca i64 
  %79 = alloca i64 
  %80 = alloca i64 
  %81 = alloca ptr addrspace(1) 
  %82 = alloca ptr addrspace(1) 
  %83 = alloca i64 
  %84 = alloca i64 
  %85 = alloca ptr addrspace(1) 
  %86 = alloca ptr addrspace(1) 
  %87 = alloca i64 
  %88 = alloca ptr addrspace(1) 
  %89 = alloca ptr addrspace(1) 
  br label %L1
L1:
  br label %L119
L119:
  %90 = load i64, ptr %4
  store i64 %90, ptr %10
  %91 = ptrtoint ptr @"\01_camlStdlib__immstring191" to i64
  store i64 %91, ptr %11
  %92 = load i64, ptr %11
  %93 = inttoptr i64 %92 to ptr addrspace(1)
  store ptr addrspace(1) %93, ptr %5
  %94 = load i64, ptr %10
  %95 = inttoptr i64 %94 to ptr addrspace(1)
  store ptr addrspace(1) %95, ptr %6
  %96 = ptrtoint ptr @"\01_caml_format_int" to i64
  %97 = load ptr addrspace(1), ptr %5
  %98 = load ptr addrspace(1), ptr %6
  %99 = load i64, ptr %ds
  %100 = load i64, ptr %alloc
  %101 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %99, i64 %100, i64 %96, ptr addrspace(1) %97, ptr addrspace(1) %98) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 280, i64 0, i64 2, i64 19, i64 0, i64 19, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7631662, i64 7235954, i64 7298919, i64 6905702, i64 29806, i64 17, i64 0, i64 67, i64 82, i64 0, i64 82, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 6646574, i64 121) ]
  %102 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %101, 0, 0
  %103 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %101, 0, 1
  store i64 %102, ptr %ds
  store i64 %103, ptr %alloc
  %104 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %101, 1, 0
  store ptr addrspace(1) %104, ptr %5
  br label %L121
L121:
  %105 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %105, ptr %12
  %106 = load ptr addrspace(1), ptr %12
  store ptr addrspace(1) %106, ptr %13
  %107 = load ptr addrspace(1), ptr %13
  %108 = getelementptr i8, ptr addrspace(1) %107, i64 -8
  store ptr addrspace(1) %108, ptr %14
  %109 = load ptr addrspace(1), ptr %14
  %110 = load i64, ptr addrspace(1) %109
  store i64 %110, ptr %15
  %111 = load i64, ptr %15
  %112 = shl i64 %111, 8
  store i64 %112, ptr %16
  %113 = load i64, ptr %16
  %114 = lshr i64 %113, 18
  store i64 %114, ptr %17
  %115 = load i64, ptr %17
  %116 = shl i64 %115, 3
  store i64 %116, ptr %18
  %117 = load i64, ptr %18
  %118 = sub i64 %117, 1
  store i64 %118, ptr %19
  %119 = load i64, ptr %19
  store i64 %119, ptr %20
  %120 = load ptr addrspace(1), ptr %13
  %121 = load i64, ptr %20
  %122 = getelementptr i8, ptr addrspace(1) %120, i64 %121
  store ptr addrspace(1) %122, ptr %21
  %123 = load ptr addrspace(1), ptr %21
  %124 = load i8, ptr addrspace(1) %123
  %125 = zext i8 %124 to i64
  store i64 %125, ptr %22
  %126 = load i64, ptr %20
  %127 = load i64, ptr %22
  %128 = sub i64 %126, %127
  store i64 %128, ptr %23
  %129 = load i64, ptr %23
  %130 = shl i64 %129, 1
  %131 = add i64 1, %130
  store i64 %131, ptr %25
  %132 = load i64, ptr %25
  store i64 %132, ptr %26
  %133 = load i64, ptr %26
  %134 = add i64 %133, 28
  store i64 %134, ptr %27
  %135 = load i64, ptr %27
  %136 = inttoptr i64 %135 to ptr addrspace(1)
  store ptr addrspace(1) %136, ptr %5
  %137 = ptrtoint ptr @"\01_caml_create_bytes" to i64
  %138 = load ptr addrspace(1), ptr %5
  %139 = load i64, ptr %ds
  %140 = load i64, ptr %alloc
  %141 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %139, i64 %140, i64 %137, ptr addrspace(1) %138) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 226, i64 0, i64 10, i64 32, i64 0, i64 32, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 10, i64 6583379, i64 6449516, i64 6170670, i64 41, i64 17, i64 0, i64 48, i64 82, i64 0, i64 82, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 6646574, i64 121) ]
  %142 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %141, 0, 0
  %143 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %141, 0, 1
  store i64 %142, ptr %ds
  store i64 %143, ptr %alloc
  %144 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %141, 1, 0
  store ptr addrspace(1) %144, ptr %5
  br label %L132
L132:
  %145 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %145, ptr %28
  %146 = load ptr addrspace(1), ptr %28
  store ptr addrspace(1) %146, ptr %29
  %147 = ptrtoint ptr @"\01_camlString_tree_first_byte_diff__immstring83" to i64
  store i64 %147, ptr %33
  %148 = load i64, ptr %33
  %149 = inttoptr i64 %148 to ptr addrspace(1)
  store ptr addrspace(1) %149, ptr %5
  %150 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %150, ptr %6
  %151 = load ptr addrspace(1), ptr %29
  store ptr addrspace(1) %151, ptr %7
  %152 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %152, ptr %8
  %153 = inttoptr i64 29 to ptr addrspace(1)
  store ptr addrspace(1) %153, ptr %9
  %154 = ptrtoint ptr @"\01_caml_blit_string" to i64
  %155 = load ptr addrspace(1), ptr %5
  %156 = load ptr addrspace(1), ptr %6
  %157 = load ptr addrspace(1), ptr %7
  %158 = load ptr addrspace(1), ptr %8
  %159 = load ptr addrspace(1), ptr %9
  %160 = load i64, ptr %ds
  %161 = load i64, ptr %alloc
  %162 = call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %160, i64 %161, ptr addrspace(1) %155, ptr addrspace(1) %156, ptr addrspace(1) %157, ptr addrspace(1) %158, ptr addrspace(1) %159) "gc-leaf-function"="true"
  %163 = extractvalue { i64, i64, ptr addrspace(1) } %162, 0
  %164 = extractvalue { i64, i64, ptr addrspace(1) } %162, 1
  store i64 %163, ptr %ds
  store i64 %164, ptr %alloc
  %165 = extractvalue { i64, i64, ptr addrspace(1) } %162, 2
  store ptr addrspace(1) %165, ptr %5
  br label %L134
L134:
  %166 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %166, ptr %34
  %167 = load ptr addrspace(1), ptr %34
  store ptr addrspace(1) %167, ptr %35
  %168 = load ptr addrspace(1), ptr %13
  store ptr addrspace(1) %168, ptr %5
  %169 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %169, ptr %6
  %170 = load ptr addrspace(1), ptr %29
  store ptr addrspace(1) %170, ptr %7
  %171 = inttoptr i64 29 to ptr addrspace(1)
  store ptr addrspace(1) %171, ptr %8
  %172 = load i64, ptr %26
  %173 = inttoptr i64 %172 to ptr addrspace(1)
  store ptr addrspace(1) %173, ptr %9
  %174 = ptrtoint ptr @"\01_caml_blit_string" to i64
  %175 = load ptr addrspace(1), ptr %5
  %176 = load ptr addrspace(1), ptr %6
  %177 = load ptr addrspace(1), ptr %7
  %178 = load ptr addrspace(1), ptr %8
  %179 = load ptr addrspace(1), ptr %9
  %180 = load i64, ptr %ds
  %181 = load i64, ptr %alloc
  %182 = call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %180, i64 %181, ptr addrspace(1) %175, ptr addrspace(1) %176, ptr addrspace(1) %177, ptr addrspace(1) %178, ptr addrspace(1) %179) "gc-leaf-function"="true"
  %183 = extractvalue { i64, i64, ptr addrspace(1) } %182, 0
  %184 = extractvalue { i64, i64, ptr addrspace(1) } %182, 1
  store i64 %183, ptr %ds
  store i64 %184, ptr %alloc
  %185 = extractvalue { i64, i64, ptr addrspace(1) } %182, 2
  store ptr addrspace(1) %185, ptr %5
  br label %L135
L135:
  %186 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %186, ptr %38
  %187 = load ptr addrspace(1), ptr %38
  store ptr addrspace(1) %187, ptr %39
  %188 = load i64, ptr %10
  %189 = and i64 %188, 51
  store i64 %189, ptr %41
  %190 = load i64, ptr %41
  %191 = add i64 %190, 130
  store i64 %191, ptr %42
  %192 = load i64, ptr %42
  store i64 %192, ptr %43
  %193 = load i64, ptr %43
  %194 = icmp slt i64 %193, 1
  br i1 %194, label %L138, label %L175
L175:
  %195 = load i64, ptr %43
  %196 = icmp sgt i64 %195, 1
  br i1 %196, label %L140, label %L140
L138:
  %197 = ptrtoint ptr @"\01_camlStdlib__Char__Pmakeblock168" to i64
  store i64 %197, ptr %44
  %198 = load i64, ptr %44
  %199 = inttoptr i64 %198 to ptr addrspace(1)
  store ptr addrspace(1) %199, ptr %5
  %200 = load ptr addrspace(1), ptr %5
  %201 = ptrtoint ptr addrspace(1) %200 to i64
  call  void @llvm.aarch64.oxcaml.raise.notrace(i64 %201) 
  unreachable
L140:
  %202 = load i64, ptr %43
  %203 = icmp slt i64 %202, 511
  br i1 %203, label %L144, label %L176
L176:
  %204 = load i64, ptr %43
  %205 = icmp sgt i64 %204, 511
  br i1 %205, label %L142, label %L144
L142:
  %206 = ptrtoint ptr @"\01_camlStdlib__Char__Pmakeblock168" to i64
  store i64 %206, ptr %45
  %207 = load i64, ptr %45
  %208 = inttoptr i64 %207 to ptr addrspace(1)
  store ptr addrspace(1) %208, ptr %5
  %209 = load ptr addrspace(1), ptr %5
  %210 = ptrtoint ptr addrspace(1) %209 to i64
  call  void @llvm.aarch64.oxcaml.raise.notrace(i64 %210) 
  unreachable
L144:
  %211 = inttoptr i64 3 to ptr addrspace(1)
  store ptr addrspace(1) %211, ptr %5
  %212 = ptrtoint ptr @"\01_caml_create_bytes" to i64
  %213 = load ptr addrspace(1), ptr %5
  %214 = load i64, ptr %ds
  %215 = load i64, ptr %alloc
  %216 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %214, i64 %215, i64 %212, ptr addrspace(1) %213) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 3, i64 60, i64 0, i64 10, i64 18, i64 0, i64 18, i64 8, i64 7633250, i64 3044197, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4349791, i64 6648953, i64 7155315, i64 6646625, i64 44, i64 0, i64 2, i64 12, i64 0, i64 12, i64 9, i64 7500915, i64 6778473, i64 7105838, i64 19, i64 6583379, i64 6449516, i64 5463903, i64 6910580, i64 3041134, i64 7037293, i64 101, i64 17, i64 0, i64 2, i64 45, i64 0, i64 45, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 6646574, i64 121) ]
  %217 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %216, 0, 0
  %218 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %216, 0, 1
  store i64 %217, ptr %ds
  store i64 %218, ptr %alloc
  %219 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %216, 1, 0
  store ptr addrspace(1) %219, ptr %5
  br label %L146
L146:
  %220 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %220, ptr %47
  %221 = load ptr addrspace(1), ptr %47
  store ptr addrspace(1) %221, ptr %48
  %222 = load ptr addrspace(1), ptr %48
  store ptr addrspace(1) %222, ptr %5
  %223 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %223, ptr %6
  %224 = inttoptr i64 3 to ptr addrspace(1)
  store ptr addrspace(1) %224, ptr %7
  %225 = load i64, ptr %43
  %226 = inttoptr i64 %225 to ptr addrspace(1)
  store ptr addrspace(1) %226, ptr %8
  %227 = ptrtoint ptr @"\01_caml_fill_bytes" to i64
  %228 = load ptr addrspace(1), ptr %5
  %229 = load ptr addrspace(1), ptr %6
  %230 = load ptr addrspace(1), ptr %7
  %231 = load ptr addrspace(1), ptr %8
  %232 = load i64, ptr %ds
  %233 = load i64, ptr %alloc
  %234 = call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_fill_bytes"(i64 %232, i64 %233, ptr addrspace(1) %228, ptr addrspace(1) %229, ptr addrspace(1) %230, ptr addrspace(1) %231) "gc-leaf-function"="true"
  %235 = extractvalue { i64, i64, ptr addrspace(1) } %234, 0
  %236 = extractvalue { i64, i64, ptr addrspace(1) } %234, 1
  store i64 %235, ptr %ds
  store i64 %236, ptr %alloc
  %237 = extractvalue { i64, i64, ptr addrspace(1) } %234, 2
  store ptr addrspace(1) %237, ptr %5
  br label %L147
L147:
  %238 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %238, ptr %51
  %239 = load ptr addrspace(1), ptr %51
  store ptr addrspace(1) %239, ptr %52
  %240 = load ptr addrspace(1), ptr %48
  %241 = getelementptr i8, ptr addrspace(1) %240, i64 -8
  store ptr addrspace(1) %241, ptr %53
  %242 = load ptr addrspace(1), ptr %53
  %243 = load i64, ptr addrspace(1) %242
  store i64 %243, ptr %54
  %244 = load i64, ptr %54
  %245 = shl i64 %244, 8
  store i64 %245, ptr %55
  %246 = load i64, ptr %55
  %247 = lshr i64 %246, 18
  store i64 %247, ptr %56
  %248 = load i64, ptr %56
  %249 = shl i64 %248, 3
  store i64 %249, ptr %57
  %250 = load i64, ptr %57
  %251 = sub i64 %250, 1
  store i64 %251, ptr %58
  %252 = load i64, ptr %58
  store i64 %252, ptr %59
  %253 = load ptr addrspace(1), ptr %48
  %254 = load i64, ptr %59
  %255 = getelementptr i8, ptr addrspace(1) %253, i64 %254
  store ptr addrspace(1) %255, ptr %60
  %256 = load ptr addrspace(1), ptr %60
  %257 = load i8, ptr addrspace(1) %256
  %258 = zext i8 %257 to i64
  store i64 %258, ptr %61
  %259 = load i64, ptr %59
  %260 = load i64, ptr %61
  %261 = sub i64 %259, %260
  store i64 %261, ptr %62
  %262 = load i64, ptr %62
  %263 = shl i64 %262, 1
  %264 = add i64 1, %263
  store i64 %264, ptr %64
  %265 = load i64, ptr %64
  store i64 %265, ptr %65
  %266 = load ptr addrspace(1), ptr %29
  %267 = getelementptr i8, ptr addrspace(1) %266, i64 -8
  store ptr addrspace(1) %267, ptr %66
  %268 = load ptr addrspace(1), ptr %66
  %269 = load i64, ptr addrspace(1) %268
  store i64 %269, ptr %67
  %270 = load i64, ptr %67
  %271 = shl i64 %270, 8
  store i64 %271, ptr %68
  %272 = load i64, ptr %68
  %273 = lshr i64 %272, 18
  store i64 %273, ptr %69
  %274 = load i64, ptr %69
  %275 = shl i64 %274, 3
  store i64 %275, ptr %70
  %276 = load i64, ptr %70
  %277 = sub i64 %276, 1
  store i64 %277, ptr %71
  %278 = load i64, ptr %71
  store i64 %278, ptr %72
  %279 = load ptr addrspace(1), ptr %29
  %280 = load i64, ptr %72
  %281 = getelementptr i8, ptr addrspace(1) %279, i64 %280
  store ptr addrspace(1) %281, ptr %73
  %282 = load ptr addrspace(1), ptr %73
  %283 = load i8, ptr addrspace(1) %282
  %284 = zext i8 %283 to i64
  store i64 %284, ptr %74
  %285 = load i64, ptr %72
  %286 = load i64, ptr %74
  %287 = sub i64 %285, %286
  store i64 %287, ptr %75
  %288 = load i64, ptr %75
  %289 = shl i64 %288, 1
  %290 = add i64 1, %289
  store i64 %290, ptr %77
  %291 = load i64, ptr %77
  store i64 %291, ptr %78
  %292 = load i64, ptr %65
  %293 = load i64, ptr %78
  %294 = add i64 %292, %293
  store i64 %294, ptr %79
  %295 = load i64, ptr %79
  %296 = add i64 %295, -1
  store i64 %296, ptr %80
  %297 = load i64, ptr %80
  %298 = inttoptr i64 %297 to ptr addrspace(1)
  store ptr addrspace(1) %298, ptr %5
  %299 = ptrtoint ptr @"\01_caml_create_bytes" to i64
  %300 = load ptr addrspace(1), ptr %5
  %301 = load i64, ptr %ds
  %302 = load i64, ptr %alloc
  %303 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %301, i64 %302, i64 %299, ptr addrspace(1) %300) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 226, i64 0, i64 10, i64 32, i64 0, i64 32, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 10, i64 6583379, i64 6449516, i64 6170670, i64 41, i64 17, i64 0, i64 2, i64 82, i64 0, i64 82, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 6646574, i64 121) ]
  %304 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %303, 0, 0
  %305 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %303, 0, 1
  store i64 %304, ptr %ds
  store i64 %305, ptr %alloc
  %306 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %303, 1, 0
  store ptr addrspace(1) %306, ptr %5
  br label %L168
L168:
  %307 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %307, ptr %81
  %308 = load ptr addrspace(1), ptr %81
  store ptr addrspace(1) %308, ptr %82
  %309 = load ptr addrspace(1), ptr %48
  store ptr addrspace(1) %309, ptr %5
  %310 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %310, ptr %6
  %311 = load ptr addrspace(1), ptr %82
  store ptr addrspace(1) %311, ptr %7
  %312 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %312, ptr %8
  %313 = load i64, ptr %65
  %314 = inttoptr i64 %313 to ptr addrspace(1)
  store ptr addrspace(1) %314, ptr %9
  %315 = ptrtoint ptr @"\01_caml_blit_string" to i64
  %316 = load ptr addrspace(1), ptr %5
  %317 = load ptr addrspace(1), ptr %6
  %318 = load ptr addrspace(1), ptr %7
  %319 = load ptr addrspace(1), ptr %8
  %320 = load ptr addrspace(1), ptr %9
  %321 = load i64, ptr %ds
  %322 = load i64, ptr %alloc
  %323 = call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %321, i64 %322, ptr addrspace(1) %316, ptr addrspace(1) %317, ptr addrspace(1) %318, ptr addrspace(1) %319, ptr addrspace(1) %320) "gc-leaf-function"="true"
  %324 = extractvalue { i64, i64, ptr addrspace(1) } %323, 0
  %325 = extractvalue { i64, i64, ptr addrspace(1) } %323, 1
  store i64 %324, ptr %ds
  store i64 %325, ptr %alloc
  %326 = extractvalue { i64, i64, ptr addrspace(1) } %323, 2
  store ptr addrspace(1) %326, ptr %5
  br label %L171
L171:
  %327 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %327, ptr %85
  %328 = load ptr addrspace(1), ptr %85
  store ptr addrspace(1) %328, ptr %86
  %329 = load ptr addrspace(1), ptr %29
  store ptr addrspace(1) %329, ptr %5
  %330 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %330, ptr %6
  %331 = load ptr addrspace(1), ptr %82
  store ptr addrspace(1) %331, ptr %7
  %332 = load i64, ptr %65
  %333 = inttoptr i64 %332 to ptr addrspace(1)
  store ptr addrspace(1) %333, ptr %8
  %334 = load i64, ptr %78
  %335 = inttoptr i64 %334 to ptr addrspace(1)
  store ptr addrspace(1) %335, ptr %9
  %336 = ptrtoint ptr @"\01_caml_blit_string" to i64
  %337 = load ptr addrspace(1), ptr %5
  %338 = load ptr addrspace(1), ptr %6
  %339 = load ptr addrspace(1), ptr %7
  %340 = load ptr addrspace(1), ptr %8
  %341 = load ptr addrspace(1), ptr %9
  %342 = load i64, ptr %ds
  %343 = load i64, ptr %alloc
  %344 = call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %342, i64 %343, ptr addrspace(1) %337, ptr addrspace(1) %338, ptr addrspace(1) %339, ptr addrspace(1) %340, ptr addrspace(1) %341) "gc-leaf-function"="true"
  %345 = extractvalue { i64, i64, ptr addrspace(1) } %344, 0
  %346 = extractvalue { i64, i64, ptr addrspace(1) } %344, 1
  store i64 %345, ptr %ds
  store i64 %346, ptr %alloc
  %347 = extractvalue { i64, i64, ptr addrspace(1) } %344, 2
  store ptr addrspace(1) %347, ptr %5
  br label %L172
L172:
  %348 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %348, ptr %88
  %349 = load ptr addrspace(1), ptr %88
  store ptr addrspace(1) %349, ptr %89
  %350 = load ptr addrspace(1), ptr %82
  store ptr addrspace(1) %350, ptr %5
  %351 = load ptr addrspace(1), ptr %5
  %352 = load i64, ptr %ds
  %353 = load i64, ptr %alloc
  %354 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %352, 0, 0
  %355 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %354, i64 %353, 0, 1
  %356 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %355, ptr addrspace(1) %351, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %356
}

define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlString_tree_first_byte_diff__build_5_13_code"(i64 %0, i64 %1, i64 %2, i64 %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %5 = alloca i64 
  store i64 %2, ptr %5
  %6 = alloca i64 
  store i64 %3, ptr %6
  %7 = alloca ptr addrspace(1) 
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
  %22 = alloca ptr addrspace(1) 
  %23 = alloca ptr addrspace(1) 
  %24 = alloca ptr addrspace(1) 
  %25 = alloca ptr addrspace(1) 
  %26 = alloca i64 
  %27 = alloca ptr addrspace(1) 
  %28 = alloca ptr addrspace(1) 
  %29 = alloca ptr addrspace(1) 
  %30 = alloca i64 
  br label %L1
L1:
  br label %L178
L178:
  %31 = load i64, ptr %5
  store i64 %31, ptr %8
  %32 = load i64, ptr %6
  store i64 %32, ptr %9
  %33 = load i64, ptr %8
  %34 = load i64, ptr %9
  %35 = icmp slt i64 %33, %34
  br i1 %35, label %L182, label %L198
L198:
  %36 = load i64, ptr %8
  %37 = load i64, ptr %9
  %38 = icmp sgt i64 %36, %37
  br i1 %38, label %L180, label %L182
L180:
  store i64 1, ptr %5
  %39 = load i64, ptr %5
  %40 = inttoptr i64 %39 to ptr addrspace(1)
  %41 = load i64, ptr %ds
  %42 = load i64, ptr %alloc
  %43 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %41, 0, 0
  %44 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %43, i64 %42, 0, 1
  %45 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %44, ptr addrspace(1) %40, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %45
L182:
  %46 = load i64, ptr %ds
  %47 = add i64 %46, 40
  %48 = inttoptr i64 %47 to ptr
  %49 = load i64, ptr %48
  %50 = add i64 %49, 376
  %51 = call i64 asm sideeffect "mov $0, sp", "=r"() "gc-leaf-function"="true"
  %52 = icmp uge i64 %51, %50
  %53 = call  i1 @llvm.expect.i1(i1 %52, i1 1) 
  br i1 %53, label %L200, label %L199
L199:
  %54 = load i64, ptr %ds
  %55 = load i64, ptr %alloc
  %56 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_llvm_call_realloc_stack"(i64 %54, i64 %55, i64 34) "statepoint-id"="0" cold
  %57 = extractvalue { { i64, i64 }, {  } } %56, 0, 0
  %58 = extractvalue { { i64, i64 }, {  } } %56, 0, 1
  store i64 %57, ptr %ds
  store i64 %58, ptr %alloc
  br label %L200
L200:
  %59 = load i64, ptr %8
  %60 = load i64, ptr %9
  %61 = add i64 %59, %60
  store i64 %61, ptr %11
  %62 = load i64, ptr %11
  %63 = add i64 %62, -1
  store i64 %63, ptr %12
  %64 = load i64, ptr %12
  %65 = ashr i64 %64, 1
  store i64 %65, ptr %13
  %66 = load i64, ptr %13
  store i64 %66, ptr %14
  %67 = load i64, ptr %14
  %68 = lshr i64 %67, 63
  store i64 %68, ptr %15
  %69 = load i64, ptr %14
  %70 = load i64, ptr %15
  %71 = add i64 %69, %70
  store i64 %71, ptr %16
  %72 = load i64, ptr %16
  %73 = ashr i64 %72, 1
  store i64 %73, ptr %17
  %74 = load i64, ptr %17
  %75 = shl i64 %74, 1
  %76 = add i64 1, %75
  store i64 %76, ptr %19
  %77 = load i64, ptr %19
  store i64 %77, ptr %20
  %78 = load i64, ptr %20
  %79 = add i64 %78, 2
  store i64 %79, ptr %21
  %80 = load i64, ptr %21
  store i64 %80, ptr %5
  %81 = load i64, ptr %9
  store i64 %81, ptr %6
  %82 = load i64, ptr %5
  %83 = load i64, ptr %6
  %84 = load i64, ptr %ds
  %85 = load i64, ptr %alloc
  %86 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlString_tree_first_byte_diff__build_5_13_code"(i64 %84, i64 %85, i64 %82, i64 %83) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 23, i64 0, i64 44, i64 62, i64 0, i64 62, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 33, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7692846, i64 6581353) ]
  %87 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %86, 0, 0
  %88 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %86, 0, 1
  store i64 %87, ptr %ds
  store i64 %88, ptr %alloc
  %89 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %86, 1, 0
  store ptr addrspace(1) %89, ptr %7
  br label %L191
L191:
  %90 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %90, ptr %22
  %91 = load ptr addrspace(1), ptr %22
  store ptr addrspace(1) %91, ptr %23
  %92 = load i64, ptr %20
  store i64 %92, ptr %5
  %93 = load i64, ptr %5
  %94 = load i64, ptr %ds
  %95 = load i64, ptr %alloc
  %96 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlString_tree_first_byte_diff__key_4_12_code"(i64 %94, i64 %95, i64 %93) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 23, i64 0, i64 30, i64 37, i64 0, i64 37, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 33, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7692846, i64 6581353) ]
  %97 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %96, 0, 0
  %98 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %96, 0, 1
  store i64 %97, ptr %ds
  store i64 %98, ptr %alloc
  %99 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %96, 1, 0
  store ptr addrspace(1) %99, ptr %7
  br label %L193
L193:
  %100 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %100, ptr %24
  %101 = load ptr addrspace(1), ptr %24
  store ptr addrspace(1) %101, ptr %25
  %102 = load i64, ptr %20
  %103 = add i64 %102, -2
  store i64 %103, ptr %26
  %104 = load i64, ptr %8
  store i64 %104, ptr %5
  %105 = load i64, ptr %26
  store i64 %105, ptr %6
  %106 = load i64, ptr %5
  %107 = load i64, ptr %6
  %108 = load i64, ptr %ds
  %109 = load i64, ptr %alloc
  %110 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlString_tree_first_byte_diff__build_5_13_code"(i64 %108, i64 %109, i64 %106, i64 %107) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 23, i64 0, i64 10, i64 28, i64 0, i64 28, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 33, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7692846, i64 6581353) ]
  %111 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %110, 0, 0
  %112 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %110, 0, 1
  store i64 %111, ptr %ds
  store i64 %112, ptr %alloc
  %113 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %110, 1, 0
  store ptr addrspace(1) %113, ptr %7
  br label %L194
L194:
  %114 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %114, ptr %27
  %115 = load ptr addrspace(1), ptr %27
  store ptr addrspace(1) %115, ptr %28
  %116 = load i64, ptr %alloc
  %117 = sub i64 %116, 40
  store i64 %117, ptr %alloc
  %118 = load i64, ptr %ds
  %119 = inttoptr i64 %118 to ptr
  %120 = load i64, ptr %119
  %121 = icmp ule i64 %120, %117
  %122 = call  i1 @llvm.expect.i1(i1 %121, i1 1) 
  br i1 %122, label %L202, label %L201
L201:
  %123 = load i64, ptr %ds
  %124 = load i64, ptr %alloc
  %125 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %123, i64 %124) "statepoint-id"="327681" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 5, i64 1, i64 23, i64 0, i64 4, i64 63, i64 0, i64 63, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 33, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7692846, i64 6581353) ]
  %126 = extractvalue { { i64, i64 }, {  } } %125, 0, 0
  %127 = extractvalue { { i64, i64 }, {  } } %125, 0, 1
  store i64 %126, ptr %ds
  store i64 %127, ptr %alloc
  br label %L202
L202:
  %128 = load i64, ptr %alloc
  %129 = add i64 %128, 8
  %130 = inttoptr i64 %129 to ptr addrspace(1)
  store ptr addrspace(1) %130, ptr %29
  %131 = load ptr addrspace(1), ptr %29
  %132 = getelementptr i8, ptr addrspace(1) %131, i64 -8
  store volatile i64 4096, ptr addrspace(1) %132
  %133 = load ptr addrspace(1), ptr %29
  %134 = load ptr addrspace(1), ptr %28
  store ptr addrspace(1) %134, ptr addrspace(1) %133
  %135 = load ptr addrspace(1), ptr %29
  %136 = getelementptr i8, ptr addrspace(1) %135, i64 8
  %137 = load ptr addrspace(1), ptr %25
  store ptr addrspace(1) %137, ptr addrspace(1) %136
  %138 = load ptr addrspace(1), ptr %29
  %139 = getelementptr i8, ptr addrspace(1) %138, i64 16
  %140 = load i64, ptr %20
  store volatile i64 %140, ptr addrspace(1) %139
  %141 = load ptr addrspace(1), ptr %29
  %142 = getelementptr i8, ptr addrspace(1) %141, i64 24
  %143 = load ptr addrspace(1), ptr %23
  store ptr addrspace(1) %143, ptr addrspace(1) %142
  %144 = load ptr addrspace(1), ptr %29
  store ptr addrspace(1) %144, ptr %7
  %145 = load ptr addrspace(1), ptr %7
  %146 = load i64, ptr %ds
  %147 = load i64, ptr %alloc
  %148 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %146, 0, 0
  %149 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %148, i64 %147, 0, 1
  %150 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %149, ptr addrspace(1) %145, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %150
}

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlString_tree_first_byte_diff__find_6_14_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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
  %12 = alloca i64 
  %13 = alloca ptr addrspace(1) 
  %14 = alloca ptr addrspace(1) 
  %15 = alloca ptr addrspace(1) 
  %16 = alloca ptr addrspace(1) 
  %17 = alloca ptr addrspace(1) 
  %18 = alloca ptr addrspace(1) 
  %19 = alloca ptr addrspace(1) 
  %20 = alloca ptr addrspace(1) 
  %21 = alloca ptr addrspace(1) 
  %22 = alloca ptr addrspace(1) 
  %23 = alloca ptr addrspace(1) 
  br label %L1
L1:
  br label %L204
L204:
  %24 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %24, ptr %8
  %25 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %25, ptr %9
  %26 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %26, ptr %11
  %27 = load ptr addrspace(1), ptr %11
  store ptr addrspace(1) %27, ptr %10
  %28 = load ptr addrspace(1), ptr %10
  %29 = ptrtoint ptr addrspace(1) %28 to i64
  %30 = trunc i64 %29 to i1
  br i1 %30, label %L211, label %L213
L211:
  store i64 -1, ptr %7
  %31 = load i64, ptr %7
  %32 = load i64, ptr %ds
  %33 = load i64, ptr %alloc
  %34 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %32, 0, 0
  %35 = insertvalue { { i64, i64 }, { i64 } } %34, i64 %33, 0, 1
  %36 = insertvalue { { i64, i64 }, { i64 } } %35, i64 %31, 1, 0
  ret { { i64, i64 }, { i64 } } %36
L213:
  %37 = load ptr addrspace(1), ptr %10
  %38 = getelementptr i8, ptr addrspace(1) %37, i64 8
  store ptr addrspace(1) %38, ptr %13
  %39 = load ptr addrspace(1), ptr %13
  %40 = load ptr addrspace(1), ptr addrspace(1) %39
  store ptr addrspace(1) %40, ptr %14
  %41 = load ptr addrspace(1), ptr %8
  store ptr addrspace(1) %41, ptr %5
  %42 = load ptr addrspace(1), ptr %14
  store ptr addrspace(1) %42, ptr %6
  %43 = ptrtoint ptr @"\01_caml_string_compare" to i64
  %44 = load ptr addrspace(1), ptr %5
  %45 = load ptr addrspace(1), ptr %6
  %46 = ptrtoint ptr addrspace(1) %44 to i64
  %47 = ptrtoint ptr addrspace(1) %45 to i64
  %48 = icmp eq i64 %46, %47
  br i1 %48, label %L236, label %L237
L236:
  %49 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %49, ptr %5
  br label %L235
L237:
  %50 = getelementptr i8, ptr addrspace(1) %44, i64 -8
  %51 = load atomic i64, ptr addrspace(1) %50 monotonic, align 8
  %52 = and i64 %51, 72057594037926912
  %53 = lshr i64 %52, 10
  %54 = shl i64 %53, 3
  %55 = sub i64 %54, 1
  %56 = getelementptr i8, ptr addrspace(1) %44, i64 %55
  %57 = load i8, ptr addrspace(1) %56, align 1
  %58 = zext i8 %57 to i64
  %59 = sub i64 %55, %58
  %60 = getelementptr i8, ptr addrspace(1) %45, i64 -8
  %61 = load atomic i64, ptr addrspace(1) %60 monotonic, align 8
  %62 = and i64 %61, 72057594037926912
  %63 = lshr i64 %62, 10
  %64 = shl i64 %63, 3
  %65 = sub i64 %64, 1
  %66 = getelementptr i8, ptr addrspace(1) %45, i64 %65
  %67 = load i8, ptr addrspace(1) %66, align 1
  %68 = zext i8 %67 to i64
  %69 = sub i64 %65, %68
  %70 = icmp ult i64 %59, %69
  %71 = select i1 %70, i64 %59, i64 %69
  %72 = icmp ugt i64 %71, 15
  br i1 %72, label %L238, label %L239
L238:
  %73 = load ptr addrspace(1), ptr %5
  %74 = load ptr addrspace(1), ptr %6
  %75 = load i64, ptr %ds
  %76 = load i64, ptr %alloc
  %77 = call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_string_compare"(i64 %75, i64 %76, ptr addrspace(1) %73, ptr addrspace(1) %74) "gc-leaf-function"="true"
  %78 = extractvalue { i64, i64, ptr addrspace(1) } %77, 0
  %79 = extractvalue { i64, i64, ptr addrspace(1) } %77, 1
  store i64 %78, ptr %ds
  store i64 %79, ptr %alloc
  %80 = extractvalue { i64, i64, ptr addrspace(1) } %77, 2
  store ptr addrspace(1) %80, ptr %5
  br label %L235
L239:
  %81 = icmp eq i64 %71, 0
  br i1 %81, label %L240, label %L241
L241:
  %82 = icmp ugt i64 %71, 8
  %83 = select i1 %82, i64 8, i64 %71
  %84 = sub i64 8, %83
  %85 = shl i64 %84, 3
  %86 = shl i64 -1, %85
  %87 = load i64, ptr addrspace(1) %44, align 8
  %88 = call  i64 @llvm.bswap.i64(i64 %87) 
  %89 = load i64, ptr addrspace(1) %45, align 8
  %90 = call  i64 @llvm.bswap.i64(i64 %89) 
  %91 = and i64 %88, %86
  %92 = and i64 %90, %86
  %93 = icmp ne i64 %91, %92
  %94 = icmp ult i64 %91, %92
  br i1 %93, label %L242, label %L243
L242:
  %95 = select i1 %94, i64 -1, i64 3
  %96 = inttoptr i64 %95 to ptr addrspace(1)
  store ptr addrspace(1) %96, ptr %5
  br label %L235
L243:
  br i1 %82, label %L244, label %L240
L244:
  %97 = sub i64 %71, 8
  %98 = sub i64 8, %97
  %99 = shl i64 %98, 3
  %100 = shl i64 -1, %99
  %101 = getelementptr i8, ptr addrspace(1) %44, i64 8
  %102 = load i64, ptr addrspace(1) %101, align 8
  %103 = call  i64 @llvm.bswap.i64(i64 %102) 
  %104 = getelementptr i8, ptr addrspace(1) %45, i64 8
  %105 = load i64, ptr addrspace(1) %104, align 8
  %106 = call  i64 @llvm.bswap.i64(i64 %105) 
  %107 = and i64 %103, %100
  %108 = and i64 %106, %100
  %109 = icmp ne i64 %107, %108
  %110 = icmp ult i64 %107, %108
  br i1 %109, label %L245, label %L240
L245:
  %111 = select i1 %110, i64 -1, i64 3
  %112 = inttoptr i64 %111 to ptr addrspace(1)
  store ptr addrspace(1) %112, ptr %5
  br label %L235
L240:
  %113 = icmp ult i64 %59, %69
  %114 = icmp ugt i64 %59, %69
  %115 = select i1 %114, i64 3, i64 1
  %116 = select i1 %113, i64 -1, i64 %115
  %117 = inttoptr i64 %116 to ptr addrspace(1)
  store ptr addrspace(1) %117, ptr %5
  br label %L235
L235:
  br label %L215
L215:
  %118 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %118, ptr %15
  %119 = load ptr addrspace(1), ptr %15
  store ptr addrspace(1) %119, ptr %16
  %120 = load ptr addrspace(1), ptr %16
  %121 = inttoptr i64 1 to ptr addrspace(1)
  %122 = icmp slt ptr addrspace(1) %120, %121
  br i1 %122, label %L222, label %L246
L246:
  %123 = load ptr addrspace(1), ptr %16
  %124 = inttoptr i64 1 to ptr addrspace(1)
  %125 = icmp sgt ptr addrspace(1) %123, %124
  br i1 %125, label %L222, label %L218
L218:
  %126 = load ptr addrspace(1), ptr %10
  %127 = getelementptr i8, ptr addrspace(1) %126, i64 16
  store ptr addrspace(1) %127, ptr %17
  %128 = load ptr addrspace(1), ptr %17
  %129 = load ptr addrspace(1), ptr addrspace(1) %128
  store ptr addrspace(1) %129, ptr %18
  %130 = load ptr addrspace(1), ptr %18
  store ptr addrspace(1) %130, ptr %5
  %131 = load ptr addrspace(1), ptr %5
  %132 = ptrtoint ptr addrspace(1) %131 to i64
  %133 = load i64, ptr %ds
  %134 = load i64, ptr %alloc
  %135 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %133, 0, 0
  %136 = insertvalue { { i64, i64 }, { i64 } } %135, i64 %134, 0, 1
  %137 = insertvalue { { i64, i64 }, { i64 } } %136, i64 %132, 1, 0
  ret { { i64, i64 }, { i64 } } %137
L222:
  %138 = load ptr addrspace(1), ptr %16
  %139 = inttoptr i64 1 to ptr addrspace(1)
  %140 = icmp slt ptr addrspace(1) %138, %139
  br i1 %140, label %L224, label %L247
L247:
  %141 = load ptr addrspace(1), ptr %16
  %142 = inttoptr i64 1 to ptr addrspace(1)
  %143 = icmp sgt ptr addrspace(1) %141, %142
  br i1 %143, label %L227, label %L227
L224:
  %144 = load ptr addrspace(1), ptr %10
  %145 = load ptr addrspace(1), ptr addrspace(1) %144
  store ptr addrspace(1) %145, ptr %19
  %146 = load ptr addrspace(1), ptr %19
  store ptr addrspace(1) %146, ptr %20
  %147 = load ptr addrspace(1), ptr %20
  store ptr addrspace(1) %147, ptr %10
  %148 = load ptr addrspace(1), ptr %10
  %149 = ptrtoint ptr addrspace(1) %148 to i64
  %150 = trunc i64 %149 to i1
  br i1 %150, label %L211, label %L213
L227:
  %151 = load ptr addrspace(1), ptr %10
  %152 = getelementptr i8, ptr addrspace(1) %151, i64 24
  store ptr addrspace(1) %152, ptr %21
  %153 = load ptr addrspace(1), ptr %21
  %154 = load ptr addrspace(1), ptr addrspace(1) %153
  store ptr addrspace(1) %154, ptr %22
  %155 = load ptr addrspace(1), ptr %22
  store ptr addrspace(1) %155, ptr %23
  %156 = load ptr addrspace(1), ptr %23
  store ptr addrspace(1) %156, ptr %10
  %157 = load ptr addrspace(1), ptr %10
  %158 = ptrtoint ptr addrspace(1) %157 to i64
  %159 = trunc i64 %158 to i1
  br i1 %159, label %L211, label %L213
}

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlString_tree_first_byte_diff__run_7_15_code"(i64 %0, i64 %1, i64 %2, i64 %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" noinline gc "oxcaml" {
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
  %10 = alloca ptr addrspace(1) 
  %11 = alloca ptr addrspace(1) 
  %12 = alloca ptr addrspace(1) 
  %13 = alloca i64 
  %14 = alloca i64 
  %15 = alloca i64 
  %16 = alloca i64 
  %17 = alloca ptr addrspace(1) 
  %18 = alloca ptr addrspace(1) 
  %19 = alloca ptr addrspace(1) 
  %20 = alloca i64 
  %21 = alloca i64 
  %22 = alloca ptr addrspace(1) 
  %23 = alloca ptr addrspace(1) 
  %24 = alloca ptr addrspace(1) 
  %25 = alloca i64 
  %26 = alloca i64 
  %27 = alloca i64 
  %28 = alloca i64 
  %29 = alloca i64 
  %30 = alloca i64 
  %31 = alloca ptr addrspace(1) 
  %32 = alloca i64 
  %33 = alloca i64 
  %34 = alloca i64 
  %35 = alloca i64 
  %36 = alloca i64 
  %37 = alloca i64 
  %38 = alloca ptr addrspace(1) 
  %39 = alloca ptr addrspace(1) 
  %40 = alloca i64 
  %41 = alloca i64 
  %42 = alloca i64 
  %43 = alloca i64 
  %44 = alloca ptr addrspace(1) 
  %45 = alloca ptr addrspace(1) 
  %46 = alloca i64 
  %47 = alloca i64 
  %48 = alloca ptr addrspace(1) 
  %49 = alloca ptr addrspace(1) 
  %50 = alloca i64 
  %51 = alloca ptr addrspace(1) 
  %52 = alloca ptr addrspace(1) 
  %53 = alloca i64 
  %54 = alloca i64 
  %55 = alloca i64 
  %56 = alloca ptr addrspace(1) 
  %57 = alloca ptr addrspace(1) 
  %58 = alloca ptr addrspace(1) 
  %59 = alloca i64 
  %60 = alloca i64 
  %61 = alloca i64 
  %62 = alloca i64 
  %63 = alloca i64 
  %64 = alloca i64 
  %65 = alloca ptr addrspace(1) 
  %66 = alloca i64 
  %67 = alloca i64 
  %68 = alloca i64 
  %69 = alloca i64 
  %70 = alloca i64 
  %71 = alloca ptr addrspace(1) 
  %72 = alloca i64 
  %73 = alloca i64 
  %74 = alloca i64 
  %75 = alloca i64 
  %76 = alloca i64 
  %77 = alloca i64 
  %78 = alloca ptr addrspace(1) 
  %79 = alloca i64 
  %80 = alloca i64 
  %81 = alloca i64 
  %82 = alloca i64 
  %83 = alloca i64 
  %84 = alloca i64 
  %85 = alloca i64 
  %86 = alloca ptr addrspace(1) 
  %87 = alloca ptr addrspace(1) 
  %88 = alloca i64 
  %89 = alloca i64 
  %90 = alloca ptr addrspace(1) 
  %91 = alloca ptr addrspace(1) 
  %92 = alloca i64 
  %93 = alloca ptr addrspace(1) 
  %94 = alloca ptr addrspace(1) 
  %95 = alloca i64 
  %96 = alloca ptr addrspace(1) 
  %97 = alloca ptr addrspace(1) 
  %98 = alloca i64 
  %99 = alloca i64 
  %100 = alloca i64 
  %101 = alloca i64 
  %102 = alloca i64 
  %103 = alloca i64 
  %104 = alloca ptr addrspace(1) 
  %105 = alloca ptr addrspace(1) 
  %106 = alloca ptr addrspace(1) 
  %107 = alloca i64 
  %108 = alloca i64 
  %109 = alloca ptr addrspace(1) 
  %110 = alloca ptr addrspace(1) 
  %111 = alloca double 
  %112 = alloca i64 
  %113 = alloca i64 
  %114 = alloca i64 
  %115 = alloca ptr addrspace(1) 
  %116 = alloca ptr addrspace(1) 
  %117 = alloca i64 
  %118 = alloca i64 
  %119 = alloca i64 
  %120 = alloca i64 
  %121 = alloca i64 
  %122 = alloca ptr addrspace(1) 
  %123 = alloca i64 
  %124 = alloca i64 
  %125 = alloca i64 
  %126 = alloca i64 
  %127 = alloca i64 
  %128 = alloca i64 
  %129 = alloca i64 
  %130 = alloca i64 
  %131 = alloca i64 
  %132 = alloca i64 
  %133 = alloca i64 
  %134 = alloca i64 
  %135 = alloca i64 
  %136 = alloca i64 
  %137 = alloca i64 
  %138 = alloca i64 
  %139 = alloca i64 
  %140 = alloca i64 
  %141 = alloca i64 
  %142 = alloca i64 
  %143 = alloca i64 
  %144 = alloca i64 
  %145 = alloca ptr addrspace(1) 
  %146 = alloca ptr addrspace(1) 
  %147 = alloca ptr addrspace(1) 
  %148 = alloca i64 
  %149 = alloca i64 
  %150 = alloca i64 
  %151 = alloca i64 
  %152 = alloca i64 
  %153 = alloca i64 
  %154 = alloca i64 
  %155 = alloca i64 
  %156 = alloca i64 
  %157 = alloca i64 
  %158 = alloca i64 
  %159 = alloca i64 
  %160 = alloca i64 
  %161 = alloca i64 
  %162 = alloca i64 
  %163 = alloca i64 
  br label %L1
L1:
  br label %L249
L249:
  %164 = load i64, ptr %ds
  %165 = add i64 %164, 40
  %166 = inttoptr i64 %165 to ptr
  %167 = load i64, ptr %166
  %168 = add i64 %167, 376
  %169 = call i64 asm sideeffect "mov $0, sp", "=r"() "gc-leaf-function"="true"
  %170 = icmp uge i64 %169, %168
  %171 = call  i1 @llvm.expect.i1(i1 %170, i1 1) 
  br i1 %171, label %L390, label %L389
L389:
  %172 = load i64, ptr %ds
  %173 = load i64, ptr %alloc
  %174 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_llvm_call_realloc_stack"(i64 %172, i64 %173, i64 34) "statepoint-id"="0" cold
  %175 = extractvalue { { i64, i64 }, {  } } %174, 0, 0
  %176 = extractvalue { { i64, i64 }, {  } } %174, 0, 1
  store i64 %175, ptr %ds
  store i64 %176, ptr %alloc
  br label %L390
L390:
  %177 = load i64, ptr %5
  store i64 %177, ptr %13
  %178 = load i64, ptr %6
  store i64 %178, ptr %14
  store i64 1, ptr %5
  store i64 127, ptr %6
  %179 = load i64, ptr %5
  %180 = load i64, ptr %6
  %181 = load i64, ptr %ds
  %182 = load i64, ptr %alloc
  %183 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlString_tree_first_byte_diff__build_5_13_code"(i64 %181, i64 %182, i64 %179, i64 %180) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 32, i64 0, i64 13, i64 23, i64 0, i64 23, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7696942, i64 110) ]
  %184 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %183, 0, 0
  %185 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %183, 0, 1
  store i64 %184, ptr %ds
  store i64 %185, ptr %alloc
  %186 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %183, 1, 0
  store ptr addrspace(1) %186, ptr %7
  br label %L251
L251:
  %187 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %187, ptr %17
  %188 = load ptr addrspace(1), ptr %17
  store ptr addrspace(1) %188, ptr %18
  %189 = ptrtoint ptr @"\01_camlStdlib__immstring191" to i64
  store i64 %189, ptr %21
  %190 = load i64, ptr %21
  %191 = inttoptr i64 %190 to ptr addrspace(1)
  store ptr addrspace(1) %191, ptr %7
  %192 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %192, ptr %9
  %193 = ptrtoint ptr @"\01_caml_format_int" to i64
  %194 = load ptr addrspace(1), ptr %7
  %195 = load ptr addrspace(1), ptr %9
  %196 = load i64, ptr %ds
  %197 = load i64, ptr %alloc
  %198 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %196, i64 %197, i64 %193, ptr addrspace(1) %194, ptr addrspace(1) %195) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 280, i64 0, i64 2, i64 19, i64 0, i64 19, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7631662, i64 7235954, i64 7298919, i64 6905702, i64 29806, i64 17, i64 0, i64 67, i64 82, i64 0, i64 82, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 6646574, i64 121, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 33, i64 0, i64 13, i64 30, i64 0, i64 30, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7696942, i64 110) ]
  %199 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %198, 0, 0
  %200 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %198, 0, 1
  store i64 %199, ptr %ds
  store i64 %200, ptr %alloc
  %201 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %198, 1, 0
  store ptr addrspace(1) %201, ptr %7
  br label %L255
L255:
  %202 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %202, ptr %22
  %203 = load ptr addrspace(1), ptr %22
  store ptr addrspace(1) %203, ptr %23
  %204 = load ptr addrspace(1), ptr %23
  %205 = getelementptr i8, ptr addrspace(1) %204, i64 -8
  store ptr addrspace(1) %205, ptr %24
  %206 = load ptr addrspace(1), ptr %24
  %207 = load i64, ptr addrspace(1) %206
  store i64 %207, ptr %25
  %208 = load i64, ptr %25
  %209 = shl i64 %208, 8
  store i64 %209, ptr %26
  %210 = load i64, ptr %26
  %211 = lshr i64 %210, 18
  store i64 %211, ptr %27
  %212 = load i64, ptr %27
  %213 = shl i64 %212, 3
  store i64 %213, ptr %28
  %214 = load i64, ptr %28
  %215 = sub i64 %214, 1
  store i64 %215, ptr %29
  %216 = load i64, ptr %29
  store i64 %216, ptr %30
  %217 = load ptr addrspace(1), ptr %23
  %218 = load i64, ptr %30
  %219 = getelementptr i8, ptr addrspace(1) %217, i64 %218
  store ptr addrspace(1) %219, ptr %31
  %220 = load ptr addrspace(1), ptr %31
  %221 = load i8, ptr addrspace(1) %220
  %222 = zext i8 %221 to i64
  store i64 %222, ptr %32
  %223 = load i64, ptr %30
  %224 = load i64, ptr %32
  %225 = sub i64 %223, %224
  store i64 %225, ptr %33
  %226 = load i64, ptr %33
  %227 = shl i64 %226, 1
  %228 = add i64 1, %227
  store i64 %228, ptr %35
  %229 = load i64, ptr %35
  store i64 %229, ptr %36
  %230 = load i64, ptr %36
  %231 = add i64 %230, 28
  store i64 %231, ptr %37
  %232 = load i64, ptr %37
  %233 = inttoptr i64 %232 to ptr addrspace(1)
  store ptr addrspace(1) %233, ptr %7
  %234 = ptrtoint ptr @"\01_caml_create_bytes" to i64
  %235 = load ptr addrspace(1), ptr %7
  %236 = load i64, ptr %ds
  %237 = load i64, ptr %alloc
  %238 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %236, i64 %237, i64 %234, ptr addrspace(1) %235) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 226, i64 0, i64 10, i64 32, i64 0, i64 32, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 10, i64 6583379, i64 6449516, i64 6170670, i64 41, i64 17, i64 0, i64 48, i64 82, i64 0, i64 82, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 6646574, i64 121, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 33, i64 0, i64 13, i64 30, i64 0, i64 30, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7696942, i64 110) ]
  %239 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %238, 0, 0
  %240 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %238, 0, 1
  store i64 %239, ptr %ds
  store i64 %240, ptr %alloc
  %241 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %238, 1, 0
  store ptr addrspace(1) %241, ptr %7
  br label %L266
L266:
  %242 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %242, ptr %38
  %243 = load ptr addrspace(1), ptr %38
  store ptr addrspace(1) %243, ptr %39
  %244 = ptrtoint ptr @"\01_camlString_tree_first_byte_diff__immstring83" to i64
  store i64 %244, ptr %43
  %245 = load i64, ptr %43
  %246 = inttoptr i64 %245 to ptr addrspace(1)
  store ptr addrspace(1) %246, ptr %7
  %247 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %247, ptr %9
  %248 = load ptr addrspace(1), ptr %39
  store ptr addrspace(1) %248, ptr %10
  %249 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %249, ptr %11
  %250 = inttoptr i64 29 to ptr addrspace(1)
  store ptr addrspace(1) %250, ptr %12
  %251 = ptrtoint ptr @"\01_caml_blit_string" to i64
  %252 = load ptr addrspace(1), ptr %7
  %253 = load ptr addrspace(1), ptr %9
  %254 = load ptr addrspace(1), ptr %10
  %255 = load ptr addrspace(1), ptr %11
  %256 = load ptr addrspace(1), ptr %12
  %257 = load i64, ptr %ds
  %258 = load i64, ptr %alloc
  %259 = call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %257, i64 %258, ptr addrspace(1) %252, ptr addrspace(1) %253, ptr addrspace(1) %254, ptr addrspace(1) %255, ptr addrspace(1) %256) "gc-leaf-function"="true"
  %260 = extractvalue { i64, i64, ptr addrspace(1) } %259, 0
  %261 = extractvalue { i64, i64, ptr addrspace(1) } %259, 1
  store i64 %260, ptr %ds
  store i64 %261, ptr %alloc
  %262 = extractvalue { i64, i64, ptr addrspace(1) } %259, 2
  store ptr addrspace(1) %262, ptr %7
  br label %L268
L268:
  %263 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %263, ptr %44
  %264 = load ptr addrspace(1), ptr %44
  store ptr addrspace(1) %264, ptr %45
  %265 = load ptr addrspace(1), ptr %23
  store ptr addrspace(1) %265, ptr %7
  %266 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %266, ptr %9
  %267 = load ptr addrspace(1), ptr %39
  store ptr addrspace(1) %267, ptr %10
  %268 = inttoptr i64 29 to ptr addrspace(1)
  store ptr addrspace(1) %268, ptr %11
  %269 = load i64, ptr %36
  %270 = inttoptr i64 %269 to ptr addrspace(1)
  store ptr addrspace(1) %270, ptr %12
  %271 = ptrtoint ptr @"\01_caml_blit_string" to i64
  %272 = load ptr addrspace(1), ptr %7
  %273 = load ptr addrspace(1), ptr %9
  %274 = load ptr addrspace(1), ptr %10
  %275 = load ptr addrspace(1), ptr %11
  %276 = load ptr addrspace(1), ptr %12
  %277 = load i64, ptr %ds
  %278 = load i64, ptr %alloc
  %279 = call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %277, i64 %278, ptr addrspace(1) %272, ptr addrspace(1) %273, ptr addrspace(1) %274, ptr addrspace(1) %275, ptr addrspace(1) %276) "gc-leaf-function"="true"
  %280 = extractvalue { i64, i64, ptr addrspace(1) } %279, 0
  %281 = extractvalue { i64, i64, ptr addrspace(1) } %279, 1
  store i64 %280, ptr %ds
  store i64 %281, ptr %alloc
  %282 = extractvalue { i64, i64, ptr addrspace(1) } %279, 2
  store ptr addrspace(1) %282, ptr %7
  br label %L269
L269:
  %283 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %283, ptr %48
  %284 = load ptr addrspace(1), ptr %48
  store ptr addrspace(1) %284, ptr %49
  %285 = inttoptr i64 3 to ptr addrspace(1)
  store ptr addrspace(1) %285, ptr %7
  %286 = ptrtoint ptr @"\01_caml_create_bytes" to i64
  %287 = load ptr addrspace(1), ptr %7
  %288 = load i64, ptr %ds
  %289 = load i64, ptr %alloc
  %290 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %288, i64 %289, i64 %286, ptr addrspace(1) %287) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 5, i64 60, i64 0, i64 10, i64 18, i64 0, i64 18, i64 8, i64 7633250, i64 3044197, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4349791, i64 6648953, i64 7155315, i64 6646625, i64 44, i64 0, i64 2, i64 12, i64 0, i64 12, i64 9, i64 7500915, i64 6778473, i64 7105838, i64 19, i64 6583379, i64 6449516, i64 5463903, i64 6910580, i64 3041134, i64 7037293, i64 101, i64 17, i64 0, i64 2, i64 45, i64 0, i64 45, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 6646574, i64 121, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 33, i64 0, i64 13, i64 30, i64 0, i64 30, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7696942, i64 110) ]
  %291 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %290, 0, 0
  %292 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %290, 0, 1
  store i64 %291, ptr %ds
  store i64 %292, ptr %alloc
  %293 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %290, 1, 0
  store ptr addrspace(1) %293, ptr %7
  br label %L270
L270:
  %294 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %294, ptr %51
  %295 = load ptr addrspace(1), ptr %51
  store ptr addrspace(1) %295, ptr %52
  %296 = load ptr addrspace(1), ptr %52
  store ptr addrspace(1) %296, ptr %7
  %297 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %297, ptr %9
  %298 = inttoptr i64 3 to ptr addrspace(1)
  store ptr addrspace(1) %298, ptr %10
  %299 = inttoptr i64 131 to ptr addrspace(1)
  store ptr addrspace(1) %299, ptr %11
  %300 = ptrtoint ptr @"\01_caml_fill_bytes" to i64
  %301 = load ptr addrspace(1), ptr %7
  %302 = load ptr addrspace(1), ptr %9
  %303 = load ptr addrspace(1), ptr %10
  %304 = load ptr addrspace(1), ptr %11
  %305 = load i64, ptr %ds
  %306 = load i64, ptr %alloc
  %307 = call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_fill_bytes"(i64 %305, i64 %306, ptr addrspace(1) %301, ptr addrspace(1) %302, ptr addrspace(1) %303, ptr addrspace(1) %304) "gc-leaf-function"="true"
  %308 = extractvalue { i64, i64, ptr addrspace(1) } %307, 0
  %309 = extractvalue { i64, i64, ptr addrspace(1) } %307, 1
  store i64 %308, ptr %ds
  store i64 %309, ptr %alloc
  %310 = extractvalue { i64, i64, ptr addrspace(1) } %307, 2
  store ptr addrspace(1) %310, ptr %7
  br label %L271
L271:
  %311 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %311, ptr %56
  %312 = load ptr addrspace(1), ptr %56
  store ptr addrspace(1) %312, ptr %57
  %313 = load ptr addrspace(1), ptr %52
  %314 = getelementptr i8, ptr addrspace(1) %313, i64 -8
  store ptr addrspace(1) %314, ptr %58
  %315 = load ptr addrspace(1), ptr %58
  %316 = load i64, ptr addrspace(1) %315
  store i64 %316, ptr %59
  %317 = load i64, ptr %59
  %318 = shl i64 %317, 8
  store i64 %318, ptr %60
  %319 = load i64, ptr %60
  %320 = lshr i64 %319, 18
  store i64 %320, ptr %61
  %321 = load i64, ptr %61
  %322 = shl i64 %321, 3
  store i64 %322, ptr %62
  %323 = load i64, ptr %62
  %324 = sub i64 %323, 1
  store i64 %324, ptr %63
  %325 = load i64, ptr %63
  store i64 %325, ptr %64
  %326 = load ptr addrspace(1), ptr %52
  %327 = load i64, ptr %64
  %328 = getelementptr i8, ptr addrspace(1) %326, i64 %327
  store ptr addrspace(1) %328, ptr %65
  %329 = load ptr addrspace(1), ptr %65
  %330 = load i8, ptr addrspace(1) %329
  %331 = zext i8 %330 to i64
  store i64 %331, ptr %66
  %332 = load i64, ptr %64
  %333 = load i64, ptr %66
  %334 = sub i64 %332, %333
  store i64 %334, ptr %67
  %335 = load i64, ptr %67
  %336 = shl i64 %335, 1
  %337 = add i64 1, %336
  store i64 %337, ptr %69
  %338 = load i64, ptr %69
  store i64 %338, ptr %70
  %339 = load ptr addrspace(1), ptr %39
  %340 = getelementptr i8, ptr addrspace(1) %339, i64 -8
  store ptr addrspace(1) %340, ptr %71
  %341 = load ptr addrspace(1), ptr %71
  %342 = load i64, ptr addrspace(1) %341
  store i64 %342, ptr %72
  %343 = load i64, ptr %72
  %344 = shl i64 %343, 8
  store i64 %344, ptr %73
  %345 = load i64, ptr %73
  %346 = lshr i64 %345, 18
  store i64 %346, ptr %74
  %347 = load i64, ptr %74
  %348 = shl i64 %347, 3
  store i64 %348, ptr %75
  %349 = load i64, ptr %75
  %350 = sub i64 %349, 1
  store i64 %350, ptr %76
  %351 = load i64, ptr %76
  store i64 %351, ptr %77
  %352 = load ptr addrspace(1), ptr %39
  %353 = load i64, ptr %77
  %354 = getelementptr i8, ptr addrspace(1) %352, i64 %353
  store ptr addrspace(1) %354, ptr %78
  %355 = load ptr addrspace(1), ptr %78
  %356 = load i8, ptr addrspace(1) %355
  %357 = zext i8 %356 to i64
  store i64 %357, ptr %79
  %358 = load i64, ptr %77
  %359 = load i64, ptr %79
  %360 = sub i64 %358, %359
  store i64 %360, ptr %80
  %361 = load i64, ptr %80
  %362 = shl i64 %361, 1
  %363 = add i64 1, %362
  store i64 %363, ptr %82
  %364 = load i64, ptr %82
  store i64 %364, ptr %83
  %365 = load i64, ptr %70
  %366 = load i64, ptr %83
  %367 = add i64 %365, %366
  store i64 %367, ptr %84
  %368 = load i64, ptr %84
  %369 = add i64 %368, -1
  store i64 %369, ptr %85
  %370 = load i64, ptr %85
  %371 = inttoptr i64 %370 to ptr addrspace(1)
  store ptr addrspace(1) %371, ptr %7
  %372 = ptrtoint ptr @"\01_caml_create_bytes" to i64
  %373 = load ptr addrspace(1), ptr %7
  %374 = load i64, ptr %ds
  %375 = load i64, ptr %alloc
  %376 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %374, i64 %375, i64 %372, ptr addrspace(1) %373) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 226, i64 0, i64 10, i64 32, i64 0, i64 32, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 10, i64 6583379, i64 6449516, i64 6170670, i64 41, i64 17, i64 0, i64 2, i64 82, i64 0, i64 82, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 6646574, i64 121, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 33, i64 0, i64 13, i64 30, i64 0, i64 30, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7696942, i64 110) ]
  %377 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %376, 0, 0
  %378 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %376, 0, 1
  store i64 %377, ptr %ds
  store i64 %378, ptr %alloc
  %379 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %376, 1, 0
  store ptr addrspace(1) %379, ptr %7
  br label %L292
L292:
  %380 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %380, ptr %86
  %381 = load ptr addrspace(1), ptr %86
  store ptr addrspace(1) %381, ptr %87
  %382 = load ptr addrspace(1), ptr %52
  store ptr addrspace(1) %382, ptr %7
  %383 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %383, ptr %9
  %384 = load ptr addrspace(1), ptr %87
  store ptr addrspace(1) %384, ptr %10
  %385 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %385, ptr %11
  %386 = load i64, ptr %70
  %387 = inttoptr i64 %386 to ptr addrspace(1)
  store ptr addrspace(1) %387, ptr %12
  %388 = ptrtoint ptr @"\01_caml_blit_string" to i64
  %389 = load ptr addrspace(1), ptr %7
  %390 = load ptr addrspace(1), ptr %9
  %391 = load ptr addrspace(1), ptr %10
  %392 = load ptr addrspace(1), ptr %11
  %393 = load ptr addrspace(1), ptr %12
  %394 = load i64, ptr %ds
  %395 = load i64, ptr %alloc
  %396 = call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %394, i64 %395, ptr addrspace(1) %389, ptr addrspace(1) %390, ptr addrspace(1) %391, ptr addrspace(1) %392, ptr addrspace(1) %393) "gc-leaf-function"="true"
  %397 = extractvalue { i64, i64, ptr addrspace(1) } %396, 0
  %398 = extractvalue { i64, i64, ptr addrspace(1) } %396, 1
  store i64 %397, ptr %ds
  store i64 %398, ptr %alloc
  %399 = extractvalue { i64, i64, ptr addrspace(1) } %396, 2
  store ptr addrspace(1) %399, ptr %7
  br label %L295
L295:
  %400 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %400, ptr %90
  %401 = load ptr addrspace(1), ptr %90
  store ptr addrspace(1) %401, ptr %91
  %402 = load ptr addrspace(1), ptr %39
  store ptr addrspace(1) %402, ptr %7
  %403 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %403, ptr %9
  %404 = load ptr addrspace(1), ptr %87
  store ptr addrspace(1) %404, ptr %10
  %405 = load i64, ptr %70
  %406 = inttoptr i64 %405 to ptr addrspace(1)
  store ptr addrspace(1) %406, ptr %11
  %407 = load i64, ptr %83
  %408 = inttoptr i64 %407 to ptr addrspace(1)
  store ptr addrspace(1) %408, ptr %12
  %409 = ptrtoint ptr @"\01_caml_blit_string" to i64
  %410 = load ptr addrspace(1), ptr %7
  %411 = load ptr addrspace(1), ptr %9
  %412 = load ptr addrspace(1), ptr %10
  %413 = load ptr addrspace(1), ptr %11
  %414 = load ptr addrspace(1), ptr %12
  %415 = load i64, ptr %ds
  %416 = load i64, ptr %alloc
  %417 = call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %415, i64 %416, ptr addrspace(1) %410, ptr addrspace(1) %411, ptr addrspace(1) %412, ptr addrspace(1) %413, ptr addrspace(1) %414) "gc-leaf-function"="true"
  %418 = extractvalue { i64, i64, ptr addrspace(1) } %417, 0
  %419 = extractvalue { i64, i64, ptr addrspace(1) } %417, 1
  store i64 %418, ptr %ds
  store i64 %419, ptr %alloc
  %420 = extractvalue { i64, i64, ptr addrspace(1) } %417, 2
  store ptr addrspace(1) %420, ptr %7
  br label %L296
L296:
  %421 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %421, ptr %93
  %422 = load ptr addrspace(1), ptr %93
  store ptr addrspace(1) %422, ptr %94
  %423 = inttoptr i64 129 to ptr addrspace(1)
  store ptr addrspace(1) %423, ptr %7
  %424 = load ptr addrspace(1), ptr %87
  store ptr addrspace(1) %424, ptr %9
  %425 = ptrtoint ptr @"\01_caml_array_make" to i64
  %426 = load ptr addrspace(1), ptr %7
  %427 = load ptr addrspace(1), ptr %9
  %428 = load i64, ptr %ds
  %429 = load i64, ptr %alloc
  %430 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %428, i64 %429, i64 %425, ptr addrspace(1) %426, ptr addrspace(1) %427) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 85, i64 0, i64 13, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 33, i64 0, i64 13, i64 30, i64 0, i64 30, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7696942, i64 110) ]
  %431 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %430, 0, 0
  %432 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %430, 0, 1
  store i64 %431, ptr %ds
  store i64 %432, ptr %alloc
  %433 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %430, 1, 0
  store ptr addrspace(1) %433, ptr %7
  br label %L297
L297:
  %434 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %434, ptr %96
  %435 = load ptr addrspace(1), ptr %96
  store ptr addrspace(1) %435, ptr %97
  store i64 1, ptr %100
  %436 = load i64, ptr %100
  store i64 %436, ptr %98
  br label %L301
L301:
  %437 = load i64, ptr %98
  %438 = shl i64 %437, 1
  %439 = add i64 1, %438
  store i64 %439, ptr %102
  %440 = load i64, ptr %102
  store i64 %440, ptr %103
  %441 = load i64, ptr %103
  store i64 %441, ptr %5
  %442 = load i64, ptr %5
  %443 = load i64, ptr %ds
  %444 = load i64, ptr %alloc
  %445 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlString_tree_first_byte_diff__key_4_12_code"(i64 %443, i64 %444, i64 %442) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 2, i64 87, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 33, i64 0, i64 13, i64 30, i64 0, i64 30, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7696942, i64 110) ]
  %446 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %445, 0, 0
  %447 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %445, 0, 1
  store i64 %446, ptr %ds
  store i64 %447, ptr %alloc
  %448 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %445, 1, 0
  store ptr addrspace(1) %448, ptr %7
  br label %L304
L304:
  %449 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %449, ptr %104
  %450 = load ptr addrspace(1), ptr %104
  store ptr addrspace(1) %450, ptr %105
  %451 = load ptr addrspace(1), ptr %97
  %452 = getelementptr i8, ptr addrspace(1) %451, i64 -8
  store ptr addrspace(1) %452, ptr %106
  %453 = load ptr addrspace(1), ptr %106
  %454 = load i8, ptr addrspace(1) %453
  %455 = zext i8 %454 to i64
  store i64 %455, ptr %107
  %456 = load i64, ptr %107
  %457 = icmp slt i64 %456, 254
  br i1 %457, label %L317, label %L391
L391:
  %458 = load i64, ptr %107
  %459 = icmp sgt i64 %458, 254
  br i1 %459, label %L317, label %L310
L310:
  %460 = load i64, ptr %103
  %461 = shl i64 %460, 2
  store i64 %461, ptr %108
  %462 = load ptr addrspace(1), ptr %97
  %463 = load i64, ptr %108
  %464 = getelementptr i8, ptr addrspace(1) %462, i64 %463
  store ptr addrspace(1) %464, ptr %109
  %465 = load ptr addrspace(1), ptr %109
  %466 = getelementptr i8, ptr addrspace(1) %465, i64 -4
  store ptr addrspace(1) %466, ptr %110
  %467 = load ptr addrspace(1), ptr %105
  %468 = load double, ptr addrspace(1) %467
  store double %468, ptr %111
  %469 = load ptr addrspace(1), ptr %110
  %470 = load double, ptr %111
  store double %470, ptr addrspace(1) %469
  store i64 1, ptr %113
  br label %L324
L317:
  %471 = load i64, ptr %103
  %472 = shl i64 %471, 2
  store i64 %472, ptr %114
  %473 = load ptr addrspace(1), ptr %97
  %474 = load i64, ptr %114
  %475 = getelementptr i8, ptr addrspace(1) %473, i64 %474
  store ptr addrspace(1) %475, ptr %115
  %476 = load ptr addrspace(1), ptr %115
  %477 = getelementptr i8, ptr addrspace(1) %476, i64 -4
  store ptr addrspace(1) %477, ptr %116
  %478 = load ptr addrspace(1), ptr %116
  store ptr addrspace(1) %478, ptr %8
  %479 = load ptr addrspace(1), ptr %105
  store ptr addrspace(1) %479, ptr %9
  %480 = ptrtoint ptr @"\01_caml_modify" to i64
  %481 = load ptr addrspace(1), ptr %8
  %482 = load ptr addrspace(1), ptr %9
  %483 = load i64, ptr %ds
  %484 = load i64, ptr %alloc
  %485 = call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %483, i64 %484, ptr addrspace(1) %481, ptr addrspace(1) %482) "gc-leaf-function"="true"
  %486 = extractvalue { i64, i64 } %485, 0
  %487 = extractvalue { i64, i64 } %485, 1
  store i64 %486, ptr %ds
  store i64 %487, ptr %alloc
  br label %L319
L319:
  store i64 1, ptr %118
  br label %L324
L324:
  %488 = load i64, ptr %98
  %489 = add i64 %488, 1
  store i64 %489, ptr %119
  %490 = load i64, ptr %119
  store i64 %490, ptr %120
  %491 = load i64, ptr %120
  %492 = icmp slt i64 %491, 63
  br i1 %492, label %L327, label %L392
L392:
  %493 = load i64, ptr %120
  %494 = icmp sgt i64 %493, 63
  br i1 %494, label %L329, label %L327
L327:
  %495 = load i64, ptr %120
  store i64 %495, ptr %121
  %496 = load i64, ptr %121
  store i64 %496, ptr %98
  br label %L301
L329:
  %497 = load ptr addrspace(1), ptr %97
  store ptr addrspace(1) %497, ptr %122
  %498 = load ptr addrspace(1), ptr %122
  store ptr addrspace(1) %498, ptr %19
  %499 = load i64, ptr %14
  %500 = icmp slt i64 %499, 3
  br i1 %500, label %L385, label %L393
L393:
  %501 = load i64, ptr %14
  %502 = icmp sgt i64 %501, 3
  br i1 %502, label %L336, label %L336
L336:
  %503 = load i64, ptr %14
  %504 = ashr i64 %503, 1
  store i64 %504, ptr %123
  %505 = load i64, ptr %123
  store i64 %505, ptr %124
  store i64 1, ptr %129
  store i64 1, ptr %130
  %506 = load i64, ptr %129
  store i64 %506, ptr %125
  %507 = load i64, ptr %130
  store i64 %507, ptr %126
  br label %L342
L342:
  %508 = load i64, ptr %125
  %509 = shl i64 %508, 1
  %510 = add i64 1, %509
  store i64 %510, ptr %132
  %511 = load i64, ptr %132
  store i64 %511, ptr %133
  %512 = load i64, ptr %13
  %513 = icmp slt i64 %512, 3
  br i1 %513, label %L372, label %L394
L394:
  %514 = load i64, ptr %13
  %515 = icmp sgt i64 %514, 3
  br i1 %515, label %L348, label %L348
L348:
  %516 = load i64, ptr %13
  %517 = ashr i64 %516, 1
  store i64 %517, ptr %135
  %518 = load i64, ptr %135
  store i64 %518, ptr %136
  store i64 1, ptr %140
  %519 = load i64, ptr %126
  store i64 %519, ptr %141
  %520 = load i64, ptr %140
  store i64 %520, ptr %137
  %521 = load i64, ptr %141
  store i64 %521, ptr %138
  br label %L354
L354:
  %522 = load i64, ptr %137
  %523 = shl i64 %522, 1
  %524 = load i64, ptr %133
  %525 = add i64 %524, %523
  store i64 %525, ptr %142
  %526 = load i64, ptr %142
  %527 = and i64 %526, 127
  store i64 %527, ptr %143
  %528 = load i64, ptr %143
  %529 = shl i64 %528, 2
  store i64 %529, ptr %144
  %530 = load ptr addrspace(1), ptr %19
  %531 = load i64, ptr %144
  %532 = getelementptr i8, ptr addrspace(1) %530, i64 %531
  store ptr addrspace(1) %532, ptr %145
  %533 = load ptr addrspace(1), ptr %145
  %534 = getelementptr i8, ptr addrspace(1) %533, i64 -4
  store ptr addrspace(1) %534, ptr %146
  %535 = load ptr addrspace(1), ptr %146
  %536 = load ptr addrspace(1), ptr addrspace(1) %535
  store ptr addrspace(1) %536, ptr %147
  %537 = load ptr addrspace(1), ptr %147
  store ptr addrspace(1) %537, ptr %7
  %538 = load ptr addrspace(1), ptr %18
  store ptr addrspace(1) %538, ptr %9
  %539 = load ptr addrspace(1), ptr %7
  %540 = load ptr addrspace(1), ptr %9
  %541 = load i64, ptr %ds
  %542 = load i64, ptr %alloc
  %543 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlString_tree_first_byte_diff__find_6_14_code"(i64 %541, i64 %542, ptr addrspace(1) %539, ptr addrspace(1) %540) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 37, i64 0, i64 20, i64 71, i64 0, i64 71, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7696942, i64 110) ]
  %544 = extractvalue { { i64, i64 }, { i64 } } %543, 0, 0
  %545 = extractvalue { { i64, i64 }, { i64 } } %543, 0, 1
  store i64 %544, ptr %ds
  store i64 %545, ptr %alloc
  %546 = extractvalue { { i64, i64 }, { i64 } } %543, 1, 0
  store i64 %546, ptr %5
  br label %L356
L356:
  %547 = load i64, ptr %5
  store i64 %547, ptr %148
  %548 = load i64, ptr %148
  store i64 %548, ptr %149
  %549 = load i64, ptr %138
  %550 = load i64, ptr %149
  %551 = add i64 %549, %550
  store i64 %551, ptr %150
  %552 = load i64, ptr %150
  %553 = add i64 %552, -1
  store i64 %553, ptr %151
  %554 = load i64, ptr %151
  store i64 %554, ptr %152
  %555 = load i64, ptr %137
  %556 = add i64 %555, 1
  store i64 %556, ptr %153
  %557 = load i64, ptr %153
  store i64 %557, ptr %154
  %558 = load i64, ptr %154
  %559 = load i64, ptr %136
  %560 = icmp slt i64 %558, %559
  br i1 %560, label %L366, label %L395
L395:
  %561 = load i64, ptr %154
  %562 = load i64, ptr %136
  %563 = icmp sgt i64 %561, %562
  br i1 %563, label %L368, label %L366
L366:
  %564 = load i64, ptr %154
  store i64 %564, ptr %155
  %565 = load i64, ptr %152
  store i64 %565, ptr %156
  %566 = load i64, ptr %155
  store i64 %566, ptr %137
  %567 = load i64, ptr %156
  store i64 %567, ptr %138
  br label %L354
L368:
  %568 = load i64, ptr %152
  store i64 %568, ptr %157
  %569 = load i64, ptr %157
  store i64 %569, ptr %134
  br label %L375
L372:
  %570 = load i64, ptr %126
  store i64 %570, ptr %158
  %571 = load i64, ptr %158
  store i64 %571, ptr %134
  br label %L375
L375:
  %572 = load i64, ptr %125
  %573 = add i64 %572, 1
  store i64 %573, ptr %159
  %574 = load i64, ptr %159
  store i64 %574, ptr %160
  %575 = load i64, ptr %160
  %576 = load i64, ptr %124
  %577 = icmp slt i64 %575, %576
  br i1 %577, label %L378, label %L396
L396:
  %578 = load i64, ptr %160
  %579 = load i64, ptr %124
  %580 = icmp sgt i64 %578, %579
  br i1 %580, label %L380, label %L378
L378:
  %581 = load i64, ptr %160
  store i64 %581, ptr %161
  %582 = load i64, ptr %134
  store i64 %582, ptr %162
  %583 = load i64, ptr %161
  store i64 %583, ptr %125
  %584 = load i64, ptr %162
  store i64 %584, ptr %126
  br label %L342
L380:
  %585 = load i64, ptr %134
  store i64 %585, ptr %5
  %586 = load i64, ptr %5
  %587 = load i64, ptr %ds
  %588 = load i64, ptr %alloc
  %589 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %587, 0, 0
  %590 = insertvalue { { i64, i64 }, { i64 } } %589, i64 %588, 0, 1
  %591 = insertvalue { { i64, i64 }, { i64 } } %590, i64 %586, 1, 0
  ret { { i64, i64 }, { i64 } } %591
L385:
  store i64 1, ptr %5
  %592 = load i64, ptr %5
  %593 = load i64, ptr %ds
  %594 = load i64, ptr %alloc
  %595 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %593, 0, 0
  %596 = insertvalue { { i64, i64 }, { i64 } } %595, i64 %594, 0, 1
  %597 = insertvalue { { i64, i64 }, { i64 } } %596, i64 %592, 1, 0
  ret { { i64, i64 }, { i64 } } %597
}

define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlString_tree_first_byte_diff__entry"(i64 %0, i64 %1) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" noinline gc "oxcaml" {
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
  %63 = alloca i64 
  %64 = alloca i64 
  %65 = alloca i64 
  %66 = alloca i64 
  %67 = alloca i64 
  %68 = alloca i64 
  %69 = alloca i64 
  %70 = alloca i64 
  %71 = alloca ptr addrspace(1) 
  %72 = alloca ptr addrspace(1) 
  %73 = alloca i64 
  %74 = alloca i64 
  %75 = alloca i64 
  %76 = alloca i64 
  %77 = alloca i64 
  %78 = alloca i64 
  %79 = alloca i64 
  br label %L1
L1:
  br label %L404
L404:
  %80 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %80, ptr %3
  %81 = ptrtoint ptr @"\01_caml_sys_argv" to i64
  %82 = load ptr addrspace(1), ptr %3
  %83 = load i64, ptr %ds
  %84 = load i64, ptr %alloc
  %85 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %83, i64 %84, i64 %81, ptr addrspace(1) %82) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 6, i64 0, i64 18, i64 26, i64 0, i64 26, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 29, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 28206) ]
  %86 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %85, 0, 0
  %87 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %85, 0, 1
  store i64 %86, ptr %ds
  store i64 %87, ptr %alloc
  %88 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %85, 1, 0
  store ptr addrspace(1) %88, ptr %3
  br label %L406
L406:
  %89 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %89, ptr %12
  %90 = load ptr addrspace(1), ptr %12
  store ptr addrspace(1) %90, ptr %13
  %91 = load ptr addrspace(1), ptr %13
  %92 = getelementptr i8, ptr addrspace(1) %91, i64 -8
  store ptr addrspace(1) %92, ptr %14
  %93 = load ptr addrspace(1), ptr %14
  %94 = load i64, ptr addrspace(1) %93
  store i64 %94, ptr %15
  %95 = load i64, ptr %15
  %96 = shl i64 %95, 8
  store i64 %96, ptr %16
  %97 = load i64, ptr %16
  %98 = lshr i64 %97, 17
  store i64 %98, ptr %17
  %99 = load i64, ptr %17
  %100 = icmp slt i64 %99, 3
  br i1 %100, label %L432, label %L487
L487:
  %101 = load i64, ptr %17
  %102 = icmp sgt i64 %101, 3
  br i1 %102, label %L411, label %L432
L411:
  %103 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %103, ptr %3
  %104 = ptrtoint ptr @"\01_caml_sys_argv" to i64
  %105 = load ptr addrspace(1), ptr %3
  %106 = load i64, ptr %ds
  %107 = load i64, ptr %alloc
  %108 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %106, i64 %107, i64 %104, ptr addrspace(1) %105) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 6, i64 0, i64 50, i64 58, i64 0, i64 58, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 29, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 28206) ]
  %109 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %108, 0, 0
  %110 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %108, 0, 1
  store i64 %109, ptr %ds
  store i64 %110, ptr %alloc
  %111 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %108, 1, 0
  store ptr addrspace(1) %111, ptr %3
  br label %L413
L413:
  %112 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %112, ptr %19
  %113 = load ptr addrspace(1), ptr %19
  store ptr addrspace(1) %113, ptr %20
  %114 = load ptr addrspace(1), ptr %20
  %115 = getelementptr i8, ptr addrspace(1) %114, i64 -8
  store ptr addrspace(1) %115, ptr %21
  %116 = load ptr addrspace(1), ptr %21
  %117 = load i64, ptr addrspace(1) %116
  store i64 %117, ptr %22
  %118 = load i64, ptr %22
  %119 = shl i64 %118, 8
  store i64 %119, ptr %23
  %120 = load i64, ptr %23
  %121 = lshr i64 %120, 17
  store i64 %121, ptr %24
  %122 = load i64, ptr %24
  %123 = icmp ult i64 %122, 3
  br i1 %123, label %L429, label %L488
L488:
  %124 = load i64, ptr %24
  %125 = icmp ugt i64 %124, 3
  br i1 %125, label %L421, label %L429
L421:
  %126 = load ptr addrspace(1), ptr %20
  %127 = getelementptr i8, ptr addrspace(1) %126, i64 8
  store ptr addrspace(1) %127, ptr %25
  %128 = load ptr addrspace(1), ptr %25
  %129 = load ptr addrspace(1), ptr addrspace(1) %128
  store ptr addrspace(1) %129, ptr %26
  %130 = load ptr addrspace(1), ptr %26
  store ptr addrspace(1) %130, ptr %3
  %131 = ptrtoint ptr @"\01_caml_int_of_string" to i64
  %132 = load ptr addrspace(1), ptr %3
  %133 = load i64, ptr %ds
  %134 = load i64, ptr %alloc
  %135 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %133, i64 %134, i64 %131, ptr addrspace(1) %132) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 6, i64 0, i64 36, i64 62, i64 0, i64 62, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 29, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 28206) ]
  %136 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %135, 0, 0
  %137 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %135, 0, 1
  store i64 %136, ptr %ds
  store i64 %137, ptr %alloc
  %138 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %135, 1, 0
  store ptr addrspace(1) %138, ptr %3
  br label %L423
L423:
  %139 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %139, ptr %27
  %140 = load ptr addrspace(1), ptr %27
  store ptr addrspace(1) %140, ptr %28
  %141 = load ptr addrspace(1), ptr %28
  store ptr addrspace(1) %141, ptr %29
  %142 = load ptr addrspace(1), ptr %29
  %143 = ptrtoint ptr addrspace(1) %142 to i64
  store i64 %143, ptr %10
  br label %L435
L429:
  %144 = ptrtoint ptr @"\01_camlString_tree_first_byte_diff__block35" to i64
  store i64 %144, ptr %30
  %145 = load i64, ptr %30
  %146 = inttoptr i64 %145 to ptr addrspace(1)
  store ptr addrspace(1) %146, ptr %3
  %147 = load ptr addrspace(1), ptr %3
  %148 = ptrtoint ptr addrspace(1) %147 to i64
  call  void @llvm.aarch64.oxcaml.raise.notrace(i64 %148) 
  unreachable
L432:
  store i64 200001, ptr %32
  %149 = load i64, ptr %32
  store i64 %149, ptr %10
  br label %L435
L435:
  %150 = ptrtoint ptr @"\01_camlString_tree_first_byte_diff" to i64
  store i64 %150, ptr %33
  %151 = load i64, ptr %33
  %152 = add i64 %151, 24
  store i64 %152, ptr %34
  %153 = load i64, ptr %34
  %154 = inttoptr i64 %153 to ptr addrspace(1)
  store ptr addrspace(1) %154, ptr %4
  %155 = load i64, ptr %10
  %156 = inttoptr i64 %155 to ptr addrspace(1)
  store ptr addrspace(1) %156, ptr %6
  %157 = ptrtoint ptr @"\01_caml_initialize" to i64
  %158 = load ptr addrspace(1), ptr %4
  %159 = load ptr addrspace(1), ptr %6
  %160 = load i64, ptr %ds
  %161 = load i64, ptr %alloc
  %162 = call oxcaml_c_directcc { i64, i64 } @"\01_caml_initialize"(i64 %160, i64 %161, ptr addrspace(1) %158, ptr addrspace(1) %159) "gc-leaf-function"="true"
  %163 = extractvalue { i64, i64 } %162, 0
  %164 = extractvalue { i64, i64 } %162, 1
  store i64 %163, ptr %ds
  store i64 %164, ptr %alloc
  br label %L437
L437:
  %165 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %165, ptr %3
  %166 = ptrtoint ptr @"\01_caml_sys_argv" to i64
  %167 = load ptr addrspace(1), ptr %3
  %168 = load i64, ptr %ds
  %169 = load i64, ptr %alloc
  %170 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %168, i64 %169, i64 %166, ptr addrspace(1) %167) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 9, i64 0, i64 18, i64 26, i64 0, i64 26, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 32, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 6648366, i64 29552) ]
  %171 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %170, 0, 0
  %172 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %170, 0, 1
  store i64 %171, ptr %ds
  store i64 %172, ptr %alloc
  %173 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %170, 1, 0
  store ptr addrspace(1) %173, ptr %3
  br label %L442
L442:
  %174 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %174, ptr %38
  %175 = load ptr addrspace(1), ptr %38
  store ptr addrspace(1) %175, ptr %39
  %176 = load ptr addrspace(1), ptr %39
  %177 = getelementptr i8, ptr addrspace(1) %176, i64 -8
  store ptr addrspace(1) %177, ptr %40
  %178 = load ptr addrspace(1), ptr %40
  %179 = load i64, ptr addrspace(1) %178
  store i64 %179, ptr %41
  %180 = load i64, ptr %41
  %181 = shl i64 %180, 8
  store i64 %181, ptr %42
  %182 = load i64, ptr %42
  %183 = lshr i64 %182, 17
  store i64 %183, ptr %43
  %184 = load i64, ptr %43
  %185 = icmp slt i64 %184, 5
  br i1 %185, label %L468, label %L489
L489:
  %186 = load i64, ptr %43
  %187 = icmp sgt i64 %186, 5
  br i1 %187, label %L447, label %L468
L447:
  %188 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %188, ptr %3
  %189 = ptrtoint ptr @"\01_caml_sys_argv" to i64
  %190 = load ptr addrspace(1), ptr %3
  %191 = load i64, ptr %ds
  %192 = load i64, ptr %alloc
  %193 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %191, i64 %192, i64 %189, ptr addrspace(1) %190) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 9, i64 0, i64 50, i64 58, i64 0, i64 58, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 32, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 6648366, i64 29552) ]
  %194 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %193, 0, 0
  %195 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %193, 0, 1
  store i64 %194, ptr %ds
  store i64 %195, ptr %alloc
  %196 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %193, 1, 0
  store ptr addrspace(1) %196, ptr %3
  br label %L449
L449:
  %197 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %197, ptr %45
  %198 = load ptr addrspace(1), ptr %45
  store ptr addrspace(1) %198, ptr %46
  %199 = load ptr addrspace(1), ptr %46
  %200 = getelementptr i8, ptr addrspace(1) %199, i64 -8
  store ptr addrspace(1) %200, ptr %47
  %201 = load ptr addrspace(1), ptr %47
  %202 = load i64, ptr addrspace(1) %201
  store i64 %202, ptr %48
  %203 = load i64, ptr %48
  %204 = shl i64 %203, 8
  store i64 %204, ptr %49
  %205 = load i64, ptr %49
  %206 = lshr i64 %205, 17
  store i64 %206, ptr %50
  %207 = load i64, ptr %50
  %208 = icmp ult i64 %207, 5
  br i1 %208, label %L465, label %L490
L490:
  %209 = load i64, ptr %50
  %210 = icmp ugt i64 %209, 5
  br i1 %210, label %L457, label %L465
L457:
  %211 = load ptr addrspace(1), ptr %46
  %212 = getelementptr i8, ptr addrspace(1) %211, i64 16
  store ptr addrspace(1) %212, ptr %51
  %213 = load ptr addrspace(1), ptr %51
  %214 = load ptr addrspace(1), ptr addrspace(1) %213
  store ptr addrspace(1) %214, ptr %52
  %215 = load ptr addrspace(1), ptr %52
  store ptr addrspace(1) %215, ptr %3
  %216 = ptrtoint ptr @"\01_caml_int_of_string" to i64
  %217 = load ptr addrspace(1), ptr %3
  %218 = load i64, ptr %ds
  %219 = load i64, ptr %alloc
  %220 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %218, i64 %219, i64 %216, ptr addrspace(1) %217) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 9, i64 0, i64 36, i64 62, i64 0, i64 62, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 32, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 6648366, i64 29552) ]
  %221 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %220, 0, 0
  %222 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %220, 0, 1
  store i64 %221, ptr %ds
  store i64 %222, ptr %alloc
  %223 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %220, 1, 0
  store ptr addrspace(1) %223, ptr %3
  br label %L459
L459:
  %224 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %224, ptr %53
  %225 = load ptr addrspace(1), ptr %53
  store ptr addrspace(1) %225, ptr %54
  %226 = load ptr addrspace(1), ptr %54
  store ptr addrspace(1) %226, ptr %55
  %227 = load ptr addrspace(1), ptr %55
  %228 = ptrtoint ptr addrspace(1) %227 to i64
  store i64 %228, ptr %36
  br label %L471
L465:
  %229 = ptrtoint ptr @"\01_camlString_tree_first_byte_diff__block35" to i64
  store i64 %229, ptr %56
  %230 = load i64, ptr %56
  %231 = inttoptr i64 %230 to ptr addrspace(1)
  store ptr addrspace(1) %231, ptr %3
  %232 = load ptr addrspace(1), ptr %3
  %233 = ptrtoint ptr addrspace(1) %232 to i64
  call  void @llvm.aarch64.oxcaml.raise.notrace(i64 %233) 
  unreachable
L468:
  store i64 21, ptr %58
  %234 = load i64, ptr %58
  store i64 %234, ptr %36
  br label %L471
L471:
  %235 = ptrtoint ptr @"\01_camlString_tree_first_byte_diff" to i64
  store i64 %235, ptr %59
  %236 = load i64, ptr %59
  %237 = add i64 %236, 32
  store i64 %237, ptr %60
  %238 = load i64, ptr %60
  %239 = inttoptr i64 %238 to ptr addrspace(1)
  store ptr addrspace(1) %239, ptr %4
  %240 = load i64, ptr %36
  %241 = inttoptr i64 %240 to ptr addrspace(1)
  store ptr addrspace(1) %241, ptr %6
  %242 = ptrtoint ptr @"\01_caml_initialize" to i64
  %243 = load ptr addrspace(1), ptr %4
  %244 = load ptr addrspace(1), ptr %6
  %245 = load i64, ptr %ds
  %246 = load i64, ptr %alloc
  %247 = call oxcaml_c_directcc { i64, i64 } @"\01_caml_initialize"(i64 %245, i64 %246, ptr addrspace(1) %243, ptr addrspace(1) %244) "gc-leaf-function"="true"
  %248 = extractvalue { i64, i64 } %247, 0
  %249 = extractvalue { i64, i64 } %247, 1
  store i64 %248, ptr %ds
  store i64 %249, ptr %alloc
  br label %L473
L473:
  %250 = load i64, ptr %ds
  %251 = add i64 %250, 40
  %252 = inttoptr i64 %251 to ptr
  %253 = load i64, ptr %252
  %254 = add i64 %253, 376
  %255 = call i64 asm sideeffect "mov $0, sp", "=r"() "gc-leaf-function"="true"
  %256 = icmp uge i64 %255, %254
  %257 = call  i1 @llvm.expect.i1(i1 %256, i1 1) 
  br i1 %257, label %L492, label %L491
L491:
  %258 = load i64, ptr %ds
  %259 = load i64, ptr %alloc
  %260 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_llvm_call_realloc_stack"(i64 %258, i64 %259, i64 34) "statepoint-id"="0" cold
  %261 = extractvalue { { i64, i64 }, {  } } %260, 0, 0
  %262 = extractvalue { { i64, i64 }, {  } } %260, 0, 1
  store i64 %261, ptr %ds
  store i64 %262, ptr %alloc
  br label %L492
L492:
  %263 = load i64, ptr %36
  store i64 %263, ptr %5
  %264 = load i64, ptr %5
  %265 = load i64, ptr %ds
  %266 = load i64, ptr %alloc
  %267 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlString_tree_first_byte_diff__black_box_int_0_8_code"(i64 %265, i64 %266, i64 %264) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 42, i64 0, i64 45, i64 65, i64 0, i64 65, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 27, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889) ]
  %268 = extractvalue { { i64, i64 }, { i64 } } %267, 0, 0
  %269 = extractvalue { { i64, i64 }, { i64 } } %267, 0, 1
  store i64 %268, ptr %ds
  store i64 %269, ptr %alloc
  %270 = extractvalue { { i64, i64 }, { i64 } } %267, 1, 0
  store i64 %270, ptr %5
  br label %L475
L475:
  %271 = load i64, ptr %5
  store i64 %271, ptr %62
  %272 = load i64, ptr %62
  store i64 %272, ptr %63
  %273 = load i64, ptr %10
  store i64 %273, ptr %5
  %274 = load i64, ptr %5
  %275 = load i64, ptr %ds
  %276 = load i64, ptr %alloc
  %277 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlString_tree_first_byte_diff__black_box_int_0_8_code"(i64 %275, i64 %276, i64 %274) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 42, i64 0, i64 27, i64 44, i64 0, i64 44, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 27, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889) ]
  %278 = extractvalue { { i64, i64 }, { i64 } } %277, 0, 0
  %279 = extractvalue { { i64, i64 }, { i64 } } %277, 0, 1
  store i64 %278, ptr %ds
  store i64 %279, ptr %alloc
  %280 = extractvalue { { i64, i64 }, { i64 } } %277, 1, 0
  store i64 %280, ptr %5
  br label %L476
L476:
  %281 = load i64, ptr %5
  store i64 %281, ptr %64
  %282 = load i64, ptr %64
  store i64 %282, ptr %65
  %283 = load i64, ptr %65
  store i64 %283, ptr %5
  %284 = load i64, ptr %63
  store i64 %284, ptr %7
  %285 = load i64, ptr %5
  %286 = load i64, ptr %7
  %287 = load i64, ptr %ds
  %288 = load i64, ptr %alloc
  %289 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlString_tree_first_byte_diff__run_7_15_code"(i64 %287, i64 %288, i64 %285, i64 %286) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 42, i64 0, i64 22, i64 66, i64 0, i64 66, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 27, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889) ]
  %290 = extractvalue { { i64, i64 }, { i64 } } %289, 0, 0
  %291 = extractvalue { { i64, i64 }, { i64 } } %289, 0, 1
  store i64 %290, ptr %ds
  store i64 %291, ptr %alloc
  %292 = extractvalue { { i64, i64 }, { i64 } } %289, 1, 0
  store i64 %292, ptr %5
  br label %L477
L477:
  %293 = load i64, ptr %5
  store i64 %293, ptr %66
  %294 = load i64, ptr %66
  store i64 %294, ptr %67
  %295 = ptrtoint ptr @"\01_camlString_tree_first_byte_diff__const_block66" to i64
  store i64 %295, ptr %68
  %296 = ptrtoint ptr @"\01_camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31" to i64
  store i64 %296, ptr %70
  %297 = load i64, ptr %70
  store i64 %297, ptr %5
  store i64 1, ptr %7
  %298 = load i64, ptr %68
  store i64 %298, ptr %8
  %299 = load i64, ptr %5
  %300 = load i64, ptr %7
  %301 = load i64, ptr %8
  %302 = load i64, ptr %ds
  %303 = load i64, ptr %alloc
  %304 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlCamlinternalFormat__make_printf_120_401_code"(i64 %302, i64 %303, i64 %299, i64 %300, i64 %301) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 5, i64 27, i64 0, i64 2, i64 63, i64 0, i64 63, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 23, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 7366251, i64 7235954, i64 26228, i64 34, i64 0, i64 21, i64 43, i64 0, i64 43, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 22, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 7499878, i64 7630441, i64 102, i64 38, i64 0, i64 17, i64 35, i64 0, i64 35, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 21, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 6910576, i64 6714478, i64 11, i64 0, i64 21, i64 63, i64 0, i64 63, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 40, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7499822, i64 7630441, i64 6648415, i64 7107955, i64 116, i64 42, i64 0, i64 9, i64 66, i64 0, i64 66, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 27, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889) ]
  %305 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %304, 0, 0
  %306 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %304, 0, 1
  store i64 %305, ptr %ds
  store i64 %306, ptr %alloc
  %307 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %304, 1, 0
  store ptr addrspace(1) %307, ptr %3
  br label %L478
L478:
  %308 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %308, ptr %71
  %309 = load ptr addrspace(1), ptr %71
  store ptr addrspace(1) %309, ptr %72
  %310 = load i64, ptr %67
  %311 = and i64 %310, 2147483647
  store i64 %311, ptr %73
  %312 = load ptr addrspace(1), ptr %72
  %313 = load i64, ptr addrspace(1) %312
  store i64 %313, ptr %74
  %314 = load i64, ptr %73
  store i64 %314, ptr %5
  %315 = load ptr addrspace(1), ptr %72
  store ptr addrspace(1) %315, ptr %6
  %316 = load i64, ptr %5
  %317 = load ptr addrspace(1), ptr %6
  %318 = load i64, ptr %ds
  %319 = load i64, ptr %alloc
  %320 = load i64, ptr %74
  %321 = inttoptr i64 %320 to ptr
  %322 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } %321(i64 %318, i64 %319, i64 %316, ptr addrspace(1) %317) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 2, i64 11, i64 0, i64 21, i64 63, i64 0, i64 63, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 40, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7499822, i64 7630441, i64 6648415, i64 7107955, i64 116, i64 42, i64 0, i64 9, i64 66, i64 0, i64 66, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 27, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889) ]
  %323 = extractvalue { { i64, i64 }, { i64 } } %322, 0, 0
  %324 = extractvalue { { i64, i64 }, { i64 } } %322, 0, 1
  store i64 %323, ptr %ds
  store i64 %324, ptr %alloc
  %325 = extractvalue { { i64, i64 }, { i64 } } %322, 1, 0
  store i64 %325, ptr %5
  br label %L479
L479:
  %326 = load i64, ptr %5
  store i64 %326, ptr %75
  %327 = load i64, ptr %75
  store i64 %327, ptr %76
  %328 = ptrtoint ptr @"\01_camlString_tree_first_byte_diff" to i64
  store i64 %328, ptr %77
  %329 = load i64, ptr %77
  store i64 %329, ptr %78
  %330 = load i64, ptr %78
  %331 = inttoptr i64 %330 to ptr addrspace(1)
  store ptr addrspace(1) %331, ptr %9
  store i64 1, ptr %5
  %332 = load i64, ptr %5
  %333 = inttoptr i64 %332 to ptr addrspace(1)
  %334 = load i64, ptr %ds
  %335 = load i64, ptr %alloc
  %336 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %334, 0, 0
  %337 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %336, i64 %335, 0, 1
  %338 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %337, ptr addrspace(1) %333, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %338
}

@"\01_camlString_tree_first_byte_diff__gc_roots" = global { ptr, i64 } { ptr @"\01_camlString_tree_first_byte_diff", i64 0 }, section "__DATA,__data", align 8
@"\01_header.camlString_tree_first_byte_diff" = global i64 11008, section "__DATA,__data", align 8
@"\01_camlString_tree_first_byte_diff" = global { ptr, ptr, ptr, i64, i64, ptr, ptr, ptr, ptr, ptr } { ptr @"\01_camlString_tree_first_byte_diff__black_box_int_8", ptr @"\01_camlString_tree_first_byte_diff__black_box_string_9", ptr @"\01_camlString_tree_first_byte_diff__black_box_10", i64 1, i64 1, ptr @"\01_camlString_tree_first_byte_diff__print_result_11", ptr @"\01_camlString_tree_first_byte_diff__key_12", ptr @"\01_camlString_tree_first_byte_diff__build_13", ptr @"\01_camlString_tree_first_byte_diff__find_14", ptr @"\01_camlString_tree_first_byte_diff__run_15" }, section "__DATA,__data", align 8
@"\01_header.camlString_tree_first_byte_diff__run_15" = global i64 4087, section "__DATA,__data", align 8
@"\01_camlString_tree_first_byte_diff__run_15" = global { ptr, i64, ptr } { ptr @"\01_caml_curry2", i64 180143985094819847, ptr @"\01_camlString_tree_first_byte_diff__run_7_15_code" }, section "__DATA,__data", align 8
@"\01_header.camlString_tree_first_byte_diff__find_14" = global i64 4087, section "__DATA,__data", align 8
@"\01_camlString_tree_first_byte_diff__find_14" = global { ptr, i64, ptr } { ptr @"\01_caml_curry2", i64 180143985094819847, ptr @"\01_camlString_tree_first_byte_diff__find_6_14_code" }, section "__DATA,__data", align 8
@"\01_header.camlString_tree_first_byte_diff__build_13" = global i64 4087, section "__DATA,__data", align 8
@"\01_camlString_tree_first_byte_diff__build_13" = global { ptr, i64, ptr } { ptr @"\01_caml_curry2", i64 180143985094819847, ptr @"\01_camlString_tree_first_byte_diff__build_5_13_code" }, section "__DATA,__data", align 8
@"\01_header.camlString_tree_first_byte_diff__key_12" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlString_tree_first_byte_diff__key_12" = global { ptr, i64 } { ptr @"\01_camlString_tree_first_byte_diff__key_4_12_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlString_tree_first_byte_diff__print_result_11" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlString_tree_first_byte_diff__print_result_11" = global { ptr, i64 } { ptr @"\01_camlString_tree_first_byte_diff__print_result_3_11_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlString_tree_first_byte_diff__black_box_10" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlString_tree_first_byte_diff__black_box_10" = global { ptr, i64 } { ptr @"\01_camlString_tree_first_byte_diff__black_box_2_10_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlString_tree_first_byte_diff__black_box_string_9" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlString_tree_first_byte_diff__black_box_string_9" = global { ptr, i64 } { ptr @"\01_camlString_tree_first_byte_diff__black_box_string_1_9_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlString_tree_first_byte_diff__black_box_int_8" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlString_tree_first_byte_diff__black_box_int_8" = global { ptr, i64 } { ptr @"\01_camlString_tree_first_byte_diff__black_box_int_0_8_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlString_tree_first_byte_diff__block35" = global i64 2816, section "__DATA,__data", align 8
@"\01_camlString_tree_first_byte_diff__block35" = global { ptr, ptr } { ptr @"\01_caml_exn_Invalid_argument", ptr @"\01_camlString_tree_first_byte_diff__string33" }, section "__DATA,__data", align 8
@"\01_header.camlString_tree_first_byte_diff__string33" = global i64 4092, section "__DATA,__data", align 8
@"\01_camlString_tree_first_byte_diff__string33" = global { [ 19 x i8 ], [ 4 x i8 ], i8 } { [ 19 x i8 ] c"\69\6e\64\65\78\20\6f\75\74\20\6f\66\20\62\6f\75\6e\64\73", [ 4 x i8 ] zeroinitializer, i8 4 }, section "__DATA,__data", align 8
@"\01_header.camlString_tree_first_byte_diff__immstring83" = global i64 3068, section "__DATA,__data", align 8
@"\01_camlString_tree_first_byte_diff__immstring83" = global { [ 14 x i8 ], [ 1 x i8 ], i8 } { [ 14 x i8 ] c"\5f\63\6f\6d\70\69\6c\65\72\5f\6b\65\79\5f", [ 1 x i8 ] zeroinitializer, i8 1 }, section "__DATA,__data", align 8
@"\01_header.camlString_tree_first_byte_diff__const_block66" = global i64 4868, section "__DATA,__data", align 8
@"\01_camlString_tree_first_byte_diff__const_block66" = global { i64, i64, i64, ptr } { i64 1, i64 1, i64 1, ptr @"\01_camlString_tree_first_byte_diff__const_block64" }, section "__DATA,__data", align 8
@"\01_header.camlString_tree_first_byte_diff__const_block64" = global i64 2828, section "__DATA,__data", align 8
@"\01_camlString_tree_first_byte_diff__const_block64" = global { i64, ptr } { i64 21, ptr @"\01_camlString_tree_first_byte_diff__const_block62" }, section "__DATA,__data", align 8
@"\01_header.camlString_tree_first_byte_diff__const_block62" = global i64 1802, section "__DATA,__data", align 8
@"\01_camlString_tree_first_byte_diff__const_block62" = global { i64 } { i64 1 }, section "__DATA,__data", align 8
@"\01_camlCamlinternalFormat__make_printf_120_401_code" = external global ptr
@"\01_camlStdlib__Char__Pmakeblock168" = external global ptr
@"\01_camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31" = external global ptr
@"\01_camlStdlib__immstring191" = external global ptr
@"\01_caml_array_make" = external global ptr
@"\01_caml_blit_string" = external global ptr
@"\01_caml_c_call" = external global ptr
@"\01_caml_call_gc" = external global ptr
@"\01_caml_create_bytes" = external global ptr
@"\01_caml_curry2" = external global ptr
@"\01_caml_exn_Invalid_argument" = external global ptr
@"\01_caml_fill_bytes" = external global ptr
@"\01_caml_format_int" = external global ptr
@"\01_caml_initialize" = external global ptr
@"\01_caml_int_of_string" = external global ptr
@"\01_caml_llvm_call_realloc_stack" = external global ptr
@"\01_caml_modify" = external global ptr
@"\01_caml_string_compare" = external global ptr
@"\01_caml_sys_argv" = external global ptr

declare void @llvm.aarch64.oxcaml.raise.notrace(i64)
declare i64 @llvm.bswap.i64(i64)
declare i1 @llvm.expect.i1(i1, i1)


!0 = !{ i32 1, !"oxcaml_module", !"String_tree_first_byte_diff" }
!llvm.module.flags = !{ !0 }
