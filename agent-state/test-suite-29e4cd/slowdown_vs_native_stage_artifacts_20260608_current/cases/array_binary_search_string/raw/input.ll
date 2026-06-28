source_filename = "array_binary_search_string.ml"

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlArray_binary_search_string__black_box_int_0_9_code"(i64 %0, i64 %1, i64 %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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

define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlArray_binary_search_string__black_box_string_1_10_code"(i64 %0, i64 %1, ptr addrspace(1) %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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

define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlArray_binary_search_string__black_box_2_11_code"(i64 %0, i64 %1, ptr addrspace(1) %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlArray_binary_search_string__print_result_3_12_code"(i64 %0, i64 %1, i64 %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" noinline gc "oxcaml" {
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
  %31 = ptrtoint ptr @"\01_camlArray_binary_search_string__const_block66" to i64
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
  %40 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlCamlinternalFormat__make_printf_120_401_code"(i64 %38, i64 %39, i64 %35, i64 %36, i64 %37) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 4, i64 27, i64 0, i64 2, i64 63, i64 0, i64 63, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 23, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 7366251, i64 7235954, i64 26228, i64 34, i64 0, i64 21, i64 43, i64 0, i64 43, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 22, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 7499878, i64 7630441, i64 102, i64 38, i64 0, i64 17, i64 35, i64 0, i64 35, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 21, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 6910576, i64 6714478, i64 11, i64 0, i64 21, i64 63, i64 0, i64 63, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 39, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 6910576, i64 6255726, i64 7562610, i64 7629941) ]
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

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlArray_binary_search_string__find_4_13_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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
  %29 = alloca ptr addrspace(1) 
  %30 = alloca ptr addrspace(1) 
  %31 = alloca ptr addrspace(1) 
  %32 = alloca ptr addrspace(1) 
  %33 = alloca ptr addrspace(1) 
  %34 = alloca ptr addrspace(1) 
  %35 = alloca ptr addrspace(1) 
  %36 = alloca ptr addrspace(1) 
  %37 = alloca ptr addrspace(1) 
  %38 = alloca i64 
  %39 = alloca i64 
  %40 = alloca i64 
  %41 = alloca i64 
  %42 = alloca i64 
  %43 = alloca i64 
  br label %L1
L1:
  br label %L119
L119:
  %44 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %44, ptr %8
  %45 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %45, ptr %9
  %46 = load ptr addrspace(1), ptr %9
  %47 = getelementptr i8, ptr addrspace(1) %46, i64 -8
  store ptr addrspace(1) %47, ptr %10
  %48 = load ptr addrspace(1), ptr %10
  %49 = load i64, ptr addrspace(1) %48
  store i64 %49, ptr %11
  %50 = load i64, ptr %11
  %51 = shl i64 %50, 8
  store i64 %51, ptr %12
  %52 = load i64, ptr %12
  %53 = lshr i64 %52, 17
  store i64 %53, ptr %13
  %54 = load i64, ptr %13
  %55 = or i64 %54, 1
  store i64 %55, ptr %14
  %56 = load i64, ptr %14
  %57 = add i64 %56, -2
  store i64 %57, ptr %15
  %58 = load i64, ptr %15
  store i64 %58, ptr %16
  store i64 1, ptr %20
  %59 = load i64, ptr %16
  store i64 %59, ptr %21
  %60 = load i64, ptr %20
  store i64 %60, ptr %17
  %61 = load i64, ptr %21
  store i64 %61, ptr %18
  %62 = load i64, ptr %17
  %63 = load i64, ptr %18
  %64 = icmp slt i64 %62, %63
  br i1 %64, label %L134, label %L162
L162:
  %65 = load i64, ptr %17
  %66 = load i64, ptr %18
  %67 = icmp sgt i64 %65, %66
  br i1 %67, label %L132, label %L134
L132:
  store i64 -1, ptr %7
  %68 = load i64, ptr %7
  %69 = load i64, ptr %ds
  %70 = load i64, ptr %alloc
  %71 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %69, 0, 0
  %72 = insertvalue { { i64, i64 }, { i64 } } %71, i64 %70, 0, 1
  %73 = insertvalue { { i64, i64 }, { i64 } } %72, i64 %68, 1, 0
  ret { { i64, i64 }, { i64 } } %73
L134:
  %74 = load i64, ptr %17
  %75 = load i64, ptr %18
  %76 = add i64 %74, %75
  store i64 %76, ptr %23
  %77 = load i64, ptr %23
  %78 = add i64 %77, -1
  store i64 %78, ptr %24
  %79 = load i64, ptr %24
  %80 = lshr i64 %79, 1
  store i64 %80, ptr %25
  %81 = load i64, ptr %25
  %82 = or i64 %81, 1
  store i64 %82, ptr %26
  %83 = load i64, ptr %26
  store i64 %83, ptr %27
  %84 = load i64, ptr %27
  %85 = shl i64 %84, 2
  store i64 %85, ptr %28
  %86 = load ptr addrspace(1), ptr %9
  %87 = load i64, ptr %28
  %88 = getelementptr i8, ptr addrspace(1) %86, i64 %87
  store ptr addrspace(1) %88, ptr %29
  %89 = load ptr addrspace(1), ptr %29
  %90 = getelementptr i8, ptr addrspace(1) %89, i64 -4
  store ptr addrspace(1) %90, ptr %30
  %91 = load ptr addrspace(1), ptr %30
  %92 = load ptr addrspace(1), ptr addrspace(1) %91
  store ptr addrspace(1) %92, ptr %31
  %93 = load ptr addrspace(1), ptr %31
  store ptr addrspace(1) %93, ptr %32
  %94 = load ptr addrspace(1), ptr %32
  %95 = load ptr addrspace(1), ptr addrspace(1) %94
  store ptr addrspace(1) %95, ptr %33
  %96 = load ptr addrspace(1), ptr %8
  store ptr addrspace(1) %96, ptr %5
  %97 = load ptr addrspace(1), ptr %33
  store ptr addrspace(1) %97, ptr %6
  %98 = ptrtoint ptr @"\01_caml_string_compare" to i64
  %99 = load ptr addrspace(1), ptr %5
  %100 = load ptr addrspace(1), ptr %6
  %101 = ptrtoint ptr addrspace(1) %99 to i64
  %102 = ptrtoint ptr addrspace(1) %100 to i64
  %103 = icmp eq i64 %101, %102
  br i1 %103, label %L164, label %L165
L164:
  %104 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %104, ptr %5
  br label %L163
L165:
  %105 = getelementptr i8, ptr addrspace(1) %99, i64 -8
  %106 = load atomic i64, ptr addrspace(1) %105 monotonic, align 8
  %107 = and i64 %106, 72057594037926912
  %108 = lshr i64 %107, 10
  %109 = shl i64 %108, 3
  %110 = sub i64 %109, 1
  %111 = getelementptr i8, ptr addrspace(1) %99, i64 %110
  %112 = load i8, ptr addrspace(1) %111, align 1
  %113 = zext i8 %112 to i64
  %114 = sub i64 %110, %113
  %115 = getelementptr i8, ptr addrspace(1) %100, i64 -8
  %116 = load atomic i64, ptr addrspace(1) %115 monotonic, align 8
  %117 = and i64 %116, 72057594037926912
  %118 = lshr i64 %117, 10
  %119 = shl i64 %118, 3
  %120 = sub i64 %119, 1
  %121 = getelementptr i8, ptr addrspace(1) %100, i64 %120
  %122 = load i8, ptr addrspace(1) %121, align 1
  %123 = zext i8 %122 to i64
  %124 = sub i64 %120, %123
  %125 = icmp ult i64 %114, %124
  %126 = select i1 %125, i64 %114, i64 %124
  %127 = icmp ugt i64 %126, 15
  br i1 %127, label %L166, label %L167
L166:
  %128 = load ptr addrspace(1), ptr %5
  %129 = load ptr addrspace(1), ptr %6
  %130 = load i64, ptr %ds
  %131 = load i64, ptr %alloc
  %132 = call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_string_compare"(i64 %130, i64 %131, ptr addrspace(1) %128, ptr addrspace(1) %129) "gc-leaf-function"="true"
  %133 = extractvalue { i64, i64, ptr addrspace(1) } %132, 0
  %134 = extractvalue { i64, i64, ptr addrspace(1) } %132, 1
  store i64 %133, ptr %ds
  store i64 %134, ptr %alloc
  %135 = extractvalue { i64, i64, ptr addrspace(1) } %132, 2
  store ptr addrspace(1) %135, ptr %5
  br label %L163
L167:
  %136 = icmp eq i64 %126, 0
  br i1 %136, label %L168, label %L169
L169:
  %137 = icmp ugt i64 %126, 8
  %138 = select i1 %137, i64 8, i64 %126
  %139 = sub i64 8, %138
  %140 = shl i64 %139, 3
  %141 = shl i64 -1, %140
  %142 = load i64, ptr addrspace(1) %99, align 8
  %143 = call  i64 @llvm.bswap.i64(i64 %142) 
  %144 = load i64, ptr addrspace(1) %100, align 8
  %145 = call  i64 @llvm.bswap.i64(i64 %144) 
  %146 = and i64 %143, %141
  %147 = and i64 %145, %141
  %148 = icmp ne i64 %146, %147
  %149 = icmp ult i64 %146, %147
  br i1 %148, label %L170, label %L171
L170:
  %150 = select i1 %149, i64 -1, i64 3
  %151 = inttoptr i64 %150 to ptr addrspace(1)
  store ptr addrspace(1) %151, ptr %5
  br label %L163
L171:
  br i1 %137, label %L172, label %L168
L172:
  %152 = sub i64 %126, 8
  %153 = sub i64 8, %152
  %154 = shl i64 %153, 3
  %155 = shl i64 -1, %154
  %156 = getelementptr i8, ptr addrspace(1) %99, i64 8
  %157 = load i64, ptr addrspace(1) %156, align 8
  %158 = call  i64 @llvm.bswap.i64(i64 %157) 
  %159 = getelementptr i8, ptr addrspace(1) %100, i64 8
  %160 = load i64, ptr addrspace(1) %159, align 8
  %161 = call  i64 @llvm.bswap.i64(i64 %160) 
  %162 = and i64 %158, %155
  %163 = and i64 %161, %155
  %164 = icmp ne i64 %162, %163
  %165 = icmp ult i64 %162, %163
  br i1 %164, label %L173, label %L168
L173:
  %166 = select i1 %165, i64 -1, i64 3
  %167 = inttoptr i64 %166 to ptr addrspace(1)
  store ptr addrspace(1) %167, ptr %5
  br label %L163
L168:
  %168 = icmp ult i64 %114, %124
  %169 = icmp ugt i64 %114, %124
  %170 = select i1 %169, i64 3, i64 1
  %171 = select i1 %168, i64 -1, i64 %170
  %172 = inttoptr i64 %171 to ptr addrspace(1)
  store ptr addrspace(1) %172, ptr %5
  br label %L163
L163:
  br label %L144
L144:
  %173 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %173, ptr %34
  %174 = load ptr addrspace(1), ptr %34
  store ptr addrspace(1) %174, ptr %35
  %175 = load ptr addrspace(1), ptr %35
  %176 = inttoptr i64 1 to ptr addrspace(1)
  %177 = icmp slt ptr addrspace(1) %175, %176
  br i1 %177, label %L150, label %L174
L174:
  %178 = load ptr addrspace(1), ptr %35
  %179 = inttoptr i64 1 to ptr addrspace(1)
  %180 = icmp sgt ptr addrspace(1) %178, %179
  br i1 %180, label %L150, label %L146
L146:
  %181 = load ptr addrspace(1), ptr %32
  %182 = getelementptr i8, ptr addrspace(1) %181, i64 8
  store ptr addrspace(1) %182, ptr %36
  %183 = load ptr addrspace(1), ptr %36
  %184 = load ptr addrspace(1), ptr addrspace(1) %183
  store ptr addrspace(1) %184, ptr %37
  %185 = load ptr addrspace(1), ptr %37
  store ptr addrspace(1) %185, ptr %5
  %186 = load ptr addrspace(1), ptr %5
  %187 = ptrtoint ptr addrspace(1) %186 to i64
  %188 = load i64, ptr %ds
  %189 = load i64, ptr %alloc
  %190 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %188, 0, 0
  %191 = insertvalue { { i64, i64 }, { i64 } } %190, i64 %189, 0, 1
  %192 = insertvalue { { i64, i64 }, { i64 } } %191, i64 %187, 1, 0
  ret { { i64, i64 }, { i64 } } %192
L150:
  %193 = load ptr addrspace(1), ptr %35
  %194 = inttoptr i64 1 to ptr addrspace(1)
  %195 = icmp slt ptr addrspace(1) %193, %194
  br i1 %195, label %L152, label %L175
L175:
  %196 = load ptr addrspace(1), ptr %35
  %197 = inttoptr i64 1 to ptr addrspace(1)
  %198 = icmp sgt ptr addrspace(1) %196, %197
  br i1 %198, label %L155, label %L155
L152:
  %199 = load i64, ptr %27
  %200 = add i64 %199, -2
  store i64 %200, ptr %38
  %201 = load i64, ptr %17
  store i64 %201, ptr %39
  %202 = load i64, ptr %38
  store i64 %202, ptr %40
  %203 = load i64, ptr %39
  store i64 %203, ptr %17
  %204 = load i64, ptr %40
  store i64 %204, ptr %18
  %205 = load i64, ptr %17
  %206 = load i64, ptr %18
  %207 = icmp slt i64 %205, %206
  br i1 %207, label %L134, label %L176
L176:
  %208 = load i64, ptr %17
  %209 = load i64, ptr %18
  %210 = icmp sgt i64 %208, %209
  br i1 %210, label %L132, label %L134
L155:
  %211 = load i64, ptr %27
  %212 = add i64 %211, 2
  store i64 %212, ptr %41
  %213 = load i64, ptr %41
  store i64 %213, ptr %42
  %214 = load i64, ptr %18
  store i64 %214, ptr %43
  %215 = load i64, ptr %42
  store i64 %215, ptr %17
  %216 = load i64, ptr %43
  store i64 %216, ptr %18
  %217 = load i64, ptr %17
  %218 = load i64, ptr %18
  %219 = icmp slt i64 %217, %218
  br i1 %219, label %L134, label %L177
L177:
  %220 = load i64, ptr %17
  %221 = load i64, ptr %18
  %222 = icmp sgt i64 %220, %221
  br i1 %222, label %L132, label %L134
}

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlArray_binary_search_string__run_6_15_code"(i64 %0, i64 %1, i64 %2, i64 %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" noinline gc "oxcaml" {
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
  %15 = alloca ptr addrspace(1) 
  %16 = alloca i64 
  %17 = alloca i64 
  %18 = alloca ptr addrspace(1) 
  %19 = alloca ptr addrspace(1) 
  %20 = alloca ptr addrspace(1) 
  %21 = alloca i64 
  %22 = alloca i64 
  %23 = alloca i64 
  %24 = alloca i64 
  %25 = alloca i64 
  %26 = alloca i64 
  %27 = alloca ptr addrspace(1) 
  %28 = alloca i64 
  %29 = alloca i64 
  %30 = alloca i64 
  %31 = alloca i64 
  %32 = alloca i64 
  %33 = alloca i64 
  %34 = alloca ptr addrspace(1) 
  %35 = alloca ptr addrspace(1) 
  %36 = alloca i64 
  %37 = alloca i64 
  %38 = alloca i64 
  %39 = alloca i64 
  %40 = alloca ptr addrspace(1) 
  %41 = alloca ptr addrspace(1) 
  %42 = alloca i64 
  %43 = alloca i64 
  %44 = alloca ptr addrspace(1) 
  %45 = alloca ptr addrspace(1) 
  %46 = alloca ptr addrspace(1) 
  %47 = alloca i64 
  %48 = alloca i64 
  %49 = alloca ptr addrspace(1) 
  %50 = alloca i64 
  %51 = alloca ptr addrspace(1) 
  %52 = alloca ptr addrspace(1) 
  %53 = alloca i64 
  %54 = alloca i64 
  %55 = alloca i64 
  %56 = alloca i64 
  %57 = alloca i64 
  %58 = alloca i64 
  %59 = alloca i64 
  %60 = alloca ptr addrspace(1) 
  %61 = alloca ptr addrspace(1) 
  %62 = alloca ptr addrspace(1) 
  %63 = alloca i64 
  %64 = alloca i64 
  %65 = alloca i64 
  %66 = alloca i64 
  %67 = alloca i64 
  %68 = alloca i64 
  %69 = alloca ptr addrspace(1) 
  %70 = alloca i64 
  %71 = alloca i64 
  %72 = alloca i64 
  %73 = alloca i64 
  %74 = alloca i64 
  %75 = alloca i64 
  %76 = alloca ptr addrspace(1) 
  %77 = alloca ptr addrspace(1) 
  %78 = alloca i64 
  %79 = alloca i64 
  %80 = alloca i64 
  %81 = alloca i64 
  %82 = alloca ptr addrspace(1) 
  %83 = alloca ptr addrspace(1) 
  %84 = alloca i64 
  %85 = alloca i64 
  %86 = alloca ptr addrspace(1) 
  %87 = alloca ptr addrspace(1) 
  %88 = alloca ptr addrspace(1) 
  %89 = alloca i64 
  %90 = alloca i64 
  %91 = alloca ptr addrspace(1) 
  %92 = alloca i64 
  %93 = alloca ptr addrspace(1) 
  %94 = alloca i64 
  %95 = alloca ptr addrspace(1) 
  %96 = alloca ptr addrspace(1) 
  %97 = alloca i64 
  %98 = alloca i64 
  %99 = alloca i64 
  %100 = alloca i64 
  %101 = alloca i64 
  %102 = alloca ptr addrspace(1) 
  %103 = alloca ptr addrspace(1) 
  %104 = alloca ptr addrspace(1) 
  %105 = alloca ptr addrspace(1) 
  %106 = alloca i64 
  %107 = alloca ptr addrspace(1) 
  %108 = alloca ptr addrspace(1) 
  %109 = alloca i64 
  %110 = alloca i64 
  %111 = alloca i64 
  %112 = alloca i64 
  %113 = alloca i64 
  %114 = alloca i64 
  %115 = alloca i64 
  %116 = alloca ptr addrspace(1) 
  %117 = alloca ptr addrspace(1) 
  %118 = alloca ptr addrspace(1) 
  %119 = alloca ptr addrspace(1) 
  %120 = alloca ptr addrspace(1) 
  %121 = alloca ptr addrspace(1) 
  %122 = alloca i64 
  %123 = alloca i64 
  %124 = alloca ptr addrspace(1) 
  %125 = alloca ptr addrspace(1) 
  %126 = alloca double 
  %127 = alloca i64 
  %128 = alloca i64 
  %129 = alloca i64 
  %130 = alloca ptr addrspace(1) 
  %131 = alloca ptr addrspace(1) 
  %132 = alloca i64 
  %133 = alloca i64 
  %134 = alloca i64 
  %135 = alloca i64 
  %136 = alloca i64 
  %137 = alloca ptr addrspace(1) 
  %138 = alloca i64 
  %139 = alloca i64 
  %140 = alloca i64 
  %141 = alloca i64 
  %142 = alloca i64 
  %143 = alloca i64 
  %144 = alloca i64 
  %145 = alloca i64 
  %146 = alloca i64 
  %147 = alloca i64 
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
  %160 = alloca ptr addrspace(1) 
  %161 = alloca ptr addrspace(1) 
  %162 = alloca ptr addrspace(1) 
  %163 = alloca i64 
  %164 = alloca i64 
  %165 = alloca i64 
  %166 = alloca i64 
  %167 = alloca i64 
  %168 = alloca i64 
  %169 = alloca i64 
  %170 = alloca i64 
  %171 = alloca i64 
  %172 = alloca i64 
  %173 = alloca i64 
  %174 = alloca i64 
  %175 = alloca i64 
  %176 = alloca i64 
  %177 = alloca i64 
  %178 = alloca i64 
  br label %L1
L1:
  br label %L179
L179:
  %179 = load i64, ptr %5
  store i64 %179, ptr %13
  %180 = load i64, ptr %6
  store i64 %180, ptr %14
  %181 = ptrtoint ptr @"\01_camlStdlib__immstring191" to i64
  store i64 %181, ptr %17
  %182 = load i64, ptr %17
  %183 = inttoptr i64 %182 to ptr addrspace(1)
  store ptr addrspace(1) %183, ptr %7
  %184 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %184, ptr %9
  %185 = ptrtoint ptr @"\01_caml_format_int" to i64
  %186 = load ptr addrspace(1), ptr %7
  %187 = load ptr addrspace(1), ptr %9
  %188 = load i64, ptr %ds
  %189 = load i64, ptr %alloc
  %190 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %188, i64 %189, i64 %185, ptr addrspace(1) %186, ptr addrspace(1) %187) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 280, i64 0, i64 2, i64 19, i64 0, i64 19, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7631662, i64 7235954, i64 7298919, i64 6905702, i64 29806, i64 27, i64 0, i64 50, i64 65, i64 0, i64 65, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 36, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026, i64 6694958, i64 2715253, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 27, i64 0, i64 4, i64 69, i64 0, i64 69, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026) ]
  %191 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %190, 0, 0
  %192 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %190, 0, 1
  store i64 %191, ptr %ds
  store i64 %192, ptr %alloc
  %193 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %190, 1, 0
  store ptr addrspace(1) %193, ptr %7
  br label %L184
L184:
  %194 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %194, ptr %18
  %195 = load ptr addrspace(1), ptr %18
  store ptr addrspace(1) %195, ptr %19
  %196 = load ptr addrspace(1), ptr %19
  %197 = getelementptr i8, ptr addrspace(1) %196, i64 -8
  store ptr addrspace(1) %197, ptr %20
  %198 = load ptr addrspace(1), ptr %20
  %199 = load i64, ptr addrspace(1) %198
  store i64 %199, ptr %21
  %200 = load i64, ptr %21
  %201 = shl i64 %200, 8
  store i64 %201, ptr %22
  %202 = load i64, ptr %22
  %203 = lshr i64 %202, 18
  store i64 %203, ptr %23
  %204 = load i64, ptr %23
  %205 = shl i64 %204, 3
  store i64 %205, ptr %24
  %206 = load i64, ptr %24
  %207 = sub i64 %206, 1
  store i64 %207, ptr %25
  %208 = load i64, ptr %25
  store i64 %208, ptr %26
  %209 = load ptr addrspace(1), ptr %19
  %210 = load i64, ptr %26
  %211 = getelementptr i8, ptr addrspace(1) %209, i64 %210
  store ptr addrspace(1) %211, ptr %27
  %212 = load ptr addrspace(1), ptr %27
  %213 = load i8, ptr addrspace(1) %212
  %214 = zext i8 %213 to i64
  store i64 %214, ptr %28
  %215 = load i64, ptr %26
  %216 = load i64, ptr %28
  %217 = sub i64 %215, %216
  store i64 %217, ptr %29
  %218 = load i64, ptr %29
  %219 = shl i64 %218, 1
  %220 = add i64 1, %219
  store i64 %220, ptr %31
  %221 = load i64, ptr %31
  store i64 %221, ptr %32
  %222 = load i64, ptr %32
  %223 = add i64 %222, 34
  store i64 %223, ptr %33
  %224 = load i64, ptr %33
  %225 = inttoptr i64 %224 to ptr addrspace(1)
  store ptr addrspace(1) %225, ptr %7
  %226 = ptrtoint ptr @"\01_caml_create_bytes" to i64
  %227 = load ptr addrspace(1), ptr %7
  %228 = load i64, ptr %ds
  %229 = load i64, ptr %alloc
  %230 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %228, i64 %229, i64 %226, ptr addrspace(1) %227) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 226, i64 0, i64 10, i64 32, i64 0, i64 32, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 10, i64 6583379, i64 6449516, i64 6170670, i64 41, i64 27, i64 0, i64 28, i64 65, i64 0, i64 65, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 36, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026, i64 6694958, i64 2715253, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 27, i64 0, i64 4, i64 69, i64 0, i64 69, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026) ]
  %231 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %230, 0, 0
  %232 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %230, 0, 1
  store i64 %231, ptr %ds
  store i64 %232, ptr %alloc
  %233 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %230, 1, 0
  store ptr addrspace(1) %233, ptr %7
  br label %L195
L195:
  %234 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %234, ptr %34
  %235 = load ptr addrspace(1), ptr %34
  store ptr addrspace(1) %235, ptr %35
  %236 = ptrtoint ptr @"\01_camlArray_binary_search_string__immstring129" to i64
  store i64 %236, ptr %39
  %237 = load i64, ptr %39
  %238 = inttoptr i64 %237 to ptr addrspace(1)
  store ptr addrspace(1) %238, ptr %7
  %239 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %239, ptr %9
  %240 = load ptr addrspace(1), ptr %35
  store ptr addrspace(1) %240, ptr %10
  %241 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %241, ptr %11
  %242 = inttoptr i64 35 to ptr addrspace(1)
  store ptr addrspace(1) %242, ptr %12
  %243 = ptrtoint ptr @"\01_caml_blit_string" to i64
  %244 = load ptr addrspace(1), ptr %7
  %245 = load ptr addrspace(1), ptr %9
  %246 = load ptr addrspace(1), ptr %10
  %247 = load ptr addrspace(1), ptr %11
  %248 = load ptr addrspace(1), ptr %12
  %249 = load i64, ptr %ds
  %250 = load i64, ptr %alloc
  %251 = call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %249, i64 %250, ptr addrspace(1) %244, ptr addrspace(1) %245, ptr addrspace(1) %246, ptr addrspace(1) %247, ptr addrspace(1) %248) "gc-leaf-function"="true"
  %252 = extractvalue { i64, i64, ptr addrspace(1) } %251, 0
  %253 = extractvalue { i64, i64, ptr addrspace(1) } %251, 1
  store i64 %252, ptr %ds
  store i64 %253, ptr %alloc
  %254 = extractvalue { i64, i64, ptr addrspace(1) } %251, 2
  store ptr addrspace(1) %254, ptr %7
  br label %L197
L197:
  %255 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %255, ptr %40
  %256 = load ptr addrspace(1), ptr %40
  store ptr addrspace(1) %256, ptr %41
  %257 = load ptr addrspace(1), ptr %19
  store ptr addrspace(1) %257, ptr %7
  %258 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %258, ptr %9
  %259 = load ptr addrspace(1), ptr %35
  store ptr addrspace(1) %259, ptr %10
  %260 = inttoptr i64 35 to ptr addrspace(1)
  store ptr addrspace(1) %260, ptr %11
  %261 = load i64, ptr %32
  %262 = inttoptr i64 %261 to ptr addrspace(1)
  store ptr addrspace(1) %262, ptr %12
  %263 = ptrtoint ptr @"\01_caml_blit_string" to i64
  %264 = load ptr addrspace(1), ptr %7
  %265 = load ptr addrspace(1), ptr %9
  %266 = load ptr addrspace(1), ptr %10
  %267 = load ptr addrspace(1), ptr %11
  %268 = load ptr addrspace(1), ptr %12
  %269 = load i64, ptr %ds
  %270 = load i64, ptr %alloc
  %271 = call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %269, i64 %270, ptr addrspace(1) %264, ptr addrspace(1) %265, ptr addrspace(1) %266, ptr addrspace(1) %267, ptr addrspace(1) %268) "gc-leaf-function"="true"
  %272 = extractvalue { i64, i64, ptr addrspace(1) } %271, 0
  %273 = extractvalue { i64, i64, ptr addrspace(1) } %271, 1
  store i64 %272, ptr %ds
  store i64 %273, ptr %alloc
  %274 = extractvalue { i64, i64, ptr addrspace(1) } %271, 2
  store ptr addrspace(1) %274, ptr %7
  br label %L198
L198:
  %275 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %275, ptr %44
  %276 = load ptr addrspace(1), ptr %44
  store ptr addrspace(1) %276, ptr %45
  %277 = load i64, ptr %alloc
  %278 = sub i64 %277, 24
  store i64 %278, ptr %alloc
  %279 = load i64, ptr %ds
  %280 = inttoptr i64 %279 to ptr
  %281 = load i64, ptr %280
  %282 = icmp ule i64 %281, %278
  %283 = call  i1 @llvm.expect.i1(i1 %282, i1 1) 
  br i1 %283, label %L346, label %L345
L345:
  %284 = load i64, ptr %ds
  %285 = load i64, ptr %alloc
  %286 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %284, i64 %285) "statepoint-id"="196609" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 3, i64 27, i64 0, i64 28, i64 68, i64 0, i64 68, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 36, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026, i64 6694958, i64 2715253, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 27, i64 0, i64 4, i64 69, i64 0, i64 69, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026) ]
  %287 = extractvalue { { i64, i64 }, {  } } %286, 0, 0
  %288 = extractvalue { { i64, i64 }, {  } } %286, 0, 1
  store i64 %287, ptr %ds
  store i64 %288, ptr %alloc
  br label %L346
L346:
  %289 = load i64, ptr %alloc
  %290 = add i64 %289, 8
  %291 = inttoptr i64 %290 to ptr addrspace(1)
  store ptr addrspace(1) %291, ptr %46
  %292 = load ptr addrspace(1), ptr %46
  %293 = getelementptr i8, ptr addrspace(1) %292, i64 -8
  store volatile i64 2048, ptr addrspace(1) %293
  %294 = load ptr addrspace(1), ptr %46
  %295 = load ptr addrspace(1), ptr %35
  store ptr addrspace(1) %295, ptr addrspace(1) %294
  %296 = load ptr addrspace(1), ptr %46
  %297 = getelementptr i8, ptr addrspace(1) %296, i64 8
  store volatile i64 1, ptr addrspace(1) %297
  %298 = load ptr addrspace(1), ptr %46
  store ptr addrspace(1) %298, ptr %49
  %299 = inttoptr i64 129 to ptr addrspace(1)
  store ptr addrspace(1) %299, ptr %7
  %300 = load ptr addrspace(1), ptr %49
  store ptr addrspace(1) %300, ptr %9
  %301 = ptrtoint ptr @"\01_caml_array_make" to i64
  %302 = load ptr addrspace(1), ptr %7
  %303 = load ptr addrspace(1), ptr %9
  %304 = load i64, ptr %ds
  %305 = load i64, ptr %alloc
  %306 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %304, i64 %305, i64 %301, ptr addrspace(1) %302, ptr addrspace(1) %303) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 85, i64 0, i64 13, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 27, i64 0, i64 4, i64 69, i64 0, i64 69, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026) ]
  %307 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %306, 0, 0
  %308 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %306, 0, 1
  store i64 %307, ptr %ds
  store i64 %308, ptr %alloc
  %309 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %306, 1, 0
  store ptr addrspace(1) %309, ptr %7
  br label %L200
L200:
  %310 = load i64, ptr %ds
  %311 = add i64 %310, 40
  %312 = inttoptr i64 %311 to ptr
  %313 = load i64, ptr %312
  %314 = add i64 %313, 376
  %315 = call i64 asm sideeffect "mov $0, sp", "=r"() "gc-leaf-function"="true"
  %316 = icmp uge i64 %315, %314
  %317 = call  i1 @llvm.expect.i1(i1 %316, i1 1) 
  br i1 %317, label %L348, label %L347
L347:
  %318 = load i64, ptr %ds
  %319 = load i64, ptr %alloc
  %320 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_llvm_call_realloc_stack"(i64 %318, i64 %319, i64 34) "statepoint-id"="0" cold
  %321 = extractvalue { { i64, i64 }, {  } } %320, 0, 0
  %322 = extractvalue { { i64, i64 }, {  } } %320, 0, 1
  store i64 %321, ptr %ds
  store i64 %322, ptr %alloc
  br label %L348
L348:
  %323 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %323, ptr %51
  %324 = load ptr addrspace(1), ptr %51
  store ptr addrspace(1) %324, ptr %52
  store i64 1, ptr %55
  %325 = load i64, ptr %55
  store i64 %325, ptr %53
  br label %L204
L204:
  %326 = load i64, ptr %53
  %327 = shl i64 %326, 1
  %328 = add i64 1, %327
  store i64 %328, ptr %57
  %329 = load i64, ptr %57
  store i64 %329, ptr %58
  %330 = ptrtoint ptr @"\01_camlStdlib__immstring191" to i64
  store i64 %330, ptr %59
  %331 = load i64, ptr %59
  %332 = inttoptr i64 %331 to ptr addrspace(1)
  store ptr addrspace(1) %332, ptr %7
  %333 = load i64, ptr %58
  %334 = inttoptr i64 %333 to ptr addrspace(1)
  store ptr addrspace(1) %334, ptr %9
  %335 = ptrtoint ptr @"\01_caml_format_int" to i64
  %336 = load ptr addrspace(1), ptr %7
  %337 = load ptr addrspace(1), ptr %9
  %338 = load i64, ptr %ds
  %339 = load i64, ptr %alloc
  %340 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %338, i64 %339, i64 %335, ptr addrspace(1) %336, ptr addrspace(1) %337) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 280, i64 0, i64 2, i64 19, i64 0, i64 19, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7631662, i64 7235954, i64 7298919, i64 6905702, i64 29806, i64 27, i64 0, i64 50, i64 65, i64 0, i64 65, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 36, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026, i64 6694958, i64 2715253, i64 87, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 27, i64 0, i64 4, i64 69, i64 0, i64 69, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026) ]
  %341 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %340, 0, 0
  %342 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %340, 0, 1
  store i64 %341, ptr %ds
  store i64 %342, ptr %alloc
  %343 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %340, 1, 0
  store ptr addrspace(1) %343, ptr %7
  br label %L207
L207:
  %344 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %344, ptr %60
  %345 = load ptr addrspace(1), ptr %60
  store ptr addrspace(1) %345, ptr %61
  %346 = load ptr addrspace(1), ptr %61
  %347 = getelementptr i8, ptr addrspace(1) %346, i64 -8
  store ptr addrspace(1) %347, ptr %62
  %348 = load ptr addrspace(1), ptr %62
  %349 = load i64, ptr addrspace(1) %348
  store i64 %349, ptr %63
  %350 = load i64, ptr %63
  %351 = shl i64 %350, 8
  store i64 %351, ptr %64
  %352 = load i64, ptr %64
  %353 = lshr i64 %352, 18
  store i64 %353, ptr %65
  %354 = load i64, ptr %65
  %355 = shl i64 %354, 3
  store i64 %355, ptr %66
  %356 = load i64, ptr %66
  %357 = sub i64 %356, 1
  store i64 %357, ptr %67
  %358 = load i64, ptr %67
  store i64 %358, ptr %68
  %359 = load ptr addrspace(1), ptr %61
  %360 = load i64, ptr %68
  %361 = getelementptr i8, ptr addrspace(1) %359, i64 %360
  store ptr addrspace(1) %361, ptr %69
  %362 = load ptr addrspace(1), ptr %69
  %363 = load i8, ptr addrspace(1) %362
  %364 = zext i8 %363 to i64
  store i64 %364, ptr %70
  %365 = load i64, ptr %68
  %366 = load i64, ptr %70
  %367 = sub i64 %365, %366
  store i64 %367, ptr %71
  %368 = load i64, ptr %71
  %369 = shl i64 %368, 1
  %370 = add i64 1, %369
  store i64 %370, ptr %73
  %371 = load i64, ptr %73
  store i64 %371, ptr %74
  %372 = load i64, ptr %74
  %373 = add i64 %372, 34
  store i64 %373, ptr %75
  %374 = load i64, ptr %75
  %375 = inttoptr i64 %374 to ptr addrspace(1)
  store ptr addrspace(1) %375, ptr %7
  %376 = ptrtoint ptr @"\01_caml_create_bytes" to i64
  %377 = load ptr addrspace(1), ptr %7
  %378 = load i64, ptr %ds
  %379 = load i64, ptr %alloc
  %380 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %378, i64 %379, i64 %376, ptr addrspace(1) %377) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 226, i64 0, i64 10, i64 32, i64 0, i64 32, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 10, i64 6583379, i64 6449516, i64 6170670, i64 41, i64 27, i64 0, i64 28, i64 65, i64 0, i64 65, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 36, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026, i64 6694958, i64 2715253, i64 87, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 27, i64 0, i64 4, i64 69, i64 0, i64 69, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026) ]
  %381 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %380, 0, 0
  %382 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %380, 0, 1
  store i64 %381, ptr %ds
  store i64 %382, ptr %alloc
  %383 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %380, 1, 0
  store ptr addrspace(1) %383, ptr %7
  br label %L218
L218:
  %384 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %384, ptr %76
  %385 = load ptr addrspace(1), ptr %76
  store ptr addrspace(1) %385, ptr %77
  %386 = ptrtoint ptr @"\01_camlArray_binary_search_string__immstring129" to i64
  store i64 %386, ptr %81
  %387 = load i64, ptr %81
  %388 = inttoptr i64 %387 to ptr addrspace(1)
  store ptr addrspace(1) %388, ptr %7
  %389 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %389, ptr %9
  %390 = load ptr addrspace(1), ptr %77
  store ptr addrspace(1) %390, ptr %10
  %391 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %391, ptr %11
  %392 = inttoptr i64 35 to ptr addrspace(1)
  store ptr addrspace(1) %392, ptr %12
  %393 = ptrtoint ptr @"\01_caml_blit_string" to i64
  %394 = load ptr addrspace(1), ptr %7
  %395 = load ptr addrspace(1), ptr %9
  %396 = load ptr addrspace(1), ptr %10
  %397 = load ptr addrspace(1), ptr %11
  %398 = load ptr addrspace(1), ptr %12
  %399 = load i64, ptr %ds
  %400 = load i64, ptr %alloc
  %401 = call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %399, i64 %400, ptr addrspace(1) %394, ptr addrspace(1) %395, ptr addrspace(1) %396, ptr addrspace(1) %397, ptr addrspace(1) %398) "gc-leaf-function"="true"
  %402 = extractvalue { i64, i64, ptr addrspace(1) } %401, 0
  %403 = extractvalue { i64, i64, ptr addrspace(1) } %401, 1
  store i64 %402, ptr %ds
  store i64 %403, ptr %alloc
  %404 = extractvalue { i64, i64, ptr addrspace(1) } %401, 2
  store ptr addrspace(1) %404, ptr %7
  br label %L220
L220:
  %405 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %405, ptr %82
  %406 = load ptr addrspace(1), ptr %82
  store ptr addrspace(1) %406, ptr %83
  %407 = load ptr addrspace(1), ptr %61
  store ptr addrspace(1) %407, ptr %7
  %408 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %408, ptr %9
  %409 = load ptr addrspace(1), ptr %77
  store ptr addrspace(1) %409, ptr %10
  %410 = inttoptr i64 35 to ptr addrspace(1)
  store ptr addrspace(1) %410, ptr %11
  %411 = load i64, ptr %74
  %412 = inttoptr i64 %411 to ptr addrspace(1)
  store ptr addrspace(1) %412, ptr %12
  %413 = ptrtoint ptr @"\01_caml_blit_string" to i64
  %414 = load ptr addrspace(1), ptr %7
  %415 = load ptr addrspace(1), ptr %9
  %416 = load ptr addrspace(1), ptr %10
  %417 = load ptr addrspace(1), ptr %11
  %418 = load ptr addrspace(1), ptr %12
  %419 = load i64, ptr %ds
  %420 = load i64, ptr %alloc
  %421 = call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %419, i64 %420, ptr addrspace(1) %414, ptr addrspace(1) %415, ptr addrspace(1) %416, ptr addrspace(1) %417, ptr addrspace(1) %418) "gc-leaf-function"="true"
  %422 = extractvalue { i64, i64, ptr addrspace(1) } %421, 0
  %423 = extractvalue { i64, i64, ptr addrspace(1) } %421, 1
  store i64 %422, ptr %ds
  store i64 %423, ptr %alloc
  %424 = extractvalue { i64, i64, ptr addrspace(1) } %421, 2
  store ptr addrspace(1) %424, ptr %7
  br label %L221
L221:
  %425 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %425, ptr %86
  %426 = load ptr addrspace(1), ptr %86
  store ptr addrspace(1) %426, ptr %87
  %427 = load ptr addrspace(1), ptr %52
  %428 = getelementptr i8, ptr addrspace(1) %427, i64 -8
  store ptr addrspace(1) %428, ptr %88
  %429 = load ptr addrspace(1), ptr %88
  %430 = load i8, ptr addrspace(1) %429
  %431 = zext i8 %430 to i64
  store i64 %431, ptr %89
  %432 = load i64, ptr %89
  %433 = icmp slt i64 %432, 254
  br i1 %433, label %L226, label %L349
L349:
  %434 = load i64, ptr %89
  %435 = icmp sgt i64 %434, 254
  br i1 %435, label %L226, label %L224
L224:
  %436 = ptrtoint ptr @"\01_camlArray_binary_search_string__invalid776" to i64
  store i64 %436, ptr %90
  %437 = load i64, ptr %90
  %438 = inttoptr i64 %437 to ptr addrspace(1)
  store ptr addrspace(1) %438, ptr %7
  %439 = ptrtoint ptr @"\01_caml_flambda2_invalid" to i64
  %440 = load ptr addrspace(1), ptr %7
  %441 = load i64, ptr %ds
  %442 = load i64, ptr %alloc
  %443 = call oxcaml_c_directcc { i64, i64 } @"\01_caml_flambda2_invalid"(i64 %441, i64 %442, ptr addrspace(1) %440) "gc-leaf-function"="true"
  %444 = extractvalue { i64, i64 } %443, 0
  %445 = extractvalue { i64, i64 } %443, 1
  store i64 %444, ptr %ds
  store i64 %445, ptr %alloc
  unreachable
L226:
  %446 = load i64, ptr %alloc
  %447 = sub i64 %446, 24
  store i64 %447, ptr %alloc
  %448 = load i64, ptr %ds
  %449 = inttoptr i64 %448 to ptr
  %450 = load i64, ptr %449
  %451 = icmp ule i64 %450, %447
  %452 = call  i1 @llvm.expect.i1(i1 %451, i1 1) 
  br i1 %452, label %L351, label %L350
L350:
  %453 = load i64, ptr %ds
  %454 = load i64, ptr %alloc
  %455 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %453, i64 %454) "statepoint-id"="196609" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 3, i64 27, i64 0, i64 28, i64 68, i64 0, i64 68, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 36, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026, i64 6694958, i64 2715253, i64 87, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 27, i64 0, i64 4, i64 69, i64 0, i64 69, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026) ]
  %456 = extractvalue { { i64, i64 }, {  } } %455, 0, 0
  %457 = extractvalue { { i64, i64 }, {  } } %455, 0, 1
  store i64 %456, ptr %ds
  store i64 %457, ptr %alloc
  br label %L351
L351:
  %458 = load i64, ptr %alloc
  %459 = add i64 %458, 8
  %460 = inttoptr i64 %459 to ptr addrspace(1)
  store ptr addrspace(1) %460, ptr %91
  %461 = load ptr addrspace(1), ptr %91
  %462 = getelementptr i8, ptr addrspace(1) %461, i64 -8
  store volatile i64 2048, ptr addrspace(1) %462
  %463 = load ptr addrspace(1), ptr %91
  %464 = load ptr addrspace(1), ptr %77
  store ptr addrspace(1) %464, ptr addrspace(1) %463
  %465 = load ptr addrspace(1), ptr %91
  %466 = getelementptr i8, ptr addrspace(1) %465, i64 8
  %467 = load i64, ptr %58
  store volatile i64 %467, ptr addrspace(1) %466
  %468 = load ptr addrspace(1), ptr %91
  store ptr addrspace(1) %468, ptr %93
  %469 = load i64, ptr %58
  %470 = shl i64 %469, 2
  store i64 %470, ptr %94
  %471 = load ptr addrspace(1), ptr %52
  %472 = load i64, ptr %94
  %473 = getelementptr i8, ptr addrspace(1) %471, i64 %472
  store ptr addrspace(1) %473, ptr %95
  %474 = load ptr addrspace(1), ptr %95
  %475 = getelementptr i8, ptr addrspace(1) %474, i64 -4
  store ptr addrspace(1) %475, ptr %96
  %476 = load ptr addrspace(1), ptr %96
  store ptr addrspace(1) %476, ptr %8
  %477 = load ptr addrspace(1), ptr %93
  store ptr addrspace(1) %477, ptr %9
  %478 = ptrtoint ptr @"\01_caml_modify" to i64
  %479 = load ptr addrspace(1), ptr %8
  %480 = load ptr addrspace(1), ptr %9
  %481 = load i64, ptr %ds
  %482 = load i64, ptr %alloc
  %483 = call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %481, i64 %482, ptr addrspace(1) %479, ptr addrspace(1) %480) "gc-leaf-function"="true"
  %484 = extractvalue { i64, i64 } %483, 0
  %485 = extractvalue { i64, i64 } %483, 1
  store i64 %484, ptr %ds
  store i64 %485, ptr %alloc
  br label %L229
L229:
  store i64 1, ptr %98
  %486 = load i64, ptr %53
  %487 = add i64 %486, 1
  store i64 %487, ptr %99
  %488 = load i64, ptr %99
  store i64 %488, ptr %100
  %489 = load i64, ptr %100
  %490 = icmp slt i64 %489, 63
  br i1 %490, label %L234, label %L352
L352:
  %491 = load i64, ptr %100
  %492 = icmp sgt i64 %491, 63
  br i1 %492, label %L236, label %L234
L234:
  %493 = load i64, ptr %100
  store i64 %493, ptr %101
  %494 = load i64, ptr %101
  store i64 %494, ptr %53
  br label %L204
L236:
  %495 = load ptr addrspace(1), ptr %52
  store ptr addrspace(1) %495, ptr %102
  %496 = load ptr addrspace(1), ptr %102
  store ptr addrspace(1) %496, ptr %15
  %497 = load ptr addrspace(1), ptr %15
  %498 = load ptr addrspace(1), ptr addrspace(1) %497
  store ptr addrspace(1) %498, ptr %104
  %499 = load ptr addrspace(1), ptr %104
  %500 = load ptr addrspace(1), ptr addrspace(1) %499
  store ptr addrspace(1) %500, ptr %105
  %501 = inttoptr i64 129 to ptr addrspace(1)
  store ptr addrspace(1) %501, ptr %7
  %502 = load ptr addrspace(1), ptr %105
  store ptr addrspace(1) %502, ptr %9
  %503 = ptrtoint ptr @"\01_caml_array_make" to i64
  %504 = load ptr addrspace(1), ptr %7
  %505 = load ptr addrspace(1), ptr %9
  %506 = load i64, ptr %ds
  %507 = load i64, ptr %alloc
  %508 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %506, i64 %507, i64 %503, ptr addrspace(1) %504, ptr addrspace(1) %505) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 155, i64 0, i64 12, i64 40, i64 0, i64 40, i64 8, i64 7500385, i64 3045729, i64 27757, i64 17, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 7155321, i64 28769, i64 29, i64 0, i64 13, i64 28, i64 0, i64 28, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026) ]
  %509 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %508, 0, 0
  %510 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %508, 0, 1
  store i64 %509, ptr %ds
  store i64 %510, ptr %alloc
  %511 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %508, 1, 0
  store ptr addrspace(1) %511, ptr %7
  br label %L246
L246:
  %512 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %512, ptr %107
  %513 = load ptr addrspace(1), ptr %107
  store ptr addrspace(1) %513, ptr %108
  store i64 1, ptr %111
  %514 = load i64, ptr %111
  store i64 %514, ptr %109
  br label %L252
L252:
  %515 = load i64, ptr %109
  %516 = shl i64 %515, 1
  %517 = add i64 1, %516
  store i64 %517, ptr %113
  %518 = load i64, ptr %113
  store i64 %518, ptr %114
  %519 = load i64, ptr %114
  %520 = shl i64 %519, 2
  store i64 %520, ptr %115
  %521 = load ptr addrspace(1), ptr %15
  %522 = load i64, ptr %115
  %523 = getelementptr i8, ptr addrspace(1) %521, i64 %522
  store ptr addrspace(1) %523, ptr %116
  %524 = load ptr addrspace(1), ptr %116
  %525 = getelementptr i8, ptr addrspace(1) %524, i64 -4
  store ptr addrspace(1) %525, ptr %117
  %526 = load ptr addrspace(1), ptr %117
  %527 = load ptr addrspace(1), ptr addrspace(1) %526
  store ptr addrspace(1) %527, ptr %118
  %528 = load ptr addrspace(1), ptr %118
  %529 = load ptr addrspace(1), ptr addrspace(1) %528
  store ptr addrspace(1) %529, ptr %119
  %530 = load ptr addrspace(1), ptr %119
  store ptr addrspace(1) %530, ptr %120
  %531 = load ptr addrspace(1), ptr %108
  %532 = getelementptr i8, ptr addrspace(1) %531, i64 -8
  store ptr addrspace(1) %532, ptr %121
  %533 = load ptr addrspace(1), ptr %121
  %534 = load i8, ptr addrspace(1) %533
  %535 = zext i8 %534 to i64
  store i64 %535, ptr %122
  %536 = load i64, ptr %122
  %537 = icmp slt i64 %536, 254
  br i1 %537, label %L272, label %L353
L353:
  %538 = load i64, ptr %122
  %539 = icmp sgt i64 %538, 254
  br i1 %539, label %L272, label %L265
L265:
  %540 = load i64, ptr %114
  %541 = shl i64 %540, 2
  store i64 %541, ptr %123
  %542 = load ptr addrspace(1), ptr %108
  %543 = load i64, ptr %123
  %544 = getelementptr i8, ptr addrspace(1) %542, i64 %543
  store ptr addrspace(1) %544, ptr %124
  %545 = load ptr addrspace(1), ptr %124
  %546 = getelementptr i8, ptr addrspace(1) %545, i64 -4
  store ptr addrspace(1) %546, ptr %125
  %547 = load ptr addrspace(1), ptr %120
  %548 = load double, ptr addrspace(1) %547
  store double %548, ptr %126
  %549 = load ptr addrspace(1), ptr %125
  %550 = load double, ptr %126
  store double %550, ptr addrspace(1) %549
  store i64 1, ptr %128
  br label %L279
L272:
  %551 = load i64, ptr %114
  %552 = shl i64 %551, 2
  store i64 %552, ptr %129
  %553 = load ptr addrspace(1), ptr %108
  %554 = load i64, ptr %129
  %555 = getelementptr i8, ptr addrspace(1) %553, i64 %554
  store ptr addrspace(1) %555, ptr %130
  %556 = load ptr addrspace(1), ptr %130
  %557 = getelementptr i8, ptr addrspace(1) %556, i64 -4
  store ptr addrspace(1) %557, ptr %131
  %558 = load ptr addrspace(1), ptr %131
  store ptr addrspace(1) %558, ptr %8
  %559 = load ptr addrspace(1), ptr %120
  store ptr addrspace(1) %559, ptr %9
  %560 = ptrtoint ptr @"\01_caml_modify" to i64
  %561 = load ptr addrspace(1), ptr %8
  %562 = load ptr addrspace(1), ptr %9
  %563 = load i64, ptr %ds
  %564 = load i64, ptr %alloc
  %565 = call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %563, i64 %564, ptr addrspace(1) %561, ptr addrspace(1) %562) "gc-leaf-function"="true"
  %566 = extractvalue { i64, i64 } %565, 0
  %567 = extractvalue { i64, i64 } %565, 1
  store i64 %566, ptr %ds
  store i64 %567, ptr %alloc
  br label %L274
L274:
  store i64 1, ptr %133
  br label %L279
L279:
  %568 = load i64, ptr %109
  %569 = add i64 %568, 1
  store i64 %569, ptr %134
  %570 = load i64, ptr %134
  store i64 %570, ptr %135
  %571 = load i64, ptr %135
  %572 = icmp slt i64 %571, 63
  br i1 %572, label %L282, label %L354
L354:
  %573 = load i64, ptr %135
  %574 = icmp sgt i64 %573, 63
  br i1 %574, label %L284, label %L282
L282:
  %575 = load i64, ptr %135
  store i64 %575, ptr %136
  %576 = load i64, ptr %136
  store i64 %576, ptr %109
  br label %L252
L284:
  %577 = load ptr addrspace(1), ptr %108
  store ptr addrspace(1) %577, ptr %137
  %578 = load ptr addrspace(1), ptr %137
  store ptr addrspace(1) %578, ptr %103
  %579 = load i64, ptr %14
  %580 = icmp slt i64 %579, 3
  br i1 %580, label %L340, label %L355
L355:
  %581 = load i64, ptr %14
  %582 = icmp sgt i64 %581, 3
  br i1 %582, label %L291, label %L291
L291:
  %583 = load i64, ptr %14
  %584 = ashr i64 %583, 1
  store i64 %584, ptr %138
  %585 = load i64, ptr %138
  store i64 %585, ptr %139
  store i64 1, ptr %144
  store i64 1, ptr %145
  %586 = load i64, ptr %144
  store i64 %586, ptr %140
  %587 = load i64, ptr %145
  store i64 %587, ptr %141
  br label %L297
L297:
  %588 = load i64, ptr %140
  %589 = shl i64 %588, 1
  %590 = add i64 1, %589
  store i64 %590, ptr %147
  %591 = load i64, ptr %147
  store i64 %591, ptr %148
  %592 = load i64, ptr %13
  %593 = icmp slt i64 %592, 3
  br i1 %593, label %L327, label %L356
L356:
  %594 = load i64, ptr %13
  %595 = icmp sgt i64 %594, 3
  br i1 %595, label %L303, label %L303
L303:
  %596 = load i64, ptr %13
  %597 = ashr i64 %596, 1
  store i64 %597, ptr %150
  %598 = load i64, ptr %150
  store i64 %598, ptr %151
  store i64 1, ptr %155
  %599 = load i64, ptr %141
  store i64 %599, ptr %156
  %600 = load i64, ptr %155
  store i64 %600, ptr %152
  %601 = load i64, ptr %156
  store i64 %601, ptr %153
  br label %L309
L309:
  %602 = load i64, ptr %152
  %603 = shl i64 %602, 1
  %604 = load i64, ptr %148
  %605 = add i64 %604, %603
  store i64 %605, ptr %157
  %606 = load i64, ptr %157
  %607 = and i64 %606, 127
  store i64 %607, ptr %158
  %608 = load i64, ptr %158
  %609 = shl i64 %608, 2
  store i64 %609, ptr %159
  %610 = load ptr addrspace(1), ptr %103
  %611 = load i64, ptr %159
  %612 = getelementptr i8, ptr addrspace(1) %610, i64 %611
  store ptr addrspace(1) %612, ptr %160
  %613 = load ptr addrspace(1), ptr %160
  %614 = getelementptr i8, ptr addrspace(1) %613, i64 -4
  store ptr addrspace(1) %614, ptr %161
  %615 = load ptr addrspace(1), ptr %161
  %616 = load ptr addrspace(1), ptr addrspace(1) %615
  store ptr addrspace(1) %616, ptr %162
  %617 = load ptr addrspace(1), ptr %162
  store ptr addrspace(1) %617, ptr %7
  %618 = load ptr addrspace(1), ptr %15
  store ptr addrspace(1) %618, ptr %9
  %619 = load ptr addrspace(1), ptr %7
  %620 = load ptr addrspace(1), ptr %9
  %621 = load i64, ptr %ds
  %622 = load i64, ptr %alloc
  %623 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlArray_binary_search_string__find_4_13_code"(i64 %621, i64 %622, ptr addrspace(1) %619, ptr addrspace(1) %620) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 33, i64 0, i64 20, i64 68, i64 0, i64 68, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026) ]
  %624 = extractvalue { { i64, i64 }, { i64 } } %623, 0, 0
  %625 = extractvalue { { i64, i64 }, { i64 } } %623, 0, 1
  store i64 %624, ptr %ds
  store i64 %625, ptr %alloc
  %626 = extractvalue { { i64, i64 }, { i64 } } %623, 1, 0
  store i64 %626, ptr %5
  br label %L311
L311:
  %627 = load i64, ptr %5
  store i64 %627, ptr %163
  %628 = load i64, ptr %163
  store i64 %628, ptr %164
  %629 = load i64, ptr %153
  %630 = load i64, ptr %164
  %631 = add i64 %629, %630
  store i64 %631, ptr %165
  %632 = load i64, ptr %165
  %633 = add i64 %632, -1
  store i64 %633, ptr %166
  %634 = load i64, ptr %166
  store i64 %634, ptr %167
  %635 = load i64, ptr %152
  %636 = add i64 %635, 1
  store i64 %636, ptr %168
  %637 = load i64, ptr %168
  store i64 %637, ptr %169
  %638 = load i64, ptr %169
  %639 = load i64, ptr %151
  %640 = icmp slt i64 %638, %639
  br i1 %640, label %L321, label %L357
L357:
  %641 = load i64, ptr %169
  %642 = load i64, ptr %151
  %643 = icmp sgt i64 %641, %642
  br i1 %643, label %L323, label %L321
L321:
  %644 = load i64, ptr %169
  store i64 %644, ptr %170
  %645 = load i64, ptr %167
  store i64 %645, ptr %171
  %646 = load i64, ptr %170
  store i64 %646, ptr %152
  %647 = load i64, ptr %171
  store i64 %647, ptr %153
  br label %L309
L323:
  %648 = load i64, ptr %167
  store i64 %648, ptr %172
  %649 = load i64, ptr %172
  store i64 %649, ptr %149
  br label %L330
L327:
  %650 = load i64, ptr %141
  store i64 %650, ptr %173
  %651 = load i64, ptr %173
  store i64 %651, ptr %149
  br label %L330
L330:
  %652 = load i64, ptr %140
  %653 = add i64 %652, 1
  store i64 %653, ptr %174
  %654 = load i64, ptr %174
  store i64 %654, ptr %175
  %655 = load i64, ptr %175
  %656 = load i64, ptr %139
  %657 = icmp slt i64 %655, %656
  br i1 %657, label %L333, label %L358
L358:
  %658 = load i64, ptr %175
  %659 = load i64, ptr %139
  %660 = icmp sgt i64 %658, %659
  br i1 %660, label %L335, label %L333
L333:
  %661 = load i64, ptr %175
  store i64 %661, ptr %176
  %662 = load i64, ptr %149
  store i64 %662, ptr %177
  %663 = load i64, ptr %176
  store i64 %663, ptr %140
  %664 = load i64, ptr %177
  store i64 %664, ptr %141
  br label %L297
L335:
  %665 = load i64, ptr %149
  store i64 %665, ptr %5
  %666 = load i64, ptr %5
  %667 = load i64, ptr %ds
  %668 = load i64, ptr %alloc
  %669 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %667, 0, 0
  %670 = insertvalue { { i64, i64 }, { i64 } } %669, i64 %668, 0, 1
  %671 = insertvalue { { i64, i64 }, { i64 } } %670, i64 %666, 1, 0
  ret { { i64, i64 }, { i64 } } %671
L340:
  store i64 1, ptr %5
  %672 = load i64, ptr %5
  %673 = load i64, ptr %ds
  %674 = load i64, ptr %alloc
  %675 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %673, 0, 0
  %676 = insertvalue { { i64, i64 }, { i64 } } %675, i64 %674, 0, 1
  %677 = insertvalue { { i64, i64 }, { i64 } } %676, i64 %672, 1, 0
  ret { { i64, i64 }, { i64 } } %677
}

define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlArray_binary_search_string__entry"(i64 %0, i64 %1) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" noinline gc "oxcaml" {
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
  br label %L366
L366:
  %80 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %80, ptr %3
  %81 = ptrtoint ptr @"\01_caml_sys_argv" to i64
  %82 = load ptr addrspace(1), ptr %3
  %83 = load i64, ptr %ds
  %84 = load i64, ptr %alloc
  %85 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %83, i64 %84, i64 %81, ptr addrspace(1) %82) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 6, i64 0, i64 18, i64 26, i64 0, i64 26, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 28, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 110) ]
  %86 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %85, 0, 0
  %87 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %85, 0, 1
  store i64 %86, ptr %ds
  store i64 %87, ptr %alloc
  %88 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %85, 1, 0
  store ptr addrspace(1) %88, ptr %3
  br label %L368
L368:
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
  br i1 %100, label %L394, label %L449
L449:
  %101 = load i64, ptr %17
  %102 = icmp sgt i64 %101, 3
  br i1 %102, label %L373, label %L394
L373:
  %103 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %103, ptr %3
  %104 = ptrtoint ptr @"\01_caml_sys_argv" to i64
  %105 = load ptr addrspace(1), ptr %3
  %106 = load i64, ptr %ds
  %107 = load i64, ptr %alloc
  %108 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %106, i64 %107, i64 %104, ptr addrspace(1) %105) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 6, i64 0, i64 50, i64 58, i64 0, i64 58, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 28, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 110) ]
  %109 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %108, 0, 0
  %110 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %108, 0, 1
  store i64 %109, ptr %ds
  store i64 %110, ptr %alloc
  %111 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %108, 1, 0
  store ptr addrspace(1) %111, ptr %3
  br label %L375
L375:
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
  br i1 %123, label %L391, label %L450
L450:
  %124 = load i64, ptr %24
  %125 = icmp ugt i64 %124, 3
  br i1 %125, label %L383, label %L391
L383:
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
  %135 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %133, i64 %134, i64 %131, ptr addrspace(1) %132) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 6, i64 0, i64 36, i64 62, i64 0, i64 62, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 28, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 110) ]
  %136 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %135, 0, 0
  %137 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %135, 0, 1
  store i64 %136, ptr %ds
  store i64 %137, ptr %alloc
  %138 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %135, 1, 0
  store ptr addrspace(1) %138, ptr %3
  br label %L385
L385:
  %139 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %139, ptr %27
  %140 = load ptr addrspace(1), ptr %27
  store ptr addrspace(1) %140, ptr %28
  %141 = load ptr addrspace(1), ptr %28
  store ptr addrspace(1) %141, ptr %29
  %142 = load ptr addrspace(1), ptr %29
  %143 = ptrtoint ptr addrspace(1) %142 to i64
  store i64 %143, ptr %10
  br label %L397
L391:
  %144 = ptrtoint ptr @"\01_camlArray_binary_search_string__block35" to i64
  store i64 %144, ptr %30
  %145 = load i64, ptr %30
  %146 = inttoptr i64 %145 to ptr addrspace(1)
  store ptr addrspace(1) %146, ptr %3
  %147 = load ptr addrspace(1), ptr %3
  %148 = ptrtoint ptr addrspace(1) %147 to i64
  call  void @llvm.aarch64.oxcaml.raise.notrace(i64 %148) 
  unreachable
L394:
  store i64 200001, ptr %32
  %149 = load i64, ptr %32
  store i64 %149, ptr %10
  br label %L397
L397:
  %150 = ptrtoint ptr @"\01_camlArray_binary_search_string" to i64
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
  br label %L399
L399:
  %165 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %165, ptr %3
  %166 = ptrtoint ptr @"\01_caml_sys_argv" to i64
  %167 = load ptr addrspace(1), ptr %3
  %168 = load i64, ptr %ds
  %169 = load i64, ptr %alloc
  %170 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %168, i64 %169, i64 %166, ptr addrspace(1) %167) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 9, i64 0, i64 18, i64 26, i64 0, i64 26, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 31, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7366002, i64 115) ]
  %171 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %170, 0, 0
  %172 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %170, 0, 1
  store i64 %171, ptr %ds
  store i64 %172, ptr %alloc
  %173 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %170, 1, 0
  store ptr addrspace(1) %173, ptr %3
  br label %L404
L404:
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
  br i1 %185, label %L430, label %L451
L451:
  %186 = load i64, ptr %43
  %187 = icmp sgt i64 %186, 5
  br i1 %187, label %L409, label %L430
L409:
  %188 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %188, ptr %3
  %189 = ptrtoint ptr @"\01_caml_sys_argv" to i64
  %190 = load ptr addrspace(1), ptr %3
  %191 = load i64, ptr %ds
  %192 = load i64, ptr %alloc
  %193 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %191, i64 %192, i64 %189, ptr addrspace(1) %190) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 9, i64 0, i64 50, i64 58, i64 0, i64 58, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 31, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7366002, i64 115) ]
  %194 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %193, 0, 0
  %195 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %193, 0, 1
  store i64 %194, ptr %ds
  store i64 %195, ptr %alloc
  %196 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %193, 1, 0
  store ptr addrspace(1) %196, ptr %3
  br label %L411
L411:
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
  br i1 %208, label %L427, label %L452
L452:
  %209 = load i64, ptr %50
  %210 = icmp ugt i64 %209, 5
  br i1 %210, label %L419, label %L427
L419:
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
  %220 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %218, i64 %219, i64 %216, ptr addrspace(1) %217) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 9, i64 0, i64 36, i64 62, i64 0, i64 62, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 31, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7366002, i64 115) ]
  %221 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %220, 0, 0
  %222 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %220, 0, 1
  store i64 %221, ptr %ds
  store i64 %222, ptr %alloc
  %223 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %220, 1, 0
  store ptr addrspace(1) %223, ptr %3
  br label %L421
L421:
  %224 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %224, ptr %53
  %225 = load ptr addrspace(1), ptr %53
  store ptr addrspace(1) %225, ptr %54
  %226 = load ptr addrspace(1), ptr %54
  store ptr addrspace(1) %226, ptr %55
  %227 = load ptr addrspace(1), ptr %55
  %228 = ptrtoint ptr addrspace(1) %227 to i64
  store i64 %228, ptr %36
  br label %L433
L427:
  %229 = ptrtoint ptr @"\01_camlArray_binary_search_string__block35" to i64
  store i64 %229, ptr %56
  %230 = load i64, ptr %56
  %231 = inttoptr i64 %230 to ptr addrspace(1)
  store ptr addrspace(1) %231, ptr %3
  %232 = load ptr addrspace(1), ptr %3
  %233 = ptrtoint ptr addrspace(1) %232 to i64
  call  void @llvm.aarch64.oxcaml.raise.notrace(i64 %233) 
  unreachable
L430:
  store i64 21, ptr %58
  %234 = load i64, ptr %58
  store i64 %234, ptr %36
  br label %L433
L433:
  %235 = ptrtoint ptr @"\01_camlArray_binary_search_string" to i64
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
  br label %L435
L435:
  %250 = load i64, ptr %ds
  %251 = add i64 %250, 40
  %252 = inttoptr i64 %251 to ptr
  %253 = load i64, ptr %252
  %254 = add i64 %253, 376
  %255 = call i64 asm sideeffect "mov $0, sp", "=r"() "gc-leaf-function"="true"
  %256 = icmp uge i64 %255, %254
  %257 = call  i1 @llvm.expect.i1(i1 %256, i1 1) 
  br i1 %257, label %L454, label %L453
L453:
  %258 = load i64, ptr %ds
  %259 = load i64, ptr %alloc
  %260 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_llvm_call_realloc_stack"(i64 %258, i64 %259, i64 34) "statepoint-id"="0" cold
  %261 = extractvalue { { i64, i64 }, {  } } %260, 0, 0
  %262 = extractvalue { { i64, i64 }, {  } } %260, 0, 1
  store i64 %261, ptr %ds
  store i64 %262, ptr %alloc
  br label %L454
L454:
  %263 = load i64, ptr %36
  store i64 %263, ptr %5
  %264 = load i64, ptr %5
  %265 = load i64, ptr %ds
  %266 = load i64, ptr %alloc
  %267 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlArray_binary_search_string__black_box_int_0_9_code"(i64 %265, i64 %266, i64 %264) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 38, i64 0, i64 45, i64 65, i64 0, i64 65, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 26, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 26478) ]
  %268 = extractvalue { { i64, i64 }, { i64 } } %267, 0, 0
  %269 = extractvalue { { i64, i64 }, { i64 } } %267, 0, 1
  store i64 %268, ptr %ds
  store i64 %269, ptr %alloc
  %270 = extractvalue { { i64, i64 }, { i64 } } %267, 1, 0
  store i64 %270, ptr %5
  br label %L437
L437:
  %271 = load i64, ptr %5
  store i64 %271, ptr %62
  %272 = load i64, ptr %62
  store i64 %272, ptr %63
  %273 = load i64, ptr %10
  store i64 %273, ptr %5
  %274 = load i64, ptr %5
  %275 = load i64, ptr %ds
  %276 = load i64, ptr %alloc
  %277 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlArray_binary_search_string__black_box_int_0_9_code"(i64 %275, i64 %276, i64 %274) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 38, i64 0, i64 27, i64 44, i64 0, i64 44, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 26, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 26478) ]
  %278 = extractvalue { { i64, i64 }, { i64 } } %277, 0, 0
  %279 = extractvalue { { i64, i64 }, { i64 } } %277, 0, 1
  store i64 %278, ptr %ds
  store i64 %279, ptr %alloc
  %280 = extractvalue { { i64, i64 }, { i64 } } %277, 1, 0
  store i64 %280, ptr %5
  br label %L438
L438:
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
  %289 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlArray_binary_search_string__run_6_15_code"(i64 %287, i64 %288, i64 %285, i64 %286) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 38, i64 0, i64 22, i64 66, i64 0, i64 66, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 26, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 26478) ]
  %290 = extractvalue { { i64, i64 }, { i64 } } %289, 0, 0
  %291 = extractvalue { { i64, i64 }, { i64 } } %289, 0, 1
  store i64 %290, ptr %ds
  store i64 %291, ptr %alloc
  %292 = extractvalue { { i64, i64 }, { i64 } } %289, 1, 0
  store i64 %292, ptr %5
  br label %L439
L439:
  %293 = load i64, ptr %5
  store i64 %293, ptr %66
  %294 = load i64, ptr %66
  store i64 %294, ptr %67
  %295 = ptrtoint ptr @"\01_camlArray_binary_search_string__const_block66" to i64
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
  %304 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlCamlinternalFormat__make_printf_120_401_code"(i64 %302, i64 %303, i64 %299, i64 %300, i64 %301) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 5, i64 27, i64 0, i64 2, i64 63, i64 0, i64 63, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 23, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 7366251, i64 7235954, i64 26228, i64 34, i64 0, i64 21, i64 43, i64 0, i64 43, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 22, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 7499878, i64 7630441, i64 102, i64 38, i64 0, i64 17, i64 35, i64 0, i64 35, i64 9, i64 6910576, i64 6714478, i64 7105838, i64 21, i64 6583379, i64 6449516, i64 5267295, i64 7235954, i64 3040884, i64 6910576, i64 6714478, i64 11, i64 0, i64 21, i64 63, i64 0, i64 63, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 39, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 6910576, i64 6255726, i64 7562610, i64 7629941, i64 38, i64 0, i64 9, i64 66, i64 0, i64 66, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 26, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 26478) ]
  %305 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %304, 0, 0
  %306 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %304, 0, 1
  store i64 %305, ptr %ds
  store i64 %306, ptr %alloc
  %307 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %304, 1, 0
  store ptr addrspace(1) %307, ptr %3
  br label %L440
L440:
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
  %322 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } %321(i64 %318, i64 %319, i64 %316, ptr addrspace(1) %317) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 2, i64 11, i64 0, i64 21, i64 63, i64 0, i64 63, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 39, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 6910576, i64 6255726, i64 7562610, i64 7629941, i64 38, i64 0, i64 9, i64 66, i64 0, i64 66, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 26, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 26478) ]
  %323 = extractvalue { { i64, i64 }, { i64 } } %322, 0, 0
  %324 = extractvalue { { i64, i64 }, { i64 } } %322, 0, 1
  store i64 %323, ptr %ds
  store i64 %324, ptr %alloc
  %325 = extractvalue { { i64, i64 }, { i64 } } %322, 1, 0
  store i64 %325, ptr %5
  br label %L441
L441:
  %326 = load i64, ptr %5
  store i64 %326, ptr %75
  %327 = load i64, ptr %75
  store i64 %327, ptr %76
  %328 = ptrtoint ptr @"\01_camlArray_binary_search_string" to i64
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

@"\01_camlArray_binary_search_string__gc_roots" = global { ptr, i64 } { ptr @"\01_camlArray_binary_search_string", i64 0 }, section "__DATA,__data", align 8
@"\01_header.camlArray_binary_search_string" = global i64 8960, section "__DATA,__data", align 8
@"\01_camlArray_binary_search_string" = global { ptr, ptr, ptr, i64, i64, ptr, ptr, ptr } { ptr @"\01_camlArray_binary_search_string__black_box_int_9", ptr @"\01_camlArray_binary_search_string__black_box_string_10", ptr @"\01_camlArray_binary_search_string__black_box_11", i64 1, i64 1, ptr @"\01_camlArray_binary_search_string__print_result_12", ptr @"\01_camlArray_binary_search_string__find_13", ptr @"\01_camlArray_binary_search_string__run_14" }, section "__DATA,__data", align 8
@"\01_header.camlArray_binary_search_string__run_14" = global i64 4087, section "__DATA,__data", align 8
@"\01_camlArray_binary_search_string__run_14" = global { ptr, i64, ptr } { ptr @"\01_caml_curry2", i64 180143985094819847, ptr @"\01_camlArray_binary_search_string__run_6_15_code" }, section "__DATA,__data", align 8
@"\01_header.camlArray_binary_search_string__invalid776" = global i64 16380, section "__DATA,__data", align 8
@"\01_camlArray_binary_search_string__invalid776" = global { [ 117 x i8 ], [ 2 x i8 ], i8 } { [ 117 x i8 ] c"\28\44\65\66\69\6e\69\6e\67\5f\65\78\70\72\5f\6f\66\5f\6c\65\74\20\28\62\6f\75\6e\64\5f\70\61\74\74\65\72\6e\20\70\72\69\6d\2f\34\35\32\4e\29\0a\20\28\64\65\66\69\6e\69\6e\67\5f\65\78\70\72\20\28\28\55\6e\62\6f\78\5f\66\6c\6f\61\74\20\61\70\70\6c\79\5f\72\65\73\75\6c\74\2f\34\33\35\4e\29\20\61\72\72\61\79\2e\6d\6c\3a\38\37\2c\35\2d\2d\32\37\29\29\29", [ 2 x i8 ] zeroinitializer, i8 2 }, section "__DATA,__data", align 8
@"\01_header.camlArray_binary_search_string__find_13" = global i64 4087, section "__DATA,__data", align 8
@"\01_camlArray_binary_search_string__find_13" = global { ptr, i64, ptr } { ptr @"\01_caml_curry2", i64 180143985094819847, ptr @"\01_camlArray_binary_search_string__find_4_13_code" }, section "__DATA,__data", align 8
@"\01_header.camlArray_binary_search_string__print_result_12" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlArray_binary_search_string__print_result_12" = global { ptr, i64 } { ptr @"\01_camlArray_binary_search_string__print_result_3_12_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlArray_binary_search_string__black_box_11" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlArray_binary_search_string__black_box_11" = global { ptr, i64 } { ptr @"\01_camlArray_binary_search_string__black_box_2_11_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlArray_binary_search_string__black_box_string_10" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlArray_binary_search_string__black_box_string_10" = global { ptr, i64 } { ptr @"\01_camlArray_binary_search_string__black_box_string_1_10_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlArray_binary_search_string__black_box_int_9" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlArray_binary_search_string__black_box_int_9" = global { ptr, i64 } { ptr @"\01_camlArray_binary_search_string__black_box_int_0_9_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlArray_binary_search_string__block35" = global i64 2816, section "__DATA,__data", align 8
@"\01_camlArray_binary_search_string__block35" = global { ptr, ptr } { ptr @"\01_caml_exn_Invalid_argument", ptr @"\01_camlArray_binary_search_string__string33" }, section "__DATA,__data", align 8
@"\01_header.camlArray_binary_search_string__string33" = global i64 4092, section "__DATA,__data", align 8
@"\01_camlArray_binary_search_string__string33" = global { [ 19 x i8 ], [ 4 x i8 ], i8 } { [ 19 x i8 ] c"\69\6e\64\65\78\20\6f\75\74\20\6f\66\20\62\6f\75\6e\64\73", [ 4 x i8 ] zeroinitializer, i8 4 }, section "__DATA,__data", align 8
@"\01_header.camlArray_binary_search_string__immstring129" = global i64 4092, section "__DATA,__data", align 8
@"\01_camlArray_binary_search_string__immstring129" = global { [ 17 x i8 ], [ 6 x i8 ], i8 } { [ 17 x i8 ] c"\61\72\72\61\79\5f\62\69\6e\61\72\79\5f\6b\65\79\5f", [ 6 x i8 ] zeroinitializer, i8 6 }, section "__DATA,__data", align 8
@"\01_header.camlArray_binary_search_string__const_block66" = global i64 4868, section "__DATA,__data", align 8
@"\01_camlArray_binary_search_string__const_block66" = global { i64, i64, i64, ptr } { i64 1, i64 1, i64 1, ptr @"\01_camlArray_binary_search_string__const_block64" }, section "__DATA,__data", align 8
@"\01_header.camlArray_binary_search_string__const_block64" = global i64 2828, section "__DATA,__data", align 8
@"\01_camlArray_binary_search_string__const_block64" = global { i64, ptr } { i64 21, ptr @"\01_camlArray_binary_search_string__const_block62" }, section "__DATA,__data", align 8
@"\01_header.camlArray_binary_search_string__const_block62" = global i64 1802, section "__DATA,__data", align 8
@"\01_camlArray_binary_search_string__const_block62" = global { i64 } { i64 1 }, section "__DATA,__data", align 8
@"\01_camlCamlinternalFormat__make_printf_120_401_code" = external global ptr
@"\01_camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31" = external global ptr
@"\01_camlStdlib__immstring191" = external global ptr
@"\01_caml_array_make" = external global ptr
@"\01_caml_blit_string" = external global ptr
@"\01_caml_c_call" = external global ptr
@"\01_caml_call_gc" = external global ptr
@"\01_caml_create_bytes" = external global ptr
@"\01_caml_curry2" = external global ptr
@"\01_caml_exn_Invalid_argument" = external global ptr
@"\01_caml_flambda2_invalid" = external global ptr
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


!0 = !{ i32 1, !"oxcaml_module", !"Array_binary_search_string" }
!llvm.module.flags = !{ !0 }
