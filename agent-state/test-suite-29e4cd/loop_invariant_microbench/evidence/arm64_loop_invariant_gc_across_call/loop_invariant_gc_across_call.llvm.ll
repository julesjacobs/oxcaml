source_filename = "/Users/julesjacobs/git/oxcaml-llvm/agents/test-suite-29e4cd/oxcaml/agent-state/test-suite-29e4cd/loop_invariant_microbench/src/loop_invariant_gc_across_call.ml"

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlLoop_invariant_gc_across_call__tick_0_4_code"(i64 %0, i64 %1, i64 %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %4 = alloca i64 
  store i64 %2, ptr %4
  %5 = alloca ptr addrspace(1) 
  %6 = alloca i64 
  %7 = alloca i64 
  %8 = alloca ptr addrspace(1) 
  br label %L1
L1:
  br label %L101
L101:
  %9 = load i64, ptr %4
  store i64 %9, ptr %6
  %10 = load i64, ptr %6
  %11 = add i64 %10, 2
  store i64 %11, ptr %7
  %12 = load i64, ptr %7
  %13 = call i64 asm  "", "=r,0"(i64 %12) "gc-leaf-function"="true"
  store i64 %13, ptr %7
  %14 = load i64, ptr %7
  %15 = inttoptr i64 %14 to ptr addrspace(1)
  store ptr addrspace(1) %15, ptr %8
  %16 = load ptr addrspace(1), ptr %8
  store ptr addrspace(1) %16, ptr %5
  %17 = load ptr addrspace(1), ptr %5
  %18 = ptrtoint ptr addrspace(1) %17 to i64
  %19 = load i64, ptr %ds
  %20 = load i64, ptr %alloc
  %21 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %19, 0, 0
  %22 = insertvalue { { i64, i64 }, { i64 } } %21, i64 %20, 0, 1
  %23 = insertvalue { { i64, i64 }, { i64 } } %22, i64 %18, 1, 0
  ret { { i64, i64 }, { i64 } } %23
}

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlLoop_invariant_gc_across_call__print_result_1_5_code"(i64 %0, i64 %1, i64 %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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
  %9 = alloca i64 
  %10 = alloca i64 
  %11 = alloca ptr addrspace(1) 
  %12 = alloca ptr addrspace(1) 
  %13 = alloca ptr addrspace(1) 
  %14 = alloca i64 
  %15 = alloca i64 
  %16 = alloca i64 
  %17 = alloca i64 
  %18 = alloca i64 
  %19 = alloca i64 
  %20 = alloca ptr addrspace(1) 
  %21 = alloca i64 
  %22 = alloca i64 
  %23 = alloca i64 
  %24 = alloca i64 
  %25 = alloca i64 
  %26 = alloca i64 
  %27 = alloca i64 
  %28 = alloca ptr addrspace(1) 
  %29 = alloca ptr addrspace(1) 
  %30 = alloca ptr addrspace(1) 
  %31 = alloca i64 
  %32 = alloca i64 
  %33 = alloca i64 
  %34 = alloca ptr addrspace(1) 
  %35 = alloca ptr addrspace(1) 
  %36 = alloca ptr addrspace(1) 
  %37 = alloca i64 
  %38 = alloca i64 
  %39 = alloca ptr addrspace(1) 
  %40 = alloca ptr addrspace(1) 
  br label %L1
L1:
  br label %L105
L105:
  %41 = load i64, ptr %4
  store i64 %41, ptr %9
  %42 = ptrtoint ptr @"\01_camlStdlib__immstring191" to i64
  store i64 %42, ptr %10
  %43 = load i64, ptr %10
  %44 = inttoptr i64 %43 to ptr addrspace(1)
  store ptr addrspace(1) %44, ptr %5
  %45 = load i64, ptr %9
  %46 = inttoptr i64 %45 to ptr addrspace(1)
  store ptr addrspace(1) %46, ptr %6
  %47 = ptrtoint ptr @"\01_caml_format_int" to i64
  %48 = load ptr addrspace(1), ptr %5
  %49 = load ptr addrspace(1), ptr %6
  %50 = load i64, ptr %ds
  %51 = load i64, ptr %alloc
  %52 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %50, i64 %51, i64 %47, ptr addrspace(1) %48, ptr addrspace(1) %49) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 3, i64 280, i64 0, i64 2, i64 19, i64 0, i64 19, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7631662, i64 7235954, i64 7298919, i64 6905702, i64 29806, i64 507, i64 0, i64 39, i64 56, i64 0, i64 56, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 16, i64 6583379, i64 6449516, i64 7499822, i64 7630441, i64 7235935, i64 116, i64 5, i64 0, i64 2, i64 13, i64 0, i64 13, i64 159, i64 7558447, i64 7565925, i64 7694895, i64 7562604, i64 6513002, i64 7561839, i64 6907695, i64 7286644, i64 6382456, i64 2976877, i64 7761004, i64 6369133, i64 7234919, i64 3109748, i64 7562612, i64 7548276, i64 7629173, i64 3288421, i64 3433785, i64 3105891, i64 6518895, i64 7105889, i64 6775087, i64 7630437, i64 7631661, i64 6648929, i64 6648879, i64 2978931, i64 6911347, i64 2975092, i64 6633778, i64 6578996, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6909279, i64 7303779, i64 7234914, i64 3106915, i64 6517363, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6514527, i64 6512991, i64 7565170, i64 6512499, i64 7105633, i64 7105838, i64 42, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 6910576, i64 6255726, i64 7562610, i64 7629941) ]
  %53 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %52, 0, 0
  %54 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %52, 0, 1
  store i64 %53, ptr %ds
  store i64 %54, ptr %alloc
  %55 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %52, 1, 0
  store ptr addrspace(1) %55, ptr %5
  br label %L107
L107:
  %56 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %56, ptr %11
  %57 = load ptr addrspace(1), ptr %11
  store ptr addrspace(1) %57, ptr %12
  %58 = load ptr addrspace(1), ptr %12
  %59 = getelementptr i8, ptr addrspace(1) %58, i64 -8
  store ptr addrspace(1) %59, ptr %13
  %60 = load ptr addrspace(1), ptr %13
  %61 = load i64, ptr addrspace(1) %60
  store i64 %61, ptr %14
  %62 = load i64, ptr %14
  %63 = shl i64 %62, 8
  store i64 %63, ptr %15
  %64 = load i64, ptr %15
  %65 = lshr i64 %64, 18
  store i64 %65, ptr %16
  %66 = load i64, ptr %16
  %67 = shl i64 %66, 3
  store i64 %67, ptr %17
  %68 = load i64, ptr %17
  %69 = sub i64 %68, 1
  store i64 %69, ptr %18
  %70 = load i64, ptr %18
  store i64 %70, ptr %19
  %71 = load ptr addrspace(1), ptr %12
  %72 = load i64, ptr %19
  %73 = getelementptr i8, ptr addrspace(1) %71, i64 %72
  store ptr addrspace(1) %73, ptr %20
  %74 = load ptr addrspace(1), ptr %20
  %75 = load i8, ptr addrspace(1) %74
  %76 = zext i8 %75 to i64
  store i64 %76, ptr %21
  %77 = load i64, ptr %19
  %78 = load i64, ptr %21
  %79 = sub i64 %77, %78
  store i64 %79, ptr %22
  %80 = load i64, ptr %22
  %81 = shl i64 %80, 1
  %82 = add i64 1, %81
  store i64 %82, ptr %24
  %83 = ptrtoint ptr @"\01_camlStdlib__print_int_136" to i64
  store i64 %83, ptr %26
  %84 = load i64, ptr %26
  %85 = add i64 %84, 16
  store i64 %85, ptr %27
  %86 = load i64, ptr %27
  %87 = inttoptr i64 %86 to ptr
  %88 = load ptr addrspace(1), ptr %87
  store ptr addrspace(1) %88, ptr %28
  %89 = load ptr addrspace(1), ptr %28
  store ptr addrspace(1) %89, ptr %5
  %90 = load ptr addrspace(1), ptr %12
  store ptr addrspace(1) %90, ptr %6
  %91 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %91, ptr %7
  %92 = load i64, ptr %24
  %93 = inttoptr i64 %92 to ptr addrspace(1)
  store ptr addrspace(1) %93, ptr %8
  %94 = ptrtoint ptr @"\01_caml_ml_output" to i64
  %95 = load ptr addrspace(1), ptr %5
  %96 = load ptr addrspace(1), ptr %6
  %97 = load ptr addrspace(1), ptr %7
  %98 = load ptr addrspace(1), ptr %8
  %99 = load i64, ptr %ds
  %100 = load i64, ptr %alloc
  %101 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %99, i64 %100, i64 %94, ptr addrspace(1) %95, ptr addrspace(1) %96, ptr addrspace(1) %97, ptr addrspace(1) %98) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 3, i64 387, i64 0, i64 2, i64 47, i64 0, i64 47, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7696174, i64 7696500, i64 7561076, i64 6910580, i64 26478, i64 507, i64 0, i64 18, i64 56, i64 0, i64 56, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 16, i64 6583379, i64 6449516, i64 7499822, i64 7630441, i64 7235935, i64 116, i64 5, i64 0, i64 2, i64 13, i64 0, i64 13, i64 159, i64 7558447, i64 7565925, i64 7694895, i64 7562604, i64 6513002, i64 7561839, i64 6907695, i64 7286644, i64 6382456, i64 2976877, i64 7761004, i64 6369133, i64 7234919, i64 3109748, i64 7562612, i64 7548276, i64 7629173, i64 3288421, i64 3433785, i64 3105891, i64 6518895, i64 7105889, i64 6775087, i64 7630437, i64 7631661, i64 6648929, i64 6648879, i64 2978931, i64 6911347, i64 2975092, i64 6633778, i64 6578996, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6909279, i64 7303779, i64 7234914, i64 3106915, i64 6517363, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6514527, i64 6512991, i64 7565170, i64 6512499, i64 7105633, i64 7105838, i64 42, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 6910576, i64 6255726, i64 7562610, i64 7629941) ]
  %102 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %101, 0, 0
  %103 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %101, 0, 1
  store i64 %102, ptr %ds
  store i64 %103, ptr %alloc
  %104 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %101, 1, 0
  store ptr addrspace(1) %104, ptr %5
  br label %L108
L108:
  %105 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %105, ptr %29
  %106 = load ptr addrspace(1), ptr %29
  store ptr addrspace(1) %106, ptr %30
  %107 = ptrtoint ptr @"\01_camlStdlib__print_newline_139" to i64
  store i64 %107, ptr %32
  %108 = load i64, ptr %32
  %109 = add i64 %108, 16
  store i64 %109, ptr %33
  %110 = load i64, ptr %33
  %111 = inttoptr i64 %110 to ptr
  %112 = load ptr addrspace(1), ptr %111
  store ptr addrspace(1) %112, ptr %34
  %113 = load ptr addrspace(1), ptr %34
  store ptr addrspace(1) %113, ptr %5
  %114 = inttoptr i64 21 to ptr addrspace(1)
  store ptr addrspace(1) %114, ptr %6
  %115 = ptrtoint ptr @"\01_caml_ml_output_char" to i64
  %116 = load ptr addrspace(1), ptr %5
  %117 = load ptr addrspace(1), ptr %6
  %118 = load i64, ptr %ds
  %119 = load i64, ptr %alloc
  %120 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %118, i64 %119, i64 %115, ptr addrspace(1) %116, ptr addrspace(1) %117) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 511, i64 0, i64 23, i64 46, i64 0, i64 46, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7499822, i64 7630441, i64 6647391, i64 6909047, i64 25966, i64 6, i64 0, i64 2, i64 18, i64 0, i64 18, i64 159, i64 7558447, i64 7565925, i64 7694895, i64 7562604, i64 6513002, i64 7561839, i64 6907695, i64 7286644, i64 6382456, i64 2976877, i64 7761004, i64 6369133, i64 7234919, i64 3109748, i64 7562612, i64 7548276, i64 7629173, i64 3288421, i64 3433785, i64 3105891, i64 6518895, i64 7105889, i64 6775087, i64 7630437, i64 7631661, i64 6648929, i64 6648879, i64 2978931, i64 6911347, i64 2975092, i64 6633778, i64 6578996, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6909279, i64 7303779, i64 7234914, i64 3106915, i64 6517363, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6514527, i64 6512991, i64 7565170, i64 6512499, i64 7105633, i64 7105838, i64 42, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 6910576, i64 6255726, i64 7562610, i64 7629941) ]
  %121 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %120, 0, 0
  %122 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %120, 0, 1
  store i64 %121, ptr %ds
  store i64 %122, ptr %alloc
  %123 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %120, 1, 0
  store ptr addrspace(1) %123, ptr %5
  br label %L121
L121:
  %124 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %124, ptr %35
  %125 = load ptr addrspace(1), ptr %35
  store ptr addrspace(1) %125, ptr %36
  %126 = ptrtoint ptr @"\01_camlStdlib__print_newline_139" to i64
  store i64 %126, ptr %37
  %127 = load i64, ptr %37
  %128 = add i64 %127, 16
  store i64 %128, ptr %38
  %129 = load i64, ptr %38
  %130 = inttoptr i64 %129 to ptr
  %131 = load ptr addrspace(1), ptr %130
  store ptr addrspace(1) %131, ptr %39
  %132 = load ptr addrspace(1), ptr %39
  store ptr addrspace(1) %132, ptr %5
  %133 = ptrtoint ptr @"\01_caml_ml_flush" to i64
  %134 = load ptr addrspace(1), ptr %5
  %135 = load i64, ptr %ds
  %136 = load i64, ptr %alloc
  %137 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %135, i64 %136, i64 %133, ptr addrspace(1) %134) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 511, i64 0, i64 48, i64 60, i64 0, i64 60, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7499822, i64 7630441, i64 6647391, i64 6909047, i64 25966, i64 6, i64 0, i64 2, i64 18, i64 0, i64 18, i64 159, i64 7558447, i64 7565925, i64 7694895, i64 7562604, i64 6513002, i64 7561839, i64 6907695, i64 7286644, i64 6382456, i64 2976877, i64 7761004, i64 6369133, i64 7234919, i64 3109748, i64 7562612, i64 7548276, i64 7629173, i64 3288421, i64 3433785, i64 3105891, i64 6518895, i64 7105889, i64 6775087, i64 7630437, i64 7631661, i64 6648929, i64 6648879, i64 2978931, i64 6911347, i64 2975092, i64 6633778, i64 6578996, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6909279, i64 7303779, i64 7234914, i64 3106915, i64 6517363, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6514527, i64 6512991, i64 7565170, i64 6512499, i64 7105633, i64 7105838, i64 42, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 6910576, i64 6255726, i64 7562610, i64 7629941) ]
  %138 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %137, 0, 0
  %139 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %137, 0, 1
  store i64 %138, ptr %ds
  store i64 %139, ptr %alloc
  %140 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %137, 1, 0
  store ptr addrspace(1) %140, ptr %5
  br label %L124
L124:
  %141 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %141, ptr %40
  %142 = load ptr addrspace(1), ptr %40
  store ptr addrspace(1) %142, ptr %5
  %143 = load ptr addrspace(1), ptr %5
  %144 = ptrtoint ptr addrspace(1) %143 to i64
  %145 = load i64, ptr %ds
  %146 = load i64, ptr %alloc
  %147 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %145, 0, 0
  %148 = insertvalue { { i64, i64 }, { i64 } } %147, i64 %146, 0, 1
  %149 = insertvalue { { i64, i64 }, { i64 } } %148, i64 %144, 1, 0
  ret { { i64, i64 }, { i64 } } %149
}

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlLoop_invariant_gc_across_call__loop_2_6_code"(i64 %0, i64 %1, ptr addrspace(1) %2, i64 %3, i64 %4) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %6 = alloca ptr addrspace(1) 
  store ptr addrspace(1) %2, ptr %6
  %7 = alloca i64 
  store i64 %3, ptr %7
  %8 = alloca i64 
  store i64 %4, ptr %8
  %9 = alloca i64 
  %10 = alloca ptr addrspace(1) 
  %11 = alloca i64 
  %12 = alloca i64 
  %13 = alloca i64 
  %14 = alloca i64 
  %15 = alloca i64 
  %16 = alloca i64 
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
  %27 = alloca i64 
  %28 = alloca i64 
  %29 = alloca i64 
  %30 = alloca i64 
  %31 = alloca i64 
  %32 = alloca i64 
  %33 = alloca i64 
  %34 = alloca i64 
  br label %L1
L1:
  br label %L128
L128:
  %35 = load i64, ptr %ds
  %36 = add i64 %35, 40
  %37 = inttoptr i64 %36 to ptr
  %38 = load i64, ptr %37
  %39 = add i64 %38, 376
  %40 = call  i64 @llvm.read_register.i64(metadata !{!"sp\00"}) 
  %41 = icmp uge i64 %40, %39
  %42 = call  i1 @llvm.expect.i1(i1 %41, i1 1) 
  br i1 %42, label %L156, label %L155
L155:
  %43 = load i64, ptr %ds
  %44 = load i64, ptr %alloc
  %45 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_llvm_call_realloc_stack"(i64 %43, i64 %44, i64 34) "statepoint-id"="0" cold
  %46 = extractvalue { { i64, i64 }, {  } } %45, 0, 0
  %47 = extractvalue { { i64, i64 }, {  } } %45, 0, 1
  store i64 %46, ptr %ds
  store i64 %47, ptr %alloc
  br label %L156
L156:
  %48 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %48, ptr %10
  %49 = load i64, ptr %7
  store i64 %49, ptr %11
  %50 = load i64, ptr %8
  store i64 %50, ptr %12
  %51 = load i64, ptr %11
  store i64 %51, ptr %15
  %52 = load i64, ptr %12
  store i64 %52, ptr %16
  %53 = load i64, ptr %15
  store i64 %53, ptr %13
  %54 = load i64, ptr %16
  store i64 %54, ptr %14
  %55 = load i64, ptr %13
  %56 = icmp slt i64 %55, 1
  br i1 %56, label %L135, label %L157
L157:
  %57 = load i64, ptr %13
  %58 = icmp sgt i64 %57, 1
  br i1 %58, label %L137, label %L135
L135:
  %59 = load i64, ptr %14
  store i64 %59, ptr %9
  %60 = load i64, ptr %9
  %61 = load i64, ptr %ds
  %62 = load i64, ptr %alloc
  %63 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %61, 0, 0
  %64 = insertvalue { { i64, i64 }, { i64 } } %63, i64 %62, 0, 1
  %65 = insertvalue { { i64, i64 }, { i64 } } %64, i64 %60, 1, 0
  ret { { i64, i64 }, { i64 } } %65
L137:
  %66 = load i64, ptr %13
  store i64 %66, ptr %9
  %67 = load i64, ptr %9
  %68 = load i64, ptr %ds
  %69 = load i64, ptr %alloc
  %70 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlLoop_invariant_gc_across_call__tick_0_4_code"(i64 %68, i64 %69, i64 %67) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 12, i64 0, i64 12, i64 18, i64 0, i64 18, i64 159, i64 7558447, i64 7565925, i64 7694895, i64 7562604, i64 6513002, i64 7561839, i64 6907695, i64 7286644, i64 6382456, i64 2976877, i64 7761004, i64 6369133, i64 7234919, i64 3109748, i64 7562612, i64 7548276, i64 7629173, i64 3288421, i64 3433785, i64 3105891, i64 6518895, i64 7105889, i64 6775087, i64 7630437, i64 7631661, i64 6648929, i64 6648879, i64 2978931, i64 6911347, i64 2975092, i64 6633778, i64 6578996, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6909279, i64 7303779, i64 7234914, i64 3106915, i64 6517363, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6514527, i64 6512991, i64 7565170, i64 6512499, i64 7105633, i64 7105838, i64 34, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 7303020, i64 112) ]
  %71 = extractvalue { { i64, i64 }, { i64 } } %70, 0, 0
  %72 = extractvalue { { i64, i64 }, { i64 } } %70, 0, 1
  store i64 %71, ptr %ds
  store i64 %72, ptr %alloc
  %73 = extractvalue { { i64, i64 }, { i64 } } %70, 1, 0
  store i64 %73, ptr %9
  br label %L139
L139:
  %74 = load i64, ptr %9
  store i64 %74, ptr %17
  %75 = load i64, ptr %17
  store i64 %75, ptr %18
  %76 = load ptr addrspace(1), ptr %10
  %77 = getelementptr i8, ptr addrspace(1) %76, i64 -8
  store ptr addrspace(1) %77, ptr %19
  %78 = load ptr addrspace(1), ptr %19
  %79 = load i64, ptr addrspace(1) %78
  store i64 %79, ptr %20
  %80 = load i64, ptr %20
  %81 = shl i64 %80, 8
  store i64 %81, ptr %21
  %82 = load i64, ptr %21
  %83 = lshr i64 %82, 18
  store i64 %83, ptr %22
  %84 = load i64, ptr %22
  %85 = shl i64 %84, 3
  store i64 %85, ptr %23
  %86 = load i64, ptr %23
  %87 = sub i64 %86, 1
  store i64 %87, ptr %24
  %88 = load i64, ptr %24
  store i64 %88, ptr %25
  %89 = load ptr addrspace(1), ptr %10
  %90 = load i64, ptr %25
  %91 = getelementptr i8, ptr addrspace(1) %89, i64 %90
  store ptr addrspace(1) %91, ptr %26
  %92 = load ptr addrspace(1), ptr %26
  %93 = load i8, ptr addrspace(1) %92
  %94 = zext i8 %93 to i64
  store i64 %94, ptr %27
  %95 = load i64, ptr %25
  %96 = load i64, ptr %27
  %97 = sub i64 %95, %96
  store i64 %97, ptr %28
  %98 = load i64, ptr %28
  %99 = shl i64 %98, 1
  %100 = load i64, ptr %14
  %101 = add i64 %100, %99
  store i64 %101, ptr %29
  %102 = load i64, ptr %29
  %103 = load i64, ptr %18
  %104 = add i64 %102, %103
  store i64 %104, ptr %30
  %105 = load i64, ptr %30
  %106 = add i64 %105, -1
  store i64 %106, ptr %31
  %107 = load i64, ptr %13
  %108 = add i64 %107, -2
  store i64 %108, ptr %32
  %109 = load i64, ptr %32
  store i64 %109, ptr %33
  %110 = load i64, ptr %31
  store i64 %110, ptr %34
  %111 = load i64, ptr %33
  store i64 %111, ptr %13
  %112 = load i64, ptr %34
  store i64 %112, ptr %14
  %113 = load i64, ptr %13
  %114 = icmp slt i64 %113, 1
  br i1 %114, label %L135, label %L158
L158:
  %115 = load i64, ptr %13
  %116 = icmp sgt i64 %115, 1
  br i1 %116, label %L137, label %L135
}

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlLoop_invariant_gc_across_call__run_3_7_code"(i64 %0, i64 %1, i64 %2, i64 %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" noinline gc "oxcaml" {
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
  %10 = alloca ptr addrspace(1) 
  %11 = alloca ptr addrspace(1) 
  %12 = alloca i64 
  %13 = alloca i64 
  %14 = alloca i64 
  %15 = alloca i64 
  %16 = alloca i64 
  %17 = alloca i64 
  %18 = alloca i64 
  %19 = alloca i64 
  %20 = alloca ptr addrspace(1) 
  %21 = alloca i64 
  %22 = alloca i64 
  %23 = alloca i64 
  %24 = alloca i64 
  %25 = alloca i64 
  %26 = alloca i64 
  %27 = alloca i64 
  %28 = alloca i64 
  %29 = alloca ptr addrspace(1) 
  %30 = alloca i64 
  %31 = alloca i64 
  %32 = alloca i64 
  %33 = alloca i64 
  %34 = alloca i64 
  %35 = alloca i64 
  %36 = alloca ptr addrspace(1) 
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
  %48 = alloca i64 
  %49 = alloca i64 
  %50 = alloca i64 
  %51 = alloca i64 
  %52 = alloca i64 
  br label %L1
L1:
  br label %L160
L160:
  %53 = load i64, ptr %5
  store i64 %53, ptr %7
  %54 = load i64, ptr %6
  store i64 %54, ptr %8
  %55 = ptrtoint ptr @"\01_camlLoop_invariant_gc_across_call__immstring48" to i64
  store i64 %55, ptr %9
  %56 = load i64, ptr %9
  %57 = call i64 asm  "", "=r,0"(i64 %56) "gc-leaf-function"="true"
  store i64 %57, ptr %9
  %58 = load i64, ptr %9
  %59 = inttoptr i64 %58 to ptr addrspace(1)
  store ptr addrspace(1) %59, ptr %10
  %60 = load ptr addrspace(1), ptr %10
  store ptr addrspace(1) %60, ptr %11
  %61 = load i64, ptr %8
  %62 = icmp slt i64 %61, 3
  br i1 %62, label %L210, label %L213
L213:
  %63 = load i64, ptr %8
  %64 = icmp sgt i64 %63, 3
  br i1 %64, label %L162, label %L162
L162:
  %65 = load i64, ptr %ds
  %66 = add i64 %65, 40
  %67 = inttoptr i64 %66 to ptr
  %68 = load i64, ptr %67
  %69 = add i64 %68, 376
  %70 = call  i64 @llvm.read_register.i64(metadata !{!"sp\00"}) 
  %71 = icmp uge i64 %70, %69
  %72 = call  i1 @llvm.expect.i1(i1 %71, i1 1) 
  br i1 %72, label %L215, label %L214
L214:
  %73 = load i64, ptr %ds
  %74 = load i64, ptr %alloc
  %75 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_llvm_call_realloc_stack"(i64 %73, i64 %74, i64 34) "statepoint-id"="0" cold
  %76 = extractvalue { { i64, i64 }, {  } } %75, 0, 0
  %77 = extractvalue { { i64, i64 }, {  } } %75, 0, 1
  store i64 %76, ptr %ds
  store i64 %77, ptr %alloc
  br label %L215
L215:
  %78 = load i64, ptr %8
  %79 = ashr i64 %78, 1
  store i64 %79, ptr %12
  %80 = load i64, ptr %12
  store i64 %80, ptr %13
  store i64 1, ptr %18
  store i64 1, ptr %19
  %81 = load i64, ptr %18
  store i64 %81, ptr %14
  %82 = load i64, ptr %19
  store i64 %82, ptr %15
  br label %L174
L174:
  %83 = load i64, ptr %7
  store i64 %83, ptr %24
  store i64 1, ptr %25
  %84 = load i64, ptr %24
  store i64 %84, ptr %21
  %85 = load i64, ptr %25
  store i64 %85, ptr %22
  %86 = load i64, ptr %21
  %87 = icmp slt i64 %86, 1
  br i1 %87, label %L178, label %L216
L216:
  %88 = load i64, ptr %21
  %89 = icmp sgt i64 %88, 1
  br i1 %89, label %L180, label %L178
L178:
  %90 = load i64, ptr %22
  store i64 %90, ptr %26
  %91 = load i64, ptr %26
  %92 = inttoptr i64 %91 to ptr addrspace(1)
  store ptr addrspace(1) %92, ptr %20
  %93 = load i64, ptr %15
  %94 = load ptr addrspace(1), ptr %20
  %95 = ptrtoint ptr addrspace(1) %94 to i64
  %96 = add i64 %93, %95
  store i64 %96, ptr %45
  %97 = load i64, ptr %45
  %98 = add i64 %97, -1
  store i64 %98, ptr %46
  %99 = load i64, ptr %46
  store i64 %99, ptr %47
  %100 = load i64, ptr %14
  %101 = add i64 %100, 1
  store i64 %101, ptr %48
  %102 = load i64, ptr %48
  store i64 %102, ptr %49
  %103 = load i64, ptr %49
  %104 = load i64, ptr %13
  %105 = icmp slt i64 %103, %104
  br i1 %105, label %L203, label %L217
L217:
  %106 = load i64, ptr %49
  %107 = load i64, ptr %13
  %108 = icmp sgt i64 %106, %107
  br i1 %108, label %L205, label %L203
L180:
  %109 = load i64, ptr %21
  store i64 %109, ptr %5
  %110 = load i64, ptr %5
  %111 = load i64, ptr %ds
  %112 = load i64, ptr %alloc
  %113 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlLoop_invariant_gc_across_call__tick_0_4_code"(i64 %111, i64 %112, i64 %110) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 2, i64 12, i64 0, i64 12, i64 18, i64 0, i64 18, i64 159, i64 7558447, i64 7565925, i64 7694895, i64 7562604, i64 6513002, i64 7561839, i64 6907695, i64 7286644, i64 6382456, i64 2976877, i64 7761004, i64 6369133, i64 7234919, i64 3109748, i64 7562612, i64 7548276, i64 7629173, i64 3288421, i64 3433785, i64 3105891, i64 6518895, i64 7105889, i64 6775087, i64 7630437, i64 7631661, i64 6648929, i64 6648879, i64 2978931, i64 6911347, i64 2975092, i64 6633778, i64 6578996, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6909279, i64 7303779, i64 7234914, i64 3106915, i64 6517363, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6514527, i64 6512991, i64 7565170, i64 6512499, i64 7105633, i64 7105838, i64 34, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 7303020, i64 112, i64 19, i64 0, i64 18, i64 28, i64 0, i64 28, i64 159, i64 7558447, i64 7565925, i64 7694895, i64 7562604, i64 6513002, i64 7561839, i64 6907695, i64 7286644, i64 6382456, i64 2976877, i64 7761004, i64 6369133, i64 7234919, i64 3109748, i64 7562612, i64 7548276, i64 7629173, i64 3288421, i64 3433785, i64 3105891, i64 6518895, i64 7105889, i64 6775087, i64 7630437, i64 7631661, i64 6648929, i64 6648879, i64 2978931, i64 6911347, i64 2975092, i64 6633778, i64 6578996, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6909279, i64 7303779, i64 7234914, i64 3106915, i64 6517363, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6514527, i64 6512991, i64 7565170, i64 6512499, i64 7105633, i64 7105838, i64 33, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 7239026) ]
  %114 = extractvalue { { i64, i64 }, { i64 } } %113, 0, 0
  %115 = extractvalue { { i64, i64 }, { i64 } } %113, 0, 1
  store i64 %114, ptr %ds
  store i64 %115, ptr %alloc
  %116 = extractvalue { { i64, i64 }, { i64 } } %113, 1, 0
  store i64 %116, ptr %5
  br label %L182
L182:
  %117 = load i64, ptr %5
  store i64 %117, ptr %27
  %118 = load i64, ptr %27
  store i64 %118, ptr %28
  %119 = load ptr addrspace(1), ptr %11
  %120 = getelementptr i8, ptr addrspace(1) %119, i64 -8
  store ptr addrspace(1) %120, ptr %29
  %121 = load ptr addrspace(1), ptr %29
  %122 = load i64, ptr addrspace(1) %121
  store i64 %122, ptr %30
  %123 = load i64, ptr %30
  %124 = shl i64 %123, 8
  store i64 %124, ptr %31
  %125 = load i64, ptr %31
  %126 = lshr i64 %125, 18
  store i64 %126, ptr %32
  %127 = load i64, ptr %32
  %128 = shl i64 %127, 3
  store i64 %128, ptr %33
  %129 = load i64, ptr %33
  %130 = sub i64 %129, 1
  store i64 %130, ptr %34
  %131 = load i64, ptr %34
  store i64 %131, ptr %35
  %132 = load ptr addrspace(1), ptr %11
  %133 = load i64, ptr %35
  %134 = getelementptr i8, ptr addrspace(1) %132, i64 %133
  store ptr addrspace(1) %134, ptr %36
  %135 = load ptr addrspace(1), ptr %36
  %136 = load i8, ptr addrspace(1) %135
  %137 = zext i8 %136 to i64
  store i64 %137, ptr %37
  %138 = load i64, ptr %35
  %139 = load i64, ptr %37
  %140 = sub i64 %138, %139
  store i64 %140, ptr %38
  %141 = load i64, ptr %38
  %142 = shl i64 %141, 1
  %143 = load i64, ptr %22
  %144 = add i64 %143, %142
  store i64 %144, ptr %39
  %145 = load i64, ptr %39
  %146 = load i64, ptr %28
  %147 = add i64 %145, %146
  store i64 %147, ptr %40
  %148 = load i64, ptr %40
  %149 = add i64 %148, -1
  store i64 %149, ptr %41
  %150 = load i64, ptr %21
  %151 = add i64 %150, -2
  store i64 %151, ptr %42
  %152 = load i64, ptr %42
  store i64 %152, ptr %43
  %153 = load i64, ptr %41
  store i64 %153, ptr %44
  %154 = load i64, ptr %43
  store i64 %154, ptr %21
  %155 = load i64, ptr %44
  store i64 %155, ptr %22
  %156 = load i64, ptr %21
  %157 = icmp slt i64 %156, 1
  br i1 %157, label %L178, label %L218
L218:
  %158 = load i64, ptr %21
  %159 = icmp sgt i64 %158, 1
  br i1 %159, label %L180, label %L178
L203:
  %160 = load i64, ptr %49
  store i64 %160, ptr %50
  %161 = load i64, ptr %47
  store i64 %161, ptr %51
  %162 = load i64, ptr %50
  store i64 %162, ptr %14
  %163 = load i64, ptr %51
  store i64 %163, ptr %15
  br label %L174
L205:
  %164 = load i64, ptr %47
  store i64 %164, ptr %5
  %165 = load i64, ptr %5
  %166 = load i64, ptr %ds
  %167 = load i64, ptr %alloc
  %168 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %166, 0, 0
  %169 = insertvalue { { i64, i64 }, { i64 } } %168, i64 %167, 0, 1
  %170 = insertvalue { { i64, i64 }, { i64 } } %169, i64 %165, 1, 0
  ret { { i64, i64 }, { i64 } } %170
L210:
  store i64 1, ptr %5
  %171 = load i64, ptr %5
  %172 = load i64, ptr %ds
  %173 = load i64, ptr %alloc
  %174 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %172, 0, 0
  %175 = insertvalue { { i64, i64 }, { i64 } } %174, i64 %173, 0, 1
  %176 = insertvalue { { i64, i64 }, { i64 } } %175, i64 %171, 1, 0
  ret { { i64, i64 }, { i64 } } %176
}

define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlLoop_invariant_gc_across_call__entry"(i64 %0, i64 %1) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %3 = alloca ptr addrspace(1) 
  %4 = alloca i64 
  %5 = alloca ptr addrspace(1) 
  %6 = alloca ptr addrspace(1) 
  %7 = alloca ptr addrspace(1) 
  %8 = alloca ptr addrspace(1) 
  %9 = alloca ptr addrspace(1) 
  %10 = alloca i64 
  %11 = alloca ptr addrspace(1) 
  %12 = alloca ptr addrspace(1) 
  %13 = alloca i64 
  %14 = alloca i64 
  %15 = alloca i64 
  %16 = alloca i64 
  %17 = alloca i64 
  %18 = alloca i64 
  %19 = alloca ptr addrspace(1) 
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
  %30 = alloca i64 
  %31 = alloca i64 
  %32 = alloca i64 
  %33 = alloca i64 
  %34 = alloca i64 
  %35 = alloca i64 
  %36 = alloca ptr addrspace(1) 
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
  %48 = alloca i64 
  %49 = alloca i64 
  %50 = alloca i64 
  %51 = alloca i64 
  %52 = alloca i64 
  %53 = alloca i64 
  %54 = alloca ptr addrspace(1) 
  %55 = alloca ptr addrspace(1) 
  %56 = alloca ptr addrspace(1) 
  %57 = alloca i64 
  %58 = alloca i64 
  %59 = alloca i64 
  %60 = alloca i64 
  %61 = alloca i64 
  %62 = alloca i64 
  %63 = alloca ptr addrspace(1) 
  %64 = alloca i64 
  %65 = alloca i64 
  %66 = alloca i64 
  %67 = alloca i64 
  %68 = alloca i64 
  %69 = alloca i64 
  %70 = alloca i64 
  %71 = alloca ptr addrspace(1) 
  %72 = alloca ptr addrspace(1) 
  %73 = alloca ptr addrspace(1) 
  %74 = alloca i64 
  %75 = alloca i64 
  %76 = alloca i64 
  %77 = alloca ptr addrspace(1) 
  %78 = alloca ptr addrspace(1) 
  %79 = alloca ptr addrspace(1) 
  %80 = alloca i64 
  %81 = alloca i64 
  %82 = alloca ptr addrspace(1) 
  %83 = alloca ptr addrspace(1) 
  %84 = alloca ptr addrspace(1) 
  %85 = alloca i64 
  %86 = alloca i64 
  %87 = alloca i64 
  br label %L1
L1:
  br label %L226
L226:
  %88 = load i64, ptr %ds
  %89 = add i64 %88, 40
  %90 = inttoptr i64 %89 to ptr
  %91 = load i64, ptr %90
  %92 = add i64 %91, 376
  %93 = call  i64 @llvm.read_register.i64(metadata !{!"sp\00"}) 
  %94 = icmp uge i64 %93, %92
  %95 = call  i1 @llvm.expect.i1(i1 %94, i1 1) 
  br i1 %95, label %L300, label %L299
L299:
  %96 = load i64, ptr %ds
  %97 = load i64, ptr %alloc
  %98 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_llvm_call_realloc_stack"(i64 %96, i64 %97, i64 34) "statepoint-id"="0" cold
  %99 = extractvalue { { i64, i64 }, {  } } %98, 0, 0
  %100 = extractvalue { { i64, i64 }, {  } } %98, 0, 1
  store i64 %99, ptr %ds
  store i64 %100, ptr %alloc
  br label %L300
L300:
  %101 = ptrtoint ptr @"\01_camlLoop_invariant_gc_across_call__immstring48" to i64
  store i64 %101, ptr %10
  %102 = load i64, ptr %10
  %103 = call i64 asm  "", "=r,0"(i64 %102) "gc-leaf-function"="true"
  store i64 %103, ptr %10
  %104 = load i64, ptr %10
  %105 = inttoptr i64 %104 to ptr addrspace(1)
  store ptr addrspace(1) %105, ptr %11
  %106 = load ptr addrspace(1), ptr %11
  store ptr addrspace(1) %106, ptr %12
  store i64 1, ptr %17
  store i64 1, ptr %18
  %107 = load i64, ptr %17
  store i64 %107, ptr %13
  %108 = load i64, ptr %18
  store i64 %108, ptr %14
  br label %L237
L237:
  store i64 24000001, ptr %24
  store i64 1, ptr %25
  %109 = load i64, ptr %24
  store i64 %109, ptr %20
  %110 = load i64, ptr %25
  store i64 %110, ptr %21
  br label %L243
L241:
  %111 = load i64, ptr %21
  store i64 %111, ptr %26
  %112 = load i64, ptr %26
  %113 = inttoptr i64 %112 to ptr addrspace(1)
  store ptr addrspace(1) %113, ptr %19
  %114 = load i64, ptr %14
  %115 = load ptr addrspace(1), ptr %19
  %116 = ptrtoint ptr addrspace(1) %115 to i64
  %117 = add i64 %114, %116
  store i64 %117, ptr %45
  %118 = load i64, ptr %45
  %119 = add i64 %118, -1
  store i64 %119, ptr %46
  %120 = load i64, ptr %46
  store i64 %120, ptr %47
  %121 = load i64, ptr %13
  %122 = add i64 %121, 1
  store i64 %122, ptr %48
  %123 = load i64, ptr %48
  store i64 %123, ptr %49
  %124 = load i64, ptr %49
  %125 = icmp slt i64 %124, 5
  br i1 %125, label %L266, label %L301
L301:
  %126 = load i64, ptr %49
  %127 = icmp sgt i64 %126, 5
  br i1 %127, label %L268, label %L266
L243:
  %128 = load i64, ptr %20
  store i64 %128, ptr %4
  %129 = load i64, ptr %4
  %130 = load i64, ptr %ds
  %131 = load i64, ptr %alloc
  %132 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlLoop_invariant_gc_across_call__tick_0_4_code"(i64 %130, i64 %131, i64 %129) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 3, i64 12, i64 0, i64 12, i64 18, i64 0, i64 18, i64 159, i64 7558447, i64 7565925, i64 7694895, i64 7562604, i64 6513002, i64 7561839, i64 6907695, i64 7286644, i64 6382456, i64 2976877, i64 7761004, i64 6369133, i64 7234919, i64 3109748, i64 7562612, i64 7548276, i64 7629173, i64 3288421, i64 3433785, i64 3105891, i64 6518895, i64 7105889, i64 6775087, i64 7630437, i64 7631661, i64 6648929, i64 6648879, i64 2978931, i64 6911347, i64 2975092, i64 6633778, i64 6578996, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6909279, i64 7303779, i64 7234914, i64 3106915, i64 6517363, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6514527, i64 6512991, i64 7565170, i64 6512499, i64 7105633, i64 7105838, i64 34, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 7303020, i64 112, i64 19, i64 0, i64 18, i64 28, i64 0, i64 28, i64 159, i64 7558447, i64 7565925, i64 7694895, i64 7562604, i64 6513002, i64 7561839, i64 6907695, i64 7286644, i64 6382456, i64 2976877, i64 7761004, i64 6369133, i64 7234919, i64 3109748, i64 7562612, i64 7548276, i64 7629173, i64 3288421, i64 3433785, i64 3105891, i64 6518895, i64 7105889, i64 6775087, i64 7630437, i64 7631661, i64 6648929, i64 6648879, i64 2978931, i64 6911347, i64 2975092, i64 6633778, i64 6578996, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6909279, i64 7303779, i64 7234914, i64 3106915, i64 6517363, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6514527, i64 6512991, i64 7565170, i64 6512499, i64 7105633, i64 7105838, i64 33, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 7239026, i64 23, i64 0, i64 22, i64 38, i64 0, i64 38, i64 159, i64 7558447, i64 7565925, i64 7694895, i64 7562604, i64 6513002, i64 7561839, i64 6907695, i64 7286644, i64 6382456, i64 2976877, i64 7761004, i64 6369133, i64 7234919, i64 3109748, i64 7562612, i64 7548276, i64 7629173, i64 3288421, i64 3433785, i64 3105891, i64 6518895, i64 7105889, i64 6775087, i64 7630437, i64 7631661, i64 6648929, i64 6648879, i64 2978931, i64 6911347, i64 2975092, i64 6633778, i64 6578996, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6909279, i64 7303779, i64 7234914, i64 3106915, i64 6517363, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6514527, i64 6512991, i64 7565170, i64 6512499, i64 7105633, i64 7105838, i64 29, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 27756) ]
  %133 = extractvalue { { i64, i64 }, { i64 } } %132, 0, 0
  %134 = extractvalue { { i64, i64 }, { i64 } } %132, 0, 1
  store i64 %133, ptr %ds
  store i64 %134, ptr %alloc
  %135 = extractvalue { { i64, i64 }, { i64 } } %132, 1, 0
  store i64 %135, ptr %4
  br label %L245
L245:
  %136 = load i64, ptr %4
  store i64 %136, ptr %27
  %137 = load i64, ptr %27
  store i64 %137, ptr %28
  %138 = load ptr addrspace(1), ptr %12
  %139 = getelementptr i8, ptr addrspace(1) %138, i64 -8
  store ptr addrspace(1) %139, ptr %29
  %140 = load ptr addrspace(1), ptr %29
  %141 = load i64, ptr addrspace(1) %140
  store i64 %141, ptr %30
  %142 = load i64, ptr %30
  %143 = shl i64 %142, 8
  store i64 %143, ptr %31
  %144 = load i64, ptr %31
  %145 = lshr i64 %144, 18
  store i64 %145, ptr %32
  %146 = load i64, ptr %32
  %147 = shl i64 %146, 3
  store i64 %147, ptr %33
  %148 = load i64, ptr %33
  %149 = sub i64 %148, 1
  store i64 %149, ptr %34
  %150 = load i64, ptr %34
  store i64 %150, ptr %35
  %151 = load ptr addrspace(1), ptr %12
  %152 = load i64, ptr %35
  %153 = getelementptr i8, ptr addrspace(1) %151, i64 %152
  store ptr addrspace(1) %153, ptr %36
  %154 = load ptr addrspace(1), ptr %36
  %155 = load i8, ptr addrspace(1) %154
  %156 = zext i8 %155 to i64
  store i64 %156, ptr %37
  %157 = load i64, ptr %35
  %158 = load i64, ptr %37
  %159 = sub i64 %157, %158
  store i64 %159, ptr %38
  %160 = load i64, ptr %38
  %161 = shl i64 %160, 1
  %162 = load i64, ptr %21
  %163 = add i64 %162, %161
  store i64 %163, ptr %39
  %164 = load i64, ptr %39
  %165 = load i64, ptr %28
  %166 = add i64 %164, %165
  store i64 %166, ptr %40
  %167 = load i64, ptr %40
  %168 = add i64 %167, -1
  store i64 %168, ptr %41
  %169 = load i64, ptr %20
  %170 = add i64 %169, -2
  store i64 %170, ptr %42
  %171 = load i64, ptr %42
  store i64 %171, ptr %43
  %172 = load i64, ptr %41
  store i64 %172, ptr %44
  %173 = load i64, ptr %43
  store i64 %173, ptr %20
  %174 = load i64, ptr %44
  store i64 %174, ptr %21
  %175 = load i64, ptr %20
  %176 = icmp slt i64 %175, 1
  br i1 %176, label %L241, label %L302
L302:
  %177 = load i64, ptr %20
  %178 = icmp sgt i64 %177, 1
  br i1 %178, label %L243, label %L241
L266:
  %179 = load i64, ptr %49
  store i64 %179, ptr %50
  %180 = load i64, ptr %47
  store i64 %180, ptr %51
  %181 = load i64, ptr %50
  store i64 %181, ptr %13
  %182 = load i64, ptr %51
  store i64 %182, ptr %14
  br label %L237
L268:
  %183 = load i64, ptr %47
  store i64 %183, ptr %52
  %184 = load i64, ptr %52
  %185 = inttoptr i64 %184 to ptr addrspace(1)
  store ptr addrspace(1) %185, ptr %9
  %186 = ptrtoint ptr @"\01_camlStdlib__immstring191" to i64
  store i64 %186, ptr %53
  %187 = load i64, ptr %53
  %188 = inttoptr i64 %187 to ptr addrspace(1)
  store ptr addrspace(1) %188, ptr %3
  %189 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %189, ptr %5
  %190 = ptrtoint ptr @"\01_caml_format_int" to i64
  %191 = load ptr addrspace(1), ptr %3
  %192 = load ptr addrspace(1), ptr %5
  %193 = load i64, ptr %ds
  %194 = load i64, ptr %alloc
  %195 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %193, i64 %194, i64 %190, ptr addrspace(1) %191, ptr addrspace(1) %192) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 280, i64 0, i64 2, i64 19, i64 0, i64 19, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7631662, i64 7235954, i64 7298919, i64 6905702, i64 29806, i64 507, i64 0, i64 39, i64 56, i64 0, i64 56, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 16, i64 6583379, i64 6449516, i64 7499822, i64 7630441, i64 7235935, i64 116, i64 5, i64 0, i64 2, i64 13, i64 0, i64 13, i64 159, i64 7558447, i64 7565925, i64 7694895, i64 7562604, i64 6513002, i64 7561839, i64 6907695, i64 7286644, i64 6382456, i64 2976877, i64 7761004, i64 6369133, i64 7234919, i64 3109748, i64 7562612, i64 7548276, i64 7629173, i64 3288421, i64 3433785, i64 3105891, i64 6518895, i64 7105889, i64 6775087, i64 7630437, i64 7631661, i64 6648929, i64 6648879, i64 2978931, i64 6911347, i64 2975092, i64 6633778, i64 6578996, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6909279, i64 7303779, i64 7234914, i64 3106915, i64 6517363, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6514527, i64 6512991, i64 7565170, i64 6512499, i64 7105633, i64 7105838, i64 42, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 6910576, i64 6255726, i64 7562610, i64 7629941, i64 23, i64 0, i64 9, i64 38, i64 0, i64 38, i64 159, i64 7558447, i64 7565925, i64 7694895, i64 7562604, i64 6513002, i64 7561839, i64 6907695, i64 7286644, i64 6382456, i64 2976877, i64 7761004, i64 6369133, i64 7234919, i64 3109748, i64 7562612, i64 7548276, i64 7629173, i64 3288421, i64 3433785, i64 3105891, i64 6518895, i64 7105889, i64 6775087, i64 7630437, i64 7631661, i64 6648929, i64 6648879, i64 2978931, i64 6911347, i64 2975092, i64 6633778, i64 6578996, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6909279, i64 7303779, i64 7234914, i64 3106915, i64 6517363, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6514527, i64 6512991, i64 7565170, i64 6512499, i64 7105633, i64 7105838, i64 29, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 27756) ]
  %196 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %195, 0, 0
  %197 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %195, 0, 1
  store i64 %196, ptr %ds
  store i64 %197, ptr %alloc
  %198 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %195, 1, 0
  store ptr addrspace(1) %198, ptr %3
  br label %L275
L275:
  %199 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %199, ptr %54
  %200 = load ptr addrspace(1), ptr %54
  store ptr addrspace(1) %200, ptr %55
  %201 = load ptr addrspace(1), ptr %55
  %202 = getelementptr i8, ptr addrspace(1) %201, i64 -8
  store ptr addrspace(1) %202, ptr %56
  %203 = load ptr addrspace(1), ptr %56
  %204 = load i64, ptr addrspace(1) %203
  store i64 %204, ptr %57
  %205 = load i64, ptr %57
  %206 = shl i64 %205, 8
  store i64 %206, ptr %58
  %207 = load i64, ptr %58
  %208 = lshr i64 %207, 18
  store i64 %208, ptr %59
  %209 = load i64, ptr %59
  %210 = shl i64 %209, 3
  store i64 %210, ptr %60
  %211 = load i64, ptr %60
  %212 = sub i64 %211, 1
  store i64 %212, ptr %61
  %213 = load i64, ptr %61
  store i64 %213, ptr %62
  %214 = load ptr addrspace(1), ptr %55
  %215 = load i64, ptr %62
  %216 = getelementptr i8, ptr addrspace(1) %214, i64 %215
  store ptr addrspace(1) %216, ptr %63
  %217 = load ptr addrspace(1), ptr %63
  %218 = load i8, ptr addrspace(1) %217
  %219 = zext i8 %218 to i64
  store i64 %219, ptr %64
  %220 = load i64, ptr %62
  %221 = load i64, ptr %64
  %222 = sub i64 %220, %221
  store i64 %222, ptr %65
  %223 = load i64, ptr %65
  %224 = shl i64 %223, 1
  %225 = add i64 1, %224
  store i64 %225, ptr %67
  %226 = ptrtoint ptr @"\01_camlStdlib__print_int_136" to i64
  store i64 %226, ptr %69
  %227 = load i64, ptr %69
  %228 = add i64 %227, 16
  store i64 %228, ptr %70
  %229 = load i64, ptr %70
  %230 = inttoptr i64 %229 to ptr
  %231 = load ptr addrspace(1), ptr %230
  store ptr addrspace(1) %231, ptr %71
  %232 = load ptr addrspace(1), ptr %71
  store ptr addrspace(1) %232, ptr %3
  %233 = load ptr addrspace(1), ptr %55
  store ptr addrspace(1) %233, ptr %5
  %234 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %234, ptr %6
  %235 = load i64, ptr %67
  %236 = inttoptr i64 %235 to ptr addrspace(1)
  store ptr addrspace(1) %236, ptr %7
  %237 = ptrtoint ptr @"\01_caml_ml_output" to i64
  %238 = load ptr addrspace(1), ptr %3
  %239 = load ptr addrspace(1), ptr %5
  %240 = load ptr addrspace(1), ptr %6
  %241 = load ptr addrspace(1), ptr %7
  %242 = load i64, ptr %ds
  %243 = load i64, ptr %alloc
  %244 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %242, i64 %243, i64 %237, ptr addrspace(1) %238, ptr addrspace(1) %239, ptr addrspace(1) %240, ptr addrspace(1) %241) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 387, i64 0, i64 2, i64 47, i64 0, i64 47, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7696174, i64 7696500, i64 7561076, i64 6910580, i64 26478, i64 507, i64 0, i64 18, i64 56, i64 0, i64 56, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 16, i64 6583379, i64 6449516, i64 7499822, i64 7630441, i64 7235935, i64 116, i64 5, i64 0, i64 2, i64 13, i64 0, i64 13, i64 159, i64 7558447, i64 7565925, i64 7694895, i64 7562604, i64 6513002, i64 7561839, i64 6907695, i64 7286644, i64 6382456, i64 2976877, i64 7761004, i64 6369133, i64 7234919, i64 3109748, i64 7562612, i64 7548276, i64 7629173, i64 3288421, i64 3433785, i64 3105891, i64 6518895, i64 7105889, i64 6775087, i64 7630437, i64 7631661, i64 6648929, i64 6648879, i64 2978931, i64 6911347, i64 2975092, i64 6633778, i64 6578996, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6909279, i64 7303779, i64 7234914, i64 3106915, i64 6517363, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6514527, i64 6512991, i64 7565170, i64 6512499, i64 7105633, i64 7105838, i64 42, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 6910576, i64 6255726, i64 7562610, i64 7629941, i64 23, i64 0, i64 9, i64 38, i64 0, i64 38, i64 159, i64 7558447, i64 7565925, i64 7694895, i64 7562604, i64 6513002, i64 7561839, i64 6907695, i64 7286644, i64 6382456, i64 2976877, i64 7761004, i64 6369133, i64 7234919, i64 3109748, i64 7562612, i64 7548276, i64 7629173, i64 3288421, i64 3433785, i64 3105891, i64 6518895, i64 7105889, i64 6775087, i64 7630437, i64 7631661, i64 6648929, i64 6648879, i64 2978931, i64 6911347, i64 2975092, i64 6633778, i64 6578996, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6909279, i64 7303779, i64 7234914, i64 3106915, i64 6517363, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6514527, i64 6512991, i64 7565170, i64 6512499, i64 7105633, i64 7105838, i64 29, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 27756) ]
  %245 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %244, 0, 0
  %246 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %244, 0, 1
  store i64 %245, ptr %ds
  store i64 %246, ptr %alloc
  %247 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %244, 1, 0
  store ptr addrspace(1) %247, ptr %3
  br label %L276
L276:
  %248 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %248, ptr %72
  %249 = load ptr addrspace(1), ptr %72
  store ptr addrspace(1) %249, ptr %73
  %250 = ptrtoint ptr @"\01_camlStdlib__print_int_136" to i64
  store i64 %250, ptr %75
  %251 = load i64, ptr %75
  %252 = add i64 %251, 16
  store i64 %252, ptr %76
  %253 = load i64, ptr %76
  %254 = inttoptr i64 %253 to ptr
  %255 = load ptr addrspace(1), ptr %254
  store ptr addrspace(1) %255, ptr %77
  %256 = load ptr addrspace(1), ptr %77
  store ptr addrspace(1) %256, ptr %3
  %257 = inttoptr i64 21 to ptr addrspace(1)
  store ptr addrspace(1) %257, ptr %5
  %258 = ptrtoint ptr @"\01_caml_ml_output_char" to i64
  %259 = load ptr addrspace(1), ptr %3
  %260 = load ptr addrspace(1), ptr %5
  %261 = load i64, ptr %ds
  %262 = load i64, ptr %alloc
  %263 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %261, i64 %262, i64 %258, ptr addrspace(1) %259, ptr addrspace(1) %260) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 3, i64 511, i64 0, i64 23, i64 46, i64 0, i64 46, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7499822, i64 7630441, i64 6647391, i64 6909047, i64 25966, i64 6, i64 0, i64 2, i64 18, i64 0, i64 18, i64 159, i64 7558447, i64 7565925, i64 7694895, i64 7562604, i64 6513002, i64 7561839, i64 6907695, i64 7286644, i64 6382456, i64 2976877, i64 7761004, i64 6369133, i64 7234919, i64 3109748, i64 7562612, i64 7548276, i64 7629173, i64 3288421, i64 3433785, i64 3105891, i64 6518895, i64 7105889, i64 6775087, i64 7630437, i64 7631661, i64 6648929, i64 6648879, i64 2978931, i64 6911347, i64 2975092, i64 6633778, i64 6578996, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6909279, i64 7303779, i64 7234914, i64 3106915, i64 6517363, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6514527, i64 6512991, i64 7565170, i64 6512499, i64 7105633, i64 7105838, i64 42, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 6910576, i64 6255726, i64 7562610, i64 7629941, i64 23, i64 0, i64 9, i64 38, i64 0, i64 38, i64 159, i64 7558447, i64 7565925, i64 7694895, i64 7562604, i64 6513002, i64 7561839, i64 6907695, i64 7286644, i64 6382456, i64 2976877, i64 7761004, i64 6369133, i64 7234919, i64 3109748, i64 7562612, i64 7548276, i64 7629173, i64 3288421, i64 3433785, i64 3105891, i64 6518895, i64 7105889, i64 6775087, i64 7630437, i64 7631661, i64 6648929, i64 6648879, i64 2978931, i64 6911347, i64 2975092, i64 6633778, i64 6578996, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6909279, i64 7303779, i64 7234914, i64 3106915, i64 6517363, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6514527, i64 6512991, i64 7565170, i64 6512499, i64 7105633, i64 7105838, i64 29, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 27756) ]
  %264 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %263, 0, 0
  %265 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %263, 0, 1
  store i64 %264, ptr %ds
  store i64 %265, ptr %alloc
  %266 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %263, 1, 0
  store ptr addrspace(1) %266, ptr %3
  br label %L289
L289:
  %267 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %267, ptr %78
  %268 = load ptr addrspace(1), ptr %78
  store ptr addrspace(1) %268, ptr %79
  %269 = ptrtoint ptr @"\01_camlStdlib__print_int_136" to i64
  store i64 %269, ptr %80
  %270 = load i64, ptr %80
  %271 = add i64 %270, 16
  store i64 %271, ptr %81
  %272 = load i64, ptr %81
  %273 = inttoptr i64 %272 to ptr
  %274 = load ptr addrspace(1), ptr %273
  store ptr addrspace(1) %274, ptr %82
  %275 = load ptr addrspace(1), ptr %82
  store ptr addrspace(1) %275, ptr %3
  %276 = ptrtoint ptr @"\01_caml_ml_flush" to i64
  %277 = load ptr addrspace(1), ptr %3
  %278 = load i64, ptr %ds
  %279 = load i64, ptr %alloc
  %280 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %278, i64 %279, i64 %276, ptr addrspace(1) %277) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 3, i64 511, i64 0, i64 48, i64 60, i64 0, i64 60, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7499822, i64 7630441, i64 6647391, i64 6909047, i64 25966, i64 6, i64 0, i64 2, i64 18, i64 0, i64 18, i64 159, i64 7558447, i64 7565925, i64 7694895, i64 7562604, i64 6513002, i64 7561839, i64 6907695, i64 7286644, i64 6382456, i64 2976877, i64 7761004, i64 6369133, i64 7234919, i64 3109748, i64 7562612, i64 7548276, i64 7629173, i64 3288421, i64 3433785, i64 3105891, i64 6518895, i64 7105889, i64 6775087, i64 7630437, i64 7631661, i64 6648929, i64 6648879, i64 2978931, i64 6911347, i64 2975092, i64 6633778, i64 6578996, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6909279, i64 7303779, i64 7234914, i64 3106915, i64 6517363, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6514527, i64 6512991, i64 7565170, i64 6512499, i64 7105633, i64 7105838, i64 42, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 6910576, i64 6255726, i64 7562610, i64 7629941, i64 23, i64 0, i64 9, i64 38, i64 0, i64 38, i64 159, i64 7558447, i64 7565925, i64 7694895, i64 7562604, i64 6513002, i64 7561839, i64 6907695, i64 7286644, i64 6382456, i64 2976877, i64 7761004, i64 6369133, i64 7234919, i64 3109748, i64 7562612, i64 7548276, i64 7629173, i64 3288421, i64 3433785, i64 3105891, i64 6518895, i64 7105889, i64 6775087, i64 7630437, i64 7631661, i64 6648929, i64 6648879, i64 2978931, i64 6911347, i64 2975092, i64 6633778, i64 6578996, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6909279, i64 7303779, i64 7234914, i64 3106915, i64 6517363, i64 7302191, i64 6254703, i64 7761513, i64 6910561, i64 7630433, i64 6514527, i64 6512991, i64 7565170, i64 6512499, i64 7105633, i64 7105838, i64 29, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 27756) ]
  %281 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %280, 0, 0
  %282 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %280, 0, 1
  store i64 %281, ptr %ds
  store i64 %282, ptr %alloc
  %283 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %280, 1, 0
  store ptr addrspace(1) %283, ptr %3
  br label %L292
L292:
  %284 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %284, ptr %83
  %285 = load ptr addrspace(1), ptr %83
  store ptr addrspace(1) %285, ptr %84
  %286 = ptrtoint ptr @"\01_camlLoop_invariant_gc_across_call" to i64
  store i64 %286, ptr %85
  %287 = load i64, ptr %85
  store i64 %287, ptr %86
  %288 = load i64, ptr %86
  %289 = inttoptr i64 %288 to ptr addrspace(1)
  store ptr addrspace(1) %289, ptr %8
  store i64 1, ptr %4
  %290 = load i64, ptr %4
  %291 = inttoptr i64 %290 to ptr addrspace(1)
  %292 = load i64, ptr %ds
  %293 = load i64, ptr %alloc
  %294 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %292, 0, 0
  %295 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %294, i64 %293, 0, 1
  %296 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %295, ptr addrspace(1) %291, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %296
}

@"\01_camlLoop_invariant_gc_across_call__gc_roots" = global { i64 } { i64 0 }, section "__DATA,__data", align 8
@"\01_header.camlLoop_invariant_gc_across_call" = global i64 4864, section "__DATA,__data", align 8
@"\01_camlLoop_invariant_gc_across_call" = global { ptr, ptr, ptr, ptr } { ptr @"\01_camlLoop_invariant_gc_across_call__tick_4", ptr @"\01_camlLoop_invariant_gc_across_call__print_result_5", ptr @"\01_camlLoop_invariant_gc_across_call__loop_6", ptr @"\01_camlLoop_invariant_gc_across_call__run_7" }, section "__DATA,__data", align 8
@"\01_header.camlLoop_invariant_gc_across_call__run_7" = global i64 4087, section "__DATA,__data", align 8
@"\01_camlLoop_invariant_gc_across_call__run_7" = global { ptr, i64, ptr } { ptr @"\01_caml_curry2", i64 180143985094819847, ptr @"\01_camlLoop_invariant_gc_across_call__run_3_7_code" }, section "__DATA,__data", align 8
@"\01_header.camlLoop_invariant_gc_across_call__loop_6" = global i64 4087, section "__DATA,__data", align 8
@"\01_camlLoop_invariant_gc_across_call__loop_6" = global { ptr, i64, ptr } { ptr @"\01_caml_curry3", i64 252201579132747783, ptr @"\01_camlLoop_invariant_gc_across_call__loop_2_6_code" }, section "__DATA,__data", align 8
@"\01_header.camlLoop_invariant_gc_across_call__print_result_5" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlLoop_invariant_gc_across_call__print_result_5" = global { ptr, i64 } { ptr @"\01_camlLoop_invariant_gc_across_call__print_result_1_5_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlLoop_invariant_gc_across_call__tick_4" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlLoop_invariant_gc_across_call__tick_4" = global { ptr, i64 } { ptr @"\01_camlLoop_invariant_gc_across_call__tick_0_4_code", i64 108086391056891909 }, section "__DATA,__data", align 8
@"\01_header.camlLoop_invariant_gc_across_call__immstring48" = global i64 4092, section "__DATA,__data", align 8
@"\01_camlLoop_invariant_gc_across_call__immstring48" = global { [ 22 x i8 ], [ 1 x i8 ], i8 } { [ 22 x i8 ] c"\6c\6f\6f\70\5f\69\6e\76\61\72\69\61\6e\74\5f\70\61\79\6c\6f\61\64", [ 1 x i8 ] zeroinitializer, i8 1 }, section "__DATA,__data", align 8
@"\01_camlStdlib__immstring191" = external global ptr
@"\01_camlStdlib__print_int_136" = external global ptr
@"\01_camlStdlib__print_newline_139" = external global ptr
@"\01_caml_c_call" = external global ptr
@"\01_caml_curry2" = external global ptr
@"\01_caml_curry3" = external global ptr
@"\01_caml_format_int" = external global ptr
@"\01_caml_llvm_call_realloc_stack" = external global ptr
@"\01_caml_ml_flush" = external global ptr
@"\01_caml_ml_output" = external global ptr
@"\01_caml_ml_output_char" = external global ptr

declare i1 @llvm.expect.i1(i1, i1)
declare i64 @llvm.read_register.i64(metadata)


!0 = !{ i32 1, !"oxcaml_module", !"Loop_invariant_gc_across_call" }
!llvm.module.flags = !{ !0 }
