source_filename = "loop_invariant_gc_across_call.ml"

; camlLoop_invariant_gc_across_call__tick_0_4_code [ loop_invariant_gc_across_call.ml:1,24--57 ]
define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlLoop_invariant_gc_across_call__tick_0_4_code"(i64 %0, i64 %1, i64 %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %4 = alloca i64 ; ; pin:anon:I/0[x0]
  store i64 %2, ptr %4
  %5 = alloca ptr addrspace(1) ; ; pin:anon:V/0[x0]
  %6 = alloca i64 ; ; i:I/60
  %7 = alloca i64 ; ; anon:I/61
  %8 = alloca ptr addrspace(1) ; ; anon:V/62
  br label %L1
L1:
  ; goto 101 
  br label %L101
L101:
  ; i:I/60 := pin:anon:I/0[x0] 
  %9 = load i64, ptr %4
  store i64 %9, ptr %6
  ; anon:I/61 := i:I/60 + 2 [ loop_invariant_gc_across_call.ml:2,22--29 ]
  %10 = load i64, ptr %6
  %11 = add i64 %10, 2
  store i64 %11, ptr %7
  ; anon:I/61 := opaque anon:I/61 [ loop_invariant_gc_across_call.ml:2,2--29 ]
  %12 = load i64, ptr %7
  %13 = call i64 asm  "", "=r,0"(i64 %12) "gc-leaf-function"="true"
  store i64 %13, ptr %7
  ; anon:V/62 := anon:I/61 
  %14 = load i64, ptr %7
  %15 = inttoptr i64 %14 to ptr addrspace(1)
  store ptr addrspace(1) %15, ptr %8
  ; pin:anon:V/0[x0] := anon:V/62 
  %16 = load ptr addrspace(1), ptr %8
  store ptr addrspace(1) %16, ptr %5
  ; reload retaddr 
  ; return pin:anon:V/0[x0] 
  %17 = load ptr addrspace(1), ptr %5
  %18 = ptrtoint ptr addrspace(1) %17 to i64
  %19 = load i64, ptr %ds
  %20 = load i64, ptr %alloc
  %21 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %19, 0, 0
  %22 = insertvalue { { i64, i64 }, { i64 } } %21, i64 %20, 0, 1
  %23 = insertvalue { { i64, i64 }, { i64 } } %22, i64 %18, 1, 0
  ret { { i64, i64 }, { i64 } } %23
}

; camlLoop_invariant_gc_across_call__print_result_1_5_code [ loop_invariant_gc_across_call.ml:4,17--54 ]
define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlLoop_invariant_gc_across_call__print_result_1_5_code"(i64 %0, i64 %1, i64 %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %4 = alloca i64 ; ; pin:anon:I/0[x0]
  store i64 %2, ptr %4
  %5 = alloca ptr addrspace(1) ; ; pin:anon:V/0[x0]
  %6 = alloca ptr addrspace(1) ; ; pin:anon:V/1[x1]
  %7 = alloca ptr addrspace(1) ; ; pin:anon:V/2[x2]
  %8 = alloca ptr addrspace(1) ; ; pin:anon:V/3[x3]
  %9 = alloca i64 ; ; x:I/60
  %10 = alloca i64 ; ; anon:I/61
  %11 = alloca ptr addrspace(1) ; ; anon:V/62
  %12 = alloca ptr addrspace(1) ; ; apply_result:V/63
  %13 = alloca ptr addrspace(1) ; ; anon:A/64
  %14 = alloca i64 ; ; anon:I/65
  %15 = alloca i64 ; ; anon:I/66
  %16 = alloca i64 ; ; anon:I/67
  %17 = alloca i64 ; ; anon:I/68
  %18 = alloca i64 ; ; anon:I/69
  %19 = alloca i64 ; ; tmp:I/70
  %20 = alloca ptr addrspace(1) ; ; anon:A/71
  %21 = alloca i64 ; ; anon:I/72
  %22 = alloca i64 ; ; anon:I/73
  %23 = alloca i64 ; ; anon:I/74
  %24 = alloca i64 ; ; anon:I/75
  %25 = alloca i64 ; ; anon:I/76
  %26 = alloca i64 ; ; anon:I/77
  %27 = alloca i64 ; ; anon:I/78
  %28 = alloca ptr addrspace(1) ; ; anon:V/79
  %29 = alloca ptr addrspace(1) ; ; anon:V/80
  %30 = alloca ptr addrspace(1) ; ; param:V/81
  %31 = alloca i64 ; ; anon:I/82
  %32 = alloca i64 ; ; anon:I/83
  %33 = alloca i64 ; ; anon:I/84
  %34 = alloca ptr addrspace(1) ; ; anon:V/85
  %35 = alloca ptr addrspace(1) ; ; anon:V/86
  %36 = alloca ptr addrspace(1) ; ; param:V/87
  %37 = alloca i64 ; ; anon:I/88
  %38 = alloca i64 ; ; anon:I/89
  %39 = alloca ptr addrspace(1) ; ; anon:V/90
  %40 = alloca ptr addrspace(1) ; ; anon:V/91
  br label %L1
L1:
  ; goto 105 
  br label %L105
L105:
  ; x:I/60 := pin:anon:I/0[x0] 
  %41 = load i64, ptr %4
  store i64 %41, ptr %9
  ; anon:I/61 := "camlStdlib__immstring191" 
  %42 = ptrtoint ptr @"\01_camlStdlib__immstring191" to i64
  store i64 %42, ptr %10
  ; pin:anon:V/0[x0] := anon:I/61 
  %43 = load i64, ptr %10
  %44 = inttoptr i64 %43 to ptr addrspace(1)
  store ptr addrspace(1) %44, ptr %5
  ; pin:anon:V/1[x1] := x:I/60 
  %45 = load i64, ptr %9
  %46 = inttoptr i64 %45 to ptr addrspace(1)
  store ptr addrspace(1) %46, ptr %6
  ; pin:anon:V/0[x0] := extcall "caml_format_int" pin:anon:V/0[x0] pin:anon:V/1[x1]goto 107 [ loop_invariant_gc_across_call.ml:5,2--13;stdlib.ml:507,39--56;stdlib.ml:280,2--19 ]
  %47 = ptrtoint ptr @"\01_caml_format_int" to i64
  %48 = load ptr addrspace(1), ptr %5
  %49 = load ptr addrspace(1), ptr %6
  %50 = load i64, ptr %ds
  %51 = load i64, ptr %alloc
  %52 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %50, i64 %51, i64 %47, ptr addrspace(1) %48, ptr addrspace(1) %49) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 3, i64 280, i64 0, i64 2, i64 19, i64 0, i64 19, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7631662, i64 7235954, i64 7298919, i64 6905702, i64 29806, i64 507, i64 0, i64 39, i64 56, i64 0, i64 56, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 16, i64 6583379, i64 6449516, i64 7499822, i64 7630441, i64 7235935, i64 116, i64 5, i64 0, i64 2, i64 13, i64 0, i64 13, i64 32, i64 7303020, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 27757, i64 42, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 6910576, i64 6255726, i64 7562610, i64 7629941) ]
  %53 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %52, 0, 0
  %54 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %52, 0, 1
  store i64 %53, ptr %ds
  store i64 %54, ptr %alloc
  %55 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %52, 1, 0
  store ptr addrspace(1) %55, ptr %5
  br label %L107
L107:
  ; anon:V/62 := pin:anon:V/0[x0] 
  %56 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %56, ptr %11
  ; apply_result:V/63 := anon:V/62 
  %57 = load ptr addrspace(1), ptr %11
  store ptr addrspace(1) %57, ptr %12
  ; anon:A/64 := apply_result:V/63 + -8 [ loop_invariant_gc_across_call.ml:5,2--13;stdlib.ml:507,18--56;stdlib.ml:387,30--47 ]
  %58 = load ptr addrspace(1), ptr %12
  %59 = getelementptr i8, ptr addrspace(1) %58, i64 -8
  store ptr addrspace(1) %59, ptr %13
  ; anon:I/65 := int  mut[anon:A/64] [ loop_invariant_gc_across_call.ml:5,2--13;stdlib.ml:507,18--56;stdlib.ml:387,30--47 ]
  %60 = load ptr addrspace(1), ptr %13
  %61 = load i64, ptr addrspace(1) %60
  store i64 %61, ptr %14
  ; anon:I/66 := anon:I/65 << 8 [ loop_invariant_gc_across_call.ml:5,2--13;stdlib.ml:507,18--56;stdlib.ml:387,30--47 ]
  %62 = load i64, ptr %14
  %63 = shl i64 %62, 8
  store i64 %63, ptr %15
  ; anon:I/67 := anon:I/66 >>u 18 [ loop_invariant_gc_across_call.ml:5,2--13;stdlib.ml:507,18--56;stdlib.ml:387,30--47 ]
  %64 = load i64, ptr %15
  %65 = lshr i64 %64, 18
  store i64 %65, ptr %16
  ; anon:I/68 := anon:I/67 << 3 [ loop_invariant_gc_across_call.ml:5,2--13;stdlib.ml:507,18--56;stdlib.ml:387,30--47 ]
  %66 = load i64, ptr %16
  %67 = shl i64 %66, 3
  store i64 %67, ptr %17
  ; anon:I/69 := anon:I/68 - 1 [ loop_invariant_gc_across_call.ml:5,2--13;stdlib.ml:507,18--56;stdlib.ml:387,30--47 ]
  %68 = load i64, ptr %17
  %69 = sub i64 %68, 1
  store i64 %69, ptr %18
  ; tmp:I/70 := anon:I/69 
  %70 = load i64, ptr %18
  store i64 %70, ptr %19
  ; anon:A/71 := apply_result:V/63 + tmp:I/70 [ loop_invariant_gc_across_call.ml:5,2--13;stdlib.ml:507,18--56;stdlib.ml:387,30--47 ]
  %71 = load ptr addrspace(1), ptr %12
  %72 = load i64, ptr %19
  %73 = getelementptr i8, ptr addrspace(1) %71, i64 %72
  store ptr addrspace(1) %73, ptr %20
  ; anon:I/72 := unsigned int8  mut[anon:A/71] [ loop_invariant_gc_across_call.ml:5,2--13;stdlib.ml:507,18--56;stdlib.ml:387,30--47 ]
  %74 = load ptr addrspace(1), ptr %20
  %75 = load i8, ptr addrspace(1) %74
  %76 = zext i8 %75 to i64
  store i64 %76, ptr %21
  ; anon:I/73 := tmp:I/70 - anon:I/72 [ loop_invariant_gc_across_call.ml:5,2--13;stdlib.ml:507,18--56;stdlib.ml:387,30--47 ]
  %77 = load i64, ptr %19
  %78 = load i64, ptr %21
  %79 = sub i64 %77, %78
  store i64 %79, ptr %22
  ; anon:I/74 := 1 
  ; anon:I/75 := anon:I/74 + anon:I/73 << 1 [ loop_invariant_gc_across_call.ml:5,2--13;stdlib.ml:507,18--56;stdlib.ml:387,30--47 ]
  %80 = load i64, ptr %22
  %81 = shl i64 %80, 1
  %82 = add i64 1, %81
  store i64 %82, ptr %24
  ; anon:I/76 := 1 
  ; anon:I/77 := "camlStdlib__print_int_136" 
  %83 = ptrtoint ptr @"\01_camlStdlib__print_int_136" to i64
  store i64 %83, ptr %26
  ; anon:I/78 := anon:I/77 + 16 [ loop_invariant_gc_across_call.ml:5,2--13 ]
  %84 = load i64, ptr %26
  %85 = add i64 %84, 16
  store i64 %85, ptr %27
  ; anon:V/79 := val [anon:I/78] [ loop_invariant_gc_across_call.ml:5,2--13 ]
  %86 = load i64, ptr %27
  %87 = inttoptr i64 %86 to ptr
  %88 = load ptr addrspace(1), ptr %87
  store ptr addrspace(1) %88, ptr %28
  ; pin:anon:V/0[x0] := anon:V/79 
  %89 = load ptr addrspace(1), ptr %28
  store ptr addrspace(1) %89, ptr %5
  ; pin:anon:V/1[x1] := apply_result:V/63 
  %90 = load ptr addrspace(1), ptr %12
  store ptr addrspace(1) %90, ptr %6
  ; pin:anon:V/2[x2] := anon:I/76 
  %91 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %91, ptr %7
  ; pin:anon:V/3[x3] := anon:I/75 
  %92 = load i64, ptr %24
  %93 = inttoptr i64 %92 to ptr addrspace(1)
  store ptr addrspace(1) %93, ptr %8
  ; pin:anon:V/0[x0] := extcall "caml_ml_output" pin:anon:V/0[x0] pin:anon:V/1[x1] pin:anon:V/2[x2] pin:anon:V/3[x3]goto 108 [ loop_invariant_gc_across_call.ml:5,2--13;stdlib.ml:507,18--56;stdlib.ml:387,2--47 ]
  %94 = ptrtoint ptr @"\01_caml_ml_output" to i64
  %95 = load ptr addrspace(1), ptr %5
  %96 = load ptr addrspace(1), ptr %6
  %97 = load ptr addrspace(1), ptr %7
  %98 = load ptr addrspace(1), ptr %8
  %99 = load i64, ptr %ds
  %100 = load i64, ptr %alloc
  %101 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %99, i64 %100, i64 %94, ptr addrspace(1) %95, ptr addrspace(1) %96, ptr addrspace(1) %97, ptr addrspace(1) %98) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 3, i64 387, i64 0, i64 2, i64 47, i64 0, i64 47, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7696174, i64 7696500, i64 7561076, i64 6910580, i64 26478, i64 507, i64 0, i64 18, i64 56, i64 0, i64 56, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 16, i64 6583379, i64 6449516, i64 7499822, i64 7630441, i64 7235935, i64 116, i64 5, i64 0, i64 2, i64 13, i64 0, i64 13, i64 32, i64 7303020, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 27757, i64 42, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 6910576, i64 6255726, i64 7562610, i64 7629941) ]
  %102 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %101, 0, 0
  %103 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %101, 0, 1
  store i64 %102, ptr %ds
  store i64 %103, ptr %alloc
  %104 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %101, 1, 0
  store ptr addrspace(1) %104, ptr %5
  br label %L108
L108:
  ; anon:V/80 := pin:anon:V/0[x0] 
  %105 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %105, ptr %29
  ; param:V/81 := anon:V/80 
  %106 = load ptr addrspace(1), ptr %29
  store ptr addrspace(1) %106, ptr %30
  ; anon:I/82 := 21 
  ; anon:I/83 := "camlStdlib__print_newline_139" 
  %107 = ptrtoint ptr @"\01_camlStdlib__print_newline_139" to i64
  store i64 %107, ptr %32
  ; anon:I/84 := anon:I/83 + 16 [ loop_invariant_gc_across_call.ml:6,2--18 ]
  %108 = load i64, ptr %32
  %109 = add i64 %108, 16
  store i64 %109, ptr %33
  ; anon:V/85 := val [anon:I/84] [ loop_invariant_gc_across_call.ml:6,2--18 ]
  %110 = load i64, ptr %33
  %111 = inttoptr i64 %110 to ptr
  %112 = load ptr addrspace(1), ptr %111
  store ptr addrspace(1) %112, ptr %34
  ; pin:anon:V/0[x0] := anon:V/85 
  %113 = load ptr addrspace(1), ptr %34
  store ptr addrspace(1) %113, ptr %5
  ; pin:anon:V/1[x1] := anon:I/82 
  %114 = inttoptr i64 21 to ptr addrspace(1)
  store ptr addrspace(1) %114, ptr %6
  ; pin:anon:V/0[x0] := extcall "caml_ml_output_char" pin:anon:V/0[x0] pin:anon:V/1[x1]goto 121 [ loop_invariant_gc_across_call.ml:6,2--18;stdlib.ml:511,23--46 ]
  %115 = ptrtoint ptr @"\01_caml_ml_output_char" to i64
  %116 = load ptr addrspace(1), ptr %5
  %117 = load ptr addrspace(1), ptr %6
  %118 = load i64, ptr %ds
  %119 = load i64, ptr %alloc
  %120 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %118, i64 %119, i64 %115, ptr addrspace(1) %116, ptr addrspace(1) %117) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 511, i64 0, i64 23, i64 46, i64 0, i64 46, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7499822, i64 7630441, i64 6647391, i64 6909047, i64 25966, i64 6, i64 0, i64 2, i64 18, i64 0, i64 18, i64 32, i64 7303020, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 27757, i64 42, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 6910576, i64 6255726, i64 7562610, i64 7629941) ]
  %121 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %120, 0, 0
  %122 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %120, 0, 1
  store i64 %121, ptr %ds
  store i64 %122, ptr %alloc
  %123 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %120, 1, 0
  store ptr addrspace(1) %123, ptr %5
  br label %L121
L121:
  ; anon:V/86 := pin:anon:V/0[x0] 
  %124 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %124, ptr %35
  ; param:V/87 := anon:V/86 
  %125 = load ptr addrspace(1), ptr %35
  store ptr addrspace(1) %125, ptr %36
  ; anon:I/88 := "camlStdlib__print_newline_139" 
  %126 = ptrtoint ptr @"\01_camlStdlib__print_newline_139" to i64
  store i64 %126, ptr %37
  ; anon:I/89 := anon:I/88 + 16 [ loop_invariant_gc_across_call.ml:6,2--18 ]
  %127 = load i64, ptr %37
  %128 = add i64 %127, 16
  store i64 %128, ptr %38
  ; anon:V/90 := val [anon:I/89] [ loop_invariant_gc_across_call.ml:6,2--18 ]
  %129 = load i64, ptr %38
  %130 = inttoptr i64 %129 to ptr
  %131 = load ptr addrspace(1), ptr %130
  store ptr addrspace(1) %131, ptr %39
  ; pin:anon:V/0[x0] := anon:V/90 
  %132 = load ptr addrspace(1), ptr %39
  store ptr addrspace(1) %132, ptr %5
  ; pin:anon:V/0[x0] := extcall "caml_ml_flush" pin:anon:V/0[x0]goto 124 [ loop_invariant_gc_across_call.ml:6,2--18;stdlib.ml:511,48--60 ]
  %133 = ptrtoint ptr @"\01_caml_ml_flush" to i64
  %134 = load ptr addrspace(1), ptr %5
  %135 = load i64, ptr %ds
  %136 = load i64, ptr %alloc
  %137 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %135, i64 %136, i64 %133, ptr addrspace(1) %134) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 511, i64 0, i64 48, i64 60, i64 0, i64 60, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7499822, i64 7630441, i64 6647391, i64 6909047, i64 25966, i64 6, i64 0, i64 2, i64 18, i64 0, i64 18, i64 32, i64 7303020, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 27757, i64 42, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 6910576, i64 6255726, i64 7562610, i64 7629941) ]
  %138 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %137, 0, 0
  %139 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %137, 0, 1
  store i64 %138, ptr %ds
  store i64 %139, ptr %alloc
  %140 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %137, 1, 0
  store ptr addrspace(1) %140, ptr %5
  br label %L124
L124:
  ; anon:V/91 := pin:anon:V/0[x0] 
  %141 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %141, ptr %40
  ; pin:anon:V/0[x0] := anon:V/91 
  %142 = load ptr addrspace(1), ptr %40
  store ptr addrspace(1) %142, ptr %5
  ; reload retaddr 
  ; return pin:anon:V/0[x0] 
  %143 = load ptr addrspace(1), ptr %5
  %144 = ptrtoint ptr addrspace(1) %143 to i64
  %145 = load i64, ptr %ds
  %146 = load i64, ptr %alloc
  %147 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %145, 0, 0
  %148 = insertvalue { { i64, i64 }, { i64 } } %147, i64 %146, 0, 1
  %149 = insertvalue { { i64, i64 }, { i64 } } %148, i64 %144, 1, 0
  ret { { i64, i64 }, { i64 } } %149
}

; camlLoop_invariant_gc_across_call__loop_2_6_code [ loop_invariant_gc_across_call.ml:9,13--119 ]
define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlLoop_invariant_gc_across_call__loop_2_6_code"(i64 %0, i64 %1, ptr addrspace(1) %2, i64 %3, i64 %4) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %6 = alloca ptr addrspace(1) ; ; pin:anon:V/0[x0]
  store ptr addrspace(1) %2, ptr %6
  %7 = alloca i64 ; ; pin:anon:I/1[x1]
  store i64 %3, ptr %7
  %8 = alloca i64 ; ; pin:anon:I/2[x2]
  store i64 %4, ptr %8
  %9 = alloca i64 ; ; pin:anon:I/0[x0]
  %10 = alloca ptr addrspace(1) ; ; x:V/60
  %11 = alloca i64 ; ; i:I/61
  %12 = alloca i64 ; ; acc:I/62
  %13 = alloca i64 ; ; i:I/63
  %14 = alloca i64 ; ; acc:I/64
  %15 = alloca i64 ; ; anon:I/65
  %16 = alloca i64 ; ; anon:I/66
  %17 = alloca i64 ; ; anon:I/67
  %18 = alloca i64 ; ; y:I/68
  %19 = alloca ptr addrspace(1) ; ; anon:A/69
  %20 = alloca i64 ; ; anon:I/70
  %21 = alloca i64 ; ; anon:I/71
  %22 = alloca i64 ; ; anon:I/72
  %23 = alloca i64 ; ; anon:I/73
  %24 = alloca i64 ; ; anon:I/74
  %25 = alloca i64 ; ; tmp:I/75
  %26 = alloca ptr addrspace(1) ; ; anon:A/76
  %27 = alloca i64 ; ; anon:I/77
  %28 = alloca i64 ; ; anon:I/78
  %29 = alloca i64 ; ; anon:I/79
  %30 = alloca i64 ; ; anon:I/80
  %31 = alloca i64 ; ; anon:I/81
  %32 = alloca i64 ; ; anon:I/82
  %33 = alloca i64 ; ; anon:I/83
  %34 = alloca i64 ; ; anon:I/84
  br label %L1
L1:
  ; goto 128 
  br label %L128
L128:
  ; stack check (16 bytes) 
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
  ; x:V/60 := pin:anon:V/0[x0] 
  %48 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %48, ptr %10
  ; i:I/61 := pin:anon:I/1[x1] 
  %49 = load i64, ptr %7
  store i64 %49, ptr %11
  ; acc:I/62 := pin:anon:I/2[x2] 
  %50 = load i64, ptr %8
  store i64 %50, ptr %12
  ; anon:I/65 := i:I/61 
  %51 = load i64, ptr %11
  store i64 %51, ptr %15
  ; anon:I/66 := acc:I/62 
  %52 = load i64, ptr %12
  store i64 %52, ptr %16
  ; i:I/63 := anon:I/65 
  %53 = load i64, ptr %15
  store i64 %53, ptr %13
  ; acc:I/64 := anon:I/66 
  %54 = load i64, ptr %16
  store i64 %54, ptr %14
  ; if i:I/63 < s 1 goto 135if i:I/63 = s 1 goto 135if i:I/63 > s 1 goto 137 
  %55 = load i64, ptr %13
  %56 = icmp slt i64 %55, 1
  br i1 %56, label %L135, label %L157
L157:
  %57 = load i64, ptr %13
  %58 = icmp sgt i64 %57, 1
  br i1 %58, label %L137, label %L135
L135:
  ; pin:anon:I/0[x0] := acc:I/64 
  %59 = load i64, ptr %14
  store i64 %59, ptr %9
  ; reload retaddr 
  ; return pin:anon:I/0[x0] 
  %60 = load i64, ptr %9
  %61 = load i64, ptr %ds
  %62 = load i64, ptr %alloc
  %63 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %61, 0, 0
  %64 = insertvalue { { i64, i64 }, { i64 } } %63, i64 %62, 0, 1
  %65 = insertvalue { { i64, i64 }, { i64 } } %64, i64 %60, 1, 0
  ret { { i64, i64 }, { i64 } } %65
L137:
  ; pin:anon:I/0[x0] := i:I/63 
  %66 = load i64, ptr %13
  store i64 %66, ptr %9
  ; pin:anon:I/0[x0] := call "camlLoop_invariant_gc_across_call__tick_0_4_code" pin:anon:I/0[x0]            goto 139 [ loop_invariant_gc_across_call.ml:12,12--18 ]
  %67 = load i64, ptr %9
  %68 = load i64, ptr %ds
  %69 = load i64, ptr %alloc
  %70 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlLoop_invariant_gc_across_call__tick_0_4_code"(i64 %68, i64 %69, i64 %67) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 12, i64 0, i64 12, i64 18, i64 0, i64 18, i64 32, i64 7303020, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 27757, i64 34, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 7303020, i64 112) ]
  %71 = extractvalue { { i64, i64 }, { i64 } } %70, 0, 0
  %72 = extractvalue { { i64, i64 }, { i64 } } %70, 0, 1
  store i64 %71, ptr %ds
  store i64 %72, ptr %alloc
  %73 = extractvalue { { i64, i64 }, { i64 } } %70, 1, 0
  store i64 %73, ptr %9
  br label %L139
L139:
  ; anon:I/67 := pin:anon:I/0[x0] 
  %74 = load i64, ptr %9
  store i64 %74, ptr %17
  ; y:I/68 := anon:I/67 
  %75 = load i64, ptr %17
  store i64 %75, ptr %18
  ; anon:A/69 := x:V/60 + -8 [ loop_invariant_gc_across_call.ml:13,26--41 ]
  %76 = load ptr addrspace(1), ptr %10
  %77 = getelementptr i8, ptr addrspace(1) %76, i64 -8
  store ptr addrspace(1) %77, ptr %19
  ; anon:I/70 := int  mut[anon:A/69] [ loop_invariant_gc_across_call.ml:13,26--41 ]
  %78 = load ptr addrspace(1), ptr %19
  %79 = load i64, ptr addrspace(1) %78
  store i64 %79, ptr %20
  ; anon:I/71 := anon:I/70 << 8 [ loop_invariant_gc_across_call.ml:13,26--41 ]
  %80 = load i64, ptr %20
  %81 = shl i64 %80, 8
  store i64 %81, ptr %21
  ; anon:I/72 := anon:I/71 >>u 18 [ loop_invariant_gc_across_call.ml:13,26--41 ]
  %82 = load i64, ptr %21
  %83 = lshr i64 %82, 18
  store i64 %83, ptr %22
  ; anon:I/73 := anon:I/72 << 3 [ loop_invariant_gc_across_call.ml:13,26--41 ]
  %84 = load i64, ptr %22
  %85 = shl i64 %84, 3
  store i64 %85, ptr %23
  ; anon:I/74 := anon:I/73 - 1 [ loop_invariant_gc_across_call.ml:13,26--41 ]
  %86 = load i64, ptr %23
  %87 = sub i64 %86, 1
  store i64 %87, ptr %24
  ; tmp:I/75 := anon:I/74 
  %88 = load i64, ptr %24
  store i64 %88, ptr %25
  ; anon:A/76 := x:V/60 + tmp:I/75 [ loop_invariant_gc_across_call.ml:13,26--41 ]
  %89 = load ptr addrspace(1), ptr %10
  %90 = load i64, ptr %25
  %91 = getelementptr i8, ptr addrspace(1) %89, i64 %90
  store ptr addrspace(1) %91, ptr %26
  ; anon:I/77 := unsigned int8  mut[anon:A/76] [ loop_invariant_gc_across_call.ml:13,26--41 ]
  %92 = load ptr addrspace(1), ptr %26
  %93 = load i8, ptr addrspace(1) %92
  %94 = zext i8 %93 to i64
  store i64 %94, ptr %27
  ; anon:I/78 := tmp:I/75 - anon:I/77 [ loop_invariant_gc_across_call.ml:13,26--41 ]
  %95 = load i64, ptr %25
  %96 = load i64, ptr %27
  %97 = sub i64 %95, %96
  store i64 %97, ptr %28
  ; anon:I/79 := acc:I/64 + anon:I/78 << 1 [ loop_invariant_gc_across_call.ml:13,20--41 ]
  %98 = load i64, ptr %28
  %99 = shl i64 %98, 1
  %100 = load i64, ptr %14
  %101 = add i64 %100, %99
  store i64 %101, ptr %29
  ; anon:I/80 := anon:I/79 + y:I/68 [ loop_invariant_gc_across_call.ml:13,19--46 ]
  %102 = load i64, ptr %29
  %103 = load i64, ptr %18
  %104 = add i64 %102, %103
  store i64 %104, ptr %30
  ; anon:I/81 := anon:I/80 + -1 [ loop_invariant_gc_across_call.ml:13,19--46 ]
  %105 = load i64, ptr %30
  %106 = add i64 %105, -1
  store i64 %106, ptr %31
  ; anon:I/82 := i:I/63 + -2 [ loop_invariant_gc_across_call.ml:13,11--18 ]
  %107 = load i64, ptr %13
  %108 = add i64 %107, -2
  store i64 %108, ptr %32
  ; anon:I/83 := anon:I/82 
  %109 = load i64, ptr %32
  store i64 %109, ptr %33
  ; anon:I/84 := anon:I/81 
  %110 = load i64, ptr %31
  store i64 %110, ptr %34
  ; i:I/63 := anon:I/83 
  %111 = load i64, ptr %33
  store i64 %111, ptr %13
  ; acc:I/64 := anon:I/84 
  %112 = load i64, ptr %34
  store i64 %112, ptr %14
  ; if i:I/63 < s 1 goto 135if i:I/63 = s 1 goto 135if i:I/63 > s 1 goto 137 
  %113 = load i64, ptr %13
  %114 = icmp slt i64 %113, 1
  br i1 %114, label %L135, label %L158
L158:
  %115 = load i64, ptr %13
  %116 = icmp sgt i64 %115, 1
  br i1 %116, label %L137, label %L135
}

; camlLoop_invariant_gc_across_call__run_3_7_code [ loop_invariant_gc_across_call.ml:15,8--162 ]
define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlLoop_invariant_gc_across_call__run_3_7_code"(i64 %0, i64 %1, i64 %2, i64 %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %5 = alloca i64 ; ; pin:anon:I/0[x0]
  store i64 %2, ptr %5
  %6 = alloca i64 ; ; pin:anon:I/1[x1]
  store i64 %3, ptr %6
  %7 = alloca i64 ; ; n:I/60
  %8 = alloca i64 ; ; reps:I/61
  %9 = alloca i64 ; ; anon:I/62
  %10 = alloca ptr addrspace(1) ; ; anon:V/63
  %11 = alloca ptr addrspace(1) ; ; x:V/64
  %12 = alloca i64 ; ; anon:I/65
  %13 = alloca i64 ; ; for_stop_naked:I/66
  %14 = alloca i64 ; ; for_counter_naked:I/67
  %15 = alloca i64 ; ; acc_452_unboxed0:I/68
  %16 = alloca i64 ; ; anon:I/69
  %17 = alloca i64 ; ; anon:I/70
  %18 = alloca i64 ; ; anon:I/71
  %19 = alloca i64 ; ; anon:I/72
  %20 = alloca ptr addrspace(1) ; ; acc:V/73
  %21 = alloca i64 ; ; i:I/74
  %22 = alloca i64 ; ; acc:I/75
  %23 = alloca i64 ; ; anon:I/76
  %24 = alloca i64 ; ; anon:I/77
  %25 = alloca i64 ; ; anon:I/78
  %26 = alloca i64 ; ; anon:I/79
  %27 = alloca i64 ; ; anon:I/80
  %28 = alloca i64 ; ; y:I/81
  %29 = alloca ptr addrspace(1) ; ; anon:A/82
  %30 = alloca i64 ; ; anon:I/83
  %31 = alloca i64 ; ; anon:I/84
  %32 = alloca i64 ; ; anon:I/85
  %33 = alloca i64 ; ; anon:I/86
  %34 = alloca i64 ; ; anon:I/87
  %35 = alloca i64 ; ; tmp:I/88
  %36 = alloca ptr addrspace(1) ; ; anon:A/89
  %37 = alloca i64 ; ; anon:I/90
  %38 = alloca i64 ; ; anon:I/91
  %39 = alloca i64 ; ; anon:I/92
  %40 = alloca i64 ; ; anon:I/93
  %41 = alloca i64 ; ; anon:I/94
  %42 = alloca i64 ; ; anon:I/95
  %43 = alloca i64 ; ; anon:I/96
  %44 = alloca i64 ; ; anon:I/97
  %45 = alloca i64 ; ; anon:I/98
  %46 = alloca i64 ; ; anon:I/99
  %47 = alloca i64 ; ; int_add:I/100
  %48 = alloca i64 ; ; anon:I/101
  %49 = alloca i64 ; ; for_next_naked:I/102
  %50 = alloca i64 ; ; anon:I/103
  %51 = alloca i64 ; ; anon:I/104
  %52 = alloca i64 ; ; anon:I/105
  br label %L1
L1:
  ; goto 160 
  br label %L160
L160:
  ; n:I/60 := pin:anon:I/0[x0] 
  %53 = load i64, ptr %5
  store i64 %53, ptr %7
  ; reps:I/61 := pin:anon:I/1[x1] 
  %54 = load i64, ptr %6
  store i64 %54, ptr %8
  ; anon:I/62 := "camlLoop_invariant_gc_across_call__immstring48" 
  %55 = ptrtoint ptr @"\01_camlLoop_invariant_gc_across_call__immstring48" to i64
  store i64 %55, ptr %9
  ; anon:I/62 := opaque anon:I/62 [ loop_invariant_gc_across_call.ml:16,10--54 ]
  %56 = load i64, ptr %9
  %57 = call i64 asm  "", "=r,0"(i64 %56) "gc-leaf-function"="true"
  store i64 %57, ptr %9
  ; anon:V/63 := anon:I/62 
  %58 = load i64, ptr %9
  %59 = inttoptr i64 %58 to ptr addrspace(1)
  store ptr addrspace(1) %59, ptr %10
  ; x:V/64 := anon:V/63 
  %60 = load ptr addrspace(1), ptr %10
  store ptr addrspace(1) %60, ptr %11
  ; if reps:I/61 < s 3 goto 210if reps:I/61 = s 3 goto 162if reps:I/61 > s 3 goto 162 
  %61 = load i64, ptr %8
  %62 = icmp slt i64 %61, 3
  br i1 %62, label %L210, label %L213
L213:
  %63 = load i64, ptr %8
  %64 = icmp sgt i64 %63, 3
  br i1 %64, label %L162, label %L162
L162:
  ; stack check (16 bytes) 
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
  ; anon:I/65 := reps:I/61 >>s 1 [ loop_invariant_gc_across_call.ml:18,2--58 ]
  %78 = load i64, ptr %8
  %79 = ashr i64 %78, 1
  store i64 %79, ptr %12
  ; for_stop_naked:I/66 := anon:I/65 
  %80 = load i64, ptr %12
  store i64 %80, ptr %13
  ; anon:I/69 := 1 
  ; anon:I/70 := 1 
  ; anon:I/71 := anon:I/70 
  store i64 1, ptr %18
  ; anon:I/72 := anon:I/69 
  store i64 1, ptr %19
  ; for_counter_naked:I/67 := anon:I/71 
  %81 = load i64, ptr %18
  store i64 %81, ptr %14
  ; acc_452_unboxed0:I/68 := anon:I/72 
  %82 = load i64, ptr %19
  store i64 %82, ptr %15
  ; goto 174 
  br label %L174
L174:
  ; anon:I/76 := 1 
  ; anon:I/77 := n:I/60 
  %83 = load i64, ptr %7
  store i64 %83, ptr %24
  ; anon:I/78 := anon:I/76 
  store i64 1, ptr %25
  ; i:I/74 := anon:I/77 
  %84 = load i64, ptr %24
  store i64 %84, ptr %21
  ; acc:I/75 := anon:I/78 
  %85 = load i64, ptr %25
  store i64 %85, ptr %22
  ; if i:I/74 < s 1 goto 178if i:I/74 = s 1 goto 178if i:I/74 > s 1 goto 180 
  %86 = load i64, ptr %21
  %87 = icmp slt i64 %86, 1
  br i1 %87, label %L178, label %L216
L216:
  %88 = load i64, ptr %21
  %89 = icmp sgt i64 %88, 1
  br i1 %89, label %L180, label %L178
L178:
  ; anon:I/79 := acc:I/75 
  %90 = load i64, ptr %22
  store i64 %90, ptr %26
  ; acc:V/73 := anon:I/79 
  %91 = load i64, ptr %26
  %92 = inttoptr i64 %91 to ptr addrspace(1)
  store ptr addrspace(1) %92, ptr %20
  ; anon:I/98 := acc_452_unboxed0:I/68 + acc:V/73 [ loop_invariant_gc_across_call.ml:19,11--28 ]
  %93 = load i64, ptr %15
  %94 = load ptr addrspace(1), ptr %20
  %95 = ptrtoint ptr addrspace(1) %94 to i64
  %96 = add i64 %93, %95
  store i64 %96, ptr %45
  ; anon:I/99 := anon:I/98 + -1 [ loop_invariant_gc_across_call.ml:19,11--28 ]
  %97 = load i64, ptr %45
  %98 = add i64 %97, -1
  store i64 %98, ptr %46
  ; int_add:I/100 := anon:I/99 
  %99 = load i64, ptr %46
  store i64 %99, ptr %47
  ; anon:I/101 := for_counter_naked:I/67 + 1 [ loop_invariant_gc_across_call.ml:18,2--58 ]
  %100 = load i64, ptr %14
  %101 = add i64 %100, 1
  store i64 %101, ptr %48
  ; for_next_naked:I/102 := anon:I/101 
  %102 = load i64, ptr %48
  store i64 %102, ptr %49
  ; if for_next_naked:I/102 < s for_stop_naked:I/66 goto 203if for_next_naked:I/102 = s for_stop_naked:I/66 goto 203if for_next_naked:I/102 > s for_stop_naked:I/66 goto 205 
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
  ; pin:anon:I/0[x0] := i:I/74 
  %109 = load i64, ptr %21
  store i64 %109, ptr %5
  ; pin:anon:I/0[x0] := call "camlLoop_invariant_gc_across_call__tick_0_4_code" pin:anon:I/0[x0]            goto 182 [ loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:12,12--18 ]
  %110 = load i64, ptr %5
  %111 = load i64, ptr %ds
  %112 = load i64, ptr %alloc
  %113 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlLoop_invariant_gc_across_call__tick_0_4_code"(i64 %111, i64 %112, i64 %110) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 2, i64 12, i64 0, i64 12, i64 18, i64 0, i64 18, i64 32, i64 7303020, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 27757, i64 34, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 7303020, i64 112, i64 19, i64 0, i64 18, i64 28, i64 0, i64 28, i64 32, i64 7303020, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 27757, i64 33, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 7239026) ]
  %114 = extractvalue { { i64, i64 }, { i64 } } %113, 0, 0
  %115 = extractvalue { { i64, i64 }, { i64 } } %113, 0, 1
  store i64 %114, ptr %ds
  store i64 %115, ptr %alloc
  %116 = extractvalue { { i64, i64 }, { i64 } } %113, 1, 0
  store i64 %116, ptr %5
  br label %L182
L182:
  ; anon:I/80 := pin:anon:I/0[x0] 
  %117 = load i64, ptr %5
  store i64 %117, ptr %27
  ; y:I/81 := anon:I/80 
  %118 = load i64, ptr %27
  store i64 %118, ptr %28
  ; anon:A/82 := x:V/64 + -8 [ loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,26--41 ]
  %119 = load ptr addrspace(1), ptr %11
  %120 = getelementptr i8, ptr addrspace(1) %119, i64 -8
  store ptr addrspace(1) %120, ptr %29
  ; anon:I/83 := int  mut[anon:A/82] [ loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,26--41 ]
  %121 = load ptr addrspace(1), ptr %29
  %122 = load i64, ptr addrspace(1) %121
  store i64 %122, ptr %30
  ; anon:I/84 := anon:I/83 << 8 [ loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,26--41 ]
  %123 = load i64, ptr %30
  %124 = shl i64 %123, 8
  store i64 %124, ptr %31
  ; anon:I/85 := anon:I/84 >>u 18 [ loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,26--41 ]
  %125 = load i64, ptr %31
  %126 = lshr i64 %125, 18
  store i64 %126, ptr %32
  ; anon:I/86 := anon:I/85 << 3 [ loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,26--41 ]
  %127 = load i64, ptr %32
  %128 = shl i64 %127, 3
  store i64 %128, ptr %33
  ; anon:I/87 := anon:I/86 - 1 [ loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,26--41 ]
  %129 = load i64, ptr %33
  %130 = sub i64 %129, 1
  store i64 %130, ptr %34
  ; tmp:I/88 := anon:I/87 
  %131 = load i64, ptr %34
  store i64 %131, ptr %35
  ; anon:A/89 := x:V/64 + tmp:I/88 [ loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,26--41 ]
  %132 = load ptr addrspace(1), ptr %11
  %133 = load i64, ptr %35
  %134 = getelementptr i8, ptr addrspace(1) %132, i64 %133
  store ptr addrspace(1) %134, ptr %36
  ; anon:I/90 := unsigned int8  mut[anon:A/89] [ loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,26--41 ]
  %135 = load ptr addrspace(1), ptr %36
  %136 = load i8, ptr addrspace(1) %135
  %137 = zext i8 %136 to i64
  store i64 %137, ptr %37
  ; anon:I/91 := tmp:I/88 - anon:I/90 [ loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,26--41 ]
  %138 = load i64, ptr %35
  %139 = load i64, ptr %37
  %140 = sub i64 %138, %139
  store i64 %140, ptr %38
  ; anon:I/92 := acc:I/75 + anon:I/91 << 1 [ loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,20--41 ]
  %141 = load i64, ptr %38
  %142 = shl i64 %141, 1
  %143 = load i64, ptr %22
  %144 = add i64 %143, %142
  store i64 %144, ptr %39
  ; anon:I/93 := anon:I/92 + y:I/81 [ loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,19--46 ]
  %145 = load i64, ptr %39
  %146 = load i64, ptr %28
  %147 = add i64 %145, %146
  store i64 %147, ptr %40
  ; anon:I/94 := anon:I/93 + -1 [ loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,19--46 ]
  %148 = load i64, ptr %40
  %149 = add i64 %148, -1
  store i64 %149, ptr %41
  ; anon:I/95 := i:I/74 + -2 [ loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,11--18 ]
  %150 = load i64, ptr %21
  %151 = add i64 %150, -2
  store i64 %151, ptr %42
  ; anon:I/96 := anon:I/95 
  %152 = load i64, ptr %42
  store i64 %152, ptr %43
  ; anon:I/97 := anon:I/94 
  %153 = load i64, ptr %41
  store i64 %153, ptr %44
  ; i:I/74 := anon:I/96 
  %154 = load i64, ptr %43
  store i64 %154, ptr %21
  ; acc:I/75 := anon:I/97 
  %155 = load i64, ptr %44
  store i64 %155, ptr %22
  ; if i:I/74 < s 1 goto 178if i:I/74 = s 1 goto 178if i:I/74 > s 1 goto 180 
  %156 = load i64, ptr %21
  %157 = icmp slt i64 %156, 1
  br i1 %157, label %L178, label %L218
L218:
  %158 = load i64, ptr %21
  %159 = icmp sgt i64 %158, 1
  br i1 %159, label %L180, label %L178
L203:
  ; anon:I/103 := for_next_naked:I/102 
  %160 = load i64, ptr %49
  store i64 %160, ptr %50
  ; anon:I/104 := int_add:I/100 
  %161 = load i64, ptr %47
  store i64 %161, ptr %51
  ; for_counter_naked:I/67 := anon:I/103 
  %162 = load i64, ptr %50
  store i64 %162, ptr %14
  ; acc_452_unboxed0:I/68 := anon:I/104 
  %163 = load i64, ptr %51
  store i64 %163, ptr %15
  ; goto 174 
  br label %L174
L205:
  ; pin:anon:I/0[x0] := int_add:I/100 
  %164 = load i64, ptr %47
  store i64 %164, ptr %5
  ; reload retaddr 
  ; return pin:anon:I/0[x0] 
  %165 = load i64, ptr %5
  %166 = load i64, ptr %ds
  %167 = load i64, ptr %alloc
  %168 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %166, 0, 0
  %169 = insertvalue { { i64, i64 }, { i64 } } %168, i64 %167, 0, 1
  %170 = insertvalue { { i64, i64 }, { i64 } } %169, i64 %165, 1, 0
  ret { { i64, i64 }, { i64 } } %170
L210:
  ; anon:I/105 := 1 
  ; pin:anon:I/0[x0] := anon:I/105 
  store i64 1, ptr %5
  ; reload retaddr 
  ; return pin:anon:I/0[x0] 
  %171 = load i64, ptr %5
  %172 = load i64, ptr %ds
  %173 = load i64, ptr %alloc
  %174 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %172, 0, 0
  %175 = insertvalue { { i64, i64 }, { i64 } } %174, i64 %173, 0, 1
  %176 = insertvalue { { i64, i64 }, { i64 } } %175, i64 %171, 1, 0
  ret { { i64, i64 }, { i64 } } %176
}

; camlLoop_invariant_gc_across_call__entry 
define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlLoop_invariant_gc_across_call__entry"(i64 %0, i64 %1) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %3 = alloca ptr addrspace(1) ; ; pin:anon:V/0[x0]
  %4 = alloca i64 ; ; pin:anon:I/0[x0]
  %5 = alloca ptr addrspace(1) ; ; pin:anon:V/1[x1]
  %6 = alloca ptr addrspace(1) ; ; pin:anon:V/2[x2]
  %7 = alloca ptr addrspace(1) ; ; pin:anon:V/3[x3]
  %8 = alloca ptr addrspace(1) ; ; *ret*:V/60
  %9 = alloca ptr addrspace(1) ; ; int_add:V/61
  %10 = alloca i64 ; ; anon:I/62
  %11 = alloca ptr addrspace(1) ; ; anon:V/63
  %12 = alloca ptr addrspace(1) ; ; x:V/64
  %13 = alloca i64 ; ; for_counter_naked:I/65
  %14 = alloca i64 ; ; acc_452_unboxed0:I/66
  %15 = alloca i64 ; ; anon:I/67
  %16 = alloca i64 ; ; anon:I/68
  %17 = alloca i64 ; ; anon:I/69
  %18 = alloca i64 ; ; anon:I/70
  %19 = alloca ptr addrspace(1) ; ; acc:V/71
  %20 = alloca i64 ; ; i:I/72
  %21 = alloca i64 ; ; acc:I/73
  %22 = alloca i64 ; ; anon:I/74
  %23 = alloca i64 ; ; anon:I/75
  %24 = alloca i64 ; ; anon:I/76
  %25 = alloca i64 ; ; anon:I/77
  %26 = alloca i64 ; ; anon:I/78
  %27 = alloca i64 ; ; anon:I/79
  %28 = alloca i64 ; ; y:I/80
  %29 = alloca ptr addrspace(1) ; ; anon:A/81
  %30 = alloca i64 ; ; anon:I/82
  %31 = alloca i64 ; ; anon:I/83
  %32 = alloca i64 ; ; anon:I/84
  %33 = alloca i64 ; ; anon:I/85
  %34 = alloca i64 ; ; anon:I/86
  %35 = alloca i64 ; ; tmp:I/87
  %36 = alloca ptr addrspace(1) ; ; anon:A/88
  %37 = alloca i64 ; ; anon:I/89
  %38 = alloca i64 ; ; anon:I/90
  %39 = alloca i64 ; ; anon:I/91
  %40 = alloca i64 ; ; anon:I/92
  %41 = alloca i64 ; ; anon:I/93
  %42 = alloca i64 ; ; anon:I/94
  %43 = alloca i64 ; ; anon:I/95
  %44 = alloca i64 ; ; anon:I/96
  %45 = alloca i64 ; ; anon:I/97
  %46 = alloca i64 ; ; anon:I/98
  %47 = alloca i64 ; ; int_add:I/99
  %48 = alloca i64 ; ; anon:I/100
  %49 = alloca i64 ; ; for_next_naked:I/101
  %50 = alloca i64 ; ; anon:I/102
  %51 = alloca i64 ; ; anon:I/103
  %52 = alloca i64 ; ; anon:I/104
  %53 = alloca i64 ; ; anon:I/105
  %54 = alloca ptr addrspace(1) ; ; anon:V/106
  %55 = alloca ptr addrspace(1) ; ; apply_result:V/107
  %56 = alloca ptr addrspace(1) ; ; anon:A/108
  %57 = alloca i64 ; ; anon:I/109
  %58 = alloca i64 ; ; anon:I/110
  %59 = alloca i64 ; ; anon:I/111
  %60 = alloca i64 ; ; anon:I/112
  %61 = alloca i64 ; ; anon:I/113
  %62 = alloca i64 ; ; tmp:I/114
  %63 = alloca ptr addrspace(1) ; ; anon:A/115
  %64 = alloca i64 ; ; anon:I/116
  %65 = alloca i64 ; ; anon:I/117
  %66 = alloca i64 ; ; anon:I/118
  %67 = alloca i64 ; ; anon:I/119
  %68 = alloca i64 ; ; anon:I/120
  %69 = alloca i64 ; ; anon:I/121
  %70 = alloca i64 ; ; anon:I/122
  %71 = alloca ptr addrspace(1) ; ; anon:V/123
  %72 = alloca ptr addrspace(1) ; ; anon:V/124
  %73 = alloca ptr addrspace(1) ; ; param:V/125
  %74 = alloca i64 ; ; anon:I/126
  %75 = alloca i64 ; ; anon:I/127
  %76 = alloca i64 ; ; anon:I/128
  %77 = alloca ptr addrspace(1) ; ; anon:V/129
  %78 = alloca ptr addrspace(1) ; ; anon:V/130
  %79 = alloca ptr addrspace(1) ; ; param:V/131
  %80 = alloca i64 ; ; anon:I/132
  %81 = alloca i64 ; ; anon:I/133
  %82 = alloca ptr addrspace(1) ; ; anon:V/134
  %83 = alloca ptr addrspace(1) ; ; anon:V/135
  %84 = alloca ptr addrspace(1) ; ; param:V/136
  %85 = alloca i64 ; ; anon:I/137
  %86 = alloca i64 ; ; anon:I/138
  %87 = alloca i64 ; ; anon:I/139
  br label %L1
L1:
  ; goto 226 
  br label %L226
L226:
  ; stack check (16 bytes) 
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
  ; anon:I/62 := "camlLoop_invariant_gc_across_call__immstring48" 
  %101 = ptrtoint ptr @"\01_camlLoop_invariant_gc_across_call__immstring48" to i64
  store i64 %101, ptr %10
  ; anon:I/62 := opaque anon:I/62 [ loop_invariant_gc_across_call.ml:23,22--38;loop_invariant_gc_across_call.ml:16,10--54 ]
  %102 = load i64, ptr %10
  %103 = call i64 asm  "", "=r,0"(i64 %102) "gc-leaf-function"="true"
  store i64 %103, ptr %10
  ; anon:V/63 := anon:I/62 
  %104 = load i64, ptr %10
  %105 = inttoptr i64 %104 to ptr addrspace(1)
  store ptr addrspace(1) %105, ptr %11
  ; x:V/64 := anon:V/63 
  %106 = load ptr addrspace(1), ptr %11
  store ptr addrspace(1) %106, ptr %12
  ; anon:I/67 := 1 
  ; anon:I/68 := 1 
  ; anon:I/69 := anon:I/68 
  store i64 1, ptr %17
  ; anon:I/70 := anon:I/67 
  store i64 1, ptr %18
  ; for_counter_naked:I/65 := anon:I/69 
  %107 = load i64, ptr %17
  store i64 %107, ptr %13
  ; acc_452_unboxed0:I/66 := anon:I/70 
  %108 = load i64, ptr %18
  store i64 %108, ptr %14
  ; goto 237 
  br label %L237
L237:
  ; anon:I/74 := 1 
  ; anon:I/75 := 24000001 
  ; anon:I/76 := anon:I/75 
  store i64 24000001, ptr %24
  ; anon:I/77 := anon:I/74 
  store i64 1, ptr %25
  ; i:I/72 := anon:I/76 
  %109 = load i64, ptr %24
  store i64 %109, ptr %20
  ; acc:I/73 := anon:I/77 
  %110 = load i64, ptr %25
  store i64 %110, ptr %21
  ; goto 243 
  br label %L243
L241:
  ; anon:I/78 := acc:I/73 
  %111 = load i64, ptr %21
  store i64 %111, ptr %26
  ; acc:V/71 := anon:I/78 
  %112 = load i64, ptr %26
  %113 = inttoptr i64 %112 to ptr addrspace(1)
  store ptr addrspace(1) %113, ptr %19
  ; anon:I/97 := acc_452_unboxed0:I/66 + acc:V/71 [ loop_invariant_gc_across_call.ml:23,22--38;loop_invariant_gc_across_call.ml:19,11--28 ]
  %114 = load i64, ptr %14
  %115 = load ptr addrspace(1), ptr %19
  %116 = ptrtoint ptr addrspace(1) %115 to i64
  %117 = add i64 %114, %116
  store i64 %117, ptr %45
  ; anon:I/98 := anon:I/97 + -1 [ loop_invariant_gc_across_call.ml:23,22--38;loop_invariant_gc_across_call.ml:19,11--28 ]
  %118 = load i64, ptr %45
  %119 = add i64 %118, -1
  store i64 %119, ptr %46
  ; int_add:I/99 := anon:I/98 
  %120 = load i64, ptr %46
  store i64 %120, ptr %47
  ; anon:I/100 := for_counter_naked:I/65 + 1 [ loop_invariant_gc_across_call.ml:23,22--38;loop_invariant_gc_across_call.ml:18,2--58 ]
  %121 = load i64, ptr %13
  %122 = add i64 %121, 1
  store i64 %122, ptr %48
  ; for_next_naked:I/101 := anon:I/100 
  %123 = load i64, ptr %48
  store i64 %123, ptr %49
  ; if for_next_naked:I/101 < s 5 goto 266if for_next_naked:I/101 = s 5 goto 266if for_next_naked:I/101 > s 5 goto 268 
  %124 = load i64, ptr %49
  %125 = icmp slt i64 %124, 5
  br i1 %125, label %L266, label %L301
L301:
  %126 = load i64, ptr %49
  %127 = icmp sgt i64 %126, 5
  br i1 %127, label %L268, label %L266
L243:
  ; pin:anon:I/0[x0] := i:I/72 
  %128 = load i64, ptr %20
  store i64 %128, ptr %4
  ; pin:anon:I/0[x0] := call "camlLoop_invariant_gc_across_call__tick_0_4_code" pin:anon:I/0[x0]            goto 245 [ loop_invariant_gc_across_call.ml:23,22--38;loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:12,12--18 ]
  %129 = load i64, ptr %4
  %130 = load i64, ptr %ds
  %131 = load i64, ptr %alloc
  %132 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlLoop_invariant_gc_across_call__tick_0_4_code"(i64 %130, i64 %131, i64 %129) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 3, i64 12, i64 0, i64 12, i64 18, i64 0, i64 18, i64 32, i64 7303020, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 27757, i64 34, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 7303020, i64 112, i64 19, i64 0, i64 18, i64 28, i64 0, i64 28, i64 32, i64 7303020, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 27757, i64 33, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 7239026, i64 23, i64 0, i64 22, i64 38, i64 0, i64 38, i64 32, i64 7303020, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 27757, i64 29, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 27756) ]
  %133 = extractvalue { { i64, i64 }, { i64 } } %132, 0, 0
  %134 = extractvalue { { i64, i64 }, { i64 } } %132, 0, 1
  store i64 %133, ptr %ds
  store i64 %134, ptr %alloc
  %135 = extractvalue { { i64, i64 }, { i64 } } %132, 1, 0
  store i64 %135, ptr %4
  br label %L245
L245:
  ; anon:I/79 := pin:anon:I/0[x0] 
  %136 = load i64, ptr %4
  store i64 %136, ptr %27
  ; y:I/80 := anon:I/79 
  %137 = load i64, ptr %27
  store i64 %137, ptr %28
  ; anon:A/81 := x:V/64 + -8 [ loop_invariant_gc_across_call.ml:23,22--38;loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,26--41 ]
  %138 = load ptr addrspace(1), ptr %12
  %139 = getelementptr i8, ptr addrspace(1) %138, i64 -8
  store ptr addrspace(1) %139, ptr %29
  ; anon:I/82 := int  mut[anon:A/81] [ loop_invariant_gc_across_call.ml:23,22--38;loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,26--41 ]
  %140 = load ptr addrspace(1), ptr %29
  %141 = load i64, ptr addrspace(1) %140
  store i64 %141, ptr %30
  ; anon:I/83 := anon:I/82 << 8 [ loop_invariant_gc_across_call.ml:23,22--38;loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,26--41 ]
  %142 = load i64, ptr %30
  %143 = shl i64 %142, 8
  store i64 %143, ptr %31
  ; anon:I/84 := anon:I/83 >>u 18 [ loop_invariant_gc_across_call.ml:23,22--38;loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,26--41 ]
  %144 = load i64, ptr %31
  %145 = lshr i64 %144, 18
  store i64 %145, ptr %32
  ; anon:I/85 := anon:I/84 << 3 [ loop_invariant_gc_across_call.ml:23,22--38;loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,26--41 ]
  %146 = load i64, ptr %32
  %147 = shl i64 %146, 3
  store i64 %147, ptr %33
  ; anon:I/86 := anon:I/85 - 1 [ loop_invariant_gc_across_call.ml:23,22--38;loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,26--41 ]
  %148 = load i64, ptr %33
  %149 = sub i64 %148, 1
  store i64 %149, ptr %34
  ; tmp:I/87 := anon:I/86 
  %150 = load i64, ptr %34
  store i64 %150, ptr %35
  ; anon:A/88 := x:V/64 + tmp:I/87 [ loop_invariant_gc_across_call.ml:23,22--38;loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,26--41 ]
  %151 = load ptr addrspace(1), ptr %12
  %152 = load i64, ptr %35
  %153 = getelementptr i8, ptr addrspace(1) %151, i64 %152
  store ptr addrspace(1) %153, ptr %36
  ; anon:I/89 := unsigned int8  mut[anon:A/88] [ loop_invariant_gc_across_call.ml:23,22--38;loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,26--41 ]
  %154 = load ptr addrspace(1), ptr %36
  %155 = load i8, ptr addrspace(1) %154
  %156 = zext i8 %155 to i64
  store i64 %156, ptr %37
  ; anon:I/90 := tmp:I/87 - anon:I/89 [ loop_invariant_gc_across_call.ml:23,22--38;loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,26--41 ]
  %157 = load i64, ptr %35
  %158 = load i64, ptr %37
  %159 = sub i64 %157, %158
  store i64 %159, ptr %38
  ; anon:I/91 := acc:I/73 + anon:I/90 << 1 [ loop_invariant_gc_across_call.ml:23,22--38;loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,20--41 ]
  %160 = load i64, ptr %38
  %161 = shl i64 %160, 1
  %162 = load i64, ptr %21
  %163 = add i64 %162, %161
  store i64 %163, ptr %39
  ; anon:I/92 := anon:I/91 + y:I/80 [ loop_invariant_gc_across_call.ml:23,22--38;loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,19--46 ]
  %164 = load i64, ptr %39
  %165 = load i64, ptr %28
  %166 = add i64 %164, %165
  store i64 %166, ptr %40
  ; anon:I/93 := anon:I/92 + -1 [ loop_invariant_gc_across_call.ml:23,22--38;loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,19--46 ]
  %167 = load i64, ptr %40
  %168 = add i64 %167, -1
  store i64 %168, ptr %41
  ; anon:I/94 := i:I/72 + -2 [ loop_invariant_gc_across_call.ml:23,22--38;loop_invariant_gc_across_call.ml:19,18--28;loop_invariant_gc_across_call.ml:13,11--18 ]
  %169 = load i64, ptr %20
  %170 = add i64 %169, -2
  store i64 %170, ptr %42
  ; anon:I/95 := anon:I/94 
  %171 = load i64, ptr %42
  store i64 %171, ptr %43
  ; anon:I/96 := anon:I/93 
  %172 = load i64, ptr %41
  store i64 %172, ptr %44
  ; i:I/72 := anon:I/95 
  %173 = load i64, ptr %43
  store i64 %173, ptr %20
  ; acc:I/73 := anon:I/96 
  %174 = load i64, ptr %44
  store i64 %174, ptr %21
  ; if i:I/72 < s 1 goto 241if i:I/72 = s 1 goto 241if i:I/72 > s 1 goto 243 
  %175 = load i64, ptr %20
  %176 = icmp slt i64 %175, 1
  br i1 %176, label %L241, label %L302
L302:
  %177 = load i64, ptr %20
  %178 = icmp sgt i64 %177, 1
  br i1 %178, label %L243, label %L241
L266:
  ; anon:I/102 := for_next_naked:I/101 
  %179 = load i64, ptr %49
  store i64 %179, ptr %50
  ; anon:I/103 := int_add:I/99 
  %180 = load i64, ptr %47
  store i64 %180, ptr %51
  ; for_counter_naked:I/65 := anon:I/102 
  %181 = load i64, ptr %50
  store i64 %181, ptr %13
  ; acc_452_unboxed0:I/66 := anon:I/103 
  %182 = load i64, ptr %51
  store i64 %182, ptr %14
  ; goto 237 
  br label %L237
L268:
  ; anon:I/104 := int_add:I/99 
  %183 = load i64, ptr %47
  store i64 %183, ptr %52
  ; int_add:V/61 := anon:I/104 
  %184 = load i64, ptr %52
  %185 = inttoptr i64 %184 to ptr addrspace(1)
  store ptr addrspace(1) %185, ptr %9
  ; anon:I/105 := "camlStdlib__immstring191" 
  %186 = ptrtoint ptr @"\01_camlStdlib__immstring191" to i64
  store i64 %186, ptr %53
  ; pin:anon:V/0[x0] := anon:I/105 
  %187 = load i64, ptr %53
  %188 = inttoptr i64 %187 to ptr addrspace(1)
  store ptr addrspace(1) %188, ptr %3
  ; pin:anon:V/1[x1] := int_add:V/61 
  %189 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %189, ptr %5
  ; pin:anon:V/0[x0] := extcall "caml_format_int" pin:anon:V/0[x0] pin:anon:V/1[x1]goto 275 [ loop_invariant_gc_across_call.ml:23,9--38;loop_invariant_gc_across_call.ml:5,2--13;stdlib.ml:507,39--56;stdlib.ml:280,2--19 ]
  %190 = ptrtoint ptr @"\01_caml_format_int" to i64
  %191 = load ptr addrspace(1), ptr %3
  %192 = load ptr addrspace(1), ptr %5
  %193 = load i64, ptr %ds
  %194 = load i64, ptr %alloc
  %195 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %193, i64 %194, i64 %190, ptr addrspace(1) %191, ptr addrspace(1) %192) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 280, i64 0, i64 2, i64 19, i64 0, i64 19, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7631662, i64 7235954, i64 7298919, i64 6905702, i64 29806, i64 507, i64 0, i64 39, i64 56, i64 0, i64 56, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 16, i64 6583379, i64 6449516, i64 7499822, i64 7630441, i64 7235935, i64 116, i64 5, i64 0, i64 2, i64 13, i64 0, i64 13, i64 32, i64 7303020, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 27757, i64 42, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 6910576, i64 6255726, i64 7562610, i64 7629941, i64 23, i64 0, i64 9, i64 38, i64 0, i64 38, i64 32, i64 7303020, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 27757, i64 29, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 27756) ]
  %196 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %195, 0, 0
  %197 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %195, 0, 1
  store i64 %196, ptr %ds
  store i64 %197, ptr %alloc
  %198 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %195, 1, 0
  store ptr addrspace(1) %198, ptr %3
  br label %L275
L275:
  ; anon:V/106 := pin:anon:V/0[x0] 
  %199 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %199, ptr %54
  ; apply_result:V/107 := anon:V/106 
  %200 = load ptr addrspace(1), ptr %54
  store ptr addrspace(1) %200, ptr %55
  ; anon:A/108 := apply_result:V/107 + -8 [ loop_invariant_gc_across_call.ml:23,9--38;loop_invariant_gc_across_call.ml:5,2--13;stdlib.ml:507,18--56;stdlib.ml:387,30--47 ]
  %201 = load ptr addrspace(1), ptr %55
  %202 = getelementptr i8, ptr addrspace(1) %201, i64 -8
  store ptr addrspace(1) %202, ptr %56
  ; anon:I/109 := int  mut[anon:A/108] [ loop_invariant_gc_across_call.ml:23,9--38;loop_invariant_gc_across_call.ml:5,2--13;stdlib.ml:507,18--56;stdlib.ml:387,30--47 ]
  %203 = load ptr addrspace(1), ptr %56
  %204 = load i64, ptr addrspace(1) %203
  store i64 %204, ptr %57
  ; anon:I/110 := anon:I/109 << 8 [ loop_invariant_gc_across_call.ml:23,9--38;loop_invariant_gc_across_call.ml:5,2--13;stdlib.ml:507,18--56;stdlib.ml:387,30--47 ]
  %205 = load i64, ptr %57
  %206 = shl i64 %205, 8
  store i64 %206, ptr %58
  ; anon:I/111 := anon:I/110 >>u 18 [ loop_invariant_gc_across_call.ml:23,9--38;loop_invariant_gc_across_call.ml:5,2--13;stdlib.ml:507,18--56;stdlib.ml:387,30--47 ]
  %207 = load i64, ptr %58
  %208 = lshr i64 %207, 18
  store i64 %208, ptr %59
  ; anon:I/112 := anon:I/111 << 3 [ loop_invariant_gc_across_call.ml:23,9--38;loop_invariant_gc_across_call.ml:5,2--13;stdlib.ml:507,18--56;stdlib.ml:387,30--47 ]
  %209 = load i64, ptr %59
  %210 = shl i64 %209, 3
  store i64 %210, ptr %60
  ; anon:I/113 := anon:I/112 - 1 [ loop_invariant_gc_across_call.ml:23,9--38;loop_invariant_gc_across_call.ml:5,2--13;stdlib.ml:507,18--56;stdlib.ml:387,30--47 ]
  %211 = load i64, ptr %60
  %212 = sub i64 %211, 1
  store i64 %212, ptr %61
  ; tmp:I/114 := anon:I/113 
  %213 = load i64, ptr %61
  store i64 %213, ptr %62
  ; anon:A/115 := apply_result:V/107 + tmp:I/114 [ loop_invariant_gc_across_call.ml:23,9--38;loop_invariant_gc_across_call.ml:5,2--13;stdlib.ml:507,18--56;stdlib.ml:387,30--47 ]
  %214 = load ptr addrspace(1), ptr %55
  %215 = load i64, ptr %62
  %216 = getelementptr i8, ptr addrspace(1) %214, i64 %215
  store ptr addrspace(1) %216, ptr %63
  ; anon:I/116 := unsigned int8  mut[anon:A/115] [ loop_invariant_gc_across_call.ml:23,9--38;loop_invariant_gc_across_call.ml:5,2--13;stdlib.ml:507,18--56;stdlib.ml:387,30--47 ]
  %217 = load ptr addrspace(1), ptr %63
  %218 = load i8, ptr addrspace(1) %217
  %219 = zext i8 %218 to i64
  store i64 %219, ptr %64
  ; anon:I/117 := tmp:I/114 - anon:I/116 [ loop_invariant_gc_across_call.ml:23,9--38;loop_invariant_gc_across_call.ml:5,2--13;stdlib.ml:507,18--56;stdlib.ml:387,30--47 ]
  %220 = load i64, ptr %62
  %221 = load i64, ptr %64
  %222 = sub i64 %220, %221
  store i64 %222, ptr %65
  ; anon:I/118 := 1 
  ; anon:I/119 := anon:I/118 + anon:I/117 << 1 [ loop_invariant_gc_across_call.ml:23,9--38;loop_invariant_gc_across_call.ml:5,2--13;stdlib.ml:507,18--56;stdlib.ml:387,30--47 ]
  %223 = load i64, ptr %65
  %224 = shl i64 %223, 1
  %225 = add i64 1, %224
  store i64 %225, ptr %67
  ; anon:I/120 := 1 
  ; anon:I/121 := "camlStdlib__print_int_136" 
  %226 = ptrtoint ptr @"\01_camlStdlib__print_int_136" to i64
  store i64 %226, ptr %69
  ; anon:I/122 := anon:I/121 + 16 [ loop_invariant_gc_across_call.ml:23,9--38;loop_invariant_gc_across_call.ml:5,2--13 ]
  %227 = load i64, ptr %69
  %228 = add i64 %227, 16
  store i64 %228, ptr %70
  ; anon:V/123 := val [anon:I/122] [ loop_invariant_gc_across_call.ml:23,9--38;loop_invariant_gc_across_call.ml:5,2--13 ]
  %229 = load i64, ptr %70
  %230 = inttoptr i64 %229 to ptr
  %231 = load ptr addrspace(1), ptr %230
  store ptr addrspace(1) %231, ptr %71
  ; pin:anon:V/0[x0] := anon:V/123 
  %232 = load ptr addrspace(1), ptr %71
  store ptr addrspace(1) %232, ptr %3
  ; pin:anon:V/1[x1] := apply_result:V/107 
  %233 = load ptr addrspace(1), ptr %55
  store ptr addrspace(1) %233, ptr %5
  ; pin:anon:V/2[x2] := anon:I/120 
  %234 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %234, ptr %6
  ; pin:anon:V/3[x3] := anon:I/119 
  %235 = load i64, ptr %67
  %236 = inttoptr i64 %235 to ptr addrspace(1)
  store ptr addrspace(1) %236, ptr %7
  ; pin:anon:V/0[x0] := extcall "caml_ml_output" pin:anon:V/0[x0] pin:anon:V/1[x1] pin:anon:V/2[x2] pin:anon:V/3[x3]goto 276 [ loop_invariant_gc_across_call.ml:23,9--38;loop_invariant_gc_across_call.ml:5,2--13;stdlib.ml:507,18--56;stdlib.ml:387,2--47 ]
  %237 = ptrtoint ptr @"\01_caml_ml_output" to i64
  %238 = load ptr addrspace(1), ptr %3
  %239 = load ptr addrspace(1), ptr %5
  %240 = load ptr addrspace(1), ptr %6
  %241 = load ptr addrspace(1), ptr %7
  %242 = load i64, ptr %ds
  %243 = load i64, ptr %alloc
  %244 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %242, i64 %243, i64 %237, ptr addrspace(1) %238, ptr addrspace(1) %239, ptr addrspace(1) %240, ptr addrspace(1) %241) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 387, i64 0, i64 2, i64 47, i64 0, i64 47, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7696174, i64 7696500, i64 7561076, i64 6910580, i64 26478, i64 507, i64 0, i64 18, i64 56, i64 0, i64 56, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 16, i64 6583379, i64 6449516, i64 7499822, i64 7630441, i64 7235935, i64 116, i64 5, i64 0, i64 2, i64 13, i64 0, i64 13, i64 32, i64 7303020, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 27757, i64 42, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 6910576, i64 6255726, i64 7562610, i64 7629941, i64 23, i64 0, i64 9, i64 38, i64 0, i64 38, i64 32, i64 7303020, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 27757, i64 29, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 27756) ]
  %245 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %244, 0, 0
  %246 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %244, 0, 1
  store i64 %245, ptr %ds
  store i64 %246, ptr %alloc
  %247 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %244, 1, 0
  store ptr addrspace(1) %247, ptr %3
  br label %L276
L276:
  ; anon:V/124 := pin:anon:V/0[x0] 
  %248 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %248, ptr %72
  ; param:V/125 := anon:V/124 
  %249 = load ptr addrspace(1), ptr %72
  store ptr addrspace(1) %249, ptr %73
  ; anon:I/126 := 21 
  ; anon:I/127 := "camlStdlib__print_int_136" 
  %250 = ptrtoint ptr @"\01_camlStdlib__print_int_136" to i64
  store i64 %250, ptr %75
  ; anon:I/128 := anon:I/127 + 16 [ loop_invariant_gc_across_call.ml:23,9--38;loop_invariant_gc_across_call.ml:5,2--13 ]
  %251 = load i64, ptr %75
  %252 = add i64 %251, 16
  store i64 %252, ptr %76
  ; anon:V/129 := val [anon:I/128] [ loop_invariant_gc_across_call.ml:23,9--38;loop_invariant_gc_across_call.ml:5,2--13 ]
  %253 = load i64, ptr %76
  %254 = inttoptr i64 %253 to ptr
  %255 = load ptr addrspace(1), ptr %254
  store ptr addrspace(1) %255, ptr %77
  ; pin:anon:V/0[x0] := anon:V/129 
  %256 = load ptr addrspace(1), ptr %77
  store ptr addrspace(1) %256, ptr %3
  ; pin:anon:V/1[x1] := anon:I/126 
  %257 = inttoptr i64 21 to ptr addrspace(1)
  store ptr addrspace(1) %257, ptr %5
  ; pin:anon:V/0[x0] := extcall "caml_ml_output_char" pin:anon:V/0[x0] pin:anon:V/1[x1]goto 289 [ loop_invariant_gc_across_call.ml:23,9--38;loop_invariant_gc_across_call.ml:6,2--18;stdlib.ml:511,23--46 ]
  %258 = ptrtoint ptr @"\01_caml_ml_output_char" to i64
  %259 = load ptr addrspace(1), ptr %3
  %260 = load ptr addrspace(1), ptr %5
  %261 = load i64, ptr %ds
  %262 = load i64, ptr %alloc
  %263 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %261, i64 %262, i64 %258, ptr addrspace(1) %259, ptr addrspace(1) %260) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 3, i64 511, i64 0, i64 23, i64 46, i64 0, i64 46, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7499822, i64 7630441, i64 6647391, i64 6909047, i64 25966, i64 6, i64 0, i64 2, i64 18, i64 0, i64 18, i64 32, i64 7303020, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 27757, i64 42, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 6910576, i64 6255726, i64 7562610, i64 7629941, i64 23, i64 0, i64 9, i64 38, i64 0, i64 38, i64 32, i64 7303020, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 27757, i64 29, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 27756) ]
  %264 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %263, 0, 0
  %265 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %263, 0, 1
  store i64 %264, ptr %ds
  store i64 %265, ptr %alloc
  %266 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %263, 1, 0
  store ptr addrspace(1) %266, ptr %3
  br label %L289
L289:
  ; anon:V/130 := pin:anon:V/0[x0] 
  %267 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %267, ptr %78
  ; param:V/131 := anon:V/130 
  %268 = load ptr addrspace(1), ptr %78
  store ptr addrspace(1) %268, ptr %79
  ; anon:I/132 := "camlStdlib__print_int_136" 
  %269 = ptrtoint ptr @"\01_camlStdlib__print_int_136" to i64
  store i64 %269, ptr %80
  ; anon:I/133 := anon:I/132 + 16 [ loop_invariant_gc_across_call.ml:23,9--38;loop_invariant_gc_across_call.ml:5,2--13 ]
  %270 = load i64, ptr %80
  %271 = add i64 %270, 16
  store i64 %271, ptr %81
  ; anon:V/134 := val [anon:I/133] [ loop_invariant_gc_across_call.ml:23,9--38;loop_invariant_gc_across_call.ml:5,2--13 ]
  %272 = load i64, ptr %81
  %273 = inttoptr i64 %272 to ptr
  %274 = load ptr addrspace(1), ptr %273
  store ptr addrspace(1) %274, ptr %82
  ; pin:anon:V/0[x0] := anon:V/134 
  %275 = load ptr addrspace(1), ptr %82
  store ptr addrspace(1) %275, ptr %3
  ; pin:anon:V/0[x0] := extcall "caml_ml_flush" pin:anon:V/0[x0]goto 292 [ loop_invariant_gc_across_call.ml:23,9--38;loop_invariant_gc_across_call.ml:6,2--18;stdlib.ml:511,48--60 ]
  %276 = ptrtoint ptr @"\01_caml_ml_flush" to i64
  %277 = load ptr addrspace(1), ptr %3
  %278 = load i64, ptr %ds
  %279 = load i64, ptr %alloc
  %280 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %278, i64 %279, i64 %276, ptr addrspace(1) %277) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 3, i64 511, i64 0, i64 48, i64 60, i64 0, i64 60, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7499822, i64 7630441, i64 6647391, i64 6909047, i64 25966, i64 6, i64 0, i64 2, i64 18, i64 0, i64 18, i64 32, i64 7303020, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 27757, i64 42, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 6910576, i64 6255726, i64 7562610, i64 7629941, i64 23, i64 0, i64 9, i64 38, i64 0, i64 38, i64 32, i64 7303020, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 3042412, i64 27757, i64 29, i64 7302988, i64 6905712, i64 6387310, i64 6383986, i64 6255726, i64 6251367, i64 7496545, i64 7566191, i64 6382431, i64 27756) ]
  %281 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %280, 0, 0
  %282 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %280, 0, 1
  store i64 %281, ptr %ds
  store i64 %282, ptr %alloc
  %283 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %280, 1, 0
  store ptr addrspace(1) %283, ptr %3
  br label %L292
L292:
  ; anon:V/135 := pin:anon:V/0[x0] 
  %284 = load ptr addrspace(1), ptr %3
  store ptr addrspace(1) %284, ptr %83
  ; param:V/136 := anon:V/135 
  %285 = load ptr addrspace(1), ptr %83
  store ptr addrspace(1) %285, ptr %84
  ; anon:I/137 := "camlLoop_invariant_gc_across_call" 
  %286 = ptrtoint ptr @"\01_camlLoop_invariant_gc_across_call" to i64
  store i64 %286, ptr %85
  ; anon:I/138 := anon:I/137 
  %287 = load i64, ptr %85
  store i64 %287, ptr %86
  ; *ret*:V/60 := anon:I/138 
  %288 = load i64, ptr %86
  %289 = inttoptr i64 %288 to ptr addrspace(1)
  store ptr addrspace(1) %289, ptr %8
  ; anon:I/139 := 1 
  ; pin:anon:I/0[x0] := anon:I/139 
  store i64 1, ptr %4
  ; reload retaddr 
  ; return pin:anon:I/0[x0] 
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
