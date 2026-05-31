(* TEST
 macos;
 arch_arm64;
 flags += " -O3 -llvm-backend";
 expect.opt;
*)

external probe_int : int -> int = "caml_set_oo_id" [@@noalloc];;

[%%expect{|
external probe_int : int -> int = "caml_set_oo_id" [@@noalloc]
|}]

let[@inline never] call_tagged_value n = probe_int n;;

[%%expect{|
val call_tagged_value : int -> int = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTOP__call_tagged_value_0_1_code"(i64 %0, i64 %1, i64 %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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
  br label %L110
L110:
  %8 = load i64, ptr %4
  store i64 %8, ptr %6
  %9 = load i64, ptr %6
  store i64 %9, ptr %4
  %10 = ptrtoint ptr @"\01_caml_set_oo_id" to i64
  %11 = load i64, ptr %4
  %12 = load i64, ptr %ds
  %13 = load i64, ptr %alloc
  %14 = call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_set_oo_id"(i64 %12, i64 %13, i64 %11) "gc-leaf-function"="true"
  %15 = extractvalue { i64, i64, ptr addrspace(1) } %14, 0
  %16 = extractvalue { i64, i64, ptr addrspace(1) } %14, 1
  store i64 %15, ptr %ds
  store i64 %16, ptr %alloc
  %17 = extractvalue { i64, i64, ptr addrspace(1) } %14, 2
  store ptr addrspace(1) %17, ptr %5
  br label %L112
L112:
  %18 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %18, ptr %7
  %19 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %19, ptr %5
  %20 = load ptr addrspace(1), ptr %5
  %21 = ptrtoint ptr addrspace(1) %20 to i64
  %22 = load i64, ptr %ds
  %23 = load i64, ptr %alloc
  %24 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %22, 0, 0
  %25 = insertvalue { { i64, i64 }, { i64 } } %24, i64 %23, 0, 1
  %26 = insertvalue { { i64, i64 }, { i64 } } %25, i64 %21, 1, 0
  ret { { i64, i64 }, { i64 } } %26
}|}]
