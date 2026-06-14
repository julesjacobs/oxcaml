define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlHash_lookup_string_equal__fn$5bhash_lookup_string_equal.ml$3a27$2c37$2d$2d40$5d_8_8_code"(i64 %0, i64 %1, ptr addrspace(1) nocapture readonly %2) #2 gc "oxcaml" {
L1:
  %3 = load ptr addrspace(1), ptr addrspace(1) %2, align 8
  %4 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %0, 0, 0
  %5 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %4, i64 %1, 0, 1
  %6 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %5, ptr addrspace(1) %3, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %6
}
