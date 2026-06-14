define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_raise_cross_function_caught__probe_4_11_code"(i64 %0, i64 %1, i64 %2) #0 gc "oxcaml" {
L1:
  %or.cond.not = icmp eq i64 %2, 1
  br i1 %or.cond.not, label %L121, label %L123

L121:                                             ; preds = %L1
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 ptrtoint (ptr @"\01_camlTry_raise_cross_function_caught__Miss245" to i64))
  unreachable

L123:                                             ; preds = %L1
  %3 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %0, 0, 0
  %4 = insertvalue { { i64, i64 }, { i64 } } %3, i64 %1, 0, 1
  %5 = insertvalue { { i64, i64 }, { i64 } } %4, i64 %2, 1, 0
  ret { { i64, i64 }, { i64 } } %5
}
