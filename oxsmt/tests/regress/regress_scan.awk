# Single-pass static scan of one .smt2 file for the regression-suite oracle
# (regress_run.sh). Emits ONE tab-separated summary line the shell uses to decide the
# fail-closed skip census and to derive the expected verdict for cvc5:
#
#   logic  n_checks  pushpop  quantifiers  cmdline_incr  n_expect_verdict  expect_verdict  setinfo
#
# This scan only drives the CENSUS and avoids solving hopeless files; it is NOT the
# soundness path. The verdict itself comes from corpus_classify, which independently
# fail-closes on incremental/unsupported input (it degrades to unknown-incremental /
# parse-fail rather than returning a definite verdict), so a mis-scan here can only mislabel
# a skip bucket, never manufacture a false MISMATCH. Raw-line regex (comments are NOT
# stripped: cvc5 carries EXPECT/COMMAND-LINE inside `;` comments); over-counting a check
# only makes a file look more incremental, which is the safe direction.
BEGIN { logic = "-"; nchk = 0; pp = 0; q = 0; ci = 0; ne = 0; ev = "-"; lastv = "-"; si = "-" }
{
  line = $0
  if (logic == "-" && match(line, /\(set-logic[ \t]+[A-Za-z_0-9]+/)) {
    s = substr(line, RSTART, RLENGTH); sub(/\(set-logic[ \t]+/, "", s); logic = s
  }
  # (check-sat and (check-sat-assuming both count as one check each.
  tmp = line; nchk += gsub(/\(check-sat/, "", tmp)
  # push/pop and reset/reset-assertions are all stateful (incremental-family) commands we do
  # not support. This is only a CHEAP PRE-FILTER (allow optional whitespace after the paren,
  # e.g. `( reset-assertions )`); the AUTHORITATIVE, head-exact gate is corpus_classify's
  # scan_commands, which degrades all of these to unknown-incremental even if this regex
  # misses one — so a miss here costs at most a wasted solve, never a false MISMATCH.
  if (line ~ /\([ \t]*push/ || line ~ /\([ \t]*pop/ || line ~ /\([ \t]*reset/) pp = 1
  if (line ~ /\(forall/ || line ~ /\(exists/) q = 1
  if (line ~ /^[ \t]*;[ \t]*EXPECT:/) {
    v = line; sub(/^[ \t]*;[ \t]*EXPECT:[ \t]*/, "", v); gsub(/[ \t\r]+$/, "", v)
    if (v == "sat" || v == "unsat" || v == "unknown") { ne++; lastv = v }
  }
  if (line ~ /^[ \t]*;[ \t]*COMMAND-LINE:/ && line ~ /--incremental/) ci = 1
  if (si == "-" && match(line, /\(set-info[ \t]+:status[ \t]+[a-z]+/)) {
    s = substr(line, RSTART, RLENGTH); sub(/.*:status[ \t]+/, "", s)
    if (s == "sat" || s == "unsat" || s == "unknown") si = s
  }
}
END {
  if (ne == 1) ev = lastv
  printf "%s\t%d\t%d\t%d\t%d\t%d\t%s\t%s\n", logic, nchk, pp, q, ci, ne, ev, si
}
