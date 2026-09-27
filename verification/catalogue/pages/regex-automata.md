title: Regular expressions and automata
blurb: A regular-expression matcher proved to accept exactly the words its membership rules derive, and a conversion to DFA tables that, when it returns a table, returns a valid one with the same language.
status: review-pending
date: 27 September 2026
sources:
  - testsuite/tests/vox/regex_semantics.ml — Regular expressions and the membership rules
  - testsuite/tests/vox/regex_language.mli — Public interface
  - testsuite/tests/vox/regex_language.ml — Proofs of the public contracts from the implementation
  - testsuite/tests/vox/regex_core.ml — Derivative matcher, internal automaton and their proofs
  - testsuite/tests/vox/regex_dfa_bridge_core.ml — Conversion of the internal automaton to a DFA table, and its proof
  - testsuite/tests/vox/dfa_semantics.ml — DFA tables and `run`, shared with the DFA demo
  - testsuite/tests/vox/regex_public_client.ml — Client using only the public interfaces
  - testsuite/tests/vox/regex.ml — Runtime tests and rejected programs for the matcher
  - testsuite/tests/vox/regex_dfa_bridge.ml — Test that checks the conversion proof
  - testsuite/tests/vox/dfa_boundary.ml — The only compile of the public interface and client, and the erasure check
---
`Regex_language.matches r w` decides whether the regular expression `r` matches the word `w`, where symbols are `int`s and the syntax is `Empty`, `Epsilon`, `Symbol`, `Alt`, `Seq` and `Star`. It is proved to agree with an inductive membership relation: `Membership.valid r p` says that `p` derives a word from `r` by the usual rules, and `Membership.word p` is that word. `sound` returns an erased derivation of `w` whenever `matches r w` is true, and `complete` shows that `matches r w` is true whenever such a derivation exists. The matcher uses Brzozowski derivatives.

`lower r` converts `r` to a table in the format of the [DFA equivalence and minimization](dfa-equivalence.html) demo, or returns `None`. If `lower r` returns `Some m`, then `lower_matches` says that `Dfa_semantics.run m w = matches r w` for every word `w`, and `lower_valid` says that `m` is `valid` in the sense the DFA operations require, so a client can pass it to `compare` and `reduce` without checking it. Nothing in the interface says that `lower` returns `Some`, so `fun _ -> None` still satisfies the interface. The implementation proves that it builds a table for every regex and returns `None` only if that table fails the validity check. That the check passes is not proved: rows are numbered with machine `int`s, which wrap, so the numbers are distinct only for tables of fewer than 2^63 rows, and no bound on the table in terms of the regex is proved.

Lowering builds one row for every subset of the list `r :: support r`, which holds `r` and the terms its partial derivatives can reach, plus two rows. A literal of `n` symbols therefore gives 2^(n+1) + 2 rows: `a` gives 6, `abcd` 34, `abcde` 66 and a 12-symbol literal 8,194. `Dfa_equivalence.reduce` accepts at most 64 rows, so lowering `abcde` and then minimizing returns `None`. Running time and memory are not proved.

## An open question: sizes of in-memory data

Proving that `lower` always succeeds runs into a problem that is not specific to this demo. Row numbers are OCaml `int`s, which wrap at 2^63, so "all row numbers differ" holds only for tables of fewer than 2^63 rows. No table that large can exist in memory, but the checker cannot use that fact. Tables are lists, and ghost code (which is erased and never allocates) can build lists of any length, so "every list is shorter than 2^62" would be false in Vox's logic. Immutable arrays do not have this problem, because ghost code cannot allocate them, and Vox can bound their length soundly.

The options each have a cost: a size premise on every such theorem, which clients must discharge; unbounded integers for identifiers, at a run-time cost; or a new mode for values that only run-time code can produce, whose size the checker could then bound. Which of these a verified-programming language for OCaml should adopt is an open design question.

## Client example

From the public client. `{u : unit | p}` is `unit` refined by the predicate `p`: returning it proves `p`. `===` is logical equality. `ghost_ (...)` is proof code, checked and then erased; the value it computes here is a `unit` whose refinement is written after the `:`. `@ total` after the function name declares it total: it terminates without raising or touching mutable state. The statement holds for every regex, limit and word, but it says nothing when either step returns `None`. `reduce` returns `None` when `limit` is not between 1 and 64 or the lowered table has more than `limit` rows, so every regex with more than 64 lowered rows is excluded.

@code testsuite/tests/vox/regex_public_client.ml "let (minimized_regex @ total)" "  ()"

The next theorem needs `lower_valid`: `reduce_complete` from the DFA interface requires a `valid` source, and here that premise is gone. The others remain: every row must list at most 64 symbols, and the table must fit in `limit`.

@code testsuite/tests/vox/regex_public_client.ml "let (lowered_reduction_finishes @ total)" "  ()"

## A rejected program

Returning a fixed derivation as the evidence for every match is a type error. `Regex` is the implementation module in `regex_core.ml`; `Regex_language.matches` is defined as `Regex.matches`. The test `regex.ml` requires this definition to fail.

@code testsuite/tests/vox/regex.ml "let fabricated_evidence r s :" "  p"

```
Line 7, characters 2-3:
7 |   p
      ^
Error: Refinement could not be proved (counterexample)
Lines 3-5, characters 6-15:
3 | ......if Regex.matches r s then
4 |         Regex.Membership.valid r p && Regex.Membership.word p === s
5 |       else true...
  The refinement is stated here.
```

## Interface

@code testsuite/tests/vox/regex_language.mli

`(x : t) -> ...` names an argument so that later types can mention it. `@@ total` on a declaration marks a total function, which may appear in refinements. `@ total` on an argument or result type marks a value at the `total` mode, which total code (here the construction inside `lower`) may consume. `Ghost.t` wraps a value that is erased at run time; `proof.ghost` reads it in specifications. The membership rules are:

@code testsuite/tests/vox/regex_semantics.ml

Each file wraps its contents in a module (`regex_core.ml` defines `Regex`) because the tests load the files into the toplevel with `#use`. `[@@inductive]` declares a datatype the checker reasons about by cases and induction. `let[@def]` defines a total function that refinements may mention, together with a lemma stating its defining equation. `Dfa_semantics.machine` and `run` are shown on the DFA page.

## Trusted base

- The test `dfa_boundary.ml` is the only one that compiles `regex_language.mli`, `regex_language.ml` (the proofs of `sound`, `complete`, `lower_matches` and `lower_valid` from the implementation) and `regex_public_client.ml`. The tests `regex.ml` and `regex_dfa_bridge.ml` check the implementation and its proofs, but not these three files.
- `lower` checks its table with `Dfa_proof.of_raw` from the DFA demo, whose module declares three `external` aliases of `%equal` and `%greaterequal` at `int` (`dfa_equivalence_proof.ml`). The checker gives them the meaning of `=` and `>=`.

## Scope

- Operations: `matches` and `lower`. There is no search within a longer word, no submatch positions, no character classes and no parser; symbols are `int`s.
- Inside `regex_core.ml`, `Regex.Dfa.correct` proves that the internal automaton accepts exactly the words `matches` accepts, for every regex. The condition on `Some` comes from the validity check on the converted table (see above).
- Lowered tables have 2^u + 2 rows, where `u` is the length of `r :: support r`. Combined with `reduce`, only regexes with `u` at most 5 are within the DFA demo's 64-row limit.
- Every lowered row lists each symbol that occurs in `r` as an explicit edge, so a regex with more than 64 distinct symbols lowers to a table without `labels_bounded`, and `compare_complete` and `reduce_complete` say nothing about it.
- `matches` is total; its running time is not bounded.
- Only normal return is specified, as on the shared page.

## Reproduce

After `make install` and `./dev init`, from the repository root:

```
./dev test vox/regex.ml vox/regex_dfa_bridge.ml vox/dfa_boundary.ml
```

`regex.ml` checks the matcher and the internal automaton, compares `matches` with a separate runtime matcher on 3,244 small regexes, and requires three false claims to fail. `regex_dfa_bridge.ml` checks the conversion proof. `dfa_boundary.ml` compiles all DFA and regex modules with both compilers, the public interfaces with `-opaque`, compiles `regex_public_client.ml` against the public interfaces only, links and runs it, and checks from `-drawlambda` output that the proof functions `sound`, `complete`, `lower_matches` and `lower_valid` make no runtime calls. It also requires a public client that claims `lower` always returns `Some` to fail with its exact error.
