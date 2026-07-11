(* Generator for the adversarial performance corpus (DESIGN.md §8.4: an adversarial perf
   corpus "grown deliberately so cliffs surface in CI rather than in the first real
   codebase"). Test-only, stdlib-only.

   Writes deterministic .smt2 files into a target directory (argv[1], default
   tests/perf/cases). The generated cases are committed so the corpus is fixed and
   reviewable; re-run `make perf-gen` (or this exe) only to regenerate after changing a
   family. Families (see tests/README "Perf corpus"):

   (a) euf_diamond_d* — depth-parameterized equality diamonds (EUF); theory today
   (b) dense_simplex_c* — many overlapping linear bounds (LIA); theory today
   (c) ite_tree_d* — balanced Int ite trees, depth 8/10/12 (construction + clausify)
   (d) wide_sum_flat_n* — flat n-ary (+ x1 .. xn); the mitigated construction path
       wide_sum_nested_n* — left-nested (+ (+ ..) x); #49's O(n^2) terms_of cliff
   (e) pushpop_n* — deep push/assert/check/pop stacks (incremental -> unknown)
   (f) pigeonhole_n* — PHP(n+1,n) pure-Boolean, actually solved today (unsat)

   Sizes are chosen so the worst case today is ~seconds, not minutes. Theory cases answer
   `unknown` fast under v1 (the perf value of the theory structure activates at M4); what
   bites today is parse/term-construction cost (wide sums, ite trees, diamonds, deep
   stacks) and the pure-Boolean pigeonhole search. *)

let buf_to_file dir name f =
  let b = Buffer.create 4096 in
  f b;
  let path = Filename.concat dir name in
  let oc = open_out path in
  output_string oc (Buffer.contents b);
  close_out oc;
  Printf.printf "  %s (%d bytes)\n" name (Buffer.length b)
;;

(* ---- (a) EUF equality diamonds --------------------------------------------- d layers;
   each layer i offers two equality paths x_i = y_i = x_[{i+1}] or x_i = z_i = x_[{i+1}],
   so under congruence x_0 = x_d is forced and (not (= x_0 x_d)) is unsat. The Boolean
   skeleton has 2^d path choices. *)
let gen_diamond dir d =
  buf_to_file dir (Printf.sprintf "euf_diamond_d%d.smt2" d) (fun b ->
    Printf.bprintf
      b
      ";; EUF equality diamond, depth %d. Unsat under congruence (M2+).\n"
      d;
    Buffer.add_string b "(set-logic QF_UF)\n(declare-sort U 0)\n";
    for i = 0 to d do
      Printf.bprintf b "(declare-const x%d U)\n" i
    done;
    for i = 0 to d - 1 do
      Printf.bprintf b "(declare-const y%d U)\n(declare-const z%d U)\n" i i
    done;
    for i = 0 to d - 1 do
      Printf.bprintf
        b
        "(assert (or (and (= x%d y%d) (= y%d x%d)) (and (= x%d z%d) (= z%d x%d))))\n"
        i
        i
        i
        (i + 1)
        i
        i
        i
        (i + 1)
    done;
    Printf.bprintf b "(assert (not (= x0 x%d)))\n(check-sat)\n" d)
;;

(* ---- (b) dense simplex: many overlapping bounds over few variables ---------- *)
let gen_dense_simplex dir n_constraints =
  buf_to_file dir (Printf.sprintf "dense_simplex_c%d.smt2" n_constraints) (fun b ->
    Printf.bprintf
      b
      ";; Dense LIA: %d overlapping bounds over 8 vars. Theory (unknown) today.\n"
      n_constraints;
    Buffer.add_string b "(set-logic QF_LIA)\n";
    let nv = 8 in
    for i = 0 to nv - 1 do
      Printf.bprintf b "(declare-const v%d Int)\n" i
    done;
    (* Deterministic overlapping pairwise bounds, both directions. *)
    let k = ref 0 in
    let i = ref 0
    and j = ref 1 in
    while !k < n_constraints do
      let lo = (!k mod 7) - 3 in
      let hi = lo + 5 + (!k mod 4) in
      Printf.bprintf b "(assert (<= (+ v%d v%d) %d))\n" !i !j hi;
      Printf.bprintf b "(assert (>= (+ v%d v%d) %d))\n" !i !j lo;
      incr k;
      j := (!j + 1) mod nv;
      if !j = 0
      then (
        incr i;
        i := !i mod nv;
        j := (!i + 1) mod nv)
    done;
    Buffer.add_string b "(check-sat)\n")
;;

(* ---- (c) balanced Int ite tree of depth d (2^d - 1 ite nodes) --------------- *)
let gen_ite_tree dir d =
  buf_to_file dir (Printf.sprintf "ite_tree_d%d.smt2" d) (fun b ->
    Printf.bprintf
      b
      ";; Balanced Int ite tree, depth %d. Theory (unknown); heavy build.\n"
      d;
    Buffer.add_string b "(set-logic QF_LIA)\n";
    for i = 0 to d - 1 do
      Printf.bprintf b "(declare-const c%d Bool)\n" i
    done;
    let leaf = ref 0 in
    let rec tree depth =
      if depth = 0
      then (
        let v = !leaf in
        incr leaf;
        Printf.bprintf b "%d" v)
      else (
        Printf.bprintf b "(ite c%d " (depth - 1);
        tree (depth - 1);
        Buffer.add_char b ' ';
        tree (depth - 1);
        Buffer.add_char b ')')
    in
    Buffer.add_string b "(assert (>= ";
    tree d;
    Buffer.add_string b " 0))\n(check-sat)\n")
;;

(* ---- (d) wide sums: flat (mitigated) and left-nested (#49 O(n^2) cliff) ------ *)
let decl_ints b n =
  for i = 1 to n do
    Printf.bprintf b "(declare-const x%d Int)\n" i
  done
;;

let gen_wide_sum_flat dir n =
  buf_to_file dir (Printf.sprintf "wide_sum_flat_n%d.smt2" n) (fun b ->
    Printf.bprintf
      b
      ";; Flat n-ary sum, n=%d (one linear_combination pass; mitigated).\n"
      n;
    Buffer.add_string b "(set-logic QF_LIA)\n";
    decl_ints b n;
    Buffer.add_string b "(assert (= (+";
    for i = 1 to n do
      Printf.bprintf b " x%d" i
    done;
    Printf.bprintf b ") %d))\n(check-sat)\n" (n + 1))
;;

let gen_wide_sum_nested dir n =
  buf_to_file dir (Printf.sprintf "wide_sum_nested_n%d.smt2" n) (fun b ->
    Printf.bprintf
      b
      ";; Left-nested pairwise sum, n=%d: (+ (+ .. x2) x3) .. — re-expands the growing\n\
       ;; Arith node each level (terms_of), the O(n^2) construction cliff (#49).\n"
      n;
    Buffer.add_string b "(set-logic QF_LIA)\n";
    decl_ints b n;
    Buffer.add_string b "(assert (= ";
    (* n-1 opening (+ ; left-nested. *)
    for _ = 2 to n do
      Buffer.add_string b "(+ "
    done;
    Buffer.add_string b "x1";
    for i = 2 to n do
      Printf.bprintf b " x%d)" i
    done;
    Printf.bprintf b " %d))\n(check-sat)\n" (n + 1))
;;

(* ---- (e) deep push/pop stack ------------------------------------------------ *)
let gen_pushpop dir n =
  buf_to_file dir (Printf.sprintf "pushpop_n%d.smt2" n) (fun b ->
    Printf.bprintf
      b
      ";; Deep push/pop stack, %d frames. Incremental -> unknown per check.\n"
      n;
    Buffer.add_string b "(set-logic QF_UF)\n";
    for i = 0 to 7 do
      Printf.bprintf b "(declare-const p%d Bool)\n" i
    done;
    for k = 0 to n - 1 do
      Printf.bprintf
        b
        "(push 1)\n(assert (or p%d (not p%d)))\n(check-sat)\n(pop 1)\n"
        (k mod 8)
        ((k + 1) mod 8)
    done)
;;

(* ---- (f) pigeonhole PHP(n+1, n), pure-Boolean, unsat, solved today ---------- *)
let gen_pigeonhole dir n =
  buf_to_file dir (Printf.sprintf "pigeonhole_n%d.smt2" n) (fun b ->
    Printf.bprintf
      b
      ";; PHP(%d,%d): %d pigeons into %d holes, pure-Boolean UNSAT (real verdict today).\n"
      (n + 1)
      n
      (n + 1)
      n;
    Buffer.add_string b "(set-logic QF_UF)\n(set-info :status unsat)\n";
    for i = 0 to n do
      for h = 0 to n - 1 do
        Printf.bprintf b "(declare-const p_%d_%d Bool)\n" i h
      done
    done;
    (* each pigeon in >= 1 hole *)
    for i = 0 to n do
      Buffer.add_string b "(assert (or";
      for h = 0 to n - 1 do
        Printf.bprintf b " p_%d_%d" i h
      done;
      Buffer.add_string b "))\n"
    done;
    (* no hole holds two pigeons *)
    for h = 0 to n - 1 do
      for i = 0 to n do
        for j = i + 1 to n do
          Printf.bprintf b "(assert (or (not p_%d_%d) (not p_%d_%d)))\n" i h j h
        done
      done
    done;
    Buffer.add_string b "(check-sat)\n")
;;

let () =
  let dir = if Array.length Sys.argv >= 2 then Sys.argv.(1) else "tests/perf/cases" in
  (try Unix.mkdir dir 0o755 with
   | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  Printf.printf "generating perf corpus into %s:\n" dir;
  List.iter (gen_diamond dir) [ 8; 16; 32 ];
  List.iter (gen_dense_simplex dir) [ 40; 120; 300 ];
  List.iter (gen_ite_tree dir) [ 8; 10; 12 ];
  List.iter (gen_wide_sum_flat dir) [ 1000; 5000 ];
  List.iter (gen_wide_sum_nested dir) [ 250; 500; 1000 ];
  List.iter (gen_pushpop dir) [ 500; 2000 ];
  List.iter (gen_pigeonhole dir) [ 5; 6; 7 ];
  print_string "done.\n"
;;
