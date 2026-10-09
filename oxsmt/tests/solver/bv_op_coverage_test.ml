(* Durable small-width oracle for the QF_BV operator SUGAR added in task #11 (fable
   MED-2). The new ops
   (bvsdiv/bvsrem/bvsmod/bvcomp/bvnand/bvnor/bvxnor/rotate_left/rotate_right/ repeat) are
   parser expansions over the bit-blaster's primitives; the exhaustive 13894-case
   primitive oracle (bv_blast_test) covers the PRIMITIVES, not these expansions. This test
   closes that gap WITHOUT a z3 dependency (z3 is dev-only, never a gate): for each new op
   it enumerates all small-width inputs, computes an INDEPENDENT reference in OCaml
   (standard signed/bitwise/rotate integer semantics — NOT the parser's expansion), and
   asserts through the REAL parse -> Session.check_sat dispatch that the built term equals
   the reference (`(not (= (op ...) (_ bv<ref> W)))` must be unsat). A wrong expansion
   makes some case sat/unknown -> loud failure. Mirrors the primitive oracle's exhaustive
   pattern. *)

module Session = Oxsmt_interface.Session
module Parser = Oxsmt_smtlib_parser.Parser

let failures = ref 0
let checks = ref 0

(* Solve a whole-document QF_BV query through the real dispatch; return the verdict
   string. *)
let solve src =
  let s = Session.create () in
  match
    Parser.parse_into
      ~internal_mint:(Session.parse_minter s)
      (Session.env s)
      (Session.context s)
      src
  with
  | exception (Parser.Malformed _ | Parser.Unsupported _) -> "unknown"
  | parsed ->
    if not (Oxsmt_query_loader.assert_all ~presolve:true s parsed)
    then "unknown"
    else (
      match Session.check_sat s with
      | Session.Sat -> "sat"
      | Session.Unsat -> "unsat"
      | Session.Unknown -> "unknown")
;;

(* A concrete ground refutation: the built op-term must EQUAL the reference value, so the
   negation is unsat. Any other verdict (sat = wrong value, unknown = failed to blast)
   fails. *)
let expect_unsat label src =
  incr checks;
  match solve src with
  | "unsat" -> ()
  | other ->
    incr failures;
    Printf.printf "  FAIL %s: expected unsat, got %s\n    %s\n" label other src
;;

let mask w = (1 lsl w) - 1

let to_bv r w =
  r land mask w (* two's-complement low w bits; OCaml land handles negatives *)
;;

let to_signed a w = if a land (1 lsl (w - 1)) <> 0 then a - (1 lsl w) else a

(* Independent SMT-LIB references (b <> 0 for the division family; div-by-zero is covered
   by dedicated goldens). *)
let ref_sdiv a b w = to_bv (to_signed a w / to_signed b w) w (* trunc toward zero *)
let ref_srem a b w = to_bv (to_signed a w mod to_signed b w) w (* sign of dividend *)

let ref_smod a b w =
  let sa = to_signed a w
  and sb = to_signed b w in
  let m = sa mod sb in
  let m = if m <> 0 && m < 0 <> (sb < 0) then m + sb else m in
  (* sign of divisor *)
  to_bv m w
;;

let ref_nand a b w = to_bv (lnot (a land b)) w
let ref_nor a b w = to_bv (lnot (a lor b)) w
let ref_xnor a b w = to_bv (lnot (a lxor b)) w

let ref_rotl a k w =
  let k = k mod w in
  to_bv ((a lsl k) lor (a lsr (w - k))) w
;;

let ref_rotr a k w =
  let k = k mod w in
  to_bv ((a lsr k) lor (a lsl (w - k))) w
;;

let ref_repeat a n w =
  let r = ref 0 in
  for i = 0 to n - 1 do
    r := !r lor (a lsl (i * w))
  done;
  !r
;;

let hdr = "(set-logic QF_BV)\n"
let lit v w = Printf.sprintf "(_ bv%d %d)" (v land mask w) w

let refute expr_str ref_v rw =
  Printf.sprintf "%s(assert (not (= %s %s)))\n(check-sat)\n" hdr expr_str (lit ref_v rw)
;;

let run_binary name w mk refv rw =
  let n = 1 lsl w in
  for a = 0 to n - 1 do
    for b = 0 to n - 1 do
      if refv a b <> min_int (* min_int sentinel = skip (e.g. b=0 for div family) *)
      then
        expect_unsat
          (Printf.sprintf "%s w=%d a=%d b=%d" name w a b)
          (refute (mk (lit a w) (lit b w)) (refv a b) (rw w))
    done
  done
;;

let () =
  print_endline "bv-op-coverage: exhaustive small-width oracle for the task-#11 op sugar";
  let bin op a b = Printf.sprintf "(%s %s %s)" op a b in
  List.iter
    (fun w ->
       run_binary "bvnand" w (bin "bvnand") (fun a b -> ref_nand a b w) (fun w -> w);
       run_binary "bvnor" w (bin "bvnor") (fun a b -> ref_nor a b w) (fun w -> w);
       run_binary "bvxnor" w (bin "bvxnor") (fun a b -> ref_xnor a b w) (fun w -> w);
       run_binary
         "bvcomp"
         w
         (bin "bvcomp")
         (fun a b -> if a = b then 1 else 0)
         (fun _ -> 1);
       (* division family: skip b=0 here (div-by-zero has dedicated goldens) *)
       run_binary
         "bvsdiv"
         w
         (bin "bvsdiv")
         (fun a b -> if b = 0 then min_int else ref_sdiv a b w)
         (fun w -> w);
       run_binary
         "bvsrem"
         w
         (bin "bvsrem")
         (fun a b -> if b = 0 then min_int else ref_srem a b w)
         (fun w -> w);
       run_binary
         "bvsmod"
         w
         (bin "bvsmod")
         (fun a b -> if b = 0 then min_int else ref_smod a b w)
         (fun w -> w))
    [ 3; 4 ];
  (* rotates + repeat: enumerate the operand and the index over one width *)
  let w = 4 in
  for a = 0 to (1 lsl w) - 1 do
    for k = 0 to w + 1 do
      expect_unsat
        (Printf.sprintf "rotate_left w=%d a=%d k=%d" w a k)
        (refute (Printf.sprintf "((_ rotate_left %d) %s)" k (lit a w)) (ref_rotl a k w) w);
      expect_unsat
        (Printf.sprintf "rotate_right w=%d a=%d k=%d" w a k)
        (refute
           (Printf.sprintf "((_ rotate_right %d) %s)" k (lit a w))
           (ref_rotr a k w)
           w)
    done;
    List.iter
      (fun n ->
         expect_unsat
           (Printf.sprintf "repeat w=%d a=%d n=%d" w a n)
           (refute
              (Printf.sprintf "((_ repeat %d) %s)" n (lit a w))
              (ref_repeat a n w)
              (n * w)))
      [ 1; 2; 3 ]
  done;
  Printf.printf "\nbv-op-coverage: %d checks, %d failure(s)\n" !checks !failures;
  if !failures > 0 then exit 1
;;
