(* Consumer test for the M4 core prelude: proves an EXTERNAL library (this one depends
   only on oxsmt_core) can name the ADR-0005 THEORY vocabulary re-exported from
   Oxsmt_core, and build a Model via the new Model.of_alist. This is exactly the surface
   the M4 theory adapters (EUF, LIA) and the CDCL(T) seam consume; if a re-export is
   missing or of_alist drifts, this fails to compile / run. Stdlib-only (I3). *)

open Oxsmt_core

let checks = ref 0
let failures = ref 0

let check name cond =
  incr checks;
  if not cond
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

let () =
  print_endline "core-prelude consumer:";
  (* Atom: nameable, minted only via the allocator, dense + deterministic. *)
  let alloc = Atom.create_allocator () in
  let a0 = Atom.fresh alloc in
  let a1 = Atom.fresh alloc in
  check "Oxsmt_core.Atom: fresh ids distinct and ordered" (Atom.compare a0 a1 < 0);
  (* Lit: packed literal round-trips atom + polarity. *)
  let lit = Lit.make a0 true in
  check
    "Oxsmt_core.Lit: make/atom/sign/negate round-trip"
    (Atom.equal (Lit.atom lit) a0 && Lit.sign lit && not (Lit.sign (Lit.negate lit)));
  (* Explanation: the premise-set + Rule_tag record is nameable. *)
  let expl = { Explanation.premises = [ lit ]; rule = Explanation.Rule_tag.Lia_farkas } in
  check
    "Oxsmt_core.Explanation: record + Rule_tag"
    (List.length expl.Explanation.premises = 1);
  (* Theory: effort / check_result constructors are nameable (the seam vocabulary). *)
  let (_ : Theory.effort) = Theory.Final in
  let (_ : Theory.check_result) = Theory.Propagations [ lit ] in
  check "Oxsmt_core.Theory: effort + check_result nameable" true;
  (* Model: build via of_alist over a real Term key, read back. *)
  let env = Env.create () in
  let x = Env.declare_fun env "x" (Rank.create [] Sort.int) in
  let y = Env.declare_fun env "y" (Rank.create [] Sort.int) in
  let ctx = Context.create env in
  let xt = Context.const ctx x in
  let yt = Context.const ctx y in
  let m = Model.of_alist [ xt, Model.Int 42; yt, Model.Int (-7) ] in
  check
    "Oxsmt_core.Model.of_alist: value round-trip"
    (Model.value m xt = Some (Model.Int 42) && Model.value m yt = Some (Model.Int (-7)));
  check
    "Model.value None for an unconstrained term"
    (Model.value m (Context.int_const ctx 0) = None);
  (* of_alist raises on a duplicate term — the deliberate loud decision (no silent
     last-wins). *)
  check
    "Model.of_alist raises Invalid_argument on a duplicate term"
    (match Model.of_alist [ xt, Model.Int 1; xt, Model.Int 2 ] with
     | (_ : Model.t) -> false
     | exception Invalid_argument _ -> true);
  Printf.printf "\n%d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
