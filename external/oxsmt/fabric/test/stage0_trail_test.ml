(* Stage-0 backtracking-substrate tests (ADR-0014 §C Stage 0).

   Three groups:
   1. Substrate unit tests — [Trail] push/record/pop/mark/rewind_to correctness.
   2. POP-ORDERING — the shared drain loop replays entries newest-first. This is the
      oracle that KILLS the [tools/mutants] "swap drain order" mutant (stage0-trail-drain-
      order): under a mutant that drains oldest-first, the observed apply order flips and
      these checks go red.
   3. DISJOINTNESS (machine-checkable, ADR-0014 §C Stage 0 item 4 / review carry-forward
      note 3) — no module's undo application mutates another module's state. Realized as a
      dynamic oracle: snapshot each module's state, apply ANOTHER module's [pop], assert
      the first is byte-for-byte unchanged. Run both at the substrate level (two [Trail]s
      over independent state) AND with the real EUF and simplex engines (their actual
      migrated trails). A structural "apply_undo only names its own fields" assertion
      would be weaker (note 3); this drives the property. *)

open Oxsmt_core
module Euf = Oxsmt_euf.Euf
module Simplex = Oxsmt_lia.Simplex
module Rational = Oxsmt_lia.Rational
module Delta = Oxsmt_lia.Delta

let checks = ref 0
let failures = ref 0

let check name ok =
  incr checks;
  if not ok
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

let check_raises name f =
  incr checks;
  match f () with
  | exception _ -> ()
  | _ ->
    incr failures;
    Printf.printf "  FAIL %s (expected an exception, got none)\n" name
;;

(* ------------------------------------------------------------------ *)
(* 1. Substrate unit tests *)
(* ------------------------------------------------------------------ *)

let test_substrate () =
  let tr : (int, unit) Trail.t = Trail.create () in
  check "create: empty" (Trail.length tr = 0 && Trail.depth tr = 0);
  Trail.push tr ();
  Trail.record tr 1;
  Trail.record tr 2;
  check
    "after push+2 records: length 2, depth 1"
    (Trail.length tr = 2 && Trail.depth tr = 1);
  Trail.push tr ();
  Trail.record tr 3;
  check "nested frame: length 3, depth 2" (Trail.length tr = 3 && Trail.depth tr = 2);
  (* pop 1 drains back to the inner frame's watermark (2). [seen] records apply order. *)
  let seen = ref [] in
  Trail.pop tr ~apply:(fun x -> seen := !seen @ [ x ]) 1;
  check "pop 1: drained the inner frame only" (!seen = [ 3 ]);
  check "pop 1: length 2, depth 1" (Trail.length tr = 2 && Trail.depth tr = 1);
  (* pop 1 more drains the outer frame (entries 2 then 1, newest-first). *)
  seen := [];
  Trail.pop tr ~apply:(fun x -> seen := !seen @ [ x ]) 1;
  check "pop to base: drained outer frame newest-first" (!seen = [ 2; 1 ]);
  check "pop to base: empty" (Trail.length tr = 0 && Trail.depth tr = 0);
  (* pop 0 is a no-op; over-pop raises. *)
  Trail.pop tr ~apply:(fun _ -> assert false) 0;
  check "pop 0: no-op leaves empty" (Trail.depth tr = 0);
  check_raises "over-pop raises" (fun () -> Trail.pop tr ~apply:ignore 1);
  check_raises "negative pop raises" (fun () -> Trail.pop tr ~apply:ignore (-1));
  (* mark / rewind_to: an intra-frame transaction (Rev4-4). *)
  Trail.push tr ();
  Trail.record tr 10;
  let m = Trail.mark tr in
  Trail.record tr 11;
  Trail.record tr 12;
  let seen = ref [] in
  Trail.rewind_to tr ~apply:(fun x -> seen := !seen @ [ x ]) m;
  check "rewind_to: replays back to mark, newest-first" (!seen = [ 12; 11 ]);
  check
    "rewind_to: length back to mark, depth unchanged"
    (Trail.length tr = 1 && Trail.depth tr = 1);
  check_raises "rewind_to beyond length raises" (fun () ->
    Trail.rewind_to tr ~apply:ignore (Trail.length tr + 1))
;;

(* ------------------------------------------------------------------ *)
(* 2. Pop-ordering (kills the swap-drain-order mutant) *)
(* ------------------------------------------------------------------ *)

let test_pop_ordering () =
  (* Record three order-sensitive entries in one frame; the substrate must apply their
     inverses newest-first (LIFO). Any reversal of the drain direction flips [order]. *)
  let order = ref [] in
  let tr : (int, unit) Trail.t = Trail.create () in
  Trail.push tr ();
  Trail.record tr 1;
  Trail.record tr 2;
  Trail.record tr 3;
  Trail.pop tr ~apply:(fun x -> order := !order @ [ x ]) 1;
  check "drain order is newest-first (3,2,1)" (!order = [ 3; 2; 1 ]);
  (* The reason order matters: a real undo entry restores a prior value; applying an OLDER
     inverse before a NEWER one leaves the newest write in place. Model that: a single
     cell with a stack of (old-value) undos. Correct LIFO restores the original; a swapped
     drain restores an intermediate value. *)
  let cell = ref 0 in
  let tr2 : (int, unit) Trail.t = Trail.create () in
  Trail.push tr2 ();
  let set v =
    Trail.record tr2 !cell;
    (* undo = the value to restore *)
    cell := v
  in
  set 1;
  set 2;
  set 3;
  check "cell holds newest write before pop" (!cell = 3);
  Trail.pop tr2 ~apply:(fun old -> cell := old) 1;
  check "LIFO drain restores the original cell value (0)" (!cell = 0)
;;

(* ------------------------------------------------------------------ *)
(* 3a. Disjointness at the substrate level *)
(* ------------------------------------------------------------------ *)

let test_disjoint_substrate () =
  (* Two trails over two independent cells. Each trail's entries close over only its own
     cell, so applying one trail's [pop] cannot touch the other's state. *)
  let a = ref 0
  and b = ref 0 in
  let ta : (int, unit) Trail.t = Trail.create ()
  and tb : (int, unit) Trail.t = Trail.create () in
  Trail.push ta ();
  Trail.push tb ();
  Trail.record ta !a;
  a := 100;
  Trail.record tb !b;
  b := 200;
  check "both cells mutated" (!a = 100 && !b = 200);
  (* Apply A's pop. B's cell must be untouched (disjoint state). *)
  let b_before = !b in
  Trail.pop ta ~apply:(fun old -> a := old) 1;
  check "A.pop reverts A" (!a = 0);
  check "A.pop leaves B unchanged (disjoint)" (!b = b_before);
  (* Now B's pop. A is untouched. *)
  let a_before = !a in
  Trail.pop tb ~apply:(fun old -> b := old) 1;
  check "B.pop reverts B" (!b = 0);
  check "B.pop leaves A unchanged (disjoint)" (!a = a_before)
;;

(* ------------------------------------------------------------------ *)
(* 3b. Disjointness with the real EUF and simplex engines *)
(* ------------------------------------------------------------------ *)

(* A digest of EUF's observable state: the equality partition over a fixed term set,
   encoded as, for each pair, whether they are equal. Two EUF states with the same digest
   agree on every equality query. *)
let euf_digest e terms =
  let buf = Buffer.create 64 in
  Array.iteri
    (fun i ti ->
       Array.iteri
         (fun j tj ->
            if i < j then Buffer.add_char buf (if Euf.are_equal e ti tj then '1' else '0'))
         terms)
    terms;
  Buffer.add_string
    buf
    (match Euf.check e with
     | Euf.Consistent -> "|C"
     | _ -> "|X");
  Buffer.contents buf
;;

(* A digest of simplex's BACKTRACKED state: the lower/upper bound of each var (value +
   premise). Deliberately excludes the var's current [value] and the tableau — those are
   derived quantities [check] recomputes and that push/pop does NOT restore (simplex.mli:
   "push/pop save and restore bounds only"). *)
let bound_str = function
  | None -> "-"
  | Some (p, d) -> Printf.sprintf "%s@%s" (Delta.to_string d) p
;;

let simplex_digest s vars =
  let buf = Buffer.create 64 in
  List.iter
    (fun v ->
       Buffer.add_string buf (bound_str (Simplex.get_lower s v));
       Buffer.add_char buf '/';
       Buffer.add_string buf (bound_str (Simplex.get_upper s v));
       Buffer.add_char buf ';')
    vars;
  Buffer.contents buf
;;

let test_disjoint_engines () =
  (* --- real EUF --- *)
  let env = Env.create () in
  let u = Env.declare_sort env "U" in
  let usort = Sort.uninterpreted u in
  let konst name = Env.declare_fun env name (Rank.create [] usort) in
  let ctx = Context.create env in
  let a = Context.const ctx (konst "a")
  and b = Context.const ctx (konst "b")
  and c = Context.const ctx (konst "c") in
  let terms = [| a; b; c |] in
  let e = Euf.create ctx in
  Euf.register_term e a;
  Euf.register_term e b;
  Euf.register_term e c;
  (* --- real simplex --- *)
  let s = Simplex.create () in
  let x = Simplex.new_problem_var s in
  let y = Simplex.new_problem_var s in
  let d c k = Delta.make (Rational.of_int c) (Rational.of_int k) in
  (* base states *)
  let euf_base = euf_digest e terms in
  let splx_base = simplex_digest s [ x; y ] in
  (* open one frame in EACH engine and mutate both *)
  Euf.push e;
  Simplex.push s;
  Euf.assert_eq e ~premise:1 a b;
  ignore (Simplex.assert_lower s x (d 1 0) "x>=1" : string Simplex.conflict option);
  let euf_mutated = euf_digest e terms in
  let splx_mutated = simplex_digest s [ x; y ] in
  check "EUF state changed under its frame" (euf_mutated <> euf_base);
  check "simplex state changed under its frame" (splx_mutated <> splx_base);
  check "EUF now sees a=b" (Euf.are_equal e a b);
  (* Apply EUF's pop. Simplex must be byte-for-byte unchanged: EUF's undo trail names only
     EUF state (disjointness). *)
  let splx_before_euf_pop = simplex_digest s [ x; y ] in
  Euf.pop e 1;
  check "EUF.pop reverts EUF to base" (euf_digest e terms = euf_base);
  check
    "EUF.pop leaves simplex unchanged (cross-theory disjoint)"
    (simplex_digest s [ x; y ] = splx_before_euf_pop);
  (* Symmetrically, simplex's pop leaves EUF untouched. Re-mutate EUF (still at base) to
     make the "EUF unchanged across simplex.pop" assertion non-vacuous. *)
  Euf.push e;
  Euf.assert_eq e ~premise:2 b c;
  let euf_before_splx_pop = euf_digest e terms in
  Simplex.pop s 1;
  check "simplex.pop reverts simplex to base" (simplex_digest s [ x; y ] = splx_base);
  check
    "simplex.pop leaves EUF unchanged (cross-theory disjoint)"
    (euf_digest e terms = euf_before_splx_pop);
  Euf.pop e 1;
  check "EUF back to base after final pop" (euf_digest e terms = euf_base)
;;

(* ------------------------------------------------------------------ *)
(* 4. Scope-aware undo addressing (ADR-0014 Stage 4.0) *)
(* ------------------------------------------------------------------ *)

let test_scope_addressing () =
  (* [watermark_at] exposes each open frame's checkpoint (the [depth -> watermark] index). *)
  let tr : (int, unit) Trail.t = Trail.create () in
  Trail.push tr ();
  Trail.record tr 1;
  Trail.record tr 2;
  Trail.push tr ();
  Trail.record tr 3;
  Trail.push tr ();
  Trail.record tr 4;
  Trail.record tr 5;
  check "watermark_at 0 = 0" (Trail.watermark_at tr 0 = 0);
  check "watermark_at 1 = 2" (Trail.watermark_at tr 1 = 2);
  check "watermark_at 2 = 3" (Trail.watermark_at tr 2 = 3);
  check_raises "watermark_at at depth raises" (fun () ->
    Trail.watermark_at tr (Trail.depth tr));
  check_raises "watermark_at negative raises" (fun () -> Trail.watermark_at tr (-1));
  check_raises "rewind_to_depth beyond depth raises" (fun () ->
    Trail.rewind_to_depth tr ~apply:ignore (Trail.depth tr + 1));
  check_raises "rewind_to_depth negative raises" (fun () ->
    Trail.rewind_to_depth tr ~apply:ignore (-1));
  (* Degeneration (the S4.0 bar): on a monotone trail [rewind_to_depth d] applies the
     IDENTICAL entry sequence and reaches the IDENTICAL (length, depth) as its equivalent
     [pop (depth - d)], for every target depth. Two independently-built identical trails. *)
  let build () =
    let t : (int, unit) Trail.t = Trail.create () in
    Trail.push t ();
    Trail.record t 1;
    Trail.record t 2;
    Trail.push t ();
    Trail.record t 3;
    Trail.push t ();
    Trail.record t 4;
    Trail.record t 5;
    t
  in
  for d = 0 to 3 do
    let tp = build ()
    and td = build () in
    let sp = ref []
    and sd = ref [] in
    Trail.pop tp ~apply:(fun x -> sp := x :: !sp) (Trail.depth tp - d);
    Trail.rewind_to_depth td ~apply:(fun x -> sd := x :: !sd) d;
    check
      (Printf.sprintf "rewind_to_depth %d: same applied entries as pop" d)
      (!sp = !sd);
    check
      (Printf.sprintf "rewind_to_depth %d: same final length/depth as pop" d)
      (Trail.length tp = Trail.length td && Trail.depth tp = Trail.depth td)
  done;
  (* [rewind_to_depth (depth t)] keeps all frames — a no-op, like [pop 0]. *)
  let t = build () in
  let n0 = Trail.length t in
  Trail.rewind_to_depth t ~apply:(fun _ -> assert false) (Trail.depth t);
  check "rewind_to_depth depth is a no-op" (Trail.length t = n0)
;;

let () =
  test_substrate ();
  test_pop_ordering ();
  test_disjoint_substrate ();
  test_disjoint_engines ();
  test_scope_addressing ();
  Printf.printf
    "\nstage0 trail/disjointness self-test: %d checks, %d failure(s)\n"
    !checks
    !failures;
  if !failures > 0 then exit 1
;;
