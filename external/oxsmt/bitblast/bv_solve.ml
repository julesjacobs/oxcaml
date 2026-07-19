open Oxsmt_core
module Sat = Oxsmt_solver.Sat

type model = (Term.t * (Bigint.t * int)) list
type bool_model = (Term.t * bool) list

type verdict =
  | Sat of model * bool_model
  | Unsat
  | Unknown of string

let two = Bigint.of_int 2

(* OXSMT_SATPRE gate, read with the SAME token set as [Sat.satpre_enabled] (sat.ml). The
   A10 eliminable marks below ([mark_aux_eliminable]) are consumed ONLY by the satpre
   inprocessing pass; with satpre OFF (the default) [Sat.set_eliminable] writes a
   [t.eliminable] slot that the solver never reads, so the whole marking scan is dead
   work. Gating the marking call on this keeps the ON path byte-identical (marks still
   run) and the OFF path byte-identical to before (the marks were already ignored) while
   skipping the O(num_vars) scan on every default BV solve. *)
let satpre_on () =
  match Sys.getenv_opt "OXSMT_SATPRE" with
  | Some ("1" | "true" | "yes" | "on") -> true
  | Some _ | None -> false
;;

(* value of a bit literal under the SAT model *)
let lit_value sat l =
  let b = Sat.value sat (Sat.var_of_lit l) in
  if Sat.sign_of_lit l then b else not b
;;

let bits_to_value sat bits =
  let acc = ref Bigint.zero in
  for i = Array.length bits - 1 downto 0 do
    let d = if lit_value sat bits.(i) then Bigint.one else Bigint.zero in
    acc := Bigint.add (Bigint.mul !acc two) d
  done;
  !acc
;;

let read_model blaster =
  let sat = Blast.sat blaster in
  List.map
    (fun (term, bits) -> term, (bits_to_value sat bits, Array.length bits))
    (Blast.bv_vars blaster)
;;

(* Free Boolean variables' truth values under the SAT model. Used two ways: surfaced in
   the returned [bool_model] so [get-model] reports the Boolean bindings, and (encoded
   0/1) fed into the shared [lookup] the re-checker reads so a re-checked assertion
   mentioning a Boolean variable can be evaluated. *)
let read_bool_model blaster =
  let sat = Blast.sat blaster in
  List.map (fun (term, l) -> term, lit_value sat l) (Blast.bool_vars blaster)
;;

(* A10 opt-in for the bit-blasted CNF: mark every SAT var EXCEPT the read-back set
   eligible for variable elimination. The read-back set is exactly the leaf bit-vector
   variables' bits ([bv_vars]) and the free Boolean variables ([bool_vars]) — the only
   vars [read_model]/[read_bool_model] and the independent re-check consult; everything
   else (gate outputs, division quotient/remainder, extension fillers) is pure aux
   structure. The forced-true constant [tru] is a level-0 unit, so bounded elimination's
   assigned-var skip already leaves it alone. Inert unless OXSMT_SATPRE is on (the core's
   gate); when on, blasted CNF is where bounded elimination pays most. Sound regardless:
   elimination preserves equisatisfiability, and every reported model is re-checked below
   over the leaf values, fail-closed. *)
let mark_aux_eliminable blaster =
  let sat = Blast.sat blaster in
  let frozen = Hashtbl.create 256 in
  List.iter
    (fun (_, bits) ->
      Array.iter (fun l -> Hashtbl.replace frozen (Sat.var_of_lit l) ()) bits)
    (Blast.bv_vars blaster);
  List.iter
    (fun (_, l) -> Hashtbl.replace frozen (Sat.var_of_lit l) ())
    (Blast.bool_vars blaster);
  for v = 0 to Sat.num_vars sat - 1 do
    if not (Hashtbl.mem frozen v) then Sat.set_eliminable sat v
  done
;;

let solve defs assertions =
  match
    let blaster = Blast.create defs in
    List.iter (Blast.assert_term blaster) assertions;
    blaster
  with
  | exception Blast.Unsupported_bv msg -> Unknown msg
  | blaster ->
    if satpre_on () then mark_aux_eliminable blaster;
    (match Sat.solve (Blast.sat blaster) with
     | Sat.Unsat -> Unsat
     | Sat.Sat ->
       let model = read_model blaster in
       let bool_model = read_bool_model blaster in
       (* soundness net: never emit a Sat the model does not actually satisfy *)
       let tbl = Term.Table.create (List.length model + List.length bool_model) in
       List.iter (fun (t, (v, _)) -> Term.Table.replace tbl t v) model;
       List.iter
         (fun (t, b) -> Term.Table.replace tbl t (if b then Bigint.one else Bigint.zero))
         bool_model;
       let lookup t = Term.Table.find_opt tbl t in
       (match List.for_all (fun a -> Bv_eval.eval_bool defs ~lookup a) assertions with
        | true -> Sat (model, bool_model)
        | false -> Unknown "sat model failed independent re-check (fail-closed)"
        | exception Bv_eval.Eval_error m -> Unknown ("model re-check error: " ^ m)))
;;
