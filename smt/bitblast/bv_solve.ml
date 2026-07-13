open Oxsmt_core
module Sat = Oxsmt_solver.Sat

type model = (Term.t * (Bigint.t * int)) list

type verdict =
  | Sat of model
  | Unsat
  | Unknown of string

let two = Bigint.of_int 2

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

(* Free Boolean variables' truth values under the SAT model, encoded as 0/1 so the shared
   [lookup] the re-checker reads (a [Term.t -> Bigint.t]) covers them too — [Bv_eval]
   interprets a looked-up Boolean value as [true] iff it is nonzero. Not surfaced in the
   returned bit-vector model; used only to complete the independent re-check. *)
let read_bool_model blaster =
  let sat = Blast.sat blaster in
  List.map
    (fun (term, l) -> term, if lit_value sat l then Bigint.one else Bigint.zero)
    (Blast.bool_vars blaster)
;;

let solve defs assertions =
  match
    let blaster = Blast.create defs in
    List.iter (Blast.assert_term blaster) assertions;
    blaster
  with
  | exception Blast.Unsupported_bv msg -> Unknown msg
  | blaster ->
    (match Sat.solve (Blast.sat blaster) with
     | Sat.Unsat -> Unsat
     | Sat.Sat ->
       let model = read_model blaster in
       let bool_model = read_bool_model blaster in
       (* soundness net: never emit a Sat the model does not actually satisfy *)
       let tbl = Term.Table.create (List.length model + List.length bool_model) in
       List.iter (fun (t, (v, _)) -> Term.Table.replace tbl t v) model;
       List.iter (fun (t, v) -> Term.Table.replace tbl t v) bool_model;
       let lookup t = Term.Table.find_opt tbl t in
       (match List.for_all (fun a -> Bv_eval.eval_bool defs ~lookup a) assertions with
        | true -> Sat model
        | false -> Unknown "sat model failed independent re-check (fail-closed)"
        | exception Bv_eval.Eval_error m -> Unknown ("model re-check error: " ^ m)))
;;
