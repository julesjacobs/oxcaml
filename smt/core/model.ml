(* A model is a term-keyed value assignment. Construction is added by the first real
   consumer (the M2 EUF adapter / M4 combinator), which also pins the [Uninterp] witness
   encoding (open q3); this module is not hash-frozen until then (ADR-0005 Tranche B). *)

type value =
  | Int of Bigint.t
  | Bool of bool
  | Uninterp of int

type t = value Term.Map.t

let value m term = Term.Map.find_opt term m

(* Duplicate-key decision (deliberate, not accidental): a model assigns each term exactly
   ONCE, so a repeated key is a construction bug in the caller — we raise rather than
   silently keep the last binding. A silent last-wins is exactly how an L1-class fault
   (two conflicting assignments, one masking the other) slips through unnoticed; failing
   loudly surfaces it. *)
let of_alist bindings =
  List.fold_left
    (fun m (term, v) ->
       if Term.Map.mem term m
       then invalid_arg "Model.of_alist: duplicate term (a model binds each term once)";
       Term.Map.add term v m)
    Term.Map.empty
    bindings
;;
