(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Embedded prelude: the spec functions live INSIDE the module as a
   [%%vox.prelude.lean] block -- no -vox-prelude file.  Same proofs as
   lean_spec.ml: every obligation is really discharged by grind, the
   recursive ones inductively.  Blocks may appear anywhere; they are
   emitted (in source order) into every solver input for the module,
   after the datatype declarations, whose solver-side names are stable
   (Vox_<Unit>_<type>). *)

type ilist =
  | Nil
  | Cons of int * ilist

[%%vox.prelude.lean {lean|
@[grind] def len : Vox_Lean_embed_ilist -> Int
  | .Nil => 0
  | .Cons _ t => 1 + len t
|lean}]

let l2 : ilist{ len _ = 2 } = refine_ (Cons (1, Cons (2, Nil)))

let push (l : ilist{ len _ = 2 }) : ilist{ len _ = 3 } =
  let refine_ l = l in
  refine_ (Cons (9, l))

(* Later blocks may use earlier blocks' definitions (source order). *)
[%%vox.prelude.lean {lean|
@[grind] def nonempty (l : Vox_Lean_embed_ilist) : Prop := len l > 0
|lean}]

let ne : ilist{ nonempty _ } = refine_ (Cons (7, Nil))

(* The textbook inductive proof, through an embedded measure. *)
let rec append : (a : ilist) -> (b : ilist) -> ilist{ len _ = len a + len b } =
  fun a b ->
    match a with
    | Nil -> refine_ b
    | Cons (h, t) ->
      let refine_ r = append t b in
      refine_ (Cons (h, r))
