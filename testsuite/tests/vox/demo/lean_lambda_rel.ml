(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Demo: LAMBDA REFLECTION for relations (task #68).  A relation is a
   dependent parameter of function type; a client supplies it as an
   ordinary OCaml lambda ([apply_step (fun p q -> p <= q) ..]).  The
   lambda is reflected to a Lean [fun .. => ..] ([Refinement.Plam]) and
   substituted at the binder; grind beta-reduces it against the spec
   function that consumes it.  The correspondence is DERIVED from the
   OCaml body (unlike [@@vox.reflect]), so it is checked, not assumed.

   Surface note: the relation binder's function type is parenthesized
   ([(r : (int -> int -> bool))]) because the dependent-binder grammar
   currently accepts only an atomic inner type; see the study doc. *)

(* The relation binders print as labelled parameters, so passing them
   positionally warns [labels-omitted]; irrelevant to the demo. *)
[@@@warning "-6"]

[%%vox.lean {lean|
def IntRel := Int -> Int -> Prop
@[grind] def rHolds (r : IntRel) (a b : Int) : Prop := r a b
|lean}]

(* Apply a producer [f] whose per-input contract is the relation [r];
   the result is [r]-related to the input.  [r] is passed WHOLE to the
   [rHolds] wrapper, so the relation is applied only inside Lean. *)
let apply_step :
      (r : (int -> int -> bool)) ->
      (f : ((x : int) -> int{ rHolds r x _ })) ->
      (x : int) -> int{ rHolds r x _ } =
  fun r f x -> ignore r; f x

(* Client: R := the lambda [fun p q -> p <= q]; f increments (a <= a+1,
   satisfying the relation); conclude the CONCRETE fact [x <= result]. *)
let client (x : int) : int{ x <= _ } =
  apply_step (fun p q -> p <= q) (fun a -> a + 1) x

(* A different lambda body ([q < p], strict): the producer decrements, so
   each step is strictly below its input, and the client concludes the
   concrete [result < x]. *)
let client2 (x : int) : int{ _ < x } =
  apply_step (fun p q -> q < p) (fun a -> a - 1) x
