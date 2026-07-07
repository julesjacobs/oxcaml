(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Demo: LAMBDA REFLECTION for relations (task #68).  A relation is a
   dependent parameter of function type; a client supplies it as an
   ordinary OCaml lambda ([apply_step (fun p q -> p <= q) ..]) or as a
   named [@@vox.reflect] value.  A lambda is reflected to a Lean
   [fun .. => ..] ([Refinement.Plam]) and substituted at the binder;
   grind beta-reduces it against the spec function that consumes it.  The
   correspondence is DERIVED from the OCaml body (unlike [@@vox.reflect]'s
   hand-pairing), so it is checked, not assumed.

   Surface note: the relation binder's function type is parenthesised
   ([(r : (int -> int -> bool))]) because the dependent-binder grammar
   currently accepts only an atomic inner type; see the study doc.  The
   parameters print as labels, so positional application warns 6. *)

[@@@warning "-6-32"]

[%%vox.lean {lean|
def IntRel := Int -> Int -> Prop
@[grind] def rHolds (r : IntRel) (a b : Int) : Prop := r a b
@[grind] def rcomp (r s : IntRel) : IntRel := fun a c => exists b, r a b /\ s b c
@[grind] def leRel : IntRel := fun a b => a <= b
|lean}]

(* Apply a producer [f] whose per-input contract is the relation [r];
   the result is [r]-related to the input. *)
let apply_step :
      (r : (int -> int -> bool)) ->
      (f : ((x : int) -> int{ rHolds r x _ })) ->
      (x : int) -> int{ rHolds r x _ } =
  fun r f x -> ignore r; f x

(* Core: the relation is a LAMBDA; the client proves the concrete fact. *)
let client (x : int) : int{ x <= _ } =
  apply_step (fun p q -> p <= q) (fun a -> a + 1) x

(* A different lambda body ([q < p], strict). *)
let client2 (x : int) : int{ _ < x } =
  apply_step (fun p q -> q < p) (fun a -> a - 1) x

(* A && goal whose conjuncts both follow from the lambda-relation fact. *)
let client_and (x : int) : int{ x <= _ && _ >= x } =
  apply_step (fun p q -> p <= q) (fun a -> a + 1) x

(* COMPOSITION: applying an r-step then an s-step is an (rcomp r s)-step,
   over lambdas supplied for r and s. *)
let compose2 :
      (r : (int -> int -> bool)) ->
      (s : (int -> int -> bool)) ->
      (f : ((x : int) -> int{ rHolds r x _ })) ->
      (g : ((y : int) -> int{ rHolds s y _ })) ->
      (x : int) -> int{ rHolds (rcomp r s) x _ } =
  fun r s f g x -> ignore (r, s); let z = f x in g z

let client_comp (x : int) : int{ x < _ } =
  compose2 (fun a b -> a <= b) (fun a b -> a < b) (fun a -> a) (fun a -> a + 1) x

(* NAMED-VALUE FLOW: a [@@vox.reflect] value passed whole flows its Lean
   symbol (leRel), so the client reasons with the concrete relation.  Here
   the value is the real [%lessequal] primitive, whose runtime meaning is
   exactly [leRel]. *)
external le_rel : (int -> int -> bool) = "%lessequal" [@@vox.reflect "leRel"]

let client_named (x : int) : int{ x <= _ } =
  apply_step le_rel (fun a -> a + 1) x

(* GHOST-INVOCATION BOUNDARY: the relation is a REAL [int -> int -> bool],
   so it may be INVOKED at runtime; its runtime meaning agrees with its
   reflection (both derive from [p <= q]) -- no ghost phantom, zero trust. *)
let runtime_call () : bool = (fun p q -> p <= q) 3 5
