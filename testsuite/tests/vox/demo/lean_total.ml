(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* vox: the TOTAL spec-function space [@vox.total].  A relation/spec
   parameter written [(r : ((int -> int -> bool) [@vox.total]))] admits
   ONLY reflectable, effect-free functions -- call-site lambdas whose
   bodies reflect, and [@@vox.reflect] values.  A non-reflectable
   argument is rejected at the type/argument layer (see
   mechanics/lean_total_fail.ml).  This is the positive twin.  The marker
   is a structural sentinel, so it rides type copy and the .cmi; it sorts
   at the arrow (S_arrow) and erases to an ordinary closure. *)

[@@@warning "-6-32"]

[%%vox.lean {lean|
abbrev IntRel := Int -> Int -> Prop
@[grind] def rHolds (r : IntRel) (a b : Int) : Prop := r a b
@[grind] def leRel : IntRel := fun a b => a <= b
|lean}]

let apply_step :
      (r : ((int -> int -> bool) [@vox.total])) ->
      (f : ((x : int) -> int{ rHolds r x _ })) ->
      (x : int) -> int{ rHolds r x _ } =
  fun r f x -> ignore r; f x

(* a reflectable call-site lambda: full precision, north-star consequence. *)
let client (x : int) : int{ x <= _ } =
  apply_step (fun p q -> p <= q) (fun a -> a + 1) x

(* a [@@vox.reflect] value flows into the total position (reflect TCB). *)
external le_rel : (int -> int -> bool) = "%lessequal" [@@vox.reflect "leRel"]

let client_named (x : int) : int{ x <= _ } =
  apply_step le_rel (fun a -> a + 1) x

(* total -> plain subsumption: a total value is usable where a plain
   function is expected. *)
let use_plain (g : (int -> int -> bool)) : bool = g 1 2

let sink :
      (r : ((int -> int -> bool) [@vox.total])) -> (x : int) -> int =
  fun r x -> ignore (use_plain r); x

(* the attribute at a USE over a plain alias also marks the arrow total. *)
type rel = int -> int -> bool

let apply_alias :
      (r : (rel [@vox.total])) ->
      (f : ((x : int) -> int{ rHolds r x _ })) ->
      (x : int) -> int{ rHolds r x _ } =
  fun r f x -> ignore r; f x

let client_alias (x : int) : int{ x <= _ } =
  apply_alias (fun p q -> p <= q) (fun a -> a + 1) x

(* the [vox_total] former named directly in an alias carries totality
   (unlike a bare arrow alias): [type t = arrow vox_total] used as [(r : t)]
   is total.  This is why the former (a nominal type) closes the
   alias-propagation hole a plain attribute would leave. *)
type trel = (int -> int -> bool) vox_total

let apply_named :
      (r : trel) ->
      (f : ((x : int) -> int{ rHolds r x _ })) ->
      (x : int) -> int{ rHolds r x _ } =
  fun r f x -> ignore r; f x

let client_named_alias (x : int) : int{ x <= _ } =
  apply_named (fun p q -> p <= q) (fun a -> a + 1) x
