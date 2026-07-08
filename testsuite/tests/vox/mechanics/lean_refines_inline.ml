(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* vox: the INLINE refines form.  [type t : value refines ([%lean "MyT"])]
   points an abstract type directly at a block-declared Lean type,
   without the ghost-sort intermediary [type myt [@@vox.sort lean "MyT"]]
   + [refines (myt)].  It elaborates to exactly the same sort ([MyT], the
   type's own parameters mapped positionally), so verification is
   identical to the two-line spelling (see lean_ghost_sort.ml). *)

[%%vox.lean {lean|
inductive MyT where
  | nil : MyT
  | cons : Int -> MyT -> MyT

@[grind] def mem (x : Int) : MyT -> Prop
  | .nil => False
  | .cons y s => x = y ∨ mem x s

@[grind] def ins (x : Int) (s : MyT) : MyT := MyT.cons x s
|lean}]

module S : sig
  type t : value refines ([%lean "MyT"])

  val add : (x : int) -> (s : t) -> t{ _ = ins x s } @ unique
  val mem : (x : int) -> (s : t) -> bool{ _ = mem x s }
end = struct
  type t = { mutable c : int } [@@vox.sort lean "MyT"]

  let add : (x : int) -> (s : t) -> t{ _ = ins x s } @ unique =
    fun x s -> ignore (x, s); assume_unchecked_ (Obj.magic_unique { c = 0 })

  let mem : (x : int) -> (s : t) -> bool{ _ = mem x s } =
    fun x s -> ignore (x, s); assume_unchecked_ false
end
[%%expect{|
module S :
  sig
    type t
    val add : (x : int) -> (s : t) -> t{ _ = ins x s } @ unique
    val mem : (x : int) -> (s : t) -> bool{ _ = mem x s }
  end
|}]

(* A client proves THROUGH the inline-refines type, exactly as through a
   ghost sort: [t = ins x s] and [S.mem x t] give [_ = mem x (ins x s)]. *)
let roundtrip : (x : int) -> (s : S.t) -> bool{ _ = mem x (ins x s) } =
  fun x s ->
    let t = S.add x s in
    S.mem x t
[%%expect{|
val roundtrip : (x : int) -> (s : S.t) -> bool{ _ = mem x (ins x s) } = <fun>
|}]

(* Ill-formed payload: the argument to [%lean ...] must be a single
   string literal. *)
module Bad1 : sig
  type t : value refines ([%lean 42])
end = struct
  type t = int
end
[%%expect{|
Line 2, characters 26-36:
2 |   type t : value refines ([%lean 42])
                              ^^^^^^^^^^
Error: vox: refines ([%lean ...]) takes a single Lean type name, e.g. [%lean "MyT"]
|}]

(* Reserved namespace: a ghost name in the [Vox_]/[v_] solver namespaces
   is rejected (it could alias an emitted datatype/value name), the same
   as the [@@vox.sort lean] attribute rejects it. *)
module Bad2 : sig
  type t : value refines ([%lean "Vox_x"])
end = struct
  type t = int
end
[%%expect{|
Line 2, characters 26-41:
2 |   type t : value refines ([%lean "Vox_x"])
                              ^^^^^^^^^^^^^^^
Error: vox: "Vox_x" may not name a ghost sort -- the Vox_ and v_ prefixes are reserved for the solver's emitted names (it would collide)
|}]
