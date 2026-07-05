(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* BORROWS MEET VIA, fail-closed: an in-place op that LIES about its
   effect is rejected at the implementation's VC.  The set is sealed
   [refines (iset)] over [varr{ .. } via (setof : iset)] (mset's shape,
   inlined here with a self-contained trusted borrow module).  The
   honest [insert] proves its residual is [ins x s] (see
   lean_mset_seal.ml); this [insert_noop] runs the SAME in-place
   insert but claims the set is UNCHANGED ([t{ _ = s }]).  The residual
   binds [setof r1 = spv p] and the bracket exports
   [spv p = ins x (setof r0)], so the coercion's goal [setof r1 = s]
   reduces to [ins x s = s], which the solver refutes -- fail-closed,
   never a silent pass. *)

type iset [@@vox.sort lean "ISet"]

[%%vox.lean {lean|
inductive ISet where
  | nil : ISet
  | cons : Int -> ISet -> ISet
@[grind] def ins (x : Int) (s : ISet) : ISet := ISet.cons x s
instance : Inhabited ISet := ⟨.nil⟩
opaque setof : VoxU -> ISet
opaque snow : VoxU -> ISet
opaque sfin : VoxU -> ISet
opaque spv  : VoxU -> ISet
|lean}]
[%%expect{|
type iset
|}]

(* Self-contained trusted borrow library, ghosts at the ISet image. *)
module Lib : sig
  type varr
  type proph
  type slice
  val new_proph : unit -> proph @ unique
  val borrow :
    (p : proph) @ unique -> (x : varr) @ unique ->
    ((m : slice{ snow _ = setof x && sfin _ = spv p }) @ local unique -> 'b @ unique)
      @ once local ->
    (varr{ setof _ = spv p } * 'b) @ unique
  val sinsert :
    (m : slice) @ local unique -> (x : int) ->
    slice{ snow _ = ins x (snow m) && sfin _ = sfin m } @ local unique
  val sdrop : (m : slice) @ local unique -> unit{ sfin m = snow m }
end = struct
  type varr = A of { mutable cell : int list }
  type proph = P of { u : unit }
  type slice = L of { holder : varr }
  let new_proph : unit -> proph @ unique =
    fun () -> Obj.magic_unique (P { u = () })
  let borrow :
    (p : proph) @ unique -> (x : varr) @ unique ->
    ((m : slice{ snow _ = setof x && sfin _ = spv p }) @ local unique -> 'b @ unique)
      @ once local ->
    (varr{ setof _ = spv p } * 'b) @ unique =
    fun p x k ->
      let (P _) = p in
      let m0 =
        (assume_unchecked_ (Obj.magic_unique (L { holder = x }))
          : slice{ snow _ = setof x && sfin _ = spv p })
      in
      let b = k m0 in
      Obj.magic_unique ((assume_unchecked_ x : varr{ setof _ = spv p }), b)
  let sinsert :
    (m : slice) @ local unique -> (x : int) ->
    slice{ snow _ = ins x (snow m) && sfin _ = sfin m } @ local unique =
    fun m x ->
      let (L { holder }) = m in
      let (A r) = holder in
      r.cell <- x :: r.cell;
      exclave_
        (Obj.magic_unique
           (assume_unchecked_ (L { holder })
             : slice{ snow _ = ins x (snow m) && sfin _ = sfin m }))
  let sdrop : (m : slice) @ local unique -> unit{ sfin m = snow m } =
    fun m -> let (L _) = m in assume_unchecked_ ()
end
[%%expect{|
module Lib :
  sig
    type varr
    type proph
    type slice
    val new_proph : unit -> proph @ unique
    val borrow :
      (p : proph) @ unique ->
      (x : varr) @ unique ->
      (slice{ ((snow _) = (setof x)) && ((sfin _) = (spv p)) } @ local
       unique -> 'b @ unique) @ local
      once -> varr{ (setof _) = (spv p) } * 'b @ unique
    val sinsert :
      (m : slice) @ local unique ->
      (x : int) ->
      slice{ ((snow _) = (ins x (snow m))) && ((sfin _) = (sfin m)) } @ local
      unique
    val sdrop : (m : slice) @ local unique -> unit{ (sfin m) = (snow m) }
  end
|}]

open Lib

type t = varr{ 0 = 0 } [@vox.via (setof : iset)]
[%%expect{|
type t = Lib.varr{ 0 = 0 via (setof : iset) }
|}]

(* LYING in-place op: inserts x but claims the set is unchanged. *)
let insert_noop : (x : int) -> (s : t) @ unique -> t{ _ = s } @ unique =
  fun x s ->
    let refine_ r0 = s in
    let p = new_proph () in
    let (r1, u) =
      borrow p r0 (fun m ->
        let m1 = sinsert m x in
        let _u = sdrop m1 in
        (() : unit{ spv p = ins x (setof r0) }))
    in
    ignore u;
    (r1 : t{ _ = s })
[%%expect{|
Line 12, characters 5-7:
12 |     (r1 : t{ _ = s })
          ^^
Error: vox: verification failed (lean).
       Goal: (0 = 0) && ((setof r1) = s)
Hypotheses:
  r1 = (fst *unknown8*)
  u = (snd *unknown8*)
  (setof r1) = (spv p)
  (spv p) = (ins x (setof r0))
  0 = 0
  (setof r0) = s
(lean: error: `grind` failed)
|}]
