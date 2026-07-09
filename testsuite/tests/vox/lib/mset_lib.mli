(* A MUTABLE finite set as a RUSTHORN-STYLE BORROW whose ghosts are
   the Lean set model ISet DIRECTLY -- the mutable twin of via_set,
   and the set-sorted analogue of bslice (whose ghosts were Htbl's
   [table]).  An owned set denotes its ISet contents [setof]; a live
   loan denotes its current/prophesied-final contents [snow]/[sfin];
   a prophecy denotes the set it resolves to [spv].  The in-place
   operation [sinsert] states its effect AT THE IMAGE SORT:
   [snow _ = ins x (snow m)] -- a borrow spec in pure set vocabulary,
   never mentioning a tree or a list.

   TRUSTED: [varr]/[proph]/[slice] are abstract and boxed; every
   [assume_unchecked_] in the implementation asserts its signature's
   ghost facts hold of the real mutable set.  This is the whole trust
   boundary (six functions); a client built on top (see mset.ml) and
   every fact it proves is DERIVED from these signatures. *)

type iset [@@vox.sort lean "ISet"]
type varr
type proph
type slice

[%%vox.lean {lean|
public inductive ISet where
  | nil : ISet
  | cons : Int -> ISet -> ISet

@[grind, expose] public def mem (x : Int) : ISet -> Prop
  | .nil => False
  | .cons y s => x = y ∨ mem x s

@[grind, expose] public def ins (x : Int) (s : ISet) : ISet := ISet.cons x s

@[grind, expose] public def card : ISet -> Int
  | .nil => 0
  | .cons _ s => 1 + card s

-- ghosts at the SET model: owned contents, loan now/fin, prophecy.
public instance : Inhabited ISet := ⟨.nil⟩
public opaque setof : VoxU -> ISet
public opaque snow : VoxU -> ISet
public opaque sfin : VoxU -> ISet
public opaque spv  : VoxU -> ISet
|lean}]

(* A fresh empty set. *)
val empty : unit -> varr{ card (setof _) = 0 } @ unique

val new_proph : unit -> proph @ unique

(* Open a borrow bracket: the continuation gets the loan over the whole
   set; the residual comes back at the prophesied contents. *)
val borrow :
  (p : proph) @ unique -> (x : varr) @ unique ->
  ((m : slice{ snow _ = setof x && sfin _ = spv p }) @ local unique -> 'b @ unique)
    @ once local ->
  (varr{ setof _ = spv p } * 'b) @ unique

(* IN-PLACE insert on a loan, stated in SET vocabulary at the image. *)
val sinsert :
  (m : slice) @ local unique -> (x : int) ->
  slice{ snow _ = ins x (snow m) && sfin _ = sfin m } @ local unique

(* Membership on a loan (reads, threads the loan unchanged). *)
val smem :
  (m : slice) @ local unique -> (x : int) ->
  (bool{ _ = mem x (snow m) } * slice{ snow _ = snow m && sfin _ = sfin m })
    @ local unique

val sdrop : (m : slice) @ local unique -> unit{ sfin m = snow m }
