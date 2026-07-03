(* RustHorn-style mutable borrows over a linear reference, with no
   dedicated checker support.

   [vref] is a mutable int reference whose LOGICAL REPRESENTATIVE is
   its contents ([@@vox.sort int]): a live vref's ghost value always
   equals what the box holds, so [vref{ _ = v }] says "contains v"
   with no spec-function wrapper.  Ops consume it [@ unique] (strong
   update); [vpeek] reads it aliased, permanently forfeiting writes
   (any aliased use destroys uniqueness, so an aliased snapshot can
   never go stale).

   A borrow is bracketed by CPS.  [borrow_mut p x k] hands [k] the
   LOAN [m], logically the RustHorn pair (now m, fin m) = (current
   contents, prophesied final contents), tied at entry to the
   borrowed ref and to the prophecy: now m = x, fin m = p.  The loan
   is [@ local unique]: it cannot escape the continuation, so when
   borrow_mut returns, the loan is dead and the residual
   [vref{ _ = p }] -- the same ref, now viewed at the prophesied
   contents -- is honest.  [proph] also denotes its value
   ([@@vox.sort int]); it is opaque and unforgeable, consumed
   [@ unique] so one prophecy serves exactly one borrow (two
   resolutions of one p could prove False).

   [mdrop] RESOLVES the prophecy: its refined unit carries
   [fin m = now m], which chains with the loan's facts to reveal
   [p].  Dropping is optional: an undropped borrow still ends at the
   bracket, and [p] simply stays opaque (a sound leak) -- the lender
   can even learn the value observationally by reading the residual.
   [mdropa] is the aliased variant for terminal read phases (aliased
   uses freeze the loan, so every resolution of a frozen snapshot
   issues the same fact).  Results flow out of the continuation
   through ['b], restated in lender-scope terms.

   TRUSTED: the implementation asserts the invariants above with
   assume_unchecked_ / mode casts; everything else is proved. *)

type vref [@@vox.sort int]
type proph [@@vox.sort int]
type mut

[%%vox.lean {lean|
opaque now : VoxU -> Int
opaque fin : VoxU -> Int
|lean}]

val vnew : (v : int) -> vref{ _ = v } @ unique
val vread : (x : vref) @ unique -> (int{ _ = x } * vref{ _ = x }) @ unique
val vpeek : (x : vref) -> int{ _ = x }
val vset : (x : vref) @ unique -> (w : int) -> vref{ _ = w } @ unique

val new_proph : unit -> proph @ unique

val borrow_mut :
  (p : proph) @ unique -> (x : vref) @ unique ->
  ((m : mut{ now _ = x && fin _ = p }) @ local unique -> 'b @ unique)
    @ once local ->
  (vref{ _ = p } * 'b) @ unique

val mget :
  (m : mut) @ local unique ->
  (int{ _ = now m } * mut{ now _ = now m && fin _ = fin m }) @ local unique

val mpeek : (m : mut) @ local -> int{ _ = now m }

val mset :
  (m : mut) @ local unique -> (w : int) ->
  mut{ now _ = w && fin _ = fin m } @ local unique

val mdrop : (m : mut) @ local unique -> unit{ fin m = now m }
val mdropa : (m : mut) @ local -> unit{ fin m = now m }
