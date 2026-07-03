(* Interior mutability via unique separation tokens (Verus
   PCell/PointsTo, expressed with no dedicated checker support).

   [icell] is a mutable cell that vox never models (it is a mutable
   record, which the datatype story refuses); [itoken] is an
   unforgeable, unduplicable ghost witness of its contents.  [cts t]
   is the contents token snapshot [t] witnesses and [tid t = cid c]
   ties it to its cell (opaque spec functions, pcell_spec.lean).
   Every operation consumes the token [@ unique] and returns a fresh
   one, so the mode checker is the borrow checker: a stale token
   cannot be presented, and no fact is ever retracted -- facts speak
   of immutable token snapshots, never of the cell.

   [alloc] returns cell and token together; the pair is a simple
   record so its refinement names the components with native
   projections, and destructuring recovers them (per-field facts) --
   two cells' tokens can be live at once.  The match-bound token is
   bare: one checked [refine_] coercion re-establishes its refined
   type, after which tokens thread rigidly.

   Contents is a dependent parameter ([k]/[old]) rather than living
   in the token's rigid type, so a caller who knows the contents
   instantiates the signature at it (rigid refinement types have no
   subsumption).

   TRUSTED: [itoken] abstract (unforgeable) and boxed (must not
   mode-cross uniqueness); every op consumes the token @ unique. *)

type icell
type itoken
type cpair = { cell : icell; tok : itoken }

val alloc :
  (v : int) -> cpair{ tid _.tok = cid _.cell && cts _.tok = v } @ unique

val read :
  (c : icell) -> (k : int) -> itoken{ tid _ = cid c && cts _ = k } @ unique ->
  (int{ _ = k } * itoken{ tid _ = cid c && cts _ = k }) @ unique

val write :
  (c : icell) -> (old : int) -> (v : int) ->
  itoken{ tid _ = cid c && cts _ = old } @ unique ->
  itoken{ tid _ = cid c && cts _ = v } @ unique
