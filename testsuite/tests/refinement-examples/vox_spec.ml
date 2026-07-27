(* A small executable specification prelude for refinement predicates. By ruling (see
   plan.html, "The predicate representation") predicates are quantifier-free bool terms,
   so this prelude carries NO forall_/exists_ combinators -- only bool-level helpers and
   measure helpers. If quantifiers return later they come back as ordinary total spec
   combinators, so nothing here forecloses them. *)

(* @pre id=implies final=ACCEPT today=ACCEPT stable=yes unlocks=- *)
let (implies @ total) premise conclusion = (not premise) || conclusion

(* @pre id=conjunction final=ACCEPT today=ACCEPT stable=yes unlocks=- *)
let (conjunction @ total) left right = left && right

(* VOX2_AWAITS_TOTAL_COMPARISONS: these int-comparison wrappers are kept PARTIAL
   to match the ruled end-state.  Comparison primitives are NOT on the general total
   allowlist: after modes integration they are admitted only INSIDE a refinement
   predicate (predicate-scoped), so a top-level [int_lt @ total] annotation is REJECTED
   today ("The value (<) is partial but is expected to be total").  The canonical plan
   ("Comparisons in specs") rules all comparison primitives partial for now; the
   immediate-comparison question is deferred, and making comparison total safely needs
   future kind-constrained declarations.  These definitions are the single substitution
   point when total comparisons land. *)

(* @pre id=int_lt final=ACCEPT today=ACCEPT stable=no unlocks=total-comparisons *)
let int_lt (left : int) (right : int) = left < right

(* @pre id=int_le final=ACCEPT today=ACCEPT stable=no unlocks=total-comparisons *)
let int_le (left : int) (right : int) = left <= right

(* @pre id=int_gt final=ACCEPT today=ACCEPT stable=no unlocks=total-comparisons *)
let int_gt (left : int) (right : int) = left > right

(* @pre id=int_ge final=ACCEPT today=ACCEPT stable=no unlocks=total-comparisons *)
let int_ge (left : int) (right : int) = left >= right

(* [List.length] is the first measure used by the examples. Like the int wrappers above
   it is not total-annotatable today, but for a different reason: the int wrappers use
   comparison primitives, which are admitted total only INSIDE a refinement predicate
   (predicate-scoped) and so are rejected at a top-level [@ total]; this one is blocked
   by the standard library's interface. [list.mli] declares [val length] with no mode,
   and an interface fixes the mode of what it describes whatever the implementation
   would have inferred, so the wrapper stays partial. This is not a limit of the
   structural check: the identical recursion written where no interface intervenes is
   total, and the same code behind an interface reading [@@ total] is admitted. *)
(* @pre id=list_length final=ACCEPT today=ACCEPT stable=no unlocks=stdlib-interface-modes *)
let list_length values = List.length values
