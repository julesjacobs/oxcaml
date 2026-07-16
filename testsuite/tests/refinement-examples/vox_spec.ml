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

(* [List.length] is the first measure used by the examples. Unlike the int wrappers above
   (which the compiler WOULD accept @ total but the ruling keeps partial), this one is
   genuinely not total yet: [List.length] is a recursive stdlib function and the mode
   checker cannot establish structural recursive totality, so the wrapper is partial. *)
(* @pre id=list_length final=ACCEPT today=ACCEPT stable=no unlocks=recursive-totality *)
let list_length values = List.length values
