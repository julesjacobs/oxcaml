(* A small executable specification prelude for refinement predicates. By ruling (see
   plan.html, "The predicate representation") predicates are quantifier-free bool terms,
   so this prelude carries NO forall_/exists_ combinators -- only bool-level helpers and
   measure helpers. If quantifiers return later they come back as ordinary total spec
   combinators, so nothing here forecloses them. *)

(* @pre id=implies final=ACCEPT today=ACCEPT stable=yes unlocks=- *)
let (implies @ total) premise conclusion = (not premise) || conclusion

(* @pre id=conjunction final=ACCEPT today=ACCEPT stable=yes unlocks=- *)
let (conjunction @ total) left right = left && right

(* VOX2_AWAITS_TOTAL_COMPARISONS: these wrappers are intentionally left partial today.
   Once kind-constrained total comparisons land, their definitions become the single
   substitution point for specifications. *)

(* @pre id=int_lt final=ACCEPT today=ACCEPT stable=no unlocks=total-comparisons *)
let int_lt (left : int) (right : int) = left < right

(* @pre id=int_le final=ACCEPT today=ACCEPT stable=no unlocks=total-comparisons *)
let int_le (left : int) (right : int) = left <= right

(* @pre id=int_gt final=ACCEPT today=ACCEPT stable=no unlocks=total-comparisons *)
let int_gt (left : int) (right : int) = left > right

(* @pre id=int_ge final=ACCEPT today=ACCEPT stable=no unlocks=total-comparisons *)
let int_ge (left : int) (right : int) = left >= right

(* [List.length] is the first measure used by the examples. Structural recursive totality
   is not expressible in the current mode checker, so this wrapper also remains partial
   today. *)
(* @pre id=list_length final=ACCEPT today=ACCEPT stable=no unlocks=recursive-totality *)
let list_length values = List.length values
