(* A small executable specification prelude.  [forall_] and [exists_] are
   ordinary total functions over lambdas.  Their executable results are
   sentinels; the verification backend recognizes calls to these names and
   gives them their logical meanings.  They are not syntax and do not add a
   formula layer to refinement predicates. *)

(* @pre id=forall final=ACCEPT today=ACCEPT stable=yes unlocks=- *)
let forall_ @ total =
  fun ((_predicate : 'a -> bool) @ total) -> true

(* @pre id=exists final=ACCEPT today=ACCEPT stable=yes unlocks=- *)
let exists_ @ total =
  fun ((_predicate : 'a -> bool) @ total) -> false

(* @pre id=implies final=ACCEPT today=ACCEPT stable=yes unlocks=- *)
let implies @ total =
  fun premise conclusion -> (not premise) || conclusion

(* @pre id=conjunction final=ACCEPT today=ACCEPT stable=yes unlocks=- *)
let conjunction @ total = fun left right -> left && right

(* VOX2_AWAITS_TOTAL_COMPARISONS: these wrappers are intentionally left
   partial today.  Once kind-constrained total comparisons land, their
   definitions become the single substitution point for specifications. *)

(* @pre id=int_lt final=ACCEPT today=ACCEPT stable=no unlocks=total-comparisons *)
let int_lt (left : int) (right : int) = left < right

(* @pre id=int_le final=ACCEPT today=ACCEPT stable=no unlocks=total-comparisons *)
let int_le (left : int) (right : int) = left <= right

(* @pre id=int_gt final=ACCEPT today=ACCEPT stable=no unlocks=total-comparisons *)
let int_gt (left : int) (right : int) = left > right

(* @pre id=int_ge final=ACCEPT today=ACCEPT stable=no unlocks=total-comparisons *)
let int_ge (left : int) (right : int) = left >= right

(* [List.length] is the first measure used by the examples.  Structural
   recursive totality is not expressible in the current mode checker, so this
   wrapper also remains partial today. *)
(* @pre id=list_length final=ACCEPT today=ACCEPT stable=no unlocks=recursive-totality *)
let list_length values = List.length values
