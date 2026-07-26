(* TEST
 expect;
*)

(* ============================================================= *)
(* ACCEPTANCE CORPUS: recursion (induction hypothesis)            *)
(*                                                                *)
(* plan.html "How checking works" (afterwards pass): "a recursive *)
(* call's refined result serves as the induction hypothesis". A   *)
(* recursive function with a refined RESULT type may assume, at    *)
(* each recursive call, that the call's own result satisfies the   *)
(* result refinement -- exactly an induction hypothesis.           *)
(*                                                                *)
(* This mirrors vox1's demo/lean_fib.ml shape (refined result,     *)
(* recursive call as IH) but uses ANNOTATIONS ONLY: vox2 has no    *)
(* intro forms (refine_ / assume_ do not exist), so the spec lives *)
(* entirely in the parameter contracts and the result annotation.  *)
(*                                                                *)
(* Marker legend: see binder_facts.ml.                            *)
(* ============================================================= *)

(* Numeric recursion is not structurally total, but result refinements are
   partial-correctness contracts and remain available on return paths.

   These state mathematical facts: the sum to n and the nth Fibonacci number
   are non-negative.  Neither holds of machine integers, where both overflow
   and wrap negative, so they are written over [Bigint.t], whose arithmetic
   has the same unbounded meaning at runtime and in proofs.  The induction
   hypothesis being exercised is unchanged. *)
let rec rec_fib_nonneg (n : Bigint.t{ Bigint.ge _ Bigint.zero })
    : Bigint.t{ Bigint.ge _ Bigint.zero } =
  if Bigint.le n Bigint.one then n
  else
    Bigint.add
      (rec_fib_nonneg (Bigint.sub n Bigint.one))
      (rec_fib_nonneg (Bigint.sub n (Bigint.of_int 2)))
[%%expect {|
val rec_fib_nonneg :
  Bigint.t{ Bigint.ge _ Bigint.zero } -> Bigint.t{ Bigint.ge _ Bigint.zero } =
  <fun>
|}]

let rec rec_sum_to (n : Bigint.t{ Bigint.ge _ Bigint.zero })
    : Bigint.t{ Bigint.ge _ Bigint.zero } =
  if Bigint.is_zero n then Bigint.zero
  else Bigint.add n (rec_sum_to (Bigint.sub n Bigint.one))
[%%expect {|
val rec_sum_to :
  Bigint.t{ Bigint.ge _ Bigint.zero } -> Bigint.t{ Bigint.ge _ Bigint.zero } =
  <fun>
|}]

(* A diverging self-call vacuously satisfies any returned-result contract. *)
let rec partial_false (x : int) : int{ false } = partial_false x
[%%expect {|
val partial_false : int -> int{ false } = <fun>
|}]

let rec mutual_partial_left (x : int) : int{ false } =
  mutual_partial_right x
and mutual_partial_right (x : int) : int{ false } =
  mutual_partial_left x
[%%expect {|
val mutual_partial_left : int -> int{ false } = <fun>
val mutual_partial_right : int -> int{ false } = <fun>
|}]

let dead_continuation () =
  let _ = partial_false 0 in
  (0 : int{ false })
[%%expect {|
val dead_continuation : unit -> int{ false } = <fun>
|}]

(* A handler can return without the call returning, so its result fact cannot
   escape into this reachable continuation. *)
let caught_continuation =
  let () = try ignore (partial_false 0) with _ -> () in
  (0 : int{ false })
[%%expect {|
Line 3, characters 2-20:
3 |   (0 : int{ false })
      ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* Requiring the same non-structural recursion to be total still rejects. *)
let rec partial_false_total @ total =
  fun (x : int) -> (partial_false_total x : int{ false })
[%%expect {|
Line 2, characters 20-39:
2 |   fun (x : int) -> (partial_false_total x : int{ false })
                        ^^^^^^^^^^^^^^^^^^^
Error: The value "partial_false_total" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 2, characters 2-57
         which is expected to be "total".
|}]

(* Mutual-group calls receive IH facts when one common structural position
   decreases throughout the group. *)
type mutual_lst = MNil | MCons of mutual_lst

(* Counting the list is unbounded in principle, so the count is a [Bigint.t]
   for the same reason as above: on machine integers a long enough list makes
   the successor overflow and the claim false.  The mutual induction being
   exercised is unchanged. *)
let rec mutual_left (value : mutual_lst)
    : Bigint.t{ Bigint.ge _ Bigint.zero } =
  match value with
  | MNil -> Bigint.zero
  | MCons tail -> Bigint.add Bigint.one (mutual_right tail)
and mutual_right (value : mutual_lst)
    : Bigint.t{ Bigint.ge _ Bigint.zero } =
  match value with
  | MNil -> Bigint.zero
  | MCons tail -> Bigint.add Bigint.one (mutual_left tail)

[%%expect {|
type mutual_lst = MNil | MCons of mutual_lst
val mutual_left : mutual_lst -> Bigint.t{ Bigint.ge _ Bigint.zero } = <fun>
val mutual_right : mutual_lst -> Bigint.t{ Bigint.ge _ Bigint.zero } = <fun>
|}]
