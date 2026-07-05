(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* Refinements are TYPES, so the language's polymorphism transports
   them with no verifier support: a 'a-generic container or combinator
   instantiated at a refined type carries the fact.  Three instances:
   a ref whose contents type is an invariant (reads return it, writes
   must re-establish it); lazy, which memoizes through internal
   mutation yet forces to the refined value; and par_lib's fork_join2,
   whose polymorphic type is what carries the quicksort tasks'
   conclusions across the domain join in demo/lean_qsort.ml. *)

let counter : int{ _ >= 0 } ref = ref (5 : int{ _ >= 0 })

let bump (u : unit) : int{ _ >= 0 } =
  ignore u;
  let v = !counter in
  counter := v + 1;
  !counter
[%%expect{|
val counter : int{ _ >= 0 } ref = {contents = 5}
val bump : unit -> int{ _ >= 0 } = <fun>
|}]

(* A write that cannot re-establish the invariant is refused. *)
let broken (u : unit) : unit =
  ignore u;
  let v = !counter in
  counter := v - 1
[%%expect{|
Line 4, characters 13-18:
4 |   counter := v - 1
                 ^^^^^
Error: vox: verification failed (lean).
       Goal: (v - 1) >= 0
Hypotheses:
  v >= 0
Possible counterexample:
  v = 0
(lean: error: `grind` failed)
|}]

let thunk : int{ 0 <= _ } lazy_t = lazy (2 + 3 : int{ 0 <= _ })

let forced : int{ 0 <= _ } = Lazy.force thunk
[%%expect{|
val thunk : int{ 0 <= _ } lazy_t = <lazy>
val forced : int{ 0 <= _ } = 5
|}]
