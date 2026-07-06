(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* vox: a declared INVARIANT, verified end to end (lean backend).
   [type nat : value refines (int{ _ >= 0 })] models [nat] at [int] but
   makes [_ >= 0] a free fact of every binder.  The fact survives
   abstraction: clients of the abstract [nat] reason with it, and the
   solver both PROVES what the invariant grants and REJECTS what it does
   not (so the invariant is not overclaimed). *)

module M : sig
  type nat : value refines (int{ _ >= 0 })

  val get : unit -> nat
end = struct
  type nat = int{ _ >= 0 }

  let get () : nat = refine_ 0
end
[%%expect{|
module M : sig type nat val get : unit -> nat end
|}]

(* The invariant proves a downstream goal: [n >= 0] gives [n + 1 >= 1]. *)
let ok () =
  let n = M.get () in
  let refine_ r = (n : M.nat{ _ + 1 >= 1 }) in
  r
[%%expect{|
val ok : unit -> M.nat = <fun>
|}]

(* The invariant is not OVERCLAIMED: [n >= 0] does not prove [n >= 1],
   and the counterexample is [n = 0]. *)
let bad () =
  let n = M.get () in
  let refine_ r = (n : M.nat{ _ >= 1 }) in
  r
[%%expect{|
Line 3, characters 19-20:
3 |   let refine_ r = (n : M.nat{ _ >= 1 }) in
                       ^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: n >= 1
Hypotheses:
  n >= 0
Counterexample (validated -- every hypothesis holds and the goal fails here):
  n = 0
|}]
