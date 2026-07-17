(* TEST
 expect;
*)

(* Freshen-on-import / add_type manifest copying.  A refined type manifest whose
   predicate carries a private binder survives copying through [module type of]
   and functor instantiation with its identity intact: it still ENFORCES its
   refinement and stays DISTINCT from a different predicate.  This pins the
   deliberate decision (see typing/subst.ml) to skip refinement-binder
   freshening for [add_type] -- the predicate binders are copied verbatim and
   alpha-equality pairs them positionally, so no stamp collision arises on these
   paths.  (Cross-unit .cmi import additionally freshens via [for_loading_cmi];
   the persistence tests cover that path.) *)
module type HasK = sig type t = int{ let k = 0 in _ >= k } end
module M = struct type t = int{ let k = 0 in _ >= k } end
module F (X : HasK) = struct type u = X.t end
module A = F (M)

[%%expect {|
module type HasK = sig type t = int{ let k = 0 in _ >= k } end
module M : sig type t = int{ let k = 0 in _ >= k } end
module F : functor (X : HasK) -> sig type u = X.t end
module A : sig type u = M.t end
|}]

(* Enforcement survives copying: a bare int does not flow to the copied
   refinement without an obligation. *)
let bad (x : int) : A.u = x

[%%expect {|
Line 1, characters 26-27:
1 | let bad (x : int) : A.u = x
                              ^
Error: The value "x" has type "int" but an expression was expected of type
         "A.u" = "int{ let k = 0 in _ >= k }"
|}]

(* Distinctness survives copying: a different predicate does not unify with the
   copied one. *)
let mismatch (x : int{ let k = 1 in _ >= k }) : A.u = x

[%%expect {|
Line 1, characters 54-55:
1 | let mismatch (x : int{ let k = 1 in _ >= k }) : A.u = x
                                                          ^
Error: The value "x" has type "int" but an expression was expected of type
         "A.u" = "int{ let k = 0 in _ >= k }"
|}]
