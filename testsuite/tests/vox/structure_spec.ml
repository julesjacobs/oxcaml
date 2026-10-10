open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec

let[@def] (linkable @ total) (h : node Pref.heap @ immutable)
    (source : tree @ immutable) (target : tree @ immutable) = ghost_ (
  let p = tree_root source in let q = tree_root target in
  finite h source && finite h target && not (p === q)
  && terminal h p && terminal h q && active h p && active h q
  && readback source === readback target
  && match at_level h p with Generic -> false | Finite n -> below h q n)

type result = #{state : node Pref.token; source : node Pref.t @@ aliased;
  target : node Pref.t @@ aliased}
