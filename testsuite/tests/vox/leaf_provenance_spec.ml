open Copy_spec
open Level_spec
let[@def] (low_var @ total) (h : Pref.heap @ immutable)
    (x : node Pref.t @ immutable) (cut : int) = ghost_ (
  below h x cut && Level_unifier_spec.observe h x === Some Var)
