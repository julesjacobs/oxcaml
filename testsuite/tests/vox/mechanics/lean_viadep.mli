(* F-1 regression provider: a via-abstracted type with a 3-parameter
   dependent operation whose result names all three parameters, the
   third being the via value.  Importing this arrow into a client used
   to alias the via argument's model slot to the first scalar
   argument's atom (a Subst binder-freshening stamp collision), so a
   client call [add 1 10 m] modelled [vadd 1 10 1] instead of
   [vadd 1 10 m].  See lean_viadep_client.ml. *)

type vml [@@vox.sort lean "VML"]
type t : value refines (vml)

[%%vox.lean {lean|
public inductive VML where
  | VN : VML
  | VC : Int -> Int -> VML -> VML

public axiom vadd : Int -> Int -> VML -> VML
|lean}]

val add : (k : int) -> (v : int) -> (m : t) -> t{ _ = vadd k v m }
