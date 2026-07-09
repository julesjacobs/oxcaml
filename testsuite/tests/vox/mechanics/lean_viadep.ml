type vml [@@vox.sort lean "VML"]
type al = AN | AC of int * int * al
type t = al{ 0 = 0 } [@vox.via (vrepr : vml)]

[%%vox.lean {lean|
inductive VML where
  | VN : VML
  | VC : Int -> Int -> VML -> VML

@[grind] def vadd (k v : Int) (m : VML) : VML := .VC k v m

@[grind] def vrepr : Vox_Lean_viadep_al -> VML
  | .AN => .VN
  | .AC k v t => .VC k v (vrepr t)
|lean}]

let add : (k : int) -> (v : int) -> (m : t) -> t{ _ = vadd k v m } =
  fun k v m ->
    let refine_ t0 = m in
    (AC (k, v, t0) : t{ _ = vadd k v m })
