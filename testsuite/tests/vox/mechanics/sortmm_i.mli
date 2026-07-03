(* interface side of the vox.sort mli/ml mismatch test: attribute here,
   missing on the implementation *)
type t [@@vox.sort int]

val mk : (v : int) -> t{ _ = v }
