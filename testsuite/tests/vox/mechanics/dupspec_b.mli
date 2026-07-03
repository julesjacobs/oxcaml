type t = C | D

[%%vox.lean {lean|
@[grind] def dup_spec : Vox_Dupspec_b_t -> Int
  | .C => 0
  | .D => 1
|lean}]

val v : t{ dup_spec _ = 0 }
