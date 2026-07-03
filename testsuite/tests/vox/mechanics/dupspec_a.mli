type t = A | B

[%%vox.lean {lean|
@[grind] def dup_spec : Vox_Dupspec_a_t -> Int
  | .A => 0
  | .B => 1
|lean}]

val v : t{ dup_spec _ = 0 }
