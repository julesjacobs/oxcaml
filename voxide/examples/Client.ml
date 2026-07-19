(* Calling refined functions from another unit.

   [good] nests two cross-module calls: [Lib.pos (Lib.one ())] discharges because
   [Lib.one]'s result contract [_ > 0] satisfies [Lib.pos]'s argument contract. [bad]
   passes [0], whose obligation [0 > 0] is disproved -- so this workspace does NOT verify. *)

let good = Lib.pos (Lib.one ())
let bad = Lib.pos 0
