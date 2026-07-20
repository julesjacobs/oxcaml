type t = int

let make a positive =
  let a = (a : Atom.t :> int) in
  (a lsl 1) lor if positive then 0 else 1
;;

let atom l = Atom_unsafe.of_int (l lsr 1)
let sign l = l land 1 = 0
let negate l = l lxor 1
let equal = Int.equal
let compare = Int.compare
let hash = Hashtbl.hash

module Key = struct
  type nonrec t = t

  let compare = compare
end

module Set = Set.Make (Key)
module Map = Map.Make (Key)
