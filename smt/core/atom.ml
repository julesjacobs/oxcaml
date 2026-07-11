type t = int

let of_int v = v
let equal = Int.equal
let compare = Int.compare
let hash = Hashtbl.hash

module Key = struct
  type nonrec t = t

  let compare = compare
  let equal = equal
  let hash = hash
end

module Set = Set.Make (Key)
module Map = Map.Make (Key)
module Table = Hashtbl.Make (Key)
