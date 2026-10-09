type t = int

(* A dense monotonic counter: [fresh] hands out 0, 1, 2, … — deterministic (I6), never an
   id chosen by the caller (ADR-0005 CONTRACT-ATOM). *)
type allocator = { mutable next : int }

let create_allocator () = { next = 0 }

let fresh a =
  let id = a.next in
  a.next <- id + 1;
  id
;;

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
