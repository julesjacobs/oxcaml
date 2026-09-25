module Match = Vox_lz4_general_match
module Hashes = Map.Make (Int)
module Positions = Map.Make (Int)

let from_source
    (source : {s : char iarray | Iarray.length s <= 4194304}) =
  let length = Iarray.length source in
  let rec scan hashes positions (position : {p : int | 0 <= p}) =
    if position > length - 12 then positions
    else
      let hash = Match.hash4 source position in
      let candidate =
        match Hashes.find_opt hash hashes with
        | None -> -1
        | Some previous -> previous in
      let hashes = Hashes.add hash position hashes in
      let positions = Positions.add position candidate positions in
      let limit = length - 5 - position in
      match Match.choose_match source position limit candidate with
      | None -> scan hashes positions (position + 1)
      | Some choice ->
        scan hashes positions (position + choice.length)
  in
  let positions = scan Hashes.empty Positions.empty 0 in
  Iarray.init length (fun position ->
    match Positions.find_opt position positions with
    | None -> -1
    | Some candidate -> candidate)
