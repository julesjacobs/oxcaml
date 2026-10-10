type hashes = (int * int) list

let[@def] rec (lookup @ total) (hash : int)
    (entries : hashes @ immutable) =
  match entries with
  | [] -> -1
  | (key, position) :: rest ->
    if key = hash then position else lookup hash rest
