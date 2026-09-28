external contents : string @ local immutable ->
  char iarray @ total immutable ghost
  @@ total = "caml_borrow_contents"

external length : (source : string) @ local immutable ->
  {n : int | n = Iarray.length (contents source)}
  @@ total = "%string_length"

external get : (source : string) @ immutable ->
  (index : {i : int | 0 <= i && i < Iarray.length (contents source)}) ->
  {c : char | Some c === Vox_iarray.at (contents source) index}
  @@ total = "%string_unsafe_get"
