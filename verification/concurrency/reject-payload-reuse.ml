module L = Unique_lock_buffer_client.L
let bad (x : Unique_lock_buffer_client.Data.t @ unique) =
  let _ = L.make x in
  L.make x
