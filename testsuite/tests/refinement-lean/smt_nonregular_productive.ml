type ('a, 'b) t =
  | Zero
  | Step of ('a list, 'a option) t

let reflexive (x : (int, bool) t @ logical) : unit{ x = x } = ()
