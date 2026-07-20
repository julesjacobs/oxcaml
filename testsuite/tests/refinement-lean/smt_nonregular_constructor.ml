type ('a, 'b) t = C of ('a list, 'a option) t

let reflexive (x : (int, bool) t @ logical) : unit{ x = x } = ()
