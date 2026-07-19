type t =
  { domain : Sort.t Iarr.t
  ; codomain : Sort.t
  }

let create domain codomain = { domain = Iarr.of_list domain; codomain }
let arity t = Iarr.length t.domain
