external raise_false : exn -> int{ false } = "%raise"

let impossible () : int{ false } = raise_false Exit

let p = true
let q = true
let law_p = (() : unit{ p = true })
let law_q = (() : unit{ q = true })

let in_function = true
let in_lazy = true
let in_loop = true
let in_branch = true
let law_function = (() : unit{ in_function = true })
let law_lazy = (() : unit{ in_lazy = true })
let law_loop = (() : unit{ in_loop = true })
let law_branch = (() : unit{ in_branch = true })
