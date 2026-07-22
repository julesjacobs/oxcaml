val impossible : unit -> int{ false }

val p : bool
val q : bool
val law_p : unit{ p = true }
val law_q : unit{ q = true }

val in_function : bool
val in_lazy : bool
val in_loop : bool
val in_branch : bool
val law_function : unit{ in_function = true }
val law_lazy : unit{ in_lazy = true }
val law_loop : unit{ in_loop = true }
val law_branch : unit{ in_branch = true }
