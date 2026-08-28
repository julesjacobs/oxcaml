type t

val zero : t @@ total

val same : unit -> {r : t | r === zero} @@ total
