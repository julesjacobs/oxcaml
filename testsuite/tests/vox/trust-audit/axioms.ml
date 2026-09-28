external bogus : unit -> {u : unit | false} @@ total = "%identity"
external trust_total : 'a -> 'a @ total = "%identity"
let rec spin () : int = spin ()
let total_spin = trust_total spin
let cast x = Obj.magic x
external stub : int -> int = "vox_audit_stub"
