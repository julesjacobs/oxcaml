module L = Unique_lock.Make(Unique_lock_demo.Data)
let bad a = L.take a (Ghost_pref.empty ())
