module L = Unique_lock.Make(Unique_lock_demo.Data)
let bad a =
  let r = L.try_acquire a in
  if r.value then begin
    ghost_ (L.owned_def a (Ghost_pref.own (borrow_ r.state)));
    let _ = L.take a r.state in
    let _ = L.take a r.state in ()
  end
