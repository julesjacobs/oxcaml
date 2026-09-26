let bad a =
  let r = Reference_lock.try_acquire a in
  if r.value then begin
    let _ = Reference_lock.release a r.state in
    Reference_lock.read_owned a (borrow_ r.state)
  end else 0
