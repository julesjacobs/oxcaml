let bad a b =
  let r = Reference_lock.try_acquire a in
  if r.value then let _ = Reference_lock.release b r.state in ()
