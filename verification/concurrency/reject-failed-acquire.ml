let bad a =
  let r = Reference_lock.try_acquire a in
  if not r.value then Reference_lock.read_owned a (borrow_ r.state) else 0
