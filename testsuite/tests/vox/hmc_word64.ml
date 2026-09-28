type limb = {n : int | 0 <= n && n < 4294967296}
type t = {lo : limb; hi : limb}

let[@def] (equal @ total) (a : t @ immutable) (b : t @ immutable) =
  a.lo = b.lo && a.hi = b.hi

let[@def] (unsigned_less @ total) (a : t @ immutable) (b : t @ immutable) =
  a.hi < b.hi || (a.hi = b.hi && a.lo < b.lo)

let[@def] (add @ total) (a : t @ immutable) (b : t @ immutable) : t @ immutable =
  let low = a.lo + b.lo in
  let carry = if low >= 4294967296 then 1 else 0 in
  let lo = if low >= 4294967296 then low - 4294967296 else low in
  let high = a.hi + b.hi + carry in
  let hi = if high >= 4294967296 then high - 4294967296 else high in
  {lo; hi}

let[@def] (subtract @ total) (a : t @ immutable) (b : t @ immutable) : t @ immutable =
  let low = a.lo - b.lo in
  let borrow = if low < 0 then 1 else 0 in
  let lo = if low < 0 then low + 4294967296 else low in
  let high = a.hi - b.hi - borrow in
  let hi = if high < 0 then high + 4294967296 else high in
  {lo; hi}

let (subtract_add @ total) : (a : t) @ immutable -> (b : t) @ immutable ->
    {u : unit | (subtract (add a b) b).lo = a.lo
      && (subtract (add a b) b).hi = a.hi} @ ghost = fun a b -> ghost_ (
  add_def a b; subtract_def (add a b) b; ())

let (add_subtract @ total) : (a : t) @ immutable -> (b : t) @ immutable ->
    {u : unit | (add (subtract a b) b).lo = a.lo
      && (add (subtract a b) b).hi = a.hi} @ ghost = fun a b -> ghost_ (
  subtract_def a b; add_def (subtract a b) b; ())

let (add_commutes @ total) : (a : t) @ immutable -> (b : t) @ immutable ->
    {u : unit | (add a b).lo = (add b a).lo
      && (add a b).hi = (add b a).hi} @ ghost = fun a b -> ghost_ (
  add_def a b; add_def b a; ())
