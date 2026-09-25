module type S = sig
  type token : void

  type partition = { left : token @@ ghost; right : token @@ ghost }

  val credits : token @ local immutable total ghost -> int @ ghost @@ total

  val nonnegative : (token : token) @ local immutable total ghost ->
    {u : unit | 0 <= credits token} @ ghost @@ total

  val empty : unit -> {t : token | credits t = 0} @ unique total ghost @@ total

  val tick : (token : {t : token | credits t > 0})
      @ unique total ghost ->
    {t : token | let refine_ token = token in
      credits t = credits token - 1} @ unique total ghost @@ total

  val split : (amount : int) @ ghost ->
    (token : {t : token | 0 <= amount && amount <= credits t})
      @ unique total ghost ->
    {p : partition | let refine_ token = token in credits p.left = amount &&
      credits p.right = credits token - amount} @ unique @@ total

  val merge : (left : token) @ unique total ghost ->
    (right : {t : token | 0 <= credits left && 0 <= credits t &&
      0 <= credits left + credits t})
      @ unique total ghost ->
    {t : token | let refine_ right = right in
      credits t = credits left + credits right} @ unique total ghost @@ total

end

module Make () = struct
  type amount = {n : int | n >= 0}
  type token = { balance : amount @@ ghost total contended }
  type partition = { left : token @@ ghost; right : token @@ ghost }

  let[@def] credits (token : token @ local immutable total ghost) =
    ghost_ (let refine_ balance = token.balance in balance)

  let (nonnegative @ total) (token : token @ local immutable total ghost) :
      {u : unit | 0 <= credits token} @ ghost = ghost_ (
    credits_def token;
    let refine_ balance = token.balance in
    let u = () in refine_ u)

  let (tick @ total) (token : {t : token | credits t > 0}
        @ unique total ghost) :
      {t : token | let refine_ token = token in
      credits t = credits token - 1} @ unique total ghost =
    let refine_ token = token in
    ghost_ (credits_def (borrow_ token));
    let result = { balance = ghost_ (
      let refine_ balance = token.balance in
      let n = balance - 1 in (refine_ n : amount)) } in
    ghost_ (credits_def token; credits_def (borrow_ result));
    refine_ result

  let (split @ total) (amount : int @ ghost)
      (token : {t : token | 0 <= amount && amount <= credits t}
        @ unique total ghost) :
      {p : partition | let refine_ token = token in credits p.left = amount &&
        credits p.right = credits token - amount} @ unique =
    let refine_ token = token in
    let left = { balance = ghost_ (refine_ amount : amount) } in
    ghost_ (credits_def (borrow_ token));
    let right = { balance = ghost_ (
      let refine_ balance = token.balance in
      let n = balance - amount in (refine_ n : amount)) } in
    let result = { left; right } in
    ghost_ (credits_def token; credits_def (borrow_ result.left);
      credits_def (borrow_ result.right));
    refine_ result

  let (merge @ total) (left : token @ unique total ghost)
      (right : {t : token | 0 <= credits left && 0 <= credits t &&
        0 <= credits left + credits t}
        @ unique total ghost) :
      {t : token | let refine_ right = right in
      credits t = credits left + credits right} @ unique total ghost =
    let refine_ right = right in
    ghost_ (credits_def (borrow_ left); credits_def (borrow_ right));
    let result = { balance = ghost_ (
      let refine_ a = left.balance in
      let refine_ b = right.balance in
      let n = a + b in (refine_ n : amount)) } in
    ghost_ (credits_def left; credits_def right; credits_def (borrow_ result));
    refine_ result

  module Budget = struct
    let (create @ total) (amount : {n : int | n >= 0} @ ghost) :
        {t : token | let refine_ amount = amount in credits t = amount}
        @ unique total ghost =
      let refine_ amount = amount in
      let result = { balance = ghost_ (refine_ amount : amount) } in
      ghost_ (credits_def (borrow_ result));
      refine_ result
  end
  let (empty @ total) () :
      {t : token | credits t = 0} @ unique total ghost =
    let zero = 0 in
    let initial : amount = refine_ zero in
    let refine_ result = Budget.create initial in
    refine_ result
end
