module type S = sig
  type token : void

  type partition = { left : token @@ ghost; right : token @@ ghost }

  val credits : token @ local immutable total ghost -> Bigint.t @ ghost @@ total

  val nonnegative : (token : token) @ local immutable total ghost ->
    {u : unit | 0Z <= credits token} @ ghost @@ total

  val empty : unit -> {t : token | credits t = 0Z} @ unique total ghost @@ total

  val tick : (token : {t : token | credits t > 0Z})
      @ unique total ghost ->
    {t : token | let refine_ token = token in
      credits t = Bigint.sub (credits token) 1Z} @ unique total ghost @@ total

  val split : (amount : Bigint.t) @ ghost ->
    (token : {t : token | 0Z <= amount && amount <= credits t})
      @ unique total ghost ->
    {p : partition | let refine_ token = token in credits p.left = amount &&
      credits p.right = Bigint.sub (credits token) amount} @ unique @@ total

  val merge : (left : token) @ unique total ghost ->
    (right : {t : token | 0Z <= credits left && 0Z <= credits t})
      @ unique total ghost ->
    {t : token | let refine_ right = right in
      credits t = Bigint.add (credits left) (credits right)} @ unique total ghost @@ total

end

module Make () = struct
  type amount = {n : Bigint.t | n >= 0Z}
  type token = { balance : amount @@ ghost total contended }
  type partition = { left : token @@ ghost; right : token @@ ghost }

  let[@def] credits (token : token @ local immutable total ghost) =
    ghost_ (let refine_ balance = token.balance in balance)

  let (nonnegative @ total) (token : token @ local immutable total ghost) :
      {u : unit | 0Z <= credits token} @ ghost = ghost_ (
    credits_def token;
    let refine_ balance = token.balance in
    let u = () in refine_ u)

  let (tick @ total) (token : {t : token | credits t > 0Z}
        @ unique total ghost) :
      {t : token | let refine_ token = token in
      credits t = Bigint.sub (credits token) 1Z} @ unique total ghost =
    let refine_ token = token in
    ghost_ (credits_def (borrow_ token));
    let result = { balance = ghost_ (
      let refine_ balance = token.balance in
      let n = Bigint.sub balance 1Z in (refine_ n : amount)) } in
    ghost_ (credits_def token; credits_def (borrow_ result));
    refine_ result

  let (split @ total) (amount : Bigint.t @ ghost)
      (token : {t : token | 0Z <= amount && amount <= credits t}
        @ unique total ghost) :
      {p : partition | let refine_ token = token in credits p.left = amount &&
        credits p.right = Bigint.sub (credits token) amount} @ unique =
    let refine_ token = token in
    let left = { balance = ghost_ (refine_ amount : amount) } in
    ghost_ (credits_def (borrow_ token));
    let right = { balance = ghost_ (
      let refine_ balance = token.balance in
      let n = Bigint.sub balance amount in (refine_ n : amount)) } in
    let result = { left; right } in
    ghost_ (credits_def token; credits_def (borrow_ result.left);
      credits_def (borrow_ result.right));
    refine_ result

  let (merge @ total) (left : token @ unique total ghost)
      (right : {t : token | 0Z <= credits left && 0Z <= credits t}
        @ unique total ghost) :
      {t : token | let refine_ right = right in
      credits t = Bigint.add (credits left) (credits right)} @ unique total ghost =
    let refine_ right = right in
    ghost_ (credits_def (borrow_ left); credits_def (borrow_ right));
    let result = { balance = ghost_ (
      let refine_ a = left.balance in
      let refine_ b = right.balance in
      let n = Bigint.add a b in (refine_ n : amount)) } in
    ghost_ (credits_def left; credits_def right; credits_def (borrow_ result));
    refine_ result

  module Budget = struct
    let (create @ total) (amount : {n : Bigint.t | n >= 0Z} @ ghost) :
        {t : token | let refine_ amount = amount in credits t = amount}
        @ unique total ghost =
      let refine_ amount = amount in
      let result = { balance = ghost_ (refine_ amount : amount) } in
      ghost_ (credits_def (borrow_ result));
      refine_ result
  end
  let (empty @ total) () :
      {t : token | credits t = 0Z} @ unique total ghost =
    let zero = ghost_ 0Z in
    let initial : amount = refine_ zero in
    let refine_ result = Budget.create initial in
    refine_ result
end
