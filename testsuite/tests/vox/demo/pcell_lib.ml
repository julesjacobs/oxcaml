type icell = { mutable v : int; id_ : int }
type itoken = Tok of int
type cpair = { cell : icell; tok : itoken }

let ctr = ref 0

let alloc :
  (v : int) -> cpair{ tid _.tok = cid _.cell && cts _.tok = v } @ unique =
  fun v ->
    incr ctr;
    (* capture the id first: [{ cell = c; tok = Tok c.id_ }] would
       use [c] twice within the literal (the unique pair consumes
       it) *)
    let id = !ctr in
    let c = { v; id_ = id } in
    assume_unchecked_ { cell = c; tok = Tok id }

let read :
  (c : icell) -> (k : int) -> itoken{ tid _ = cid c && cts _ = k } @ unique ->
  (int{ _ = k } * itoken{ tid _ = cid c && cts _ = k }) @ unique =
  fun c k t ->
    ignore t; ignore k;
    ( (assume_unchecked_ c.v : int{ _ = k }),
      (assume_unchecked_ (Tok c.id_) : itoken{ tid _ = cid c && cts _ = k }) )

let write :
  (c : icell) -> (old : int) -> (v : int) ->
  itoken{ tid _ = cid c && cts _ = old } @ unique ->
  itoken{ tid _ = cid c && cts _ = v } @ unique =
  fun c _old v t -> ignore t; c.v <- v; assume_unchecked_ (Tok c.id_)
