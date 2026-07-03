type icell = { mutable v : int; id_ : int }

(* The inline-record argument keeps [itoken] OUT of the solver's
   datatype story (non-simple, still boxed), so this implementation
   models it at VoxU exactly as the interface's abstract [itoken]
   does: the interface's .cmi-exported rendering of [cpair] and the
   local one must agree on their field sorts. *)
type itoken = Tok of { id : int }

type cpair = { cell : icell; tok : itoken }

let ctr = ref 0

let alloc :
  (v : int) -> cpair{ tid _.tok = cid _.cell && cts _.tok = v } @ unique =
  fun v ->
    incr ctr;
    (* capture the id first: reading [c.id_] inside the pair literal
       would use [c] twice (the unique pair consumes it) *)
    let id = !ctr in
    let c = { v; id_ = id } in
    assume_unchecked_ { cell = c; tok = Tok { id } }

let read :
  (c : icell) -> (k : int) -> itoken{ tid _ = cid c && cts _ = k } @ unique ->
  (int{ _ = k } * itoken{ tid _ = cid c && cts _ = k }) @ unique =
  fun c k t ->
    ignore t; ignore k;
    ( (assume_unchecked_ c.v : int{ _ = k }),
      (assume_unchecked_ (Tok { id = c.id_ })
        : itoken{ tid _ = cid c && cts _ = k }) )

let write :
  (c : icell) -> (old : int) -> (v : int) ->
  itoken{ tid _ = cid c && cts _ = old } @ unique ->
  itoken{ tid _ = cid c && cts _ = v } @ unique =
  fun c _old v t ->
    ignore t;
    c.v <- v;
    assume_unchecked_ (Tok { id = c.id_ })
