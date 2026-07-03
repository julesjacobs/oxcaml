(* Implementation of sep_lib: real references behind the ghost
   assertion.  [href] carries the live cell; [htoken] is a boxed unit
   (the inline record keeps it out of the datatype story, still boxed
   so uniqueness does not mode-cross).  Every operation performs the
   machine effect and assumes the resulting assertion -- the
   [assume_unchecked_]s below are the library's entire trust story:
   [alloc]'s carries freshness (a new cell is disjoint from every
   chunk of the frame), [get]/[set]'s carry the points-to reading of
   the actual load and store. *)

type href = { mutable c_ : int }
type htoken = Tok of { g_ : unit }

type hprop =
  | Emp
  | Pts of href * int
  | Star of hprop * hprop

let init : unit -> htoken{ sat Emp (hp _) } @ unique =
  fun () -> assume_unchecked_ (Tok { g_ = () })

let alloc :
  (p : hprop) -> (v : int) ->
  htoken{ sat p (hp _) } @ unique ->
  (href * htoken){ sat (Star (Pts (fst _, v), p)) (hp (snd _)) } @ unique =
  fun p v t ->
    ignore p; ignore t;
    assume_unchecked_ ({ c_ = v }, Tok { g_ = () })

let get :
  (r : href) -> (k : int) -> (p : hprop) ->
  htoken{ sat (Star (Pts (r, k), p)) (hp _) } @ unique ->
  (int{ _ = k } * htoken{ sat (Star (Pts (r, k), p)) (hp _) }) @ unique =
  fun r k p t ->
    ignore k; ignore p; ignore t;
    ( (assume_unchecked_ r.c_ : int{ _ = k }),
      (assume_unchecked_ (Tok { g_ = () })
        : htoken{ sat (Star (Pts (r, k), p)) (hp _) }) )

let set :
  (r : href) -> (old : int) -> (v : int) -> (p : hprop) ->
  htoken{ sat (Star (Pts (r, old), p)) (hp _) } @ unique ->
  htoken{ sat (Star (Pts (r, v), p)) (hp _) } @ unique =
  fun r _old v p t ->
    ignore p; ignore t;
    r.c_ <- v;
    assume_unchecked_ (Tok { g_ = () })
