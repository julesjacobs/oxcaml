(* Implementation of seplist_lib: real mutable nodes behind the ghost
   assertion.  Every operation performs the machine effect and assumes
   the resulting assertion; the [assume_unchecked_]s below are the
   library's entire trust story -- [alloc_node]'s carries freshness,
   [uncons]'s carries the unfold (the machine next pointer IS the
   segment's witness), [get_val]/[set_next]'s carry the points-to
   reading of the actual load and store. *)

type href =
  { mutable v_ : int
  ; mutable n_ : link
  }

and link =
  | Null
  | Ptr of href

type htoken = Tok of { g_ : unit }

type ilist =
  | INil
  | ICons of int * ilist

type hprop =
  | Emp
  | Node of href * int * link
  | Lseg of link * ilist
  | Star of hprop * hprop

type alloced =
  { nod : href @@ aliased
  ; ntok : htoken
  }

type stepped =
  { lnk : link @@ aliased
  ; stok : htoken
  }

let init : unit -> htoken{ sat Emp (hp _) } @ unique =
  fun () -> assume_unchecked_ (Tok { g_ = () })

let alloc_node :
  (p : hprop) -> (v : int) -> (nxt : link) ->
  htoken{ sat p (hp _) } @ unique ->
  alloced{ sat (Star (Node (_.nod, v, nxt), p)) (hp _.ntok) } @ unique =
  fun p v nxt t ->
    ignore p; ignore t;
    assume_unchecked_ { nod = { v_ = v; n_ = nxt }; ntok = Tok { g_ = () } }

let uncons :
  (r : href) -> (v : int) -> (vs : ilist) -> (p : hprop) ->
  htoken{ sat (Star (Lseg (Ptr r, ICons (v, vs)), p)) (hp _) } @ unique ->
  stepped{ sat (Star (Node (r, v, _.lnk), Star (Lseg (_.lnk, vs), p)))
               (hp _.stok) } @ unique =
  fun r v vs p t ->
    ignore v; ignore vs; ignore p; ignore t;
    assume_unchecked_ { lnk = r.n_; stok = Tok { g_ = () } }

let get_val :
  (r : href) -> (v : int) -> (nxt : link) -> (p : hprop) ->
  htoken{ sat (Star (Node (r, v, nxt), p)) (hp _) } @ unique ->
  (int{ _ = v } * htoken{ sat (Star (Node (r, v, nxt), p)) (hp _) }) @ unique =
  fun r v nxt p t ->
    ignore v; ignore nxt; ignore p; ignore t;
    ( (assume_unchecked_ r.v_ : int{ _ = v }),
      (assume_unchecked_ (Tok { g_ = () })
        : htoken{ sat (Star (Node (r, v, nxt), p)) (hp _) }) )

let set_next :
  (r : href) -> (v : int) -> (old : link) -> (nxt : link) -> (p : hprop) ->
  htoken{ sat (Star (Node (r, v, old), p)) (hp _) } @ unique ->
  htoken{ sat (Star (Node (r, v, nxt), p)) (hp _) } @ unique =
  fun r v old nxt p t ->
    ignore v; ignore old; ignore p; ignore t;
    r.n_ <- nxt;
    assume_unchecked_ (Tok { g_ = () })
