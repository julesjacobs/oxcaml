(* Implementation of htbl.mli, checked against its interface's model.
   A hash table is a fixed-width SPINE of buckets; each bucket is an
   association list of (key, value) pairs.  [find] hashes the key and
   inspects exactly ONE bucket; the interface proves that equals a
   scan of the WHOLE table.  Every arm carries its own obligation and
   nothing is assumed. *)

type bucket =
  | BNil
  | BCons of int * int * bucket

type table =
  | TNil
  | TCons of bucket * table

type opt =
  | Missing
  | Found of int

type t = table{ twf _ 0 && tlen _ = 8 }

let empty
  : t{ _
       = TCons
           ( BNil,
             TCons
               ( BNil,
                 TCons
                   ( BNil,
                     TCons
                       ( BNil,
                         TCons
                           (BNil, TCons (BNil, TCons (BNil, TCons (BNil, TNil))))
                       ) ) ) ) }
  =
  TCons
    ( BNil,
      TCons
        ( BNil,
          TCons
            ( BNil,
              TCons (BNil, TCons (BNil, TCons (BNil, TCons (BNil, TCons (BNil, TNil)))))
            ) ) )

let index : (k : int{ 0 <= _ }) -> int{ _ = index k } = fun k -> k mod 8

(* Assoc-list search: proved equal to the model [bfind]. *)
let rec bucket_find : (k : int) -> (b : bucket) -> opt{ _ = bfind k b } =
  fun k b ->
    match b with
    | BNil -> Missing
    | BCons (k', v, r) -> if k = k' then Found v else bucket_find k r

(* Jump to the bucket at spine offset [o]. *)
let rec nth_bucket : (t : table) -> (o : int) -> bucket{ _ = tnth t o } =
  fun t o ->
    match t with
    | TNil -> BNil
    | TCons (b, r) -> if o <= 0 then b else nth_bucket r (o - 1)

(* One-bucket lookup equals the whole-table scan: [tfind_eq_jump]
   bridges the bucket the code visits to the buckets it skips. *)
let find : (k : int{ 0 <= _ }) -> (t : t) -> opt{ _ = tfind k t } =
  fun k t ->
    let i = index k in
    let b = nth_bucket t i in
    bucket_find k b

(* Assoc-list insert/replace: proved equal to the model [badd]. *)
let rec bucket_add : (k : int) -> (v : int) -> (b : bucket) -> bucket{ _ = badd k v b } =
  fun k v b ->
    match b with
    | BNil -> BCons (k, v, BNil)
    | BCons (k', v', r) ->
      if k = k' then BCons (k, v, r)
      else begin
        let r' = bucket_add k v r in
        BCons (k', v', r')
      end

(* Rebuild the spine with bucket at offset [o] replaced. *)
let rec spine_set : (t : table) -> (o : int) -> (nb : bucket) -> table{ _ = tset t o nb } =
  fun t o nb ->
    match t with
    | TNil -> TNil
    | TCons (b, r) ->
      if o <= 0 then TCons (nb, r)
      else begin
        let r' = spine_set r (o - 1) nb in
        TCons (b, r')
      end

(* In the home bucket only: the result is exactly the model [madd],
   and it lands back in [t] (invariant preserved, width unchanged). *)
let add : (k : int{ 0 <= _ }) -> (v : int) -> (t : t) -> t{ _ = madd k v t } =
  fun k v t ->
    let i = index k in
    let b = nth_bucket t i in
    let b' = bucket_add k v b in
    spine_set t i b'
