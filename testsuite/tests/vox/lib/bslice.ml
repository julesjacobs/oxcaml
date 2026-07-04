(* Inline-record constructors keep the three types boxed and out of
   the solver's datatype story, modelled at VoxU exactly as the
   interface's abstract types.  A loan [L] denotes the segment
   [base.(off_) .. base.(off_ + len_ - 1)]; every assume_unchecked_
   asserts its signature's ghost facts of that reading, where a
   bucket array's ghost is the SPINE [table] its cells spell in
   order.  A read hands back a GLOBAL bucket because the stored
   value is immutable. *)

open Htbl

type varr = A of { base : bucket array }
type proph = P of { u : unit }
type slice = L of { base : bucket array; off_ : int; len_ : int }

let rec model_len (t : table) =
  match t with
  | TNil -> 0
  | TCons (_, r) -> 1 + model_len r

let of_model : (m : table) -> varr{ bcts _ = m } @ unique =
  fun m ->
    let base = Array.make (model_len m) BNil in
    let rec fill (t : table) (i : int) =
      match t with
      | TNil -> ()
      | TCons (b, r) ->
        base.(i) <- b;
        fill r (i + 1)
    in
    fill m 0;
    assume_unchecked_ (Obj.magic_unique (A { base }))

let new_proph : unit -> proph @ unique = fun () -> Obj.magic_unique (P { u = () })

let borrow :
  (p : proph) @ unique -> (x : varr) @ unique ->
  ((m : slice{ bnow _ = bcts x && bfin _ = bpv p }) @ local unique -> 'b @ unique)
    @ once local ->
  (varr{ bcts _ = bpv p } * 'b) @ unique =
  fun p x k ->
    let (P _) = p in
    let (A { base }) = x in
    let m0 =
      (assume_unchecked_
         (Obj.magic_unique (L { base; off_ = 0; len_ = Array.length base }))
        : slice{ bnow _ = bcts x && bfin _ = bpv p })
    in
    let b = k m0 in
    Obj.magic_unique ((assume_unchecked_ (A { base }) : varr{ bcts _ = bpv p }), b)

let sget :
  (m : slice) @ local unique -> (i : int{ 0 <= _ && _ < tlen (bnow m) }) ->
  (bucket{ _ = tnth (bnow m) i } * slice{ bnow _ = bnow m && bfin _ = bfin m })
    @ local unique =
  fun m i ->
    let (L { base; off_; len_ }) = m in
    let v = base.(off_ + i) in
    exclave_
      (Obj.magic_unique
         ( (assume_unchecked_ v : bucket{ _ = tnth (bnow m) i }),
           (assume_unchecked_ (L { base; off_; len_ })
             : slice{ bnow _ = bnow m && bfin _ = bfin m }) ))

external unsafe_gbl : (b : bucket) @ local -> bucket = "%identity"

let gbl : (b : bucket) @ local -> bucket{ _ = b } =
  fun b -> assume_unchecked_ (unsafe_gbl b)

let sset :
  (m : slice) @ local unique -> (i : int{ 0 <= _ && _ < tlen (bnow m) }) ->
  (b : bucket) ->
  slice{ bnow _ = tset (bnow m) i b && bfin _ = bfin m } @ local unique =
  fun m i b ->
    let (L { base; off_; len_ }) = m in
    base.(off_ + i) <- b;
    exclave_
      (Obj.magic_unique
         (assume_unchecked_ (L { base; off_; len_ })
           : slice{ bnow _ = tset (bnow m) i b && bfin _ = bfin m }))

let sdrop : (m : slice) @ local unique -> unit{ bfin m = bnow m } =
  fun m ->
    let (L _) = m in
    assume_unchecked_ ()
