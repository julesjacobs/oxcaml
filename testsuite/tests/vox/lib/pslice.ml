(* Inline-record constructors keep the three types boxed, modelled at
   their parameterized opaque sorts exactly as the interface's
   abstract types.  A loan [L] denotes the segment
   [base.(off_) .. base.(off_ + len_ - 1)]; every assume_unchecked_
   asserts its signature's ghost facts of that reading, at whatever
   element type the caller instantiated.  A read hands back the
   element [local]; [gbl] re-exposes it globally, sound because it
   came from the (global) array. *)

type 'a varr = A of { base : 'a array } [@@vox.poly]
type 'a proph = P of { u : unit } [@@vox.poly]
type 'a slice = L of { base : 'a array; off_ : int; len_ : int } [@@vox.poly]

let pnew :
  (n : int{ 0 <= _ }) -> (x : 'a) ->
  'a varr{ plen (pcts _) = n && pconst (pcts _) x } @ unique =
  fun n x -> assume_unchecked_ (Obj.magic_unique (A { base = Array.make n x }))

let new_proph : unit -> 'a proph @ unique = fun () -> Obj.magic_unique (P { u = () })

let borrow :
  (p : 'a proph) @ unique -> (x : 'a varr) @ unique ->
  ((m : 'a slice{ pnow _ = pcts x && pfin _ = ppv p }) @ local unique -> 'b @ unique)
    @ once local ->
  ('a varr{ pcts _ = ppv p } * 'b) @ unique =
  fun p x k ->
    let (P _) = p in
    let (A { base }) = x in
    let m0 =
      (assume_unchecked_
         (Obj.magic_unique (L { base; off_ = 0; len_ = Array.length base }))
        : 'a slice{ pnow _ = pcts x && pfin _ = ppv p })
    in
    let b = k m0 in
    Obj.magic_unique
      ((assume_unchecked_ (A { base }) : 'a varr{ pcts _ = ppv p }), b)

let sget :
  (m : 'a slice) @ local unique -> (i : int{ 0 <= _ && _ < plen (pnow m) }) ->
  ('a{ _ = pelem (pnow m) i } * 'a slice{ pnow _ = pnow m && pfin _ = pfin m })
    @ local unique =
  fun m i ->
    let (L { base; off_; len_ }) = m in
    let v = base.(off_ + i) in
    exclave_
      (Obj.magic_unique
         ( (assume_unchecked_ v : 'a{ _ = pelem (pnow m) i }),
           (assume_unchecked_ (L { base; off_; len_ })
             : 'a slice{ pnow _ = pnow m && pfin _ = pfin m }) ))

external unsafe_gbl : (x : 'a) @ local -> 'a = "%identity"

let gbl : (x : 'a) @ local -> 'a{ _ = x } =
  fun x -> assume_unchecked_ (unsafe_gbl x)

let sset :
  (m : 'a slice) @ local unique -> (i : int{ 0 <= _ && _ < plen (pnow m) }) ->
  (x : 'a) ->
  'a slice{ pnow _ = pupd (pnow m) i x && pfin _ = pfin m } @ local unique =
  fun m i x ->
    let (L { base; off_; len_ }) = m in
    base.(off_ + i) <- x;
    exclave_
      (Obj.magic_unique
         (assume_unchecked_ (L { base; off_; len_ })
           : 'a slice{ pnow _ = pupd (pnow m) i x && pfin _ = pfin m }))

let sdrop : (m : 'a slice) @ local unique -> unit{ pfin m = pnow m } =
  fun m ->
    let (L _) = m in
    assume_unchecked_ ()
