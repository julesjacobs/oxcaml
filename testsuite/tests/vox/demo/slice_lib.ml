(* Inline-record constructors keep all three types out of the
   solver's datatype story (non-simple, still boxed), so this
   implementation models them at VoxU exactly as the interface's
   abstract types do.  The ghosts' interpretation: a loan [L] denotes
   the segment [base.(off_) .. base.(off_ + len_ - 1)]; [now] is that
   segment's current contents, [fin] the contents it will hold when
   the loan dies, [pv p] the sequence its loan's segment holds at
   resolution; every assume_unchecked_ below asserts its signature's
   facts are true of that reading. *)

type varr = A of { base : int array }
type proph = P of { u : unit }
type slice = L of { base : int array; off_ : int; len_ : int }

let anew : (n : int{ 0 <= _ }) -> (v : int) -> varr{ len (cts _) = n } @ unique =
  fun n v -> assume_unchecked_ (Obj.magic_unique (A { base = Array.make n v }))

let alen :
  (x : varr) @ unique ->
  (int{ _ = len (cts x) } * varr{ cts _ = cts x }) @ unique =
  fun x ->
    let (A { base }) = x in
    let n = Array.length base in
    Obj.magic_unique
      ( (assume_unchecked_ n : int{ _ = len (cts x) }),
        (assume_unchecked_ (A { base }) : varr{ cts _ = cts x }) )

let aget :
  (x : varr) @ unique -> (i : int{ 0 <= _ && _ < len (cts x) }) ->
  (int{ _ = elem (cts x) i } * varr{ cts _ = cts x }) @ unique =
  fun x i ->
    let (A { base }) = x in
    let v = base.(i) in
    Obj.magic_unique
      ( (assume_unchecked_ v : int{ _ = elem (cts x) i }),
        (assume_unchecked_ (A { base }) : varr{ cts _ = cts x }) )

let new_proph : unit -> proph @ unique =
  fun () -> Obj.magic_unique (P { u = () })

let borrow :
  (p : proph) @ unique -> (x : varr) @ unique ->
  ((m : slice{ now _ = cts x && fin _ = pv p }) @ local unique -> 'b @ unique)
    @ once local ->
  (varr{ cts _ = pv p } * 'b) @ unique =
  fun p x k ->
    let (P _) = p in
    let (A { base }) = x in
    let m0 =
      (assume_unchecked_
         (Obj.magic_unique (L { base; off_ = 0; len_ = Array.length base }))
        : slice{ now _ = cts x && fin _ = pv p })
    in
    let b = k m0 in
    Obj.magic_unique
      ((assume_unchecked_ (A { base }) : varr{ cts _ = pv p }), b)

let slen :
  (m : slice) @ local unique ->
  (int{ _ = len (now m) } * slice{ now _ = now m && fin _ = fin m })
    @ local unique =
  fun m ->
    let (L { base; off_; len_ }) = m in
    exclave_
      (Obj.magic_unique
         ( (assume_unchecked_ len_ : int{ _ = len (now m) }),
           (assume_unchecked_ (L { base; off_; len_ })
             : slice{ now _ = now m && fin _ = fin m }) ))

let sget :
  (m : slice) @ local unique -> (i : int{ 0 <= _ && _ < len (now m) }) ->
  (int{ _ = elem (now m) i } * slice{ now _ = now m && fin _ = fin m })
    @ local unique =
  fun m i ->
    let (L { base; off_; len_ }) = m in
    let v = base.(off_ + i) in
    exclave_
      (Obj.magic_unique
         ( (assume_unchecked_ v : int{ _ = elem (now m) i }),
           (assume_unchecked_ (L { base; off_; len_ })
             : slice{ now _ = now m && fin _ = fin m }) ))

let sset :
  (m : slice) @ local unique -> (i : int{ 0 <= _ && _ < len (now m) }) ->
  (v : int) ->
  slice{ now _ = upd (now m) i v && fin _ = fin m } @ local unique =
  fun m i v ->
    let (L { base; off_; len_ }) = m in
    base.(off_ + i) <- v;
    exclave_
      (Obj.magic_unique
         (assume_unchecked_ (L { base; off_; len_ })
           : slice{ now _ = upd (now m) i v && fin _ = fin m }))

let split :
  (pl : proph) @ unique ->
  (pr : proph) @ unique ->
  (m : slice) @ local unique ->
  (i : int{ 0 <= _ && _ <= len (now m) }) ->
  ((left : slice{ now _ = take i (now m) && fin _ = pv pl }) @ local unique ->
   (right : slice{ now _ = drop i (now m) && fin _ = pv pr }) @ local unique ->
   'a @ unique)
    @ once local ->
  (slice{ now _ = app (pv pl) (pv pr) && fin _ = fin m } * 'a) @ local unique =
  fun pl pr m i k ->
    let (P _) = pl in
    let (P _) = pr in
    let (L { base; off_; len_ }) = m in
    let left =
      (assume_unchecked_ (Obj.magic_unique (L { base; off_; len_ = i }))
        : slice{ now _ = take i (now m) && fin _ = pv pl })
    in
    let right =
      (assume_unchecked_
         (Obj.magic_unique (L { base; off_ = off_ + i; len_ = len_ - i }))
        : slice{ now _ = drop i (now m) && fin _ = pv pr })
    in
    let a = k left right in
    exclave_
      (Obj.magic_unique
         ( (assume_unchecked_ (L { base; off_; len_ })
             : slice{ now _ = app (pv pl) (pv pr) && fin _ = fin m }),
           a ))

let split3 :
  (p1 : proph) @ unique ->
  (p2 : proph) @ unique ->
  (p3 : proph) @ unique ->
  (m : slice) @ local unique ->
  (i : int{ 0 <= _ }) ->
  (j : int{ i <= _ && _ <= len (now m) }) ->
  ((a : slice{ now _ = take i (now m) && fin _ = pv p1 }) @ local unique ->
   (b : slice{ now _ = seg i j (now m) && fin _ = pv p2 }) @ local unique ->
   (c : slice{ now _ = drop j (now m) && fin _ = pv p3 }) @ local unique ->
   'a @ unique)
    @ once local ->
  (slice{ now _ = app (pv p1) (app (pv p2) (pv p3)) && fin _ = fin m } * 'a)
    @ local unique =
  fun p1 p2 p3 m i j k ->
    let (P _) = p1 in
    let (P _) = p2 in
    let (P _) = p3 in
    let (L { base; off_; len_ }) = m in
    let a =
      (assume_unchecked_ (Obj.magic_unique (L { base; off_; len_ = i }))
        : slice{ now _ = take i (now m) && fin _ = pv p1 })
    in
    let b =
      (assume_unchecked_
         (Obj.magic_unique (L { base; off_ = off_ + i; len_ = j - i }))
        : slice{ now _ = seg i j (now m) && fin _ = pv p2 })
    in
    let c =
      (assume_unchecked_
         (Obj.magic_unique (L { base; off_ = off_ + j; len_ = len_ - j }))
        : slice{ now _ = drop j (now m) && fin _ = pv p3 })
    in
    let r = k a b c in
    exclave_
      (Obj.magic_unique
         ( (assume_unchecked_ (L { base; off_; len_ })
             : slice{ now _ = app (pv p1) (app (pv p2) (pv p3))
                      && fin _ = fin m }),
           r ))

let sdrop : (m : slice) @ local unique -> unit{ fin m = now m } =
  fun m ->
    let (L _) = m in
    assume_unchecked_ ()

let sdropa : (m : slice) @ local -> unit{ fin m = now m } =
  fun m ->
    let (L _) = m in
    assume_unchecked_ ()
