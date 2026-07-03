type vref = { mutable c : int } [@@vox.sort int]
type proph = P of { u : unit } [@@vox.sort int]
type mut = M of { global_ r : vref }

let vnew : (v : int) -> vref{ _ = v } @ unique =
  fun v -> assume_unchecked_ (Obj.magic_unique { c = v })

let vread : (x : vref) @ unique -> (int{ _ = x } * vref{ _ = x }) @ unique =
  fun x ->
    let c = x.c in
    Obj.magic_unique
      ((assume_unchecked_ c : int{ _ = x }),
       (assume_unchecked_ x : vref{ _ = x }))

let vpeek : (x : vref) -> int{ _ = x } = fun x -> assume_unchecked_ x.c

let vset : (x : vref) @ unique -> (w : int) -> vref{ _ = w } @ unique =
  (* Obj.magic is an identity retype: same box, new refined view *)
  fun x w -> x.c <- w; assume_unchecked_ (Obj.magic_unique (Obj.magic x))

let new_proph : unit -> proph @ unique =
  fun () -> Obj.magic_unique (P { u = () })

let borrow_mut :
  (p : proph) @ unique -> (x : vref) @ unique ->
  ((m : mut{ now _ = x && fin _ = p }) @ local unique -> 'b @ unique)
    @ once local ->
  (vref{ _ = p } * 'b) @ unique =
  fun p x k ->
    let (P _) = p in
    let m0 =
      (assume_unchecked_ (Obj.magic_unique (M { r = x }))
        : mut{ now _ = x && fin _ = p })
    in
    (* the plain let binds m0 at the skeleton with its facts, so it
       passes the continuation's contract directly *)
    let b = k m0 in
    (* identity retype: the residual IS x, now viewed at the
       prophesied contents *)
    Obj.magic_unique ((assume_unchecked_ (Obj.magic x) : vref{ _ = p }), b)

let mget :
  (m : mut) @ local unique ->
  (int{ _ = now m } * mut{ now _ = now m && fin _ = fin m }) @ local unique =
  fun m ->
    let (M { r }) = m in
    let c = r.c in
    exclave_
      (Obj.magic_unique
         ((assume_unchecked_ c : int{ _ = now m }),
          (assume_unchecked_ (M { r })
            : mut{ now _ = now m && fin _ = fin m })))

let mpeek : (m : mut) @ local -> int{ _ = now m } =
  fun m ->
    let (M { r }) = m in
    assume_unchecked_ r.c

let mset :
  (m : mut) @ local unique -> (w : int) ->
  mut{ now _ = w && fin _ = fin m } @ local unique =
  fun m w ->
    let (M { r }) = m in
    r.c <- w;
    exclave_
      (Obj.magic_unique
         (assume_unchecked_ (M { r }) : mut{ now _ = w && fin _ = fin m }))

let mdrop : (m : mut) @ local unique -> unit{ fin m = now m } =
  fun m ->
    let (M _) = m in
    assume_unchecked_ ()

let mdropa : (m : mut) @ local -> unit{ fin m = now m } =
  fun m ->
    let (M _) = m in
    assume_unchecked_ ()
