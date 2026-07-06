(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* Borrow soundness probes: the attacks that must NOT verify, each
   pinned to its rejection layer.  The API is ascribed inline (same
   signature as demo/borrow_lib, which the positive test
   demo/lean_borrow.ml exercises).  Lean rejects forged exports and
   claims about unresolved prophecies; the uniqueness mode checker
   rejects loan reuse, prophecy reuse, and writes through frozen
   loans; the locality checker rejects loan escape. *)

[%%vox.lean {lean|
opaque now : VoxU -> Int
opaque fin : VoxU -> Int
|lean}]
[%%expect{|
|}]

module B : sig
  type vref [@@vox.sort int]
  type proph [@@vox.sort int]
  type mut

  val vnew : (v : int) -> vref{ _ = v } @ unique
  val vpeek : (x : vref) -> int{ _ = x }
  val new_proph : unit -> proph @ unique

  val borrow_mut :
    (p : proph) @ unique -> (x : vref) @ unique ->
    ((m : mut{ now _ = x && fin _ = p }) @ local unique -> 'b @ unique)
      @ once local ->
    (vref{ _ = p } * 'b) @ unique

  val mpeek : (m : mut) @ local -> int{ _ = now m }

  val mset :
    (m : mut) @ local unique -> (w : int) ->
    mut{ now _ = w && fin _ = fin m } @ local unique

  val mdrop : (m : mut) @ local unique -> unit{ fin m = now m }
  val mdropa : (m : mut) @ local -> unit{ fin m = now m }
end = struct
  type vref = { mutable c : int } [@@vox.sort int]
  type proph = P of { u : unit } [@@vox.sort int]
  type mut = M of { global_ r : vref }

  let vnew : (v : int) -> vref{ _ = v } @ unique =
    fun v -> assume_unchecked_ (Obj.magic_unique { c = v })

  let vpeek : (x : vref) -> int{ _ = x } = fun x -> assume_unchecked_ x.c

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
      let b = k m0 in
      Obj.magic_unique
        ((assume_unchecked_ (Obj.magic x) : vref{ _ = p }), b)

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
           (assume_unchecked_ (M { r })
             : mut{ now _ = w && fin _ = fin m }))

  let mdrop : (m : mut) @ local unique -> unit{ fin m = now m } =
    fun m ->
      let (M _) = m in
      assume_unchecked_ ()

  let mdropa : (m : mut) @ local -> unit{ fin m = now m } =
    fun m ->
      let (M _) = m in
      assume_unchecked_ ()
end
[%%expect{|
module B :
  sig
    type vref
    type proph
    type mut
    val vnew : (v : int) -> vref{ _ = v } @ unique
    val vpeek : (x : vref) -> int{ _ = x }
    val new_proph : unit -> proph @ unique
    val borrow_mut :
      (p : proph) @ unique ->
      (x : vref) @ unique ->
      (mut{ now _ = x && fin _ = p } @ local unique -> 'b @ unique) @ local
      once -> vref{ _ = p } * 'b @ unique
    val mpeek : (m : mut) @ local -> int{ _ = now m }
    val mset :
      (m : mut) @ local unique ->
      (w : int) -> mut{ now _ = w && fin _ = fin m } @ local unique
    val mdrop : (m : mut) @ local unique -> unit{ fin m = now m }
    val mdropa : (m : mut) @ local -> unit{ fin m = now m }
  end
|}]

open B
[%%expect{|
|}]

(* A false export about the prophecy: the borrower set a + 1 but
   claims a + 2.  Lean rejects. *)
let lie : (a : int) -> int =
  fun a ->
  let x = vnew a in
  let p = new_proph () in
  let (x', u) = borrow_mut p x (fun m ->
    let m1 = mset m (a + 1) in
    let _u0 = mdrop m1 in
    (() : unit{ p = a + 2 }))
  in
  ignore x'; ignore u; 0
[%%expect{|
Line 8, characters 5-7:
8 |     (() : unit{ p = a + 2 }))
         ^^
Error: vox: verification failed (lean).
       Goal: p = a + 2
Hypotheses:
  fin m1 = now m1
  now m1 = a + 1 && fin m1 = fin m
  now m = x && fin m = p
  x = a
Possible counterexample:
  a = -1
  p = 0
  x = -1
  fin m1 = 0
  fin m = 0
  now m1 = 0
  now m = -1
(lean: error: `grind` failed)
|}]

(* An UNDROPPED borrow is a sound leak: the prophecy stays opaque, so
   nothing concrete about the residual is provable. *)
let leak : (a : int) -> int =
  fun a ->
  let x = vnew a in
  let p = new_proph () in
  let (x', u) = borrow_mut p x (fun m ->
    let m1 = mset m 0 in
    let _ = m1 in
    (() : unit{ 1 = 1 }))
  in
  ignore u;
  let (_ : unit{ x' = a }) = () in
  ignore x'; 0
[%%expect{|
Line 11, characters 29-31:
11 |   let (_ : unit{ x' = a }) = () in
                                  ^^
Error: vox: verification failed (lean).
       Goal: x' = a
Hypotheses:
  x' = fst *unknown18*
  u = snd *unknown18*
  x' = p
  1 = 1
  x = a
(lean: error: `grind` failed)
|}]

(* Writing through a loan after an ALIASED use: any aliased use
   permanently destroys uniqueness, so the frozen snapshot can never
   go stale.  Mode-rejected. *)
let freeze : (a : int) -> int =
  fun a ->
  let x = vnew a in
  let p = new_proph () in
  let (x', u) = borrow_mut p x (fun m ->
    let _r = mpeek m in
    let m1 = mset m 0 in
    let _u0 = mdropa m1 in
    (() : unit{ 1 = 1 }))
  in
  ignore x'; ignore u; 0
[%%expect{|
Line 7, characters 18-19:
7 |     let m1 = mset m 0 in
                      ^
Error: This value is used here as unique, but it has already been used at:
Line 6, characters 19-20:
6 |     let _r = mpeek m in
                       ^

|}]

(* Using a loan twice for writes: mode-rejected. *)
let dup : (a : int) -> int =
  fun a ->
  let x = vnew a in
  let p = new_proph () in
  let (x', u) = borrow_mut p x (fun m ->
    let m1 = mset m 1 in
    let m2 = mset m 2 in
    let _ = m1 in
    let _ = m2 in
    (() : unit{ 1 = 1 }))
  in
  ignore x'; ignore u; 0
[%%expect{|
Line 7, characters 18-19:
7 |     let m2 = mset m 2 in
                      ^
Error: This value is used here, but it has already been used as unique at:
Line 6, characters 18-19:
6 |     let m1 = mset m 1 in
                      ^

|}]

(* One prophecy for two borrows would allow two resolutions (and
   pv p = w1 = w2 proves False for w1 <> w2): mode-rejected at the
   second consumption. *)
let reuse : (a : int) -> int =
  fun a ->
  let x = vnew a in
  let y = vnew a in
  let p = new_proph () in
  let (x', ux) = borrow_mut p x (fun m ->
    let _ = m in
    (() : unit{ 1 = 1 }))
  in
  let (y', uy) = borrow_mut p y (fun m ->
    let _ = m in
    (() : unit{ 1 = 1 }))
  in
  ignore x'; ignore ux; ignore y'; ignore uy; 0
[%%expect{|
Line 10, characters 28-29:
10 |   let (y', uy) = borrow_mut p y (fun m ->
                                 ^
Error: This value is used here, but it has already been used as unique at:
Line 6, characters 28-29:
6 |   let (x', ux) = borrow_mut p x (fun m ->
                                ^

|}]

(* Stashing the loan in a global ref would let it outlive the borrow:
   locality-rejected. *)
let stash : (a : int) -> int =
  fun a ->
  let cell = ref None in
  let x = vnew a in
  let p = new_proph () in
  let (x', u) = borrow_mut p x (fun m ->
    cell := Some m;
    (() : unit{ 1 = 1 }))
  in
  ignore x'; ignore u; 0
[%%expect{|
Line 7, characters 17-18:
7 |     cell := Some m;
                     ^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is contained (via constructor "Some") in the value at line 7, characters 12-18
         which is expected to be "global".
|}]
