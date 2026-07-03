(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Block-index borrows: borrow_elem lends ONE SLOT of a linear int
   array.  The loan is the SAME mut as scalar borrows (at runtime it
   holds block + index); the residual array's spec pins the borrowed
   slot to the prophecy and FRAMES the rest with a quantifier. *)

[%%vox.lean {lean|
opaque now : VoxU -> Int
opaque fin : VoxU -> Int
opaque len : VoxU -> Int
opaque elem : VoxU -> Int -> Int
|lean}]

module A : sig
  type varr
  type proph [@@vox.sort int]
  type mut

  val anew :
    (n : int) -> (v : int) ->
    varr{ len _ = n && (forall_ i. 0 <= i && i < n -> elem _ i = v) }
      @ unique

  val apeek :
    (a : varr) -> (j : int{ 0 <= _ && _ < len a }) -> int{ _ = elem a j }

  val aset :
    (a : varr) @ unique -> (j : int{ 0 <= _ && _ < len a }) -> (w : int) ->
    varr{ len _ = len a && elem _ j = w
       && (forall_ i. not (i = j) -> elem _ i = elem a i) } @ unique

  val new_proph : unit -> proph @ unique

  val borrow_elem :
    (p : proph) @ unique -> (a : varr) @ unique ->
    (j : int{ 0 <= _ && _ < len a }) ->
    ((m : mut{ now _ = elem a j && fin _ = p }) @ local unique
      -> 'b @ unique) @ once local ->
    (varr{ len _ = len a && elem _ j = p
        && (forall_ i. not (i = j) -> elem _ i = elem a i) } * 'b)
      @ unique

  val mget :
    (m : mut) @ local unique ->
    (int{ _ = now m } * mut{ now _ = now m && fin _ = fin m })
      @ local unique
  val mset :
    (m : mut) @ local unique -> (w : int) ->
    mut{ now _ = w && fin _ = fin m } @ local unique
  val mdrop : (m : mut) @ local unique -> unit{ fin m = now m }
end = struct
  type varr = { mutable arr : int array }
  type proph = P of { u : unit } [@@vox.sort int]
  type mut = M of { global_ blk : int array; global_ idx : int }

  let anew :
    (n : int) -> (v : int) ->
    varr{ len _ = n && (forall_ i. 0 <= i && i < n -> elem _ i = v) }
      @ unique =
    fun n v -> assume_unchecked_ (Obj.magic_unique { arr = Array.make n v })

  let apeek :
    (a : varr) -> (j : int{ 0 <= _ && _ < len a }) -> int{ _ = elem a j } =
    fun a j -> assume_unchecked_ a.arr.(j)

  let aset :
    (a : varr) @ unique -> (j : int{ 0 <= _ && _ < len a }) -> (w : int) ->
    varr{ len _ = len a && elem _ j = w
       && (forall_ i. not (i = j) -> elem _ i = elem a i) } @ unique =
    fun a j w ->
      a.arr.(j) <- w;
      assume_unchecked_ (Obj.magic_unique (Obj.magic a))

  let new_proph : unit -> proph @ unique =
    fun () -> Obj.magic_unique (P { u = () })

  let borrow_elem :
    (p : proph) @ unique -> (a : varr) @ unique ->
    (j : int{ 0 <= _ && _ < len a }) ->
    ((m : mut{ now _ = elem a j && fin _ = p }) @ local unique
      -> 'b @ unique) @ once local ->
    (varr{ len _ = len a && elem _ j = p
        && (forall_ i. not (i = j) -> elem _ i = elem a i) } * 'b)
      @ unique =
    fun p a j k ->
      let (P _) = p in
      let m0 =
        (assume_unchecked_ (Obj.magic_unique (M { blk = a.arr; idx = j }))
          : mut{ now _ = elem a j && fin _ = p })
      in
      let b = k m0 in
      Obj.magic_unique
        ((assume_unchecked_ (Obj.magic a)
           : varr{ len _ = len a && elem _ j = p
                && (forall_ i. not (i = j) -> elem _ i = elem a i) }),
         b)

  let mget :
    (m : mut) @ local unique ->
    (int{ _ = now m } * mut{ now _ = now m && fin _ = fin m })
      @ local unique =
    fun m ->
      let (M { blk; idx }) = m in
      let c = blk.(idx) in
      exclave_
        (Obj.magic_unique
           ((assume_unchecked_ c : int{ _ = now m }),
            (assume_unchecked_ (M { blk; idx })
              : mut{ now _ = now m && fin _ = fin m })))

  let mset :
    (m : mut) @ local unique -> (w : int) ->
    mut{ now _ = w && fin _ = fin m } @ local unique =
    fun m w ->
      let (M { blk; idx }) = m in
      blk.(idx) <- w;
      exclave_
        (Obj.magic_unique
           (assume_unchecked_ (M { blk; idx })
             : mut{ now _ = w && fin _ = fin m }))

  let mdrop : (m : mut) @ local unique -> unit{ fin m = now m } =
    fun m ->
      let (M _) = m in
      assume_unchecked_ ()
end

open A

(* Borrow slot 1 of [0;0;0], bump it through the loan to 7, drop;
   then read BOTH the borrowed slot (revealed by the resolution) and
   an untouched slot (the FRAME, a forall_ fact instantiated at 0). *)
let demo : unit -> int{ _ = 7 } =
  fun () ->
  let a = anew 3 0 in
  let p = new_proph () in
  let (a', u) = borrow_elem p a 1 (fun m ->
    let (r, m1) = mget m in              (* r = elem a 1 = 0 *)
    let m2 = mset m1 (r + 7) in
    let _u0 = mdrop m2 in
    (() : unit{ p = 7 }))
  in
  ignore u;
  let s1 = apeek a' 1 in                 (* = elem a' 1 = p = 7 *)
  let s0 = apeek a' 0 in                 (* = elem a' 0 = elem a 0 = 0 *)
  let r = s1 + s0 in
  (r : int{ _ = 7 })
