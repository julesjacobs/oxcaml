(* Inline-record constructors keep the three types boxed and modelled
   at VoxU, exactly as the interface's abstract types.  The runtime
   set is a mutable [int list] cell; its ghost [setof] is the ISet the
   list spells.  Every assume_unchecked_ asserts its signature's ghost
   facts of that reading.  The list is genuinely maintained (insert
   prepends, [smem] scans), so the assumed facts are TRUE, though the
   .ml cannot enforce them. *)

type iset [@@vox.sort lean "ISet"]
type varr = A of { mutable cell : int list }
type proph = P of { u : unit }
type slice = L of { holder : varr }

let empty : unit -> varr{ card (setof _) = 0 } @ unique =
  fun () -> assume_unchecked_ (Obj.magic_unique (A { cell = [] }))

let new_proph : unit -> proph @ unique = fun () -> Obj.magic_unique (P { u = () })

let borrow :
  (p : proph) @ unique -> (x : varr) @ unique ->
  ((m : slice{ snow _ = setof x && sfin _ = spv p }) @ local unique -> 'b @ unique)
    @ once local ->
  (varr{ setof _ = spv p } * 'b) @ unique =
  fun p x k ->
    let (P _) = p in
    let m0 =
      (assume_unchecked_ (Obj.magic_unique (L { holder = x }))
        : slice{ snow _ = setof x && sfin _ = spv p })
    in
    let b = k m0 in
    Obj.magic_unique ((assume_unchecked_ x : varr{ setof _ = spv p }), b)

let sinsert :
  (m : slice) @ local unique -> (x : int) ->
  slice{ snow _ = ins x (snow m) && sfin _ = sfin m } @ local unique =
  fun m x ->
    let (L { holder }) = m in
    let (A r) = holder in
    r.cell <- x :: r.cell;
    exclave_
      (Obj.magic_unique
         (assume_unchecked_ (L { holder })
           : slice{ snow _ = ins x (snow m) && sfin _ = sfin m }))

let smem :
  (m : slice) @ local unique -> (x : int) ->
  (bool{ _ = mem x (snow m) } * slice{ snow _ = snow m && sfin _ = sfin m })
    @ local unique =
  fun m x ->
    let (L { holder }) = m in
    let (A r) = holder in
    let b = List.mem x r.cell in
    exclave_
      (Obj.magic_unique
         ( (assume_unchecked_ b : bool{ _ = mem x (snow m) }),
           (assume_unchecked_ (L { holder })
             : slice{ snow _ = snow m && sfin _ = sfin m }) ))

let sdrop : (m : slice) @ local unique -> unit{ sfin m = snow m } =
  fun m ->
    let (L _) = m in
    assume_unchecked_ ()
