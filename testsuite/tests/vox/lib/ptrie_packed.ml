(* Implementation of ptrie_packed.mli: the compiler's Patricia
   representation (middle_end/flambda2/algorithms/patricia_tree.ml),
   packed [prefix_and_bit] included, checked against its interface's
   model.

   The bridges below are the ENTIRE trust boundary, and each one is
   the compiler's own line: [unpack] is [x land (-x)] (a NATIVE PAIR,
   specified by the [lbit] theorems), [pack] is [lor], [mask] is
   [i land -(b lsl 1)], [zero_bit] is [i land b = 0].  The one
   departure: the compiler finds the highest differing bit with a
   [clz] builtin the stdlib lacks, so [branching_bit] carries the
   same contract over a doubling loop.  Every consequence -- that
   unpack inverts pack, the prefix algebra, the invariant -- is
   PROVED in the interface; [join], [mem] and [insert] are verified
   arm by arm against the model with nothing else assumed. *)

type t =
  | Empty
  | Leaf of int
  | Branch of int * t * t

type set = t{ trie _ }

(* The compiler's unpack, verbatim -- bit and prefix from one packed
   int; the contract is the [lbit] characterization. *)
let unpack :
  (x : int{ 0 < _ }) -> (int * int){ fst _ = x - lbit x && snd _ = lbit x } =
  fun x ->
    assume_unchecked_ (let bit = x land (-x) in (x lxor bit, bit))

(* The compiler's pack, verbatim: disjointness makes [lor] addition. *)
let pack :
  (p : int{ 0 <= _ }) -> (b : int{ isbit _ && packmod p _ }) ->
  int{ _ = p + b } =
  fun p b -> assume_unchecked_ (p lor b)

let zero_bit : (i : int) -> (b : int{ isbit _ }) -> bool{ _ = zbit i b } =
  fun i b -> assume_unchecked_ (i land b = 0)

(* Keep only the bits strictly higher than [b] (the compiler's
   [mask]). *)
let mask : (i : int) -> (b : int{ isbit _ }) -> int{ _ = hmask i b } =
  fun i b -> assume_unchecked_ (i land (-(b lsl 1)))

(* Highest bit at which two distinct nonnegative prefixes differ; the
   compiler computes this from [p0 lxor p1] with a [clz] builtin, the
   toy by doubling -- the contract is the same. *)
let branching_bit :
  (p0 : int{ 0 <= _ }) -> (p1 : int{ 0 <= _ && _ <> p0 }) ->
  int{ _ = hbit p0 p1 } =
  fun p0 p1 ->
    assume_unchecked_
      (let x = p0 lxor p1 in
       let rec top b = if x land (-(b lsl 1)) = 0 then b else top (b lsl 1) in
       top 1)

(* Split two subtrees with distinct prefixes at their branching bit,
   zero side left; the packed node int is [pack]ed exactly as the
   model packs it. *)
let join :
  (p0 : int{ 0 <= _ }) -> (t0 : t) -> (p1 : int{ 0 <= _ && _ <> p0 }) ->
  (t1 : t) -> t{ _ = join p0 t0 p1 t1 } =
  fun p0 t0 p1 t1 ->
    let b = branching_bit p0 p1 in
    let p = mask p0 b in
    let x = pack p b in
    let z = zero_bit p0 b in
    if z then Branch (x, t0, t1) else Branch (x, t1, t0)

let empty : set{ _ = Empty } = Empty

(* One path decides membership in the whole tree, exactly as in the
   little-endian toy -- but here the node's prefix and bit are
   RECOVERED from the packed int, and the recovery is proved. *)
let rec mem : (i : int) -> (s : set) -> bool{ _ = mem i s } =
  fun i s ->
    match s with
    | Empty -> false
    | Leaf j -> i = j
    | Branch (x, t0, t1) ->
      let (p, b) = unpack x in
      let m = mask i b in
      if m <> p then false
      else begin
        let z = zero_bit i b in
        if z then mem i t0 else mem i t1
      end

let rec insert :
  (i : int{ 0 <= _ }) -> (s : set) -> set{ _ = insert i s && mem i _ } =
  fun i s ->
    match s with
    | Empty -> Leaf i
    | Leaf j ->
      if i = j then s
      else begin
        let l = Leaf i in
        join i l j s
      end
    | Branch (x, t0, t1) ->
      let (p, b) = unpack x in
      let m = mask i b in
      if m = p then begin
        let z = zero_bit i b in
        if z then begin
          let t0' = insert i t0 in
          Branch (x, t0', t1)
        end
        else begin
          let t1' = insert i t1 in
          Branch (x, t0, t1')
        end
      end
      else begin
        let l = Leaf i in
        join i l p s
      end
