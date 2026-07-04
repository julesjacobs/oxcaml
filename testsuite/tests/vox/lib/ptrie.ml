(* Implementation of ptrie.mli: the compiler's own integer-set
   structure (middle_end/flambda2/algorithms/patricia_tree.ml) as the
   little-endian toy, checked against its interface's model.

   The three primitives below are the ENTIRE trust boundary.  vox does
   not reflect the bitwise operators, so each one-liner carries its
   arithmetic model as an unchecked contract: what is trusted is only
   that the hardware tricks compute [mask]/[zbit]/[bbit] -- e.g. that
   [x land (-x)] isolates the lowest set bit, the same trick the
   compiler's [unpack] uses.  (The equations are exact on ideal
   integers; like all vox arithmetic, the 63-bit width edge -- here, a
   key pair differing only at the sign bit -- is outside the model.)
   Every consequence of those contracts, from prefix algebra to the
   invariant, is PROVED in the interface; [join], [mem] and [insert]
   are verified arm by arm against the model with nothing else
   assumed. *)

type t =
  | Empty
  | Leaf of int
  | Branch of int * int * t * t

type set = t{ trie _ }

let zero_bit (i : int) (b : int{ isbit _ }) : bool{ _ = zbit i b } =
  assume_unchecked_ (i land b = 0)

let mask (i : int) (b : int{ isbit _ }) : int{ _ = mask i b } =
  assume_unchecked_ (i land (b - 1))

let branching_bit (p0 : int) (p1 : int{ _ <> p0 }) : int{ _ = bbit p0 p1 } =
  assume_unchecked_ (let x = p0 lxor p1 in x land (-x))

(* Split two subtrees with distinct prefixes at their branching bit,
   zero side left.  The result is proved to be the model's [join];
   note [mask]'s precondition is discharged by [bbit_isbit]. *)
let join (p0 : int) (t0 : t) (p1 : int{ _ <> p0 }) (t1 : t)
  : t{ _ = join p0 t0 p1 t1 } =
  let b = branching_bit p0 p1 in
  let p = mask p0 b in
  let z = zero_bit p0 b in
  if z then Branch (p, b, t0, t1) else Branch (p, b, t1, t0)

let empty : set{ _ = Empty } = Empty

(* One path decides membership in the whole tree: a failed prefix
   test proves the key is in NEITHER subtree, and the branching bit
   proves it is not in the sibling we skip. *)
let rec mem (i : int) (s : set) : bool{ _ = mem i s } =
  match s with
  | Empty -> false
  | Leaf j -> i = j
  | Branch (p, b, t0, t1) ->
    let m = mask i b in
    if m <> p then false
    else begin
      let z = zero_bit i b in
      if z then mem i t0 else mem i t1
    end

let rec insert (i : int) (s : set) : set{ _ = insert i s && mem i _ } =
  match s with
  | Empty -> Leaf i
  | Leaf j ->
    if i = j then s
    else begin
      let l = Leaf i in
      join i l j s
    end
  | Branch (p, b, t0, t1) ->
    let m = mask i b in
    if m = p then begin
      let z = zero_bit i b in
      if z then begin
        let t0' = insert i t0 in
        Branch (p, b, t0', t1)
      end
      else begin
        let t1' = insert i t1 in
        Branch (p, b, t0, t1')
      end
    end
    else begin
      let l = Leaf i in
      join i l p s
    end
