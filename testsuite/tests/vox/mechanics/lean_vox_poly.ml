(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* [@@vox.poly] mechanics: a parameterized abstract type sorts its
   instances at a parameterized opaque ([Vox_varr Vox_fruit] here at
   the toplevel), so ONE sort-polymorphic ghost serves every element
   type -- including facts that mention no element-sorted term (the
   length chain below), which bare-VoxU sorting cannot elaborate
   (the ghost's type argument has nothing to unify with).  Distinct
   instantiations are distinct opaque sorts: the cross-instantiation
   claim fails with a counterexample, and an element type without an
   [Inhabited] instance fails closed at elaboration. *)

type fruit = Apple | Pear of int

type veg = Leek of int (* no Inhabited instance declared below *)

type 'a varr [@@vox.poly]

[%%vox.lean {lean|
instance : Inhabited Vox_fruit := ⟨.Apple⟩

opaque pcts {a : Type} : Vox_varr a -> List a

@[grind] def plen {a : Type} : List a -> Int
  | [] => 0
  | _ :: t => 1 + plen t

@[grind] def pelem {a : Type} [Inhabited a] : List a -> Int -> a
  | [], _ => default
  | x :: t, i => if i = 0 then x else pelem t (i - 1)
|lean}]
[%%expect{|
type fruit = Apple | Pear of int
type veg = Leek of int
type 'a varr
|}]

module S : sig
  val make : (n : int{ 0 <= _ }) -> (x : 'a) -> 'a varr{ plen (pcts _) = n }
  val make1 : (x : 'a) -> 'a varr{ plen (pcts _) = 1 && pelem (pcts _) 0 = x }
  val fst_ : (m : 'a varr{ 0 < plen (pcts _) }) -> 'a{ _ = pelem (pcts m) 0 }
  val len_of : (m : 'a varr) -> int{ _ = plen (pcts m) }
end = struct
  let make : (n : int{ 0 <= _ }) -> (x : 'a) -> 'a varr{ plen (pcts _) = n } =
    fun n x -> assume_unchecked_ (Obj.magic (Array.make n x))

  let make1 : (x : 'a) -> 'a varr{ plen (pcts _) = 1 && pelem (pcts _) 0 = x } =
    fun x -> assume_unchecked_ (Obj.magic (Array.make 1 x))

  let fst_ : (m : 'a varr{ 0 < plen (pcts _) }) -> 'a{ _ = pelem (pcts m) 0 } =
    fun m -> assume_unchecked_ (Array.unsafe_get (Obj.magic m) 0)

  let len_of : (m : 'a varr) -> int{ _ = plen (pcts m) } =
    fun m -> assume_unchecked_ (Array.length (Obj.magic m))
end
[%%expect{|
module S :
  sig
    val make : (n : int{ 0 <= _ }) -> 'a -> 'a varr{ (plen (pcts _)) = n }
    val make1 :
      (x : 'a) ->
      'a varr{ ((plen (pcts _)) = 1) && ((pelem (pcts _) 0) = x) }
    val fst_ :
      (m : 'a varr{ 0 < (plen (pcts _)) }) -> 'a{ _ = (pelem (pcts m) 0) }
    val len_of : (m : 'a varr) -> int{ _ = (plen (pcts m)) }
  end
|}]

open S

(* PASS: a length-only fact -- no element-sorted term anywhere; the
   element sort rides the varr's own parameterized sort, at BOTH a
   datatype and int instantiation. *)
let lens : (n : int{ 0 <= _ }) -> int{ _ = n } =
  fun n ->
    let a = make n Apple in
    len_of a

let lens_int : (n : int{ 0 <= _ }) -> int{ _ = n } =
  fun n ->
    let a = make n 42 in
    len_of a
[%%expect{|
val lens : (n : int{ 0 <= _ }) -> int{ _ = n } = <fun>
val lens_int : (n : int{ 0 <= _ }) -> int{ _ = n } = <fun>
|}]

(* PASS: an element read at the datatype instantiation, its fact used
   through a match ([make1] carries the contents fact; [make] is
   length-only by design). *)
let through : (n : int) -> int{ _ = n } =
  fun n ->
    let p : fruit = Pear n in
    let a = make1 p in
    let q = fst_ a in
    match q with
    | Apple -> assume_ 0
    | Pear m -> refine_ m
[%%expect{|
val through : (n : int) -> int{ _ = n } = <fun>
|}]

(* FAIL, LEAN LAYER: lengths of two UNRELATED tables (distinct element
   types, distinct widths) claimed equal -- the instantiations'
   ghosts are independent, so a counterexample refutes it. *)
let bogus : unit -> unit =
  fun () ->
    let a = make 3 Apple in
    let b = make 4 42 in
    let c = len_of a in
    let d : int{ _ = plen (pcts b) } = refine_ c in
    ignore d
[%%expect{|
Line 6, characters 47-48:
6 |     let d : int{ _ = plen (pcts b) } = refine_ c in
                                                   ^
Error: vox: verification failed (lean).
       Goal: c = (plen (pcts b))
Hypotheses:
  c = (plen (pcts a))
  (plen (pcts b)) = 4
  (plen (pcts a)) = 3
Possible counterexample:
  c = 3
  plen (pcts b) = 4
  plen (pcts a) = 3
(lean: error: `grind` failed)
|}]

(* FAIL, ELABORATION: [pelem] wants [Inhabited] at the element sort
   (its out-of-range default); [veg] declares none.  The fact is only
   elaborated when a VC carries it (an unrefined result forces no
   check), so the refined goal below is what fails closed. *)
let noinst : (n : int) -> int{ 0 <= _ } =
  fun n ->
    let x : veg = Leek n in
    let a = make1 x in
    let q = fst_ a in
    match q with
    | Leek m -> refine_ (if m < 0 then 0 else m)
[%%expect{|
Line 5, characters 17-18:
5 |     let q = fst_ a in
                     ^
Error: vox: verification failed (lean).
       Goal: 0 < (plen (pcts a))
Hypotheses:
  ((plen (pcts a)) = 1) && ((pelem (pcts a) 0) = x)
  x = (Leek n)
(lean: error(lean.synthInstanceFailed): failed to synthesize instance of type class)
|}]
