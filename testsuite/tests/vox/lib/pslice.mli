(* POLYMORPHIC mutable-array slices as RustHorn-style borrows: ONE
   trusted borrow library for EVERY element type, where slice_lib
   (int) and bslice (Htbl.bucket) were monomorphic twins.  The three
   carrier types are declared [@@vox.poly], so a value of ['a varr]
   is modelled at the PARAMETERIZED opaque sort [Vox_Pslice_varr a] --
   its element sort rides the value itself, and the sort-polymorphic
   ghosts and list theory below elaborate at every instantiation,
   including facts (lengths, ghost-to-ghost equations like [borrow]'s)
   that mention no element-sorted term.  Distinct instantiations can
   never exchange facts: their sorts are distinct opaque types.

   The list theory ([plen]/[pelem]/[pupd]/[pconst] and lemmas) is
   PROVED once, polymorphically, and specializes to every element
   type by parametricity.  [pelem] requires an [Inhabited] instance
   at the element sort (its out-of-range default): Int/Prop have one;
   a client instantiating at its own datatype declares one in its
   block ([instance : Inhabited Vox_U_t := ⟨.C⟩]).

   The prophecy discipline is slice_lib's ([@ local unique] loans,
   now/fin, resolution by [sdrop]).  A read hands the element back
   [local] to the bracket; [gbl] globalizes one -- sound for values
   that live in the (global) array, the only place a loan's reads
   come from.  Only the ops a hash table needs are provided: no
   [split], no parallelism.

   TRUSTED: [varr]/[proph]/[slice] are abstract and boxed; every
   [assume_unchecked_] in the implementation asserts its signature's
   ghost facts hold of the real array, at every instantiation.  This
   is the whole trust boundary (seven functions); every client fact
   is PROVED from these signatures and the theory below. *)

type 'a varr [@@vox.poly]
type 'a proph [@@vox.poly]
type 'a slice [@@vox.poly]

[%%vox.lean {lean|
-- ghosts: an owned array denotes its contents [pcts]; a live loan
-- denotes current/prophesied-final contents [pnow]/[pfin]; a
-- prophecy denotes the sequence it resolves to [ppv].  All are
-- lists at the value's own element sort.
public opaque pcts {a : Type} : Vox_Pslice_varr a -> List a
public opaque pnow {a : Type} : Vox_Pslice_slice a -> List a
public opaque pfin {a : Type} : Vox_Pslice_slice a -> List a
public opaque ppv {a : Type} : Vox_Pslice_proph a -> List a

-- The generic list theory, structural like slice_lib's so every
-- lemma is a uniform induction.
@[grind, expose] public def plen {a : Type} : List a -> Int
  | [] => 0
  | _ :: t => 1 + plen t

@[grind, expose] public def pelem {a : Type} [Inhabited a] : List a -> Int -> a
  | [], _ => default
  | x :: t, i => if i = 0 then x else pelem t (i - 1)

@[grind, expose] public def pupd {a : Type} : List a -> Int -> a -> List a
  | [], _, _ => []
  | x :: t, i, v => if i = 0 then v :: t else x :: pupd t (i - 1) v

-- Every element is [x] (the ghost of a fresh constant array; kept
-- structural so no well-founded recursion enters the theory).
@[grind, expose] public def pconst {a : Type} : List a -> a -> Prop
  | [], _ => True
  | y :: t, x => y = x ∧ pconst t x

public theorem plen_nonneg {a : Type} (l : List a) : 0 <= plen l := by
  induction l <;> grind
grind_pattern plen_nonneg => plen l

public theorem plen_pupd {a : Type} (l : List a) (i : Int) (v : a) :
    plen (pupd l i v) = plen l := by
  induction l generalizing i <;> grind
grind_pattern plen_pupd => plen (pupd l i v)

public theorem pelem_pupd {a : Type} [Inhabited a] (l : List a) (i j : Int) (v : a)
    (h1 : 0 <= i) (h2 : i < plen l) :
    pelem (pupd l i v) j = if j = i then v else pelem l j := by
  induction l generalizing i j <;> grind
grind_pattern pelem_pupd => pelem (pupd l i v) j

public theorem pelem_pconst {a : Type} [Inhabited a] (l : List a) (x : a) (i : Int)
    (hc : pconst l x) (h1 : 0 <= i) (h2 : i < plen l) : pelem l i = x := by
  induction l generalizing i <;> grind
grind_pattern pelem_pconst => pelem l i, pconst l x

public theorem pconst_pupd {a : Type} (l : List a) (i : Int) (x : a)
    (hc : pconst l x) : pconst (pupd l i x) x := by
  induction l generalizing i <;> grind
grind_pattern pconst_pupd => pconst (pupd l i x) x
|lean}]

(* Allocate [n] copies of [x]. *)
val pnew :
  (n : int{ 0 <= _ }) -> (x : 'a) ->
  'a varr{ plen (pcts _) = n && pconst (pcts _) x } @ unique

val new_proph : unit -> 'a proph @ unique

(* Open a borrow bracket: the continuation gets the root loan over the
   whole array; the residual comes back at the prophesied contents. *)
val borrow :
  (p : 'a proph) @ unique -> (x : 'a varr) @ unique ->
  ((m : 'a slice{ pnow _ = pcts x && pfin _ = ppv p }) @ local unique -> 'b @ unique)
    @ once local ->
  ('a varr{ pcts _ = ppv p } * 'b) @ unique

(* Read a cell (the loan is threaded, so the read element comes back
   [local] to the bracket). *)
val sget :
  (m : 'a slice) @ local unique -> (i : int{ 0 <= _ && _ < plen (pnow m) }) ->
  ('a{ _ = pelem (pnow m) i } * 'a slice{ pnow _ = pnow m && pfin _ = pfin m })
    @ local unique

(* Globalize an element read from a loan: sound because the value
   lives in the globally allocated array and cells are only replaced,
   never mutated in place.  TRUSTED. *)
val gbl : (x : 'a) @ local -> 'a{ _ = x }

(* Write a cell: consumes the loan, re-issues it advanced. *)
val sset :
  (m : 'a slice) @ local unique -> (i : int{ 0 <= _ && _ < plen (pnow m) }) ->
  (x : 'a) ->
  'a slice{ pnow _ = pupd (pnow m) i x && pfin _ = pfin m } @ local unique

val sdrop : (m : 'a slice) @ local unique -> unit{ pfin m = pnow m }
