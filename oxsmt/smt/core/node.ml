(* Term nodes, hash-consing, and the Env-free smart constructors (ADR-0003 Decision 5). A
   term's record is built ONLY here: it is re-exported with a [private] view by {!Term},
   so {!Context} (and every consumer) can match on it but never forge one — the smart
   constructors are the sole construction path (INVARIANTS.md I1/I2). This module is
   [private_modules]: invisible outside the core library.

   Construction is effect-free until [hashcons]: every constructor computes its normalized
   node first, and only then interns it, so a raised [Sort_error]/[Unsupported] never
   leaves partial state in the intern table (ADR-0003 Unsupported contract; INVARIANTS.md
   I8).

   Arithmetic is arbitrary-precision ({!Bigint}), so an integer literal or coefficient of
   any size is representable and construction never overflows. [Overflow] is retained (it
   is part of the frozen {!Term} surface and callers still catch it) but is no longer
   raised here; the residual native-[int] precision boundary moved downstream to the model
   / B&B int-projection sinks, which degrade to [unknown] via [Rational.Overflow]
   (core-bignum W2 R1) and never wrap. *)

exception Overflow
exception Sort_error of string
exception Unsupported of string

type t =
  { node : node
  ; sort : Sort.t
  ; tag : int
  }

and node =
  | Bool_const of bool
  | Int_const of Bigint.t
  | App of Symbol.t * t Iarr.t
  | Arith of linear
  | Le of t
  | Eq of t * t
  | Not of t
  | And of t Iarr.t
  | Or of t Iarr.t
  | Ite of t * t * t

and linear =
  { coeffs : (t * Bigint.t) Iarr.t
  ; const : Bigint.t
  }

(* ------------------------------------------------------------------ *)
(* Structural equality/hash for the intern bucket. Children are already canonical, so they
   are compared by [tag] (O(1), = structural equality). Every scalar payload is included
   (ADR-0003 required #8): the [bool], the [int], the [Symbol.t], and every coefficient
   AND the [const] of a [linear]. Dropping any collapses e.g. [x+2] with [x+3] — silent
   unsoundness. *)

let same_children xs ys = Iarr.equal (fun a b -> a.tag = b.tag) xs ys

let equal_node (a : node) (b : node) =
  match a, b with
  | Bool_const x, Bool_const y -> Bool.equal x y
  | Int_const x, Int_const y -> Bigint.equal x y
  | App (f, xs), App (g, ys) -> Symbol.equal f g && same_children xs ys
  | Arith l1, Arith l2 ->
    Bigint.equal l1.const l2.const
    && Iarr.equal
         (fun (t1, c1) (t2, c2) -> t1.tag = t2.tag && Bigint.equal c1 c2)
         l1.coeffs
         l2.coeffs
  | Le x, Le y -> x.tag = y.tag
  | Eq (a1, a2), Eq (b1, b2) -> a1.tag = b1.tag && a2.tag = b2.tag
  | Not x, Not y -> x.tag = y.tag
  | And xs, And ys -> same_children xs ys
  | Or xs, Or ys -> same_children xs ys
  | Ite (a1, a2, a3), Ite (b1, b2, b3) ->
    a1.tag = b1.tag && a2.tag = b2.tag && a3.tag = b3.tag
  | ( ( Bool_const _
      | Int_const _
      | App _
      | Arith _
      | Le _
      | Eq _
      | Not _
      | And _
      | Or _
      | Ite _ )
    , _ ) -> false
;;

(* Monomorphic int mixing (31-based, matching [Iarr.hash_fold] above): allocation-free and
   avoids the polymorphic [Hashtbl.hash] (caml_hash) that dominated the hash-cons profile
   on large inputs. Only the bucket choice depends on this; [equal_node] decides identity,
   so the sole invariant is [equal_node a b => hash_node a = hash_node b], which holds
   because the hash is a pure function of the same value parts [equal_node] compares (the
   constructor's discriminant seed, every child [tag], and every scalar payload). *)
let mix acc x = (acc * 31) + x

let hash_node (n : node) : int =
  let tags acc xs = Iarr.hash_fold (fun acc x -> mix acc x.tag) acc xs in
  match n with
  | Bool_const b -> mix 0 (Bool.to_int b)
  | Int_const k -> mix 1 (Bigint.hash k)
  | App (s, xs) -> tags (mix 2 (Symbol.hash s)) xs
  | Arith l ->
    Iarr.hash_fold
      (fun acc (t, c) -> mix (mix acc t.tag) (Bigint.hash c))
      (mix 3 (Bigint.hash l.const))
      l.coeffs
  | Le a -> mix 4 a.tag
  | Eq (a, b) -> mix (mix 5 a.tag) b.tag
  | Not a -> mix 6 a.tag
  | And xs -> tags 7 xs
  | Or xs -> tags 8 xs
  | Ite (a, b, c) -> mix (mix (mix 9 a.tag) b.tag) c.tag
;;

(* Strong (non-weak) monotonic intern table + tag counter (ADR-0003 Decision 4). SOUNDNESS
   SURFACE: this table IS term identity — the whole solver, certificate chain, and model
   checkers rest on "same structure <=> same tag". Aliasing two distinct nodes to one tag,
   or reusing a tag for a different node, is the worst defect class in the codebase, so
   the [equal_node] confirmation on a hash match below is load-bearing (RED: core_test's
   hash-collision fixture — two structurally-distinct nodes with equal [hash_node] must
   get DISTINCT tags; a mutant that accepts the first hash-matching slot fails it).

   Representation: a FLAT open-addressing table (design credit: thr-term-fable) instead of
   [Hashtbl.Make(node)]. The chained table stored a LIVE [Cons {key; data; next}] box per
   distinct term that persists for the Context's lifetime (promoted, hence marked every
   major GC); this stores two flat [int] arrays plus the tag-indexed [terms] we allocate
   anyway, removing ~4 live words per distinct term from the marked heap (front-end lane
   round 2: shrinks the promoted heap that drives [do_some_marking]).

   Per-Context, so two fresh Contexts building the same terms in the same order assign
   identical tags (I6): a MISS interns at [next_tag] and increments it, exactly as the old
   table did — probe order and grow are never observed (the table is never iterated for a
   result). *)
type state =
  { mutable next_tag : int
  ; mutable slot_tag : int array
    (* slot -> (tag + 1); 0 = empty (so tag 0 is representable). Length is a power of
         two. *)
  ; mutable slot_hash :
      int array (* cached [hash_node] at each slot, parallel to [slot_tag] *)
  ; terms : t Dynarray.t (* tag-indexed: [Dynarray.get terms tag] is the interned term *)
  }

(* Power of two so [h land (cap - 1)] is the bucket. *)
let initial_cap = 1024

let create_state () =
  { next_tag = 0
  ; slot_tag = Array.make initial_cap 0
  ; slot_hash = Array.make initial_cap 0
  ; terms = Dynarray.create ()
  }
;;

(* = number of distinct interned terms = [Dynarray.length st.terms]; every MISS adds one
   term and bumps [next_tag] in lockstep. *)
let term_count st = st.next_tag

(* Double the slot arrays and reinsert, re-masking from the STORED [slot_hash] (never
   recompute [hash_node] — same bucket math, no term re-walk). [terms] is tag-indexed and
   untouched, so tags are stable across a grow. *)
let grow st =
  let old_tag = st.slot_tag
  and old_hash = st.slot_hash in
  let ncap = Array.length old_tag * 2 in
  let nmask = ncap - 1 in
  let ntag = Array.make ncap 0
  and nhash = Array.make ncap 0 in
  for j = 0 to Array.length old_tag - 1 do
    let occ = old_tag.(j) in
    if occ <> 0
    then (
      let h = old_hash.(j) in
      let rec place i =
        if ntag.(i) = 0
        then (
          ntag.(i) <- occ;
          nhash.(i) <- h)
        else place ((i + 1) land nmask)
      in
      place (h land nmask))
  done;
  st.slot_tag <- ntag;
  st.slot_hash <- nhash
;;

let hashcons st node sort =
  let h = hash_node node in
  let mask = Array.length st.slot_tag - 1 in
  let rec probe i =
    let occ = st.slot_tag.(i) in
    if occ = 0
    then (
      (* MISS: intern at [next_tag], mirroring the old table's insertion-order tagging. *)
      let t = { node; sort; tag = st.next_tag } in
      Dynarray.add_last st.terms t;
      st.slot_tag.(i) <- st.next_tag + 1;
      st.slot_hash.(i) <- h;
      st.next_tag <- st.next_tag + 1;
      (* Grow at 3/4 load: open addressing degrades sharply past ~0.7, so cap the load
         factor and keep [cap] a power of two by doubling. Checked after the insert. *)
      if st.next_tag * 4 > Array.length st.slot_tag * 3 then grow st;
      t)
    else if st.slot_hash.(i) = h && equal_node (Dynarray.get st.terms (occ - 1)).node node
    then
      (* HIT: cached-hash gate means [equal_node] runs only on a full-hash match. *)
      Dynarray.get st.terms (occ - 1)
    else probe ((i + 1) land mask)
  in
  probe (h land mask)
;;

(* ------------------------------------------------------------------ *)
(* Arbitrary-precision integer arithmetic ({!Bigint}). Coefficients and constants are
   [Bigint.t], so no op wraps and none raises [Overflow] (the pre-W2 native-[int] guards
   are gone). Local aliases keep the constructor bodies readable. *)

let bzero = Bigint.zero
let bone = Bigint.one
let badd = Bigint.add
let bsub = Bigint.sub
let bmul = Bigint.mul
let bneg = Bigint.neg
let bis_zero = Bigint.is_zero

(* Exact division [a / b] when [b] divides [a] (the gcd-reduction case): the remainder is
   zero, so truncation-toward-zero is exact. *)
let bdiv_exact a b = fst (Bigint.divmod a b)

(* ceil (k / g) for g > 0. [divmod] truncates toward zero, so the remainder carries the
   sign of [k]: bump the quotient up by one exactly when [k > 0] and the division is
   inexact (remainder > 0); for [k <= 0] truncation-toward-zero already rounds up. *)
let bceil_div k g =
  let q, r = Bigint.divmod k g in
  if Bigint.sign r > 0 then badd q bone else q
;;

(* ------------------------------------------------------------------ *)
(* Sort guards. *)

let require_int what t =
  if not (Sort.equal t.sort Sort.int)
  then raise (Sort_error (Printf.sprintf "%s expects an Int operand" what))
;;

let require_bool what t =
  if not (Sort.equal t.sort Sort.bool)
  then raise (Sort_error (Printf.sprintf "%s expects a Bool operand" what))
;;

(* ------------------------------------------------------------------ *)
(* Linear form. [terms_of] reads any Int term as (coefficient list, constant), unwrapping
   [Arith] (so no coefficient term is ever itself an [Arith]) and folding [Int_const] into
   the constant. *)

let terms_of t =
  match t.node with
  | Int_const k -> [], k
  | Arith l -> Iarr.to_list l.coeffs, l.const
  | _ -> [ t, bone ], bzero
;;

(* Merge coefficient contributions: sort by child tag, sum duplicates, drop zeros. Result
   is tag-sorted, distinct, nonzero. *)
let merge_terms terms =
  let sorted = List.stable_sort (fun (a, _) (b, _) -> Int.compare a.tag b.tag) terms in
  let rec go = function
    | [] -> []
    | (t1, c1) :: (t2, c2) :: rest when t1.tag = t2.tag -> go ((t1, badd c1 c2) :: rest)
    | (t, c) :: rest -> if bis_zero c then go rest else (t, c) :: go rest
  in
  go sorted
;;

(* Build the canonical Int term for [Σ terms + const]: unwrap [{[]; k} -> k] and
   [{[(a,1)]; 0} -> a] (ADR-0003 Decision 5). *)
let build_linear st terms const =
  let coeffs = merge_terms terms in
  match coeffs with
  | [] -> hashcons st (Int_const const) Sort.int
  | [ (t, c) ] when Bigint.equal c bone && bis_zero const -> t
  | _ -> hashcons st (Arith { coeffs = Iarr.of_list coeffs; const }) Sort.int
;;

let int_const st n = hashcons st (Int_const n) Sort.int
let bool_const st b = hashcons st (Bool_const b) Sort.bool

let add st a b =
  require_int "add" a;
  require_int "add" b;
  let ta, ka = terms_of a
  and tb, kb = terms_of b in
  build_linear st (ta @ tb) (badd ka kb)
;;

let neg st a =
  require_int "neg" a;
  let ta, ka = terms_of a in
  build_linear st (List.map (fun (t, c) -> t, bneg c) ta) (bneg ka)
;;

let sub st a b =
  require_int "sub" a;
  require_int "sub" b;
  let ta, ka = terms_of a
  and tb, kb = terms_of b in
  build_linear st (ta @ List.map (fun (t, c) -> t, bneg c) tb) (bsub ka kb)
;;

let mul_const st c a =
  require_int "mul_const" a;
  if bis_zero c
  then int_const st bzero
  else (
    let ta, ka = terms_of a in
    build_linear st (List.map (fun (t, ci) -> t, bmul ci c) ta) (bmul ka c))
;;

let linear_combination st pairs const =
  let terms, k =
    List.fold_left
      (fun (acc, k) (c, term) ->
         require_int "linear_combination" term;
         let tt, kt = terms_of term in
         ( List.rev_append (List.map (fun (t, ci) -> t, bmul ci c) tt) acc
         , badd k (bmul kt c) ))
      ([], const)
      pairs
  in
  build_linear st terms k
;;

(* Order atom: build [expr <= 0] from (terms, const) already representing [expr]. Divide
   by g = gcd(|coeffs|) and ceil-tighten the constant (ADR-0003 Decision 1, required #5):
   exact over ℤ, and makes [2x<=2] and [x<=1] one node. *)
let mk_le st terms const =
  let coeffs = merge_terms terms in
  match coeffs with
  | [] -> bool_const st (Bigint.compare const bzero <= 0)
  | _ ->
    let g = List.fold_left (fun g (_, c) -> Bigint.gcd g c) bzero coeffs in
    let coeffs' = List.map (fun (t, c) -> t, bdiv_exact c g) coeffs in
    let const' = bceil_div const g in
    let arg = build_linear st coeffs' const' in
    hashcons st (Le arg) Sort.bool
;;

let le st a b =
  require_int "le" a;
  require_int "le" b;
  let ta, ka = terms_of a
  and tb, kb = terms_of b in
  mk_le st (ta @ List.map (fun (t, c) -> t, bneg c) tb) (bsub ka kb)
;;

let lt st a b =
  require_int "lt" a;
  require_int "lt" b;
  let ta, ka = terms_of a
  and tb, kb = terms_of b in
  (* a < b <=> a - b + 1 <= 0 *)
  mk_le st (ta @ List.map (fun (t, c) -> t, bneg c) tb) (badd (bsub ka kb) bone)
;;

let ge st a b = le st b a
let gt st a b = lt st b a

let eq st a b =
  if not (Sort.equal a.sort b.sort)
  then raise (Sort_error "eq expects operands of the same sort");
  if a.tag = b.tag
  then bool_const st true
  else (
    match a.node, b.node with
    | Int_const x, Int_const y -> bool_const st (Bigint.equal x y)
    | Bool_const x, Bool_const y -> bool_const st (Bool.equal x y)
    | _ ->
      let lo, hi = if a.tag < b.tag then a, b else b, a in
      hashcons st (Eq (lo, hi)) Sort.bool)
;;

let not_ st a =
  require_bool "not" a;
  match a.node with
  | Not b -> b
  | Bool_const v -> bool_const st (not v)
  | _ -> hashcons st (Not a) Sort.bool
;;

(* n-ary And/Or: flatten same connective, constant-fold, dedup + tag-sort.
   [absorbing]/[identity] are the short-circuit and drop values. *)
let nary st ~is_and children =
  let identity = is_and (* And drops true, Or drops false *)
  and absorbing = not is_and in
  List.iter (require_bool (if is_and then "and" else "or")) children;
  let exception Short in
  let flat = ref [] in
  let rec push t =
    match t.node with
    | And xs when is_and -> Iarr.iter push xs
    | Or xs when not is_and -> Iarr.iter push xs
    | Bool_const v ->
      if Bool.equal v absorbing then raise Short else () (* drop identity *)
    | _ -> flat := t :: !flat
  in
  match List.iter push children with
  | exception Short -> bool_const st absorbing
  | () ->
    let sorted = List.sort_uniq (fun a b -> Int.compare a.tag b.tag) (List.rev !flat) in
    (match sorted with
     | [] -> bool_const st identity
     | [ x ] -> x
     | _ ->
       let node =
         if is_and then And (Iarr.of_list sorted) else Or (Iarr.of_list sorted)
       in
       hashcons st node Sort.bool)
;;

let and_ st children = nary st ~is_and:true children
let or_ st children = nary st ~is_and:false children
let implies st a b = or_ st [ not_ st a; b ]

(* iff / xor reuse Eq/Not over Bool-sorted args (ADR-0003 Decision 2). *)
let iff st a b = eq st a b

let ite st c a b =
  require_bool "ite" c;
  if not (Sort.equal a.sort b.sort)
  then raise (Sort_error "ite branches must share a sort");
  match c.node with
  | Bool_const true -> a
  | Bool_const false -> b
  | _ -> if a.tag = b.tag then a else hashcons st (Ite (c, a, b)) a.sort
;;

let distinct st ts =
  match ts with
  | [] | [ _ ] -> bool_const st true
  | _ ->
    let rec pairs = function
      | [] | [ _ ] -> []
      | x :: rest -> List.map (fun y -> not_ st (eq st x y)) rest @ pairs rest
    in
    and_ st (pairs ts)
;;

let abs st a =
  require_int "abs" a;
  (* [abs x = ite (x >= 0) x (-x)]. Arbitrary-precision arithmetic never overflows, so the
     old min_int-magnitude raise-before-intern ordering (codex C3) is moot; the two
     branches are interned unconditionally. *)
  let neg_a = neg st a in
  let nonneg = ge st a (int_const st bzero) in
  ite st nonneg a neg_a
;;

(* App: caller ([Context]) has already checked the rank; we intern with the codomain
   [Context] supplies. *)
let app st sym args sort = hashcons st (App (sym, Iarr.of_list args)) sort
