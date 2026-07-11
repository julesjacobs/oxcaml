(* Term nodes, hash-consing, and the Env-free smart constructors (ADR-0003 Decision 5). A
   term's record is built ONLY here: it is re-exported with a [private] view by {!Term},
   so {!Context} (and every consumer) can match on it but never forge one — the smart
   constructors are the sole construction path (INVARIANTS.md I1/I2). This module is
   [private_modules]: invisible outside the core library.

   Construction is effect-free until [hashcons]: every constructor computes and
   overflow-checks its normalized node first, and only then interns it, so a raised
   [Overflow]/[Sort_error]/[Unsupported] never leaves partial state in the intern table
   (ADR-0003 Overflow/Unsupported contract; I7/I8). *)

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
  | Int_const of int
  | App of Symbol.t * t Iarr.t
  | Arith of linear
  | Le of t
  | Eq of t * t
  | Not of t
  | And of t Iarr.t
  | Or of t Iarr.t
  | Ite of t * t * t

and linear =
  { coeffs : (t * int) Iarr.t
  ; const : int
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
  | Int_const x, Int_const y -> Int.equal x y
  | App (f, xs), App (g, ys) -> Symbol.equal f g && same_children xs ys
  | Arith l1, Arith l2 ->
    Int.equal l1.const l2.const
    && Iarr.equal
         (fun (t1, c1) (t2, c2) -> t1.tag = t2.tag && Int.equal c1 c2)
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

let hash_node (n : node) : int =
  let tags acc xs = Iarr.hash_fold (fun acc x -> (acc * 31) + x.tag) acc xs in
  match n with
  | Bool_const b -> Hashtbl.hash (0, b)
  | Int_const k -> Hashtbl.hash (1, k)
  | App (s, xs) -> tags (Hashtbl.hash (2, Symbol.hash s)) xs
  | Arith l ->
    Iarr.hash_fold
      (fun acc (t, c) -> (((acc * 31) + t.tag) * 31) + c)
      (Hashtbl.hash (3, l.const))
      l.coeffs
  | Le a -> Hashtbl.hash (4, a.tag)
  | Eq (a, b) -> Hashtbl.hash (5, a.tag, b.tag)
  | Not a -> Hashtbl.hash (6, a.tag)
  | And xs -> tags (Hashtbl.hash 7) xs
  | Or xs -> tags (Hashtbl.hash 8) xs
  | Ite (a, b, c) -> Hashtbl.hash (9, a.tag, b.tag, c.tag)
;;

module Node_tbl = Hashtbl.Make (struct
    type nonrec t = node

    let equal = equal_node
    let hash = hash_node
  end)

(* Strong (non-weak) monotonic intern table + tag counter (ADR-0003 Decision 4).
   Per-Context, so two fresh Contexts building the same terms in the same order assign
   identical tags (I6). Never iterated in a way that leaks order. *)
type state =
  { mutable next_tag : int
  ; tbl : t Node_tbl.t
  }

let create_state () = { next_tag = 0; tbl = Node_tbl.create 1024 }
let term_count st = Node_tbl.length st.tbl

let hashcons st node sort =
  match Node_tbl.find_opt st.tbl node with
  | Some t -> t
  | None ->
    let t = { node; sort; tag = st.next_tag } in
    st.next_tag <- st.next_tag + 1;
    Node_tbl.add st.tbl node t;
    t
;;

(* ------------------------------------------------------------------ *)
(* Overflow-guarded integer arithmetic. Native [int] is a v1 decision; every op that can
   wrap raises [Overflow] BEFORE any interning (ADR-0003 Overflow contract). *)

let add_ovf a b =
  let s = a + b in
  if Bool.equal (a >= 0) (b >= 0) && not (Bool.equal (s >= 0) (a >= 0))
  then raise Overflow
  else s
;;

let neg_ovf a = if a = min_int then raise Overflow else -a

let mul_ovf a b =
  if a = 0 || b = 0
  then 0
  else if (a = min_int && b = -1) || (b = min_int && a = -1)
  then raise Overflow
  else (
    let p = a * b in
    if p / b <> a then raise Overflow else p)
;;

let sub_ovf a b = add_ovf a (neg_ovf b)

(* gcd, returned non-negative. min_int guard keeps it in range (TCB). *)
let rec gcd_raw a b = if b = 0 then a else gcd_raw b (a mod b)

let gcd_pos a b =
  let g = gcd_raw a b in
  if g < 0 then if g = min_int then raise Overflow else -g else g
;;

(* ceil (k / g) for g > 0, exact over native int (no overflow: q+1 only when g > 1, so q <
   k). *)
let ceil_div k g =
  let q = k / g in
  if k mod g <> 0 && k > 0 then q + 1 else q
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
  | _ -> [ t, 1 ], 0
;;

(* Merge coefficient contributions: sort by child tag, sum duplicates (overflow-checked),
   drop zeros. Result is tag-sorted, distinct, nonzero. *)
let merge_terms terms =
  let sorted = List.stable_sort (fun (a, _) (b, _) -> Int.compare a.tag b.tag) terms in
  let rec go = function
    | [] -> []
    | (t1, c1) :: (t2, c2) :: rest when t1.tag = t2.tag -> go ((t1, add_ovf c1 c2) :: rest)
    | (t, c) :: rest -> if c = 0 then go rest else (t, c) :: go rest
  in
  go sorted
;;

(* Build the canonical Int term for [Σ terms + const]: unwrap [{[]; k} -> k] and
   [{[(a,1)]; 0} -> a] (ADR-0003 Decision 5). *)
let build_linear st terms const =
  let coeffs = merge_terms terms in
  match coeffs with
  | [] -> hashcons st (Int_const const) Sort.int
  | [ (t, 1) ] when const = 0 -> t
  | _ -> hashcons st (Arith { coeffs = Iarr.of_list coeffs; const }) Sort.int
;;

let int_const st n = hashcons st (Int_const n) Sort.int
let bool_const st b = hashcons st (Bool_const b) Sort.bool

let add st a b =
  require_int "add" a;
  require_int "add" b;
  let ta, ka = terms_of a
  and tb, kb = terms_of b in
  build_linear st (ta @ tb) (add_ovf ka kb)
;;

let neg st a =
  require_int "neg" a;
  let ta, ka = terms_of a in
  build_linear st (List.map (fun (t, c) -> t, neg_ovf c) ta) (neg_ovf ka)
;;

let sub st a b =
  require_int "sub" a;
  require_int "sub" b;
  let ta, ka = terms_of a
  and tb, kb = terms_of b in
  build_linear st (ta @ List.map (fun (t, c) -> t, neg_ovf c) tb) (sub_ovf ka kb)
;;

let mul_const st c a =
  require_int "mul_const" a;
  if c = 0
  then int_const st 0
  else (
    let ta, ka = terms_of a in
    build_linear st (List.map (fun (t, ci) -> t, mul_ovf ci c) ta) (mul_ovf ka c))
;;

let linear_combination st pairs const =
  let terms, k =
    List.fold_left
      (fun (acc, k) (c, term) ->
         require_int "linear_combination" term;
         let tt, kt = terms_of term in
         ( List.rev_append (List.map (fun (t, ci) -> t, mul_ovf ci c) tt) acc
         , add_ovf k (mul_ovf kt c) ))
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
  | [] -> bool_const st (const <= 0)
  | _ ->
    let g = List.fold_left (fun g (_, c) -> gcd_pos g c) 0 coeffs in
    let coeffs' = List.map (fun (t, c) -> t, c / g) coeffs in
    let const' = ceil_div const g in
    let arg = build_linear st coeffs' const' in
    hashcons st (Le arg) Sort.bool
;;

let le st a b =
  require_int "le" a;
  require_int "le" b;
  let ta, ka = terms_of a
  and tb, kb = terms_of b in
  mk_le st (ta @ List.map (fun (t, c) -> t, neg_ovf c) tb) (sub_ovf ka kb)
;;

let lt st a b =
  require_int "lt" a;
  require_int "lt" b;
  let ta, ka = terms_of a
  and tb, kb = terms_of b in
  (* a < b <=> a - b + 1 <= 0 *)
  mk_le st (ta @ List.map (fun (t, c) -> t, neg_ovf c) tb) (add_ovf (sub_ovf ka kb) 1)
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
    | Int_const x, Int_const y -> bool_const st (Int.equal x y)
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
  let nonneg = ge st a (int_const st 0) in
  ite st nonneg a (neg st a)
;;

(* App: caller ([Context]) has already checked the rank; we intern with the codomain
   [Context] supplies. *)
let app st sym args sort = hashcons st (App (sym, Iarr.of_list args)) sort
