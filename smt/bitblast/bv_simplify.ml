open Oxsmt_core

(* Per-call memo: term -> its simplified form. Keyed by the hash-consed term, so it is
   created fresh inside [simplify] and never shared across calls/sessions (term tags are
   per-Context hash-cons ids that collide across contexts). *)

let pow2 w =
  let two = Bigint.of_int 2 in
  let rec go acc i = if i <= 0 then acc else go (Bigint.mul acc two) (i - 1) in
  go Bigint.one w
;;

(* reduce [v] (possibly negative) into the canonical residue [0, 2^w). *)
let reduce v ~modulus =
  let r = snd (Bigint.divmod v modulus) in
  if Bigint.sign r < 0 then Bigint.add r modulus else r
;;

let width_of (t : Term.t) =
  match Bv.width_of_sort t.sort with
  | Some w -> w
  | None -> -1
;;

(* A linear combination over the additive group of [BitVec w] (which is Z/2^w under
   [bvadd]/[bvsub]/[bvneg]): a constant plus a coefficient per distinct "atom" (a maximal
   non-additive bitvector subterm, itself already simplified). Sound because [bvadd] is
   associative + commutative and [bvsub]/[bvneg] are the group inverse, all mod 2^w, and a
   coefficient [k] is [k]-fold addition = multiplication by the constant [k] mod 2^w. *)

(* Depth guard on flattening: bounds recursion (stack safety on a single-use megachain)
   and, with the [shared] gate, bounds rebuild cost. Beyond it a sub-node becomes an atom
   (still simplified, just not inlined) — harmless, only less flattening. *)
let max_flatten_depth = 64

(* [lin_of] flattens an additive expression, but STOPS at any additive sub-node that is
   [shared] (referenced by more than one parent in the asserted DAG), treating it as an
   atom. This is the load-bearing guard: the DAG blaster encodes a shared node once, so
   inlining it into every use would re-expand it — e.g. a sequential accumulator chain
   [x1, x1+x2, x1+x2+x3, ...] (each prefix shared) would go from O(n) shared adds to
   O(n^2). Gating on sharing preserves the blaster's structure while still normalizing the
   UNSHARED, typically shallow, cross-term patterns that expose a common subterm. *)
let neg_one = Bigint.of_int (-1)

let lin_of ~simp ~shared (t : Term.t) =
  let w = width_of t in
  (* tag -> (atom term, running coefficient as a signed Bigint, reduced only at rebuild) *)
  let atoms : (int, Term.t * Bigint.t) Hashtbl.t = Hashtbl.create 16 in
  let const = ref Bigint.zero in
  let add_atom atom (sign : Bigint.t) =
    let tag = atom.Term.tag in
    let prev =
      match Hashtbl.find_opt atoms tag with
      | Some (_, c) -> c
      | None -> Bigint.zero
    in
    Hashtbl.replace atoms tag (atom, Bigint.add prev sign)
  in
  let rec go depth (sign : Bigint.t) (t : Term.t) =
    (* descend into an additive child only if it is unshared and within the depth budget;
       otherwise it is an atom (simplified standalone, encoded once by the blaster). *)
    let into child (sgn : Bigint.t) =
      if depth < max_flatten_depth && not (shared child)
      then go (depth + 1) sgn child
      else add_atom (simp child) sgn
    in
    match Bv.view t with
    | Some (Bv.Const { value; width = _ }) ->
      const := Bigint.add !const (Bigint.mul sign value)
    | Some (Bv.Op { op = Bv.Bvadd; args = [ a; b ]; _ }) ->
      into a sign;
      into b sign
    | Some (Bv.Op { op = Bv.Bvsub; args = [ a; b ]; _ }) ->
      into a sign;
      into b (Bigint.mul sign neg_one)
    | Some (Bv.Op { op = Bv.Bvneg; args = [ a ]; _ }) -> into a (Bigint.mul sign neg_one)
    | _ -> add_atom (simp t) sign
  in
  go 0 Bigint.one t;
  w, !const, atoms
;;

let rebuild ctx mint (w, const, atoms) =
  let modulus = pow2 w in
  let contribs =
    Hashtbl.fold
      (fun tag (atom, coeff) acc ->
         let c = reduce coeff ~modulus in
         if Bigint.is_zero c then acc else (tag, atom, c) :: acc)
      atoms
      []
  in
  (* canonical, deterministic order by hash-cons tag *)
  let contribs = List.sort (fun (t1, _, _) (t2, _, _) -> Int.compare t1 t2) contribs in
  let mk atom c =
    if Bigint.equal c Bigint.one
    then atom
    else if Bigint.equal c (Bigint.sub modulus Bigint.one)
    then Bv.unop ctx mint Bv.Bvneg atom
    else
      (* k*atom = multiplication by the constant [k]; [k] is the SECOND operand so the
         blaster's shift-add multiplier folds the (constant) multiplier's bits. *)
      Bv.binop ctx mint Bv.Bvmul atom (Bv.const ctx mint ~value:c ~width:w)
  in
  let terms = List.map (fun (_, atom, c) -> mk atom c) contribs in
  let rc = reduce const ~modulus in
  let terms =
    if Bigint.is_zero rc && terms <> []
    then terms
    else terms @ [ Bv.const ctx mint ~value:rc ~width:w ]
  in
  match terms with
  | [] -> Bv.const ctx mint ~value:Bigint.zero ~width:w
  | first :: rest ->
    List.fold_left (fun acc x -> Bv.binop ctx mint Bv.Bvadd acc x) first rest
;;

(* [simplify ctx mint terms] rewrites each asserted term into an equivalent one, exposing
   shared additive structure. The ONLY value-changing transform is additive normalization
   (constant folding + cross-term cancellation + shared-subterm exposure over the mod-2^w
   group); every other node is rebuilt with simplified children (re-applying the same
   operator symbol), so operator semantics are untouched. Equivalence is the
   responsibility of the exhaustive small-width oracle + the symbolic-equivalence goldens:
   the bv model re-check validates the REWRITTEN formula, so it does not by itself catch
   an unsound rewrite. *)
let children (t : Term.t) : Term.t list =
  match t.Term.node with
  | Not a | Le a -> [ a ]
  | And xs | Or xs -> Iarr.to_list xs
  | Ite (c, a, b) -> [ c; a; b ]
  | Eq (a, b) -> [ a; b ]
  | App (_, args) -> Iarr.to_list args
  | Bool_const _ | Int_const _ | Arith _ -> []
;;

(* Occurrence count over the asserted DAG: for each node, how many parent edges point to
   it (counting multiplicity). A node with more than one is SHARED — the blaster encodes
   it once, so [lin_of] must not inline (re-expand) it. Each node's children are counted
   once, the DAG walked once. *)
let occurrences (terms : Term.t list) : (int, int) Hashtbl.t =
  let count : (int, int) Hashtbl.t = Hashtbl.create 1024 in
  let visited : (int, unit) Hashtbl.t = Hashtbl.create 1024 in
  let bump (t : Term.t) =
    Hashtbl.replace
      count
      t.Term.tag
      (1
       +
       match Hashtbl.find_opt count t.Term.tag with
       | Some n -> n
       | None -> 0)
  in
  let rec walk (t : Term.t) =
    if not (Hashtbl.mem visited t.Term.tag)
    then (
      Hashtbl.add visited t.Term.tag ();
      List.iter
        (fun c ->
           bump c;
           walk c)
        (children t))
  in
  List.iter walk terms;
  count
;;

let simplify ctx mint (terms : Term.t list) : Term.t list =
  let count = occurrences terms in
  let shared (t : Term.t) =
    match Hashtbl.find_opt count t.Term.tag with
    | Some n -> n > 1
    | None -> false
  in
  let memo : Term.t Term.Table.t = Term.Table.create 256 in
  let rec simp (t : Term.t) : Term.t =
    match Term.Table.find_opt memo t with
    | Some r -> r
    | None ->
      let r = simp_uncached t in
      Term.Table.replace memo t r;
      r
  and simp_uncached (t : Term.t) : Term.t =
    match t.Term.node with
    | Not a -> Context.not_ ctx (simp a)
    | And xs -> Context.and_ ctx (List.map simp (Iarr.to_list xs))
    | Or xs -> Context.or_ ctx (List.map simp (Iarr.to_list xs))
    | Ite (c, a, b) -> Context.ite ctx (simp c) (simp a) (simp b)
    | Eq (a, b) -> Context.eq ctx (simp a) (simp b)
    | Bool_const _ | Int_const _ | Arith _ | Le _ -> t
    | App (sym, orig_args) ->
      (match Bv.view t with
       | Some (Bv.Op { op = Bv.Bvadd | Bv.Bvsub | Bv.Bvneg; _ }) ->
         rebuild ctx mint (lin_of ~simp ~shared t)
       | _ ->
         let orig = Iarr.to_list orig_args in
         let simplified = List.map simp orig in
         if List.for_all2 (fun a b -> a == b) simplified orig
         then t
         else Context.app ctx sym simplified)
  in
  List.map simp terms
;;
