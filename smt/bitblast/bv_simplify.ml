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

(* ---- Family 1 (task #36): extract/concat normalization + equality-over-concat
   splitting. Env-gated dark (OXSMT_BV_REWRITE2); with it off the whole block is inert and
   [simplify] is byte-identical to the additive-only pass. Every rewrite here is sound by
   fixed-width extract/concat algebra, and is certified exhaustively by the Layer-4
   simplifier-equivalence oracle (assert not(e = simplify e) -> Unsat). Census (task #20)
   showed 95% of QF_BV losses are build/blast-bound and these rewrites (esp. eq-concat)
   sidestep the eager materialization of wide equalities over concat trees. ---- *)

let rewrite2_enabled () =
  match Sys.getenv_opt "OXSMT_BV_REWRITE2" with
  | Some ("1" | "true" | "yes" | "on") -> true
  | Some _ | None -> false
;;

(* Equality-over-concat splitting (family 1) is measured NET-NEUTRAL and REGRESSES
   wide-width files (ext_con_004_001_1024: OFF unsat 3.6 s -> ON killed) because it
   reshapes but still bit-blasts. It is therefore a separate opt-in sub-flag, OFF even
   when the main gate is on, so the clean-positive normalizers + families 2/3 ship without
   it. Kept for A/B attribution and as an enabler if a later blast-avoiding path uses it. *)
let eqsplit_enabled () =
  match Sys.getenv_opt "OXSMT_BV_REWRITE2_EQSPLIT" with
  | Some ("1" | "true" | "yes" | "on") -> true
  | Some _ | None -> false
;;

(* ---- rw3 (bv-rw3 probe): comparison-vs-bound rewriting + extract-into-op width
   propagation. A SEPARATE env gate (OXSMT_BV_RW3, default off, byte-identical off) so it
   composes with OXSMT_BV_REWRITE2 (additive + extract/concat + bitwise/shift) without
   disturbing it. UNSAT-fold direction only: every rule turns a comparison into a Bool
   constant / equality / narrower comparison, which cascades through the ALREADY-folding
   Context.[{ite,and_,or_,not_,eq}] constructors to collapse an unsatisfiable formula to
   [false] pre-blast. Mirrors z3 4.8.5 bv_rewriter.cpp's ALWAYS-ON core (mk_leq_core bound
   rules + the is_zero_bit ule split at :548-576, and the extract-into-op pushdown at
   :761-774 — NOT the param-gated propagate_extract). Each rule is certified by the
   Layer-4 simplifier-equivalence oracle; see logs/bv-rw3-probe-log.md for the
   rule-to-source map. *)
let rw3_enabled () =
  match Sys.getenv_opt "OXSMT_BV_RW3" with
  | Some ("1" | "true" | "yes" | "on") -> true
  | Some _ | None -> false
;;

let bvwidth (t : Term.t) =
  match Bv.width_of_sort t.Term.sort with
  | Some w -> w
  | None -> -1
;;

(* [norm_extract ~simp ~shared ctx mint ~i ~j x] = (extract [i:j] x). [x] is an ORIGINAL
   (un-simplified) term; the extract is pushed through concat/extract/const/extend toward
   the leaves, and each leaf it stops at is passed through [simp]. [i >= j >= 0],
   [i < width x].

   SHARING GATE (O-bv2, the -15/-21 scar class, same discipline as {!lin_of}): the DAG
   blaster encodes a node referenced by more than one parent ONCE. Pushing an extract INTO
   such a [shared] node re-expands it per extract (e.g. eq-concat splitting emits many
   extracts of one shared concat -> O(n^2)). So at a shared node we STOP: emit a plain
   [Bv.extract] on [simp x] and let the blaster slice the once-encoded node. Only UNSHARED
   structure is restructured. Every arm is equivalence-preserving (certified by the
   Layer-4 oracle); the gate is a cost guard, not a soundness one — the
   [ext_con_004_001_1024] canary is the evidence it is load-bearing on the eq-split path. *)
let rec norm_extract ~simp ~shared ~rw3 ctx mint ~i ~j (x : Term.t) : Term.t =
  let w = bvwidth x in
  let recur = norm_extract ~simp ~shared ~rw3 ctx mint in
  if w >= 1 && j = 0 && i = w - 1
  then simp x (* full-width extract is the identity *)
  else if shared x
  then Bv.extract ctx mint ~i ~j (simp x) (* do not re-expand a shared node *)
  else (
    match Bv.view x with
    | Some (Bv.Const { value; width = _ }) ->
      let q = fst (Bigint.divmod value (pow2 j)) in
      let v = snd (Bigint.divmod q (pow2 (i - j + 1))) in
      Bv.const ctx mint ~value:v ~width:(i - j + 1)
    | Some (Bv.Op { op = Bv.Concat; args = [ hi; lo ]; _ }) ->
      let wl = bvwidth lo in
      if i < wl
      then recur ~i ~j lo
      else if j >= wl
      then recur ~i:(i - wl) ~j:(j - wl) hi
      else Bv.concat ctx mint (recur ~i:(i - wl) ~j:0 hi) (recur ~i:(wl - 1) ~j lo)
    | Some (Bv.Op { op = Bv.Extract (_a, b); args = [ y ]; _ }) ->
      recur ~i:(b + i) ~j:(b + j) y
    | Some (Bv.Op { op = Bv.Zero_extend _; args = [ y ]; _ }) ->
      let w0 = bvwidth y in
      if i < w0
      then recur ~i ~j y
      else if j >= w0
      then Bv.const ctx mint ~value:Bigint.zero ~width:(i - j + 1)
      else
        Bv.concat
          ctx
          mint
          (Bv.const ctx mint ~value:Bigint.zero ~width:(i - w0 + 1))
          (recur ~i:(w0 - 1) ~j y)
    | Some (Bv.Op { op = Bv.Sign_extend _; args = [ y ]; _ }) when i < bvwidth y ->
      recur ~i ~j y
    (* rw3: extract-into-op width propagation (z3 bv_rewriter.cpp mk_extract :761-774,
       ALWAYS-ON there). Bitwise ops distribute over any slice:
       [(op a b)[i:j] = a[i:j] op b[i:j]]; add/mul only at [j = 0] because the low [i+1]
       bits of a sum/product depend only on the low [i+1] bits of the operands (carry
       flows upward), so slicing off the high bits is sound only when the slice is
       anchored at bit 0. Sound + certified by the Layer-4 oracle; the [shared]-gate above
       still bounds re-expansion. *)
    | Some (Bv.Op { op = Bv.Bvnot; args = [ y ]; _ }) when rw3 ->
      Bv.unop ctx mint Bv.Bvnot (recur ~i ~j y)
    | Some (Bv.Op { op = (Bv.Bvand | Bv.Bvor | Bv.Bvxor) as op; args = [ a; b ]; _ })
      when rw3 -> Bv.binop ctx mint op (recur ~i ~j a) (recur ~i ~j b)
    | Some (Bv.Op { op = (Bv.Bvadd | Bv.Bvmul) as op; args = [ a; b ]; _ })
      when rw3 && j = 0 -> Bv.binop ctx mint op (recur ~i ~j:0 a) (recur ~i ~j:0 b)
    | _ -> Bv.extract ctx mint ~i ~j (simp x))
;;

(* Concat of adjacent constants folds; adjacent contiguous UNSHARED extracts of the SAME
   base merge (a reduction, not a re-expansion). [hi]/[lo] are ORIGINAL terms; the default
   arm rebuilds on [simp]-ed children. *)
let norm_concat ~simp ~shared ctx mint (hi : Term.t) (lo : Term.t) : Term.t =
  match Bv.view hi, Bv.view lo with
  | Some (Bv.Const { value = vh; width = _ }), Some (Bv.Const { value = vl; width = wl })
    ->
    let v = Bigint.add (Bigint.mul vh (pow2 wl)) vl in
    Bv.const ctx mint ~value:v ~width:(bvwidth hi + wl)
  | ( Some (Bv.Op { op = Bv.Extract (a, b); args = [ y1 ]; _ })
    , Some (Bv.Op { op = Bv.Extract (c, d); args = [ y2 ]; _ }) )
    when y1 == y2 && b = c + 1 && (not (shared hi)) && not (shared lo) ->
    norm_extract ~simp ~shared ~rw3:false ctx mint ~i:a ~j:d y1
  | _ -> Bv.concat ctx mint (simp hi) (simp lo)
;;

(* Concat seam positions inside (0, width t): the bit indices where a concat boundary
   falls, flattened through nested concats. [] for a non-concat. *)
let rec seams (t : Term.t) : int list =
  match Bv.view t with
  | Some (Bv.Op { op = Bv.Concat; args = [ hi; lo ]; _ }) ->
    let wl = bvwidth lo in
    seams lo @ [ wl ] @ List.map (fun s -> s + wl) (seams hi)
  | _ -> []
;;

(* Split (= a b) (a, b already-simplified equal-width bitvectors) into a conjunction of
   per-slice equalities cut at the union of both sides' concat seams; each slice is pushed
   into leaves by [norm_extract]. Sound: equal-width bitvectors are equal iff every bit
   slice is. Returns a plain [Context.eq] when there is no concat structure (no churn). *)
let split_eq_concat ctx mint (a : Term.t) (b : Term.t) : Term.t =
  let w = bvwidth a in
  let cuts = List.sort_uniq Int.compare (seams a @ seams b) in
  match cuts with
  | [] -> Context.eq ctx a b
  | _ ->
    let bounds = (0 :: cuts) @ [ w ] in
    let rec pairs = function
      | x :: (y :: _ as rest) -> (x, y) :: pairs rest
      | _ -> []
    in
    (* [a]/[b] are already simplified; slices use identity-[simp]. No sharing gate here is
       deliberate — this eq-split path is where the re-expansion scar lives, which is why
       it stays behind the [OXSMT_BV_REWRITE2_EQSPLIT] sub-flag (default off) with the
       [ext_con_004_001_1024] canary as its evidence. *)
    let ne = norm_extract ~simp:Fun.id ~shared:(fun _ -> false) ~rw3:false ctx mint in
    let eqs =
      List.map
        (fun (loc, hic) ->
          let j = loc
          and i = hic - 1 in
          Context.eq ctx (ne ~i ~j a) (ne ~i ~j b))
        (pairs bounds)
    in
    Context.and_ ctx eqs
;;

(* ---- Family 2 (task #36): bitwise identities + generic constant folding. Same dark
   gate. Sound by bit-vector algebra; certified by the Layer-4 oracle. ---- *)

let as_const (t : Term.t) : (Bigint.t * int) option =
  match Bv.view t with
  | Some (Bv.Const { value; width }) -> Some (value, width)
  | _ -> None
;;

let is_zero_bv t =
  match as_const t with
  | Some (v, _) -> Bigint.is_zero v
  | None -> false
;;

let is_allones_bv t =
  match as_const t with
  | Some (v, w) -> Bigint.equal v (Bigint.sub (pow2 w) Bigint.one)
  | None -> false
;;

let is_one_bv t =
  match as_const t with
  | Some (v, _) -> Bigint.equal v Bigint.one
  | None -> false
;;

let same_term (a : Term.t) (b : Term.t) = a == b || a.Term.tag = b.Term.tag
let zero_bv ctx mint w = Bv.const ctx mint ~value:Bigint.zero ~width:w

let allones_bv ctx mint w =
  Bv.const ctx mint ~value:(Bigint.sub (pow2 w) Bigint.one) ~width:w
;;

(* Fold a fully-constant bit-vector node via the independent evaluator (the same total
   SMT-LIB semantics the blaster is checked against). Returns [None] if any leaf is a free
   variable (Eval_error) — i.e. not all-const. *)
let try_fold_const ctx mint (t : Term.t) : Term.t option =
  match Bv_eval.eval_bv Bv_adapter.defs ~lookup:(fun _ -> None) t with
  | value, width -> Some (Bv.const ctx mint ~value ~width)
  | exception Bv_eval.Eval_error _ -> None
;;

(* Rewrite a bitwise/mul bit-vector op given already-simplified args. Tries constant
   folding first, then operator identities; falls back to rebuilding the op. [w] is the
   result width. *)
let rewrite_bv_op ctx mint (op : Bv.op) (args : Term.t list) (w : int) : Term.t =
  let rebuild () =
    match op, args with
    | Bv.Bvnot, [ a ] -> Bv.unop ctx mint Bv.Bvnot a
    | _, [ a; b ] -> Bv.binop ctx mint op a b
    | _ -> failwith "rewrite_bv_op: unexpected arity"
  in
  (* all-const fold *)
  let all_const = List.for_all (fun a -> as_const a <> None) args in
  let folded = if all_const then try_fold_const ctx mint (rebuild ()) else None in
  match folded with
  | Some c -> c
  | None ->
    (match op, args with
     | Bv.Bvnot, [ a ] ->
       (match Bv.view a with
        | Some (Bv.Op { op = Bv.Bvnot; args = [ x ]; _ }) -> x (* ~~x = x *)
        | _ -> rebuild ())
     | Bv.Bvand, [ a; b ] ->
       if is_zero_bv a || is_zero_bv b
       then zero_bv ctx mint w
       else if is_allones_bv a
       then b
       else if is_allones_bv b
       then a
       else if same_term a b
       then a
       else rebuild ()
     | Bv.Bvor, [ a; b ] ->
       if is_allones_bv a || is_allones_bv b
       then allones_bv ctx mint w
       else if is_zero_bv a
       then b
       else if is_zero_bv b
       then a
       else if same_term a b
       then a
       else rebuild ()
     | Bv.Bvxor, [ a; b ] ->
       if is_zero_bv a
       then b
       else if is_zero_bv b
       then a
       else if same_term a b
       then zero_bv ctx mint w
       else if is_allones_bv a
       then Bv.unop ctx mint Bv.Bvnot b
       else if is_allones_bv b
       then Bv.unop ctx mint Bv.Bvnot a
       else rebuild ()
     | Bv.Bvmul, [ a; b ] ->
       if is_zero_bv a || is_zero_bv b
       then zero_bv ctx mint w
       else if is_one_bv a
       then b
       else if is_one_bv b
       then a
       else rebuild ()
     (* Family 3: shift by a CONSTANT amount folds to extract/concat (fill bits), so the
        blaster encodes a wire re-routing instead of a full barrel shifter. Sound by the
        SMT-LIB shift semantics (over-width shl/lshr = 0, ashr = sign). *)
     | (Bv.Bvshl | Bv.Bvlshr | Bv.Bvashr), [ x; kterm ] ->
       (* [x] is already simplified here, so extract/concat of it uses identity-[simp] and
          no sharing gate (the fold emits a bounded number of extracts — not the eq-concat
          re-expansion the gate guards). *)
       let ne = norm_extract ~simp:Fun.id ~shared:(fun _ -> false) ~rw3:false ctx mint in
       let nc = norm_concat ~simp:Fun.id ~shared:(fun _ -> false) ctx mint in
       (match as_const kterm with
        | None -> rebuild ()
        | Some (kval, _) ->
          let over = Bigint.compare kval (Bigint.of_int w) >= 0 in
          let sign_bit () = ne ~i:(w - 1) ~j:(w - 1) x in
          (match Bigint.to_int_opt kval with
           | Some 0 -> x
           | _ when over ->
             (match op with
              | Bv.Bvashr -> Bv.sign_extend ctx mint ~n:(w - 1) (sign_bit ())
              | _ -> zero_bv ctx mint w)
           | Some k when k > 0 && k < w ->
             (match op with
              | Bv.Bvshl -> nc (ne ~i:(w - 1 - k) ~j:0 x) (zero_bv ctx mint k)
              | Bv.Bvlshr -> nc (zero_bv ctx mint k) (ne ~i:(w - 1) ~j:k x)
              | Bv.Bvashr ->
                nc
                  (Bv.sign_extend ctx mint ~n:(k - 1) (sign_bit ()))
                  (ne ~i:(w - 1) ~j:k x)
              | _ -> rebuild ())
           | _ -> rebuild ()))
     | _ -> rebuild ())
;;

(* ---- rw3: comparison-vs-bound rewriting (z3 bv_rewriter.cpp mk_leq_core :439-589). ---- *)

(* [is_zero_bit t idx] iff bit [idx] of [t] is KNOWN to be zero from its structure:
   constants (bit test) and the zero sub-parts of a concat. Anything else -> [false]
   (unknown, conservatively not-zero). Mirrors z3 is_zero_bit :2147. *)
let rec is_zero_bit (t : Term.t) (idx : int) : bool =
  match Bv.view t with
  | Some (Bv.Const { value; width = _ }) ->
    let q = fst (Bigint.divmod value (pow2 idx)) in
    Bigint.is_zero (snd (Bigint.divmod q (Bigint.of_int 2)))
  | Some (Bv.Op { op = Bv.Concat; args = [ hi; lo ]; _ }) ->
    let wl = bvwidth lo in
    if idx < wl then is_zero_bit lo idx else is_zero_bit hi (idx - wl)
  | _ -> false
;;

(* signed interpretation of the canonical unsigned residue [v] at width [w]. *)
let signed_val v w =
  if Bigint.compare v (pow2 (w - 1)) >= 0 then Bigint.sub v (pow2 w) else v
;;

(* [rw_leq ~signed ctx mint a b] rewrites [a <= b] (unsigned or signed, [a]/[b] already
   simplified) toward a Bool constant / equality / narrower comparison. Only ever weakens
   to an EQUIVALENT form (certified by the Layer-4 oracle); on no match it rebuilds the
   plain comparison. Order mirrors z3 mk_leq_core: reflexivity, both-const decision, the
   numeral bound tautologies/contradictions (both operand positions), then the unsigned
   is_zero_bit high-bit split. *)
let rw_leq ~signed ctx mint (a : Term.t) (b : Term.t) : Term.t =
  let w = bvwidth a in
  let plain () = Bv.binop ctx mint (if signed then Bv.Bvsle else Bv.Bvule) a b in
  if w < 1
  then plain ()
  else if same_term a b
  then Context.bool_const ctx true (* a <= a *)
  else (
    let ca = as_const a
    and cb = as_const b in
    match ca, cb with
    | Some (va, _), Some (vb, _) ->
      let le =
        if signed
        then Bigint.compare (signed_val va w) (signed_val vb w) <= 0
        else Bigint.compare va vb <= 0
      in
      Context.bool_const ctx le
    | _ ->
      let lower = if signed then pow2 (w - 1) else Bigint.zero in
      let upper =
        if signed
        then Bigint.sub (pow2 (w - 1)) Bigint.one
        else Bigint.sub (pow2 w) Bigint.one
      in
      let bound_match =
        match cb, ca with
        (* b is the lower bound: a <= lower <=> a = lower *)
        | Some (vb, _), _ when Bigint.equal vb lower -> Some (Context.eq ctx a b)
        (* b is the upper bound: a <= upper is a tautology *)
        | Some (vb, _), _ when Bigint.equal vb upper -> Some (Context.bool_const ctx true)
        (* a is the lower bound: lower <= b is a tautology *)
        | _, Some (va, _) when Bigint.equal va lower -> Some (Context.bool_const ctx true)
        (* a is the upper bound: upper <= b <=> b = upper *)
        | _, Some (va, _) when Bigint.equal va upper -> Some (Context.eq ctx a b)
        | _ -> None
      in
      (match bound_match with
       | Some r -> r
       | None ->
         if signed
         then plain ()
         else (
           (* unsigned is_zero_bit split (z3 :548-576): find the top set/unknown bit of b;
              a <= b then forces a's bits above it to zero and compares the low slices.
              The extracts are pushed into a/b's arithmetic (rw3:true, no sharing gate:
              the slice is narrow, so this is bounded and is exactly the width-shrink that
              exposes a folded high-half contradiction). *)
           let ex t ~i ~j =
             norm_extract ~simp:Fun.id ~shared:(fun _ -> false) ~rw3:true ctx mint ~i ~j t
           in
           let first_non_zero = ref (-1) in
           let i = ref w in
           (try
              while !i > 0 do
                decr i;
                if not (is_zero_bit b !i)
                then (
                  first_non_zero := !i;
                  raise Exit)
              done
            with
            | Exit -> ());
           let fnz = !first_non_zero in
           if fnz = -1
           then Context.eq ctx a (zero_bv ctx mint w) (* all bits of b are zero *)
           else if fnz < w - 1
           then (
             let hi_zero =
               Context.eq
                 ctx
                 (ex a ~i:(w - 1) ~j:(fnz + 1))
                 (zero_bv ctx mint (w - 1 - fnz))
             in
             let lo_cmp =
               Bv.binop ctx mint Bv.Bvule (ex a ~i:fnz ~j:0) (ex b ~i:fnz ~j:0)
             in
             Context.and_ ctx [ hi_zero; lo_cmp ])
           else plain ())))
;;

let simplify ctx mint (terms : Term.t list) : Term.t list =
  let rewrite2 = rewrite2_enabled () in
  let eqsplit = rewrite2 && eqsplit_enabled () in
  let rw3 = rw3_enabled () in
  (* One bottom-up simplification pass over [ts]. [occurrences]/[shared] are recomputed
     per pass so the sharing gate stays accurate as the term set changes under the
     fixpoint. *)
  let one_pass (ts : Term.t list) : Term.t list =
    let count = occurrences ts in
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
      | Eq (a, b) ->
        let a' = simp a
        and b' = simp b in
        if eqsplit && bvwidth a' >= 1
        then split_eq_concat ctx mint a' b'
        else Context.eq ctx a' b'
      | Bool_const _ | Int_const _ | Arith _ | Le _ -> t
      | App (sym, orig_args) ->
        (match Bv.view t with
         | Some (Bv.Op { op = Bv.Bvadd | Bv.Bvsub | Bv.Bvneg; _ }) ->
           rebuild ctx mint (lin_of ~simp ~shared t)
         | Some (Bv.Op { op = Bv.Extract (i, j); args = [ a ]; _ }) when rewrite2 || rw3
           ->
           (* pass the ORIGINAL operand: [norm_extract] walks its structure with the
              [shared] gate and [simp]s the leaves it stops at. [~rw3] additionally
              enables the extract-into-op width propagation. *)
           norm_extract ~simp ~shared ~rw3 ctx mint ~i ~j a
         | Some (Bv.Op { op = Bv.Concat; args = [ hi; lo ]; _ }) when rewrite2 ->
           norm_concat ~simp ~shared ctx mint hi lo
         | Some (Bv.Op { op = Bv.Bvule; args = [ a; b ]; _ }) when rw3 ->
           rw_leq ~signed:false ctx mint (simp a) (simp b)
         | Some (Bv.Op { op = Bv.Bvsle; args = [ a; b ]; _ }) when rw3 ->
           rw_leq ~signed:true ctx mint (simp a) (simp b)
         | Some (Bv.Op { op = Bv.Bvult; args = [ a; b ]; _ }) when rw3 ->
           (* a <u b <=> not (b <=u a) *)
           Context.not_ ctx (rw_leq ~signed:false ctx mint (simp b) (simp a))
         | Some (Bv.Op { op = Bv.Bvslt; args = [ a; b ]; _ }) when rw3 ->
           (* a <s b <=> not (b <=s a) *)
           Context.not_ ctx (rw_leq ~signed:true ctx mint (simp b) (simp a))
         | Some
             (Bv.Op
               { op =
                   ( Bv.Bvnot
                   | Bv.Bvand
                   | Bv.Bvor
                   | Bv.Bvxor
                   | Bv.Bvmul
                   | Bv.Bvshl
                   | Bv.Bvlshr
                   | Bv.Bvashr ) as op
               ; args
               ; result_width = Some w
               })
           when rewrite2 -> rewrite_bv_op ctx mint op (List.map simp args) w
         | _ ->
           let orig = Iarr.to_list orig_args in
           let simplified = List.map simp orig in
           if List.for_all2 (fun a b -> a == b) simplified orig
           then t
           else Context.app ctx sym simplified)
    in
    List.map simp ts
  in
  (* rw3: bounded-fixpoint re-simplification. A single bottom-up pass leaves the fresh
     terms that rw_leq's is_zero_bit split / extract-pushdown build UN-renormalized (e.g.
     an extract of an additive term is left as [bvadd(c1,c2,...)] instead of folded to a
     constant), so a word-level contradiction stays hidden until bit-blast. Re-running the
     pass lets the additive normalizer fold those outputs, which cascades through
     Context.[{eq,and_,ite}] to [false]. BOUNDED + total: at most [max_passes] passes,
     clean early-stop the instant a pass is a fixpoint (term set unchanged by hash-cons
     tag); at the cap we return the last COMPLETE pass (never a partial rewrite). Only
     under rw3 — rewrite2/baseline are exactly one pass (byte-identical to before this
     change). *)
  let max_passes = 4 in
  let same (a : Term.t list) (b : Term.t list) =
    List.length a = List.length b
    && List.for_all2 (fun (x : Term.t) (y : Term.t) -> x.tag = y.tag) a b
  in
  let result =
    if not rw3
    then one_pass terms
    else (
      let rec fix (ts : Term.t list) (n : int) : Term.t list =
        if n >= max_passes
        then ts
        else (
          let ts' = one_pass ts in
          if same ts ts' then ts' else fix ts' (n + 1))
      in
      fix terms 0)
  in
  result
;;
