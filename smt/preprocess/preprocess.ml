(* Preprocessing passes (ADR-0003 Decision 5 pipeline invariants). Each pass is a memoized
   DAG rewrite through the session's smart constructors: results stay normalized,
   well-sorted and hash-consed (INVARIANTS.md I1/I2), and fresh nullary symbols carry the
   guarded/euclidean side constraints that make the rewrite equisatisfiable.

   Determinism (I6): rewriting is a left-to-right DFS ([map_lr], sequential [let]s), and
   fresh symbols are numbered off a monotonic per-session counter, so a fixed input yields
   identical fresh names and identical output every run.

   Overflow/Unsupported (I8): the pass builds through {!Context}, whose constructors raise
   {!Term.Overflow}/{!Term.Unsupported} before interning; the pass does not catch them —
   the session boundary does, degrading to verdict [unknown]. *)

open Oxsmt_core

type t =
  { env : Env.t
  ; ctx : Context.t
  ; mutable counter : int
  }

let create env ctx = { env; ctx; counter = 0 }

type definition =
  { symbol : Symbol.t
  ; value : Term.t
  }

(* Reserved fresh-symbol namespace: [".oxsmt.<kind>.<n>"], deterministic in the session
   counter. Front ends must keep user names out of this namespace (see .mli). *)
let fresh_symbol t ~kind sort =
  let name = Printf.sprintf ".oxsmt.%s.%d" kind t.counter in
  t.counter <- t.counter + 1;
  Env.declare_fun t.env name (Rank.create [] sort)
;;

let rec map_lr f = function
  | [] -> []
  | x :: xs ->
    let y = f x in
    y :: map_lr f xs
;;

(* ------------------------------------------------------------------ *)
(* ite_removal. *)

let ite_removal t root =
  let ctx = t.ctx in
  let memo : Term.t Term.Table.t = Term.Table.create 256 in
  let defs = ref [] in
  let constraints = ref [] in
  let rec go term =
    match Term.Table.find_opt memo term with
    | Some r -> r
    | None ->
      let r = rewrite term in
      Term.Table.replace memo term r;
      r
  and rewrite (term : Term.t) =
    match term.node with
    | Bool_const _ | Int_const _ -> term
    | App (sym, args) -> Context.app ctx sym (map_lr go (Iarr.to_list args))
    | Arith l ->
      Context.linear_combination
        ctx
        (map_lr (fun (tm, c) -> c, go tm) (Iarr.to_list l.coeffs))
        l.const
    | Le a -> Context.le ctx (go a) (Context.int_const ctx 0)
    | Eq (a, b) ->
      let a' = go a in
      let b' = go b in
      Context.eq ctx a' b'
    | Not a -> Context.not_ ctx (go a)
    | And xs -> Context.and_ ctx (map_lr go (Iarr.to_list xs))
    | Or xs -> Context.or_ ctx (map_lr go (Iarr.to_list xs))
    | Ite (c, a, b) ->
      let c' = go c in
      let a' = go a in
      let b' = go b in
      if Sort.equal term.sort Sort.bool
      then Context.ite ctx c' a' b' (* Bool-Ite: a connective, left for the clausifier *)
      else (
        (* non-Bool value-Ite: lift to a fresh constant with guarded equalities. *)
        let sym = fresh_symbol t ~kind:"ite" term.sort in
        let v = Context.const ctx sym in
        let guard =
          Context.and_
            ctx
            [ Context.implies ctx c' (Context.eq ctx v a')
            ; Context.implies ctx (Context.not_ ctx c') (Context.eq ctx v b')
            ]
        in
        constraints := guard :: !constraints;
        defs := { symbol = sym; value = Context.ite ctx c' a' b' } :: !defs;
        v)
  in
  let root' = go root in
  let result =
    match !constraints with
    | [] -> root'
    | cs -> Context.and_ ctx (root' :: List.rev cs)
  in
  result, List.rev !defs
;;

(* ------------------------------------------------------------------ *)
(* div_mod_elimination. *)

let div_mod_elimination t root =
  let ctx = t.ctx in
  let div_sym = Env.div_sym t.env in
  let mod_sym = Env.mod_sym t.env in
  let memo : Term.t Term.Table.t = Term.Table.create 256 in
  let defs = ref [] in
  let constraints = ref [] in
  (* (dividend tag, divisor value) -> (q, r); constraints + defs emitted once so a div and
     a mod of the same operands share one euclidean witness. *)
  let cache : (int * int, Term.t * Term.t) Hashtbl.t = Hashtbl.create 64 in
  let get_qr x' dv =
    match Hashtbl.find_opt cache (x'.Term.tag, dv) with
    | Some qr -> qr
    | None ->
      let qsym = fresh_symbol t ~kind:"q" Sort.int in
      let rsym = fresh_symbol t ~kind:"r" Sort.int in
      let q = Context.const ctx qsym in
      let r = Context.const ctx rsym in
      (* x = d*q + r *)
      let rhs = Context.linear_combination ctx [ dv, q; 1, r ] 0 in
      let c_eq = Context.eq ctx x' rhs in
      (* 0 <= r < |d| *)
      let absd = if dv = min_int then raise Term.Overflow else abs dv in
      let c_lo = Context.le ctx (Context.int_const ctx 0) r in
      let c_hi = Context.lt ctx r (Context.int_const ctx absd) in
      constraints := c_hi :: c_lo :: c_eq :: !constraints;
      defs
      := { symbol = rsym
         ; value = Context.app ctx mod_sym [ x'; Context.int_const ctx dv ]
         }
         :: { symbol = qsym
            ; value = Context.app ctx div_sym [ x'; Context.int_const ctx dv ]
            }
         :: !defs;
      Hashtbl.replace cache (x'.Term.tag, dv) (q, r);
      q, r
  in
  let rec go term =
    match Term.Table.find_opt memo term with
    | Some r -> r
    | None ->
      let r = rewrite term in
      Term.Table.replace memo term r;
      r
  and rewrite (term : Term.t) =
    match term.node with
    | App (sym, args) when Symbol.equal sym div_sym || Symbol.equal sym mod_sym ->
      let x' = go (Iarr.get args 0) in
      let d = Iarr.get args 1 in
      let dv =
        match d.node with
        | Int_const k when k <> 0 -> k
        | Int_const _ -> raise (Term.Unsupported "div/mod by zero")
        | _ -> raise (Term.Unsupported "div/mod by a non-constant divisor")
      in
      let q, r = get_qr x' dv in
      if Symbol.equal sym div_sym then q else r
    | Bool_const _ | Int_const _ -> term
    | App (sym, args) -> Context.app ctx sym (map_lr go (Iarr.to_list args))
    | Arith l ->
      Context.linear_combination
        ctx
        (map_lr (fun (tm, c) -> c, go tm) (Iarr.to_list l.coeffs))
        l.const
    | Le a -> Context.le ctx (go a) (Context.int_const ctx 0)
    | Eq (a, b) ->
      let a' = go a in
      let b' = go b in
      Context.eq ctx a' b'
    | Not a -> Context.not_ ctx (go a)
    | And xs -> Context.and_ ctx (map_lr go (Iarr.to_list xs))
    | Or xs -> Context.or_ ctx (map_lr go (Iarr.to_list xs))
    | Ite (c, a, b) ->
      let c' = go c in
      let a' = go a in
      let b' = go b in
      Context.ite ctx c' a' b'
  in
  let root' = go root in
  let result =
    match !constraints with
    | [] -> root'
    | cs -> Context.and_ ctx (root' :: List.rev cs)
  in
  result, List.rev !defs
;;

(* ------------------------------------------------------------------ *)
(* simplify: bottom-up rebuild through the constructors (identity on normalized terms). *)

let simplify t root =
  let ctx = t.ctx in
  let memo : Term.t Term.Table.t = Term.Table.create 256 in
  let rec go term =
    match Term.Table.find_opt memo term with
    | Some r -> r
    | None ->
      let r = rebuild term in
      Term.Table.replace memo term r;
      r
  and rebuild (term : Term.t) =
    match term.node with
    | Bool_const _ | Int_const _ -> term
    | App (sym, args) -> Context.app ctx sym (map_lr go (Iarr.to_list args))
    | Arith l ->
      Context.linear_combination
        ctx
        (map_lr (fun (tm, c) -> c, go tm) (Iarr.to_list l.coeffs))
        l.const
    | Le a -> Context.le ctx (go a) (Context.int_const ctx 0)
    | Eq (a, b) ->
      let a' = go a in
      let b' = go b in
      Context.eq ctx a' b'
    | Not a -> Context.not_ ctx (go a)
    | And xs -> Context.and_ ctx (map_lr go (Iarr.to_list xs))
    | Or xs -> Context.or_ ctx (map_lr go (Iarr.to_list xs))
    | Ite (c, a, b) ->
      let c' = go c in
      let a' = go a in
      let b' = go b in
      Context.ite ctx c' a' b'
  in
  go root
;;

(* ------------------------------------------------------------------ *)
(* run. div/mod first: it may surface an Ite into a euclidean side constraint, which
   ite_removal then clears; and it removes every div/mod before ite_removal runs, which
   reintroduces neither (ADR-0003 pipeline invariant). *)

let run_with_definitions t root =
  let t1, d1 = div_mod_elimination t root in
  let t2, d2 = ite_removal t t1 in
  t2, d1 @ d2
;;

let run t root = fst (run_with_definitions t root)
