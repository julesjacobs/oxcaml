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
  ; cap : Env.reserved_cap
      (* ADR-0012 R1: fresh reserved symbols are minted through the cap-gated
         [Env.declare_reserved], since the public [Env.declare_fun] now rejects
         [.oxsmt.*]. *)
  ; mutable counter : int
  }

let create cap env ctx = { env; ctx; cap; counter = 0 }

type definition =
  { symbol : Symbol.t
  ; value : Term.t
  }

(* Reserved fresh-symbol namespace: [".oxsmt.<kind>.<n>"], deterministic in the session
   counter. The prefix / predicate now live in {!Env} as the single source of truth
   (ADR-0012 R1); re-exported here for the parser + session guards that reference
   [Preprocess.is_reserved_name]. *)
let reserved_prefix = Env.reserved_prefix
let is_reserved_name = Env.is_reserved_name

let fresh_symbol t ~kind sort =
  let name = Printf.sprintf "%s%s.%d" reserved_prefix kind t.counter in
  t.counter <- t.counter + 1;
  (* cap-gated mint: the public door rejects [.oxsmt.*], this is the legitimate minter. *)
  Env.declare_reserved t.cap t.env name (Rank.create [] sort)
;;

let rec map_lr f = function
  | [] -> []
  | x :: xs ->
    let y = f x in
    y :: map_lr f xs
;;

(* Hash-consing makes tag equality physical identity, so a rewrite that returns a child
   unchanged returns the identical node. *)
let same_tag (a : Term.t) (b : Term.t) = a.Term.tag = b.Term.tag

(* Apply [f] left-to-right over [xs]; return [None] when every result is the identical
   node as its input (nothing under this list was rewritten), letting the caller reuse its
   original hash-consed node instead of paying a reconstruction (list/array allocation + a
   hash-cons lookup) that would only re-derive the same node. Every element is still
   visited, in order, so any fresh-symbol side effect fires exactly as before.

   Allocation: [None] (the common unchanged fast path) now conses NOTHING — [f] is applied
   to every element (side effects in order) but no result list is built. When something
   changed, the rebuilt list shares the unchanged suffix with the original [xs] (the
   changed prefix is freshly consed, so total conses <= the former full [map_lr]). Result
   is byte-identical: [Some ys]'s element tags equal the former all-fresh list's tags (an
   unchanged element re-derives the identical hash-consed node), so the caller rebuilds
   the same node either way. *)
let rec map_go f = function
  | [] -> None
  | x :: rest ->
    let y = f x in
    (match map_go f rest with
     | None -> if same_tag x y then None else Some (y :: rest)
     | Some rest' -> Some (y :: rest'))
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
    | Bool_const _ | Int_const _ | Real_const _ -> term
    | App (sym, args) ->
      (match map_go go (Iarr.to_list args) with
       | None -> term
       | Some args' -> Context.app ctx sym args')
    | Arith l ->
      let changed = ref false in
      let coeffs' =
        map_lr
          (fun (tm, c) ->
            let tm' = go tm in
            if not (same_tag tm tm') then changed := true;
            c, tm')
          (Iarr.to_list l.coeffs)
      in
      if !changed then Context.linear_combination_big ctx coeffs' l.const else term
    | Real_arith l ->
      let changed = ref false in
      let coeffs' =
        map_lr
          (fun (tm, c) ->
            let tm' = go tm in
            if not (same_tag tm tm') then changed := true;
            c, tm')
          (Iarr.to_list l.coeffs)
      in
      if !changed then Context.real_linear_combination_big ctx coeffs' l.const else term
    | Le a when Sort.equal a.sort Sort.int ->
      (* Preserve the OLD unconditional rebuild's side effect: [Context.int_const ctx 0]
         first-interns the standalone [Int_const 0] term. It is an orphan here, but LIA
         search may later reuse it and its hash-cons tag must match trunk; skipping it
         would shift that tag on a formula with no explicit [0] literal. Interned BEFORE
         [go a] to match OCaml's right-to-left argument evaluation of the old
         [Context.le ctx (go a) (Context.int_const ctx 0)]. *)
      let zero = Context.int_const ctx 0 in
      let a' = go a in
      if same_tag a a' then term else Context.le ctx a' zero
    | Le a ->
      let zero = Context.real_const_big ctx ~num:Bigint.zero ~den:Bigint.one in
      let a' = go a in
      if same_tag a a' then term else Context.le ctx a' zero
    | Eq (a, b) ->
      let a' = go a in
      let b' = go b in
      if same_tag a a' && same_tag b b' then term else Context.eq ctx a' b'
    | Not a ->
      let a' = go a in
      if same_tag a a' then term else Context.not_ ctx a'
    | And xs ->
      (match map_go go (Iarr.to_list xs) with
       | None -> term
       | Some xs' -> Context.and_ ctx xs')
    | Or xs ->
      (match map_go go (Iarr.to_list xs) with
       | None -> term
       | Some xs' -> Context.or_ ctx xs')
    | Ite (c, a, b) ->
      let c' = go c in
      let a' = go a in
      let b' = go b in
      if Sort.equal term.sort Sort.bool
      then
        (* Bool-Ite: a connective, left for the clausifier *)
        if same_tag c c' && same_tag a a' && same_tag b b'
        then term
        else Context.ite ctx c' a' b'
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
        | Int_const k when Bigint.is_zero k -> raise (Term.Unsupported "div/mod by zero")
        | Int_const k ->
          (* Divisor must fit native [int] for [get_qr] (core-bignum W2): a >int63
             constant divisor is out of the supported fragment — degrade rather than
             truncate. *)
          (match Bigint.to_int_opt k with
           | Some kv -> kv
           | None ->
             raise (Term.Unsupported "div/mod by an out-of-range constant divisor"))
        | _ -> raise (Term.Unsupported "div/mod by a non-constant divisor")
      in
      let q, r = get_qr x' dv in
      if Symbol.equal sym div_sym then q else r
    | Bool_const _ | Int_const _ | Real_const _ -> term
    | App (sym, args) ->
      (match map_go go (Iarr.to_list args) with
       | None -> term
       | Some args' -> Context.app ctx sym args')
    | Arith l ->
      let changed = ref false in
      let coeffs' =
        map_lr
          (fun (tm, c) ->
            let tm' = go tm in
            if not (same_tag tm tm') then changed := true;
            c, tm')
          (Iarr.to_list l.coeffs)
      in
      if !changed then Context.linear_combination_big ctx coeffs' l.const else term
    | Real_arith l ->
      let changed = ref false in
      let coeffs' =
        map_lr
          (fun (tm, c) ->
            let tm' = go tm in
            if not (same_tag tm tm') then changed := true;
            c, tm')
          (Iarr.to_list l.coeffs)
      in
      if !changed then Context.real_linear_combination_big ctx coeffs' l.const else term
    | Le a when Sort.equal a.sort Sort.int ->
      (* Preserve the OLD rebuild's [Int_const 0] interning at the same point
         (right-to-left arg eval interns it before [go a]); see the matching note in
         [ite_removal]. *)
      let zero = Context.int_const ctx 0 in
      let a' = go a in
      if same_tag a a' then term else Context.le ctx a' zero
    | Le a ->
      let zero = Context.real_const_big ctx ~num:Bigint.zero ~den:Bigint.one in
      let a' = go a in
      if same_tag a a' then term else Context.le ctx a' zero
    | Eq (a, b) ->
      let a' = go a in
      let b' = go b in
      if same_tag a a' && same_tag b b' then term else Context.eq ctx a' b'
    | Not a ->
      let a' = go a in
      if same_tag a a' then term else Context.not_ ctx a'
    | And xs ->
      (match map_go go (Iarr.to_list xs) with
       | None -> term
       | Some xs' -> Context.and_ ctx xs')
    | Or xs ->
      (match map_go go (Iarr.to_list xs) with
       | None -> term
       | Some xs' -> Context.or_ ctx xs')
    | Ite (c, a, b) ->
      let c' = go c in
      let a' = go a in
      let b' = go b in
      if same_tag c c' && same_tag a a' && same_tag b b'
      then term
      else Context.ite ctx c' a' b'
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
(* run. div/mod first: it may surface an Ite into a euclidean side constraint, which
   ite_removal then clears; and it removes every div/mod before ite_removal runs, which
   reintroduces neither (ADR-0003 pipeline invariant). *)

let run_with_definitions t root =
  let t1, d1 = div_mod_elimination t root in
  let t2, d2 = ite_removal t t1 in
  t2, d1 @ d2
;;

let run t root = fst (run_with_definitions t root)
