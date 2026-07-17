(* Unit tests for the front-end quantified-pipeline formula IR
   ({!Oxsmt_smtlib_parser.Fol}): NNF/polarity normalization, binder rename-apart, and the
   two canonical clausification exemplars at the NNF level. Pure — no Session/Context — so
   it exercises the transforms in isolation from the solver. Nonzero exit on any failed
   check (folded into [make test] via the [fol-test] target). *)

open Oxsmt_core
module Fol = Oxsmt_smtlib_parser.Fol

(* A test leaf that carries its referenced binder ids, so rename-apart is OBSERVABLE at
   the atom level (a plain string leaf would hide whether [rename_atom] rewrote the atom). *)
type leaf =
  { lname : string
  ; refs : int list
  }

let rename_atom remap a = { a with refs = List.map remap a.refs }
let leaf_str a = a.lname
let checks = ref 0
let failures = ref 0

let check name cond =
  incr checks;
  if not cond
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

let check_str name ~expected ~got =
  incr checks;
  if not (String.equal expected got)
  then (
    incr failures;
    Printf.printf "  FAIL %s\n    expected: %s\n    got:      %s\n" name expected got)
;;

let atom ?(refs = []) lname = Fol.Atom { lname; refs }
let show phi = Fol.to_string leaf_str phi
let nnf phi = Fol.nnf phi

(* ------------------------------------------------------------------ *)
(* NNF/polarity table: each connective under both polarities. *)

let a = atom "a"
let b = atom "b"
let c = atom "c"

let test_nnf_table () =
  (* implication *)
  check_str "nnf (=> a b)" ~expected:"(or (not a) b)" ~got:(show (nnf (Implies (a, b))));
  check_str
    "nnf (not (=> a b))"
    ~expected:"(and a (not b))"
    ~got:(show (nnf (Not (Implies (a, b)))));
  (* conjunction / disjunction under negation (de Morgan) *)
  check_str
    "nnf (not (and a b))"
    ~expected:"(or (not a) (not b))"
    ~got:(show (nnf (Not (And [ a; b ]))));
  check_str
    "nnf (not (or a b))"
    ~expected:"(and (not a) (not b))"
    ~got:(show (nnf (Not (Or [ a; b ]))));
  (* double negation *)
  check_str "nnf (not (not a))" ~expected:"a" ~got:(show (nnf (Not (Not a))));
  (* iff, both polarities *)
  check_str
    "nnf (= a b)"
    ~expected:"(and (or (not a) b) (or (not b) a))"
    ~got:(show (nnf (Iff (a, b))));
  check_str
    "nnf (not (= a b))"
    ~expected:"(and (or a b) (or (not a) (not b)))"
    ~got:(show (nnf (Not (Iff (a, b)))));
  (* xor, both polarities *)
  check_str
    "nnf (xor a b)"
    ~expected:"(and (or a b) (or (not a) (not b)))"
    ~got:(show (nnf (Xor (a, b))));
  check_str
    "nnf (not (xor a b))"
    ~expected:"(and (or (not a) b) (or (not b) a))"
    ~got:(show (nnf (Not (Xor (a, b)))));
  (* boolean ite, both polarities *)
  check_str
    "nnf (ite a b c)"
    ~expected:"(and (or (not a) b) (or a c))"
    ~got:(show (nnf (Ite (a, b, c))));
  check_str
    "nnf (not (ite a b c))"
    ~expected:"(and (or (not a) (not b)) (or a (not c)))"
    ~got:(show (nnf (Not (Ite (a, b, c)))));
  (* every result is a well-formed NNF *)
  List.iter
    (fun phi -> check ("is_nnf: " ^ show phi) (Fol.is_nnf phi))
    [ nnf (Implies (a, b))
    ; nnf (Not (Iff (a, b)))
    ; nnf (Xor (a, b))
    ; nnf (Not (Ite (a, b, c)))
    ]
;;

(* ------------------------------------------------------------------ *)
(* Quantifier dualization under negation. *)

let test_quantifier_duals () =
  let x = Fol.fresh_binder ~name:"x" ~sort:Sort.int in
  let px = atom "P" in
  check_str
    "nnf (not (forall x. P))"
    ~expected:"(exists (x) (not P))"
    ~got:(show (nnf (Not (Forall ([ x ], px)))));
  check_str
    "nnf (not (exists x. P))"
    ~expected:"(forall (x) (not P))"
    ~got:(show (nnf (Not (Exists ([ x ], px)))));
  (* dualization descends: not (forall x. (and P Q)) = exists x. (or ~P ~Q) *)
  check_str
    "nnf (not (forall x. (and P Q)))"
    ~expected:"(exists (x) (or (not P) (not Q)))"
    ~got:(show (nnf (Not (Forall ([ x ], And [ atom "P"; atom "Q" ])))))
;;

(* ------------------------------------------------------------------ *)
(* Rename-apart: NNF duplicates a quantified operand of an Iff, sharing binder ids; after
   rename_apart every binder occurrence is distinct AND the atom refs track the new ids. *)

let has_dups l =
  let seen = Hashtbl.create 16 in
  List.exists
    (fun i ->
      if Hashtbl.mem seen i
      then true
      else (
        Hashtbl.replace seen i ();
        false))
    l
;;

let test_rename_apart () =
  let x = Fol.fresh_binder ~name:"x" ~sort:Sort.int in
  (* B = forall x. P(x); an atom P referencing x by id. *)
  let bformula = Fol.Forall ([ x ], atom "P" ~refs:[ x.id ]) in
  let phi = Fol.Iff (atom "A", bformula) in
  let n = nnf phi in
  (* NNF duplicated the quantifier over x: two binder occurrences sharing the same id. *)
  check "nnf shares binder ids before rename" (has_dups (Fol.binder_ids n));
  let r = Fol.rename_apart ~rename_atom n in
  check "rename_apart: all binder ids distinct" (not (has_dups (Fol.binder_ids r)));
  (* the two copies of the P atom now reference DIFFERENT ids (the two fresh x binders) *)
  let refs = ref [] in
  Fol.iter_atoms (fun a -> if String.equal a.lname "P" then refs := a.refs @ !refs) r;
  let distinct_ref_targets =
    List.sort_uniq compare (List.filter (fun i -> i >= 0) !refs)
  in
  check
    "rename_apart: the two P copies reference two distinct fresh binders"
    (List.length distinct_ref_targets = 2);
  (* each P copy's ref must equal a binder id actually in scope after renaming *)
  let bound = Fol.binder_ids r in
  check
    "rename_apart: every P ref is a live (renamed) binder id"
    (List.for_all (fun i -> List.mem i bound) distinct_ref_targets)
;;

(* ------------------------------------------------------------------ *)
(* Canonical exemplars at the NNF level (Skolemization is stage 2). *)

let test_canonical_rodin () =
  (* ¬(∀x.P(x) ∧ ∀y.Q(y)) → (∃x.¬P(x)) ∨ (∃y.¬Q(y)) *)
  let x = Fol.fresh_binder ~name:"x" ~sort:Sort.int in
  let y = Fol.fresh_binder ~name:"y" ~sort:Sort.int in
  let phi =
    Fol.Not
      (And
         [ Forall ([ x ], atom "P" ~refs:[ x.id ])
         ; Forall ([ y ], atom "Q" ~refs:[ y.id ])
         ])
  in
  check_str
    "canonical Rodin NNF"
    ~expected:"(or (exists (x) (not P)) (exists (y) (not Q)))"
    ~got:(show (nnf phi))
;;

let test_canonical_ufdt () =
  (* ∀s. A(s) ↔ ∀n.P(s,n) → ∀s. (¬A(s) ∨ ∀n.P) ∧ (∃n.¬P ∨ A(s)) *)
  let s = Fol.fresh_binder ~name:"s" ~sort:Sort.int in
  let n = Fol.fresh_binder ~name:"n" ~sort:Sort.int in
  let phi =
    Fol.Forall
      ([ s ], Iff (atom "A" ~refs:[ s.id ], Forall ([ n ], atom "P" ~refs:[ s.id; n.id ])))
  in
  check_str
    "canonical UFDT iff NNF"
    ~expected:"(forall (s) (and (or (not A) (forall (n) P)) (or (exists (n) (not P)) A)))"
    ~got:(show (nnf phi))
;;

let () =
  test_nnf_table ();
  test_quantifier_duals ();
  test_rename_apart ();
  test_canonical_rodin ();
  test_canonical_ufdt ();
  Printf.printf "fol_test: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
