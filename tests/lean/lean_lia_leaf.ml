(* LIA Farkas theory-leaf -> Lean (lean-proofs lane, rung 2).

   Consumes a recorded [Recorder.theory_event] carrying a [lia_conflict_witness] (the
   Farkas multipliers) plus the certificate's atom table (SAT var -> core Term), and emits
   an explicit core-Lean-4 proof, over the reflective {!Farkas_prelude}, that the leaf's
   premise half-planes are jointly infeasible (a proof of [False]) — the theory-lemma
   validity of the leaf clause (which is the negation of those premises).

   Every inference is the certificate's: the row coefficients come from the recorded
   atoms, the multipliers from the witness. Lean only CHECKS — the variable cancellation
   is a ground [by decide] over the coefficient arithmetic, never a search, never [omega].

   Emitter-side NORMALIZATION of each premise into a single nonnegative-weighted
   half-plane [eval row rho <= 0] (documented, sound, mirrors the checker's
   [verify_lia_conflict]):
   - a positive [Le] premise [(e <= 0)] is the row [e] with the recorded (>=0) multiplier;
   - a negative [Le] premise [not (e <= 0)] over the integers is the strengthened row
     [-e + 1] (the standard LIA cut) with the recorded (>=0) multiplier;
   - an equality premise [a = b] contributes [m * (a - b)]; we emit the single directional
     half-plane the certificate uses ([a - b] if [m >= 0], else [b - a] with [-m]).
     Rational multipliers are cleared to integers by scaling every row by the product of
     the denominators (Farkas is scale-invariant: the combination still cancels and its
     constant stays strictly positive). *)

module Term = Oxsmt_core.Term
module Sort = Oxsmt_core.Sort
module Bigint = Oxsmt_core.Bigint
module Iarr = Oxsmt_core.Iarr
module Sat = Oxsmt_solver.Sat
module Recorder = Oxsmt_certificate.Recorder
module Rational = Oxsmt_lia.Rational

exception Gap of string

let gapf fmt = Printf.ksprintf (fun s -> raise (Gap s)) fmt

(* a linear form: (coefficient, atomic-term) pairs + integer constant. Mirrors the
   checker's [linear_of]. *)
type linform =
  { terms : (Term.t * Bigint.t) list
  ; const : Bigint.t
  }

let linear_of (t : Term.t) : linform =
  match t.node with
  | Term.Arith { coeffs; const } -> { terms = Iarr.to_list coeffs; const }
  | Term.Int_const c -> { terms = []; const = c }
  | _ -> { terms = [ t, Bigint.one ]; const = Bigint.zero }
;;

let neg_form (f : linform) : linform =
  { terms = List.map (fun (t, c) -> t, Bigint.neg c) f.terms; const = Bigint.neg f.const }
;;

let sub_form (a : linform) (b : linform) : linform =
  { terms = a.terms @ List.map (fun (t, c) -> t, Bigint.neg c) b.terms
  ; const = Bigint.sub a.const b.const
  }
;;

let scale_form (m : Bigint.t) (f : linform) : linform =
  { terms = List.map (fun (t, c) -> t, Bigint.mul m c) f.terms
  ; const = Bigint.mul m f.const
  }
;;

(* one normalized reflective premise: a nonnegative integer multiplier applied to a
   half-plane [row <= 0]. *)
type entry =
  { mult : Bigint.t
  ; row : linform
  }

(* Build the normalized entries + the SORTED distinct atomic terms (their list position is
   the reflective variable index). Requires the integer-cleared multipliers. *)
let entries_of_witness
  ~(resolve : Sat.var -> Term.t option)
  (event : Recorder.theory_event)
  (witness : Recorder.lia_conflict_witness)
  : entry list
  =
  let premises = witness.Recorder.premises in
  if premises = [] then gapf "empty Farkas witness";
  (* clear denominators: D = product of all denominators; integer mult_i = num_i *
     D/den_i, computed as num_i * product_[{j<>i}] den_j to avoid Bigint division. *)
  let dens =
    List.map
      (fun (p : Recorder.lia_premise) -> Rational.den_bigint p.Recorder.multiplier)
      premises
  in
  let int_mult (i : int) (p : Recorder.lia_premise) : Bigint.t =
    let num = Rational.num_bigint p.Recorder.multiplier in
    let prod_other =
      List.fold_left
        (fun acc (j, d) -> if j = i then acc else Bigint.mul acc d)
        Bigint.one
        (List.mapi (fun j d -> j, d) dens)
    in
    Bigint.mul num prod_other
  in
  ignore event;
  List.mapi
    (fun i (p : Recorder.lia_premise) ->
      let polarity = Sat.sign_of_lit p.Recorder.lit in
      let m = int_mult i p in
      match resolve (Sat.var_of_lit p.Recorder.lit) with
      | None -> gapf "Farkas premise has no theory-atom declaration"
      | Some atom ->
        (match atom.Term.node with
         | Term.Le arg ->
           if not (Sort.equal arg.Term.sort Sort.int)
           then gapf "non-integer Le premise (LRA out of scope for reflective Int Farkas)";
           if Bigint.compare m Bigint.zero < 0
           then gapf "negative cleared multiplier on an inequality premise";
           if polarity
           then { mult = m; row = linear_of arg }
           else (
             (* not (e <= 0) over Int => -e + 1 <= 0 *)
             let f = linear_of arg in
             { mult = m
             ; row =
                 { (neg_form f) with const = Bigint.add (Bigint.neg f.const) Bigint.one }
             })
         | Term.Eq (a, b) ->
           if not polarity then gapf "disequality premise is not a Farkas half-plane";
           if not (Sort.equal a.Term.sort Sort.int)
           then gapf "non-integer equality premise (out of scope)";
           (* m * (a - b): emit the single directional half-plane the certificate uses. *)
           if Bigint.compare m Bigint.zero >= 0
           then { mult = m; row = sub_form (linear_of a) (linear_of b) }
           else { mult = Bigint.neg m; row = sub_form (linear_of b) (linear_of a) }
         | _ -> gapf "Farkas premise is not an integer <= or = atom"))
    premises
;;

(* Assign each distinct atomic term a stable reflective variable index (first-seen order). *)
let index_terms (entries : entry list) : (Term.t, int) Hashtbl.t =
  let tbl = Hashtbl.create 32 in
  let counter = ref 0 in
  List.iter
    (fun e ->
      List.iter
        (fun (t, _) ->
          if not (Hashtbl.mem tbl t)
          then (
            Hashtbl.replace tbl t !counter;
            incr counter))
        e.row.terms)
    entries;
  tbl
;;

let lean_int b = Printf.sprintf "(%s : Int)" (Bigint.to_string b)

let render_linform (idx : (Term.t, int) Hashtbl.t) (f : linform) : string =
  let terms =
    List.map
      (fun (t, c) -> Printf.sprintf "(%s, %d)" (lean_int c) (Hashtbl.find idx t))
      f.terms
  in
  Printf.sprintf "([%s], %s)" (String.concat ", " terms) (lean_int f.const)
;;

(* Emit the leaf theorem (assumes {!Farkas_prelude} text is prepended and OxsmtFarkas is
   opened by the caller). Returns the theorem source. *)
let emit_theorem ~(name : string) (entries : entry list) : string =
  let idx = index_terms entries in
  let rows = List.map (fun e -> render_linform idx e.row) entries in
  let list_elems =
    List.map2 (fun e r -> Printf.sprintf "(%s, %s)" (lean_int e.mult) r) entries rows
  in
  let hyps =
    List.mapi (fun i r -> Printf.sprintf "  (h%d : eval %s rho <= 0)" i r) rows
  in
  let arms =
    List.mapi (fun i _ -> Printf.sprintf "  · exact ⟨by decide, h%d⟩" i) rows
    |> String.concat "\n"
  in
  let rcases =
    match rows with
    | [] -> gapf "no premises"
    | [ _ ] -> "rfl"
    | _ -> String.concat " | " (List.map (fun _ -> "rfl") rows)
  in
  Printf.sprintf
    "theorem %s (rho : Assign)\n\
     %s\n\
    \    : False := by\n\
    \  refine farkas_false [%s] rho ?_ (by decide) (by decide)\n\
    \  intro me hme\n\
    \  simp only [List.mem_cons, List.not_mem_nil, or_false] at hme\n\
    \  rcases hme with %s\n\
     %s\n"
    name
    (String.concat "\n" hyps)
    (String.concat ", " list_elems)
    rcases
    arms
;;

(* Full emitted file body (open + theorem); the prelude is prepended by the driver. *)
let emit_lia_leaf
  ~(resolve : Sat.var -> Term.t option)
  ~(name : string)
  (event : Recorder.theory_event)
  (witness : Recorder.lia_conflict_witness)
  : string
  =
  let entries = entries_of_witness ~resolve event witness in
  Printf.sprintf "open OxsmtFarkas\n\n%s" (emit_theorem ~name entries)
;;
