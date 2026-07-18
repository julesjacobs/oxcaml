(* Certificate/model -> Lean 4 TRANSLATION (lean-proofs lane).

   This is NOT the tests/gate encoder: that one re-solves the query in Lean via
   grind/decide-on-open-goal. This module NEVER asks Lean to solve. It takes oxsmt's OWN
   output — for [Sat], the reconstructed model; for [Unsat] (later rungs), the recorded
   certificate — and emits a self-contained core-Lean-4 source file (no mathlib, no lake)
   whose kernel check succeeds iff oxsmt's verdict is corroborated:

   - SAT: the model is rendered as concrete witnesses. Every uninterpreted-sort element is
     its integer index (equality of elements = integer equality — faithful for the
     equality/disequality-only operations of the quantifier-free fragment). Every
     uninterpreted function/predicate with a finite [get_model] table is emitted as a
     concrete Lean [def] by cases + default. Each original assertion is lowered to a
     CLOSED GROUND [Prop], and their conjunction is proved by [decide] — pure kernel
     EVALUATION of a ground formula under the given model, not proof search. A companion
     "refutation control" claims the NEGATION of that same conjunction and MUST be
     rejected by the kernel: that is what shows the check can fail (a wrong model is
     rejected, not passed).

   Anything the model layer does not carry self-checkably (arrays, datatypes, Reals — no
   rational in core Lean) is a LOUD gap ([Gap]); it degrades to a documented non-result,
   never a fake proof. *)

module Term = Oxsmt_core.Term
module Sort = Oxsmt_core.Sort
module Symbol = Oxsmt_core.Symbol
module Iarr = Oxsmt_core.Iarr
module Bigint = Oxsmt_core.Bigint
module Session = Oxsmt_interface.Session

(* Raised while lowering when we hit a construct this rung cannot faithfully translate.
   Caught at the top and turned into [Unsupported]. *)
exception Gap of string

let gapf fmt = Printf.ksprintf (fun s -> raise (Gap s)) fmt

(* A resolved model: nullary consts by name, and function tables by name paired with a
   fresh collision-free Lean identifier + the rank recovered from a representative
   application in the assertions. *)
type fun_info =
  { lean_name : string
  ; arg_sorts : Sort.t list
  ; result_sort : Sort.t
  ; table : Session.fun_table
  }

type resolved =
  { consts : (string, Session.model_value) Hashtbl.t
  ; funs : (string, fun_info) Hashtbl.t
  }

(* Render a Bigint as a Lean [Int] literal, always parenthesised (negatives). *)
let lean_int_of_bigint b = Printf.sprintf "(%s : Int)" (Bigint.to_string b)

(* The Lean data-TYPE for an SMT sort used in a function signature / value position: Int
   and uninterpreted (element index) -> Int; Bool -> Bool. Others are gaps. *)
let lean_type_of_sort (s : Sort.t) : string =
  match s with
  | Sort.Int _ | Sort.Uninterpreted _ -> "Int"
  | Sort.Bool -> "Bool"
  | Sort.Real -> gapf "Real (no core-Lean rational)"
  | Sort.Datatype _ -> gapf "datatype sort in model"
  | Sort.Array _ -> gapf "array sort in model"
  | Sort.BitVec _ -> gapf "bitvector sort in model"
;;

(* Render a model value at a known SMT sort, as a Lean literal of [lean_type_of_sort]. *)
let lean_value_at_sort (s : Sort.t) (v : Session.model_value) : string =
  match s, v with
  | (Sort.Int _ | Sort.Uninterpreted _), Session.VInt n -> lean_int_of_bigint n
  | (Sort.Int _ | Sort.Uninterpreted _), Session.VUninterp i ->
    Printf.sprintf "(%d : Int)" i
  | Sort.Bool, Session.VBool b -> if b then "true" else "false"
  | _, Session.VReal _ -> gapf "Real value (no core-Lean rational)"
  | _ -> gapf "model value does not match its declared sort"
;;

(* Pre-resolve: bind every function table from the model to a Lean name + rank discovered
   from a representative application in the assertions. A table the assertions never apply
   is simply unused (dropped). A symbol applied in the assertions but absent from the
   model is a loud gap (an unconstrained application in a "self-checkable" model). *)
let resolve_model ~(bindings : Session.model_binding list) ~(assertions : Term.t list)
  : resolved
  =
  let consts = Hashtbl.create 32 in
  let tables = Hashtbl.create 16 in
  List.iter
    (fun (b : Session.model_binding) ->
      match b with
      | Session.Const (name, v) -> Hashtbl.replace consts name v
      | Session.Fun (name, tbl) -> Hashtbl.replace tables name tbl)
    bindings;
  let funs = Hashtbl.create 16 in
  let counter = ref 0 in
  let rec scan (t : Term.t) =
    match t.node with
    | Term.App (sym, args) when Iarr.length args = 0 -> ignore sym
    | Term.App (sym, args) ->
      let name = Symbol.name sym in
      let arg_list = Iarr.to_list args in
      if not (Hashtbl.mem funs name)
      then (
        match Hashtbl.find_opt tables name with
        | None -> gapf "applied symbol %s has no function table in the model" name
        | Some table ->
          let lean_name = Printf.sprintf "fn%d_%s" !counter (Symbol.name sym) in
          incr counter;
          let lean_name =
            String.map
              (fun c ->
                if (c >= 'a' && c <= 'z')
                   || (c >= 'A' && c <= 'Z')
                   || (c >= '0' && c <= '9')
                   || c = '_'
                then c
                else '_')
              lean_name
          in
          Hashtbl.replace
            funs
            name
            { lean_name
            ; arg_sorts = List.map (fun (a : Term.t) -> a.sort) arg_list
            ; result_sort = t.sort
            ; table
            });
      List.iter scan arg_list
    | Term.Arith { coeffs; _ } -> List.iter (fun (c, _) -> scan c) (Iarr.to_list coeffs)
    | Term.Real_arith { coeffs; _ } ->
      List.iter (fun (c, _) -> scan c) (Iarr.to_list coeffs)
    | Term.Le a -> scan a
    | Term.Eq (a, b) ->
      scan a;
      scan b
    | Term.Not a -> scan a
    | Term.And xs | Term.Or xs -> List.iter scan (Iarr.to_list xs)
    | Term.Ite (a, b, c) ->
      scan a;
      scan b;
      scan c
    | Term.Bool_const _ | Term.Int_const _ | Term.Real_const _ -> ()
  in
  List.iter scan assertions;
  { consts; funs }
;;

(* ---- lowering ---- *)

let const_value resolved (sym : Symbol.t) : Session.model_value =
  let name = Symbol.name sym in
  match Hashtbl.find_opt resolved.consts name with
  | Some v -> v
  | None -> gapf "model omits a value for %s" name
;;

(* Lower an Int- or uninterpreted-sorted term to a ground Lean [Int] expression. *)
let rec int_of_term resolved (t : Term.t) : string =
  match t.node with
  | Term.Int_const n -> lean_int_of_bigint n
  | Term.App (sym, args) when Iarr.length args = 0 ->
    (match const_value resolved sym with
     | Session.VInt n -> lean_int_of_bigint n
     | Session.VUninterp i -> Printf.sprintf "(%d : Int)" i
     | Session.VBool _ -> gapf "Bool value for Int/element symbol %s" (Symbol.name sym)
     | Session.VReal _ -> gapf "Real value (no core-Lean rational)")
  | Term.App (sym, args) -> apply_fun resolved sym (Iarr.to_list args)
  | Term.Arith { coeffs; const } ->
    let terms =
      List.map
        (fun (child, coeff) ->
          Printf.sprintf
            "(%s * %s)"
            (lean_int_of_bigint coeff)
            (int_of_term resolved child))
        (Iarr.to_list coeffs)
    in
    "(" ^ String.concat " + " (lean_int_of_bigint const :: terms) ^ ")"
  | Term.Ite (c, a, b) ->
    Printf.sprintf
      "(cond %s %s %s)"
      (bool_of_term resolved c)
      (int_of_term resolved a)
      (int_of_term resolved b)
  | Term.Real_const _ | Term.Real_arith _ ->
    gapf "Real arithmetic (no core-Lean rational)"
  | Term.Bool_const _ | Term.Le _ | Term.Eq _ | Term.Not _ | Term.And _ | Term.Or _ ->
    gapf "Bool-sorted node in Int position (ill-sorted?)"

(* Lower a Bool-sorted term to a Lean [Bool] DATA expression (for function arguments and
   inside emitted function bodies). *)
and bool_of_term resolved (t : Term.t) : string =
  match t.node with
  | Term.Bool_const b -> if b then "true" else "false"
  | Term.App (sym, args) when Iarr.length args = 0 ->
    (match const_value resolved sym with
     | Session.VBool b -> if b then "true" else "false"
     | _ -> gapf "non-Bool value for Bool symbol %s" (Symbol.name sym))
  | Term.App (sym, args) -> apply_fun resolved sym (Iarr.to_list args)
  | Term.Le arg ->
    (match arg.sort with
     | Sort.Int _ ->
       Printf.sprintf "(decide (%s <= (0 : Int)))" (int_of_term resolved arg)
     | Sort.Real -> gapf "Real inequality (no core-Lean rational)"
     | _ -> gapf "Le over non-arith sort")
  | Term.Eq (a, b) ->
    (match a.sort with
     | Sort.Bool ->
       Printf.sprintf "(%s == %s)" (bool_of_term resolved a) (bool_of_term resolved b)
     | Sort.Int _ | Sort.Uninterpreted _ ->
       Printf.sprintf
         "(decide (%s = %s))"
         (int_of_term resolved a)
         (int_of_term resolved b)
     | Sort.Real -> gapf "Real equality (no core-Lean rational)"
     | _ -> gapf "Eq over unsupported sort")
  | Term.Not a -> Printf.sprintf "(!%s)" (bool_of_term resolved a)
  | Term.And xs ->
    "(" ^ String.concat " && " (List.map (bool_of_term resolved) (Iarr.to_list xs)) ^ ")"
  | Term.Or xs ->
    "(" ^ String.concat " || " (List.map (bool_of_term resolved) (Iarr.to_list xs)) ^ ")"
  | Term.Ite (c, a, b) ->
    Printf.sprintf
      "(cond %s %s %s)"
      (bool_of_term resolved c)
      (bool_of_term resolved a)
      (bool_of_term resolved b)
  | Term.Int_const _ | Term.Arith _ | Term.Real_const _ | Term.Real_arith _ ->
    gapf "arith-sorted node in Bool position (ill-sorted?)"

(* Emit an application [(fnK a0 a1 ...)] lowering each argument by its own sort. Works in
   both Int and Bool result positions (the emitted def's return type matches). *)
and apply_fun resolved (sym : Symbol.t) (args : Term.t list) : string =
  let name = Symbol.name sym in
  match Hashtbl.find_opt resolved.funs name with
  | None -> gapf "applied symbol %s not resolved to a table" name
  | Some fi ->
    let arg_strs =
      List.map
        (fun (a : Term.t) ->
          match a.sort with
          | Sort.Int _ | Sort.Uninterpreted _ -> int_of_term resolved a
          | Sort.Bool -> bool_of_term resolved a
          | _ -> gapf "unsupported argument sort to %s" name)
        args
    in
    "(" ^ String.concat " " (fi.lean_name :: arg_strs) ^ ")"
;;

(* Lower a Bool-sorted term to a Lean [Prop] (top-level assertions: nicer output + a clean
   [Not]-based refutation control). *)
let rec prop_of_term resolved (t : Term.t) : string =
  match t.node with
  | Term.Bool_const true -> "True"
  | Term.Bool_const false -> "False"
  | Term.App (sym, args) when Iarr.length args = 0 ->
    (match const_value resolved sym with
     | Session.VBool true -> "True"
     | Session.VBool false -> "False"
     | _ -> gapf "non-Bool value for Bool symbol %s" (Symbol.name sym))
  | Term.App (sym, args) ->
    (* predicate application: the emitted def returns Bool; assert it equals true. *)
    Printf.sprintf "(%s = true)" (apply_fun resolved sym (Iarr.to_list args))
  | Term.Le arg ->
    (match arg.sort with
     | Sort.Int _ -> Printf.sprintf "(%s <= (0 : Int))" (int_of_term resolved arg)
     | Sort.Real -> gapf "Real inequality (no core-Lean rational)"
     | _ -> gapf "Le over non-arith sort")
  | Term.Eq (a, b) ->
    (match a.sort with
     | Sort.Bool ->
       Printf.sprintf "(%s <-> %s)" (prop_of_term resolved a) (prop_of_term resolved b)
     | Sort.Int _ | Sort.Uninterpreted _ ->
       Printf.sprintf "(%s = %s)" (int_of_term resolved a) (int_of_term resolved b)
     | Sort.Real -> gapf "Real equality (no core-Lean rational)"
     | _ -> gapf "Eq over unsupported sort")
  | Term.Not a -> Printf.sprintf "(Not %s)" (prop_of_term resolved a)
  | Term.And xs ->
    "(" ^ String.concat " /\\ " (List.map (prop_of_term resolved) (Iarr.to_list xs)) ^ ")"
  | Term.Or xs ->
    "(" ^ String.concat " \\/ " (List.map (prop_of_term resolved) (Iarr.to_list xs)) ^ ")"
  | Term.Ite (c, a, b) ->
    (* mixed: Prop branches guarded by a Bool-data condition. *)
    Printf.sprintf
      "(if %s = true then %s else %s)"
      (bool_of_term resolved c)
      (prop_of_term resolved a)
      (prop_of_term resolved b)
  | Term.Int_const _ | Term.Arith _ | Term.Real_const _ | Term.Real_arith _ ->
    gapf "arith-sorted node in Bool position (ill-sorted?)"
;;

(* ---- function definitions ---- *)

let emit_fun_def (fi : fun_info) : string =
  let params =
    List.mapi
      (fun i s -> Printf.sprintf "(x%d : %s)" i (lean_type_of_sort s))
      fi.arg_sorts
  in
  let ret = lean_type_of_sort fi.result_sort in
  let default = lean_value_at_sort fi.result_sort fi.table.default in
  (* Build the case chain from the LAST case backwards, folding into the default. Each
     case matches its argument tuple exactly (first match wins in the table -> outermost
     if first). *)
  let case_cond (args : Session.model_value list) : string =
    let conds =
      List.mapi
        (fun i (v : Session.model_value) ->
          let s = List.nth fi.arg_sorts i in
          Printf.sprintf "x%d = %s" i (lean_value_at_sort s v))
        args
    in
    match conds with
    | [] -> "True"
    | _ -> String.concat " /\\ " conds
  in
  let body =
    List.fold_left
      (fun acc (args, result) ->
        let cond = case_cond args in
        let res = lean_value_at_sort fi.result_sort result in
        Printf.sprintf "if %s then %s else %s" cond res acc)
      default
      (List.rev fi.table.cases)
  in
  Printf.sprintf "def %s %s : %s := %s\n" fi.lean_name (String.concat " " params) ret body
;;

(* Axiom-whitelist gate. Parse the [#print axioms NAME] output and require the proof to
   depend on NOTHING outside [{propext, Quot.sound}]. Any of sorryAx / Classical.choice /
   Lean.ofReduceBool / ofReduceNat (or any other axiom) is a HARD FAIL — this is what
   keeps a `sorry`, a native-`decide` oracle, or a smuggled classical axiom out of the
   proofs. Returns [Ok ()] if the axioms are within the whitelist (including "does not
   depend on any axioms"), else [Error msg].

   Hardening (rider R5, defends codex finding F6): (1) reject output that carries MORE
   than one axioms line — a smuggled clean [#print axioms] prepended before the real
   theorem can no longer whitewash the verdict; the emitters each produce EXACTLY one. (2)
   When [?theorem_name] is given, the sole axioms line must name that theorem ([Lean]
   prints ['<name>' depends on axioms: …] / ['<name>' does not depend on any axioms]) — so
   the gate reads the axioms OF THE THEOREM IT CHECKED, not of some other declaration. *)
let allowed_axioms = [ "propext"; "Quot.sound" ]

let is_sub hay needle =
  let nl = String.length needle
  and hl = String.length hay in
  let rec loop i =
    if i + nl > hl
    then false
    else if String.sub hay i nl = needle
    then true
    else loop (i + 1)
  in
  nl = 0 || loop 0
;;

let check_axioms ?theorem_name (lean_output : string) : (unit, string) result =
  let lines = String.split_on_char '\n' lean_output in
  let axiom_lines =
    List.filter
      (fun l -> is_sub l "does not depend on any axioms" || is_sub l "depends on axioms")
      lines
  in
  match axiom_lines with
  | [] -> Error "no `#print axioms` line found (proof may not have elaborated)"
  | _ :: _ :: _ ->
    Error
      (Printf.sprintf
         "expected exactly one `#print axioms` line, found %d (possible prepended-axioms \
          bypass)"
         (List.length axiom_lines))
  | [ line ] ->
    let name_ok =
      match theorem_name with
      | None -> true
      | Some n -> is_sub line ("'" ^ n ^ "'")
    in
    if not name_ok
    then
      Error
        (Printf.sprintf
           "axioms line does not name the checked theorem %s: %s"
           (Option.value ~default:"?" theorem_name)
           line)
    else if is_sub line "does not depend on any axioms"
    then Ok ()
    else (
      (* extract the [...] list *)
      match String.index_opt line '[', String.rindex_opt line ']' with
      | Some i, Some j when j > i ->
        let inner = String.sub line (i + 1) (j - i - 1) in
        let names =
          String.split_on_char ',' inner
          |> List.map String.trim
          |> List.filter (fun s -> s <> "")
        in
        let bad = List.filter (fun n -> not (List.mem n allowed_axioms)) names in
        if bad = [] then Ok () else Error ("forbidden axiom(s): " ^ String.concat " " bad)
      | _ -> Error ("unparseable axioms line: " ^ line))
;;

type sat_source =
  { positive : string (* proves the assertions hold under the model *)
  ; refutation_control : string (* claims the NEGATION; MUST be rejected by the kernel *)
  }

(* Build the two Lean sources for a SAT model + assertion batch. Raises {!Gap} (caught by
   caller) on any unsupported construct. An empty assertion batch is a degenerate [True]. *)
let emit_sat ~(model : Session.model) ~(assertions : Term.t list) : sat_source =
  let _sort_cards, bindings = model in
  let resolved = resolve_model ~bindings ~assertions in
  let defs =
    Hashtbl.fold (fun _ fi acc -> emit_fun_def fi :: acc) resolved.funs []
    |> List.sort compare
    |> String.concat ""
  in
  (* Top level is a Bool COMPUTATION, not a Prop: [decide] then only needs
     [DecidableEq Bool] and the kernel reduces the whole Bool expression. This sidesteps
     Decidable-instance synthesis over a deeply nested [∧] (which overflows the instance
     search on e.g. an n-element quasigroup's all-distinct constraints), while remaining
     pure kernel evaluation of the model — no proof search. [prop_of_term] is retained for
     future readable-Prop uses. *)
  ignore prop_of_term;
  let conj =
    match assertions with
    | [] -> "true"
    | _ -> "(" ^ String.concat " && " (List.map (bool_of_term resolved) assertions) ^ ")"
  in
  let header =
    "-- oxsmt SAT model, kernel-EVALUATED (lean-proofs lane). Core Lean 4, no mathlib.\n\
     -- `decide` here is ground evaluation of a Bool under a fixed model, not proof \
     search.\n\
     set_option maxRecDepth 8000\n"
  in
  { positive =
      Printf.sprintf
        "%s%stheorem oxsmt_sat : %s = true := by decide\n#print axioms oxsmt_sat\n"
        header
        defs
        conj
  ; refutation_control =
      Printf.sprintf
        "%s%s-- NEGATIVE CONTROL: the model satisfies the assertions, so the conjunction \
         is\n\
         -- true; claiming it is false MUST be rejected by the kernel.\n\
         example : %s = false := by decide\n"
        header
        defs
        conj
  }
;;
