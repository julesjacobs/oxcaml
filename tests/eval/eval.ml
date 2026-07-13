open Oxsmt_core

exception Eval_error of string

(* Overflow-guarded integer arithmetic: raise, never wrap (I8 spirit). *)
let add_ovf a b =
  let s = a + b in
  if (a >= 0 && b >= 0 && s < 0) || (a < 0 && b < 0 && s >= 0)
  then raise (Eval_error "integer overflow (addition)");
  s
;;

let neg_ovf a =
  if a = min_int then raise (Eval_error "integer overflow (negation of min_int)");
  -a
;;

let sub_ovf a b = add_ovf a (neg_ovf b)

let mul_ovf a b =
  if a = 0 || b = 0
  then 0
  else (
    let p = a * b in
    if p / a <> b || (a = -1 && b = min_int) || (b = -1 && a = min_int)
    then raise (Eval_error "integer overflow (multiplication)");
    p)
;;

(* Euclidean division/remainder (ADR-0003 Decision 5): 0 <= r < |d|, x = d*q + r. Derived
   from OCaml's truncated [/]/[mod] (remainder carries the dividend's sign) to avoid any
   overflowing intermediate: no [x - r] and no explicit [q] in the [mod] path. The one
   genuinely non-representable case, [min_int / -1] (whose true quotient -min_int
   overflows), raises rather than wrapping — an abstain (exit 2), never a wrong accept
   (codex E1). [d = min_int] also abstains: [abs min_int] is unrepresentable. *)

let guard_divisor d =
  if d = 0 then raise (Eval_error "division by zero");
  if d = min_int then raise (Eval_error "integer overflow (divisor = min_int)")
;;

(* Euclidean remainder alone — total for every representable [d <> 0, min_int]; [r] is
   always in [0, |d|) and its computation cannot overflow (we only ever add |d| to a
   strictly-negative truncated remainder, landing in (0, |d|)). *)
let euclid_rem x d =
  guard_divisor d;
  let tr = x mod d in
  if tr >= 0 then tr else tr + abs d
;;

(* Euclidean quotient. Guards [min_int / -1] before the hardware-overflowing [x / d]; the
   +/-1 truncated-to-euclidean adjustment is itself overflow-checked (abstain, never
   wrap). *)
let euclid_quot x d =
  guard_divisor d;
  if x = min_int && d = -1 then raise (Eval_error "integer overflow (min_int / -1)");
  let tq = x / d in
  let tr = x mod d in
  if tr >= 0 then tq else if d > 0 then sub_ovf tq 1 else add_ovf tq 1
;;

let as_int = function
  | Value.Int n -> n
  | v -> raise (Eval_error ("expected Int, got " ^ Value.to_string v))
;;

let as_bool = function
  | Value.Bool b -> b
  | v -> raise (Eval_error ("expected Bool, got " ^ Value.to_string v))
;;

let eval_general ~consts ~funs (root : Term.t) : Value.t =
  let rec go (t : Term.t) : Value.t =
    match t.node with
    | Term.Bool_const b -> Value.Bool b
    | Term.Int_const n -> Value.Int n
    | Term.Not a -> Value.Bool (not (as_bool (go a)))
    | Term.And xs ->
      (* fold so every operand is forced (model errors stay loud even past a false one) *)
      Value.Bool
        (Iarr.fold
           (fun acc x ->
              let b = as_bool (go x) in
              acc && b)
           true
           xs)
    | Term.Or xs ->
      Value.Bool
        (Iarr.fold
           (fun acc x ->
              let b = as_bool (go x) in
              acc || b)
           false
           xs)
    | Term.Ite (c, a, b) -> if as_bool (go c) then go a else go b
    | Term.Eq (a, b) -> Value.Bool (Value.equal (go a) (go b))
    | Term.Le arg -> Value.Bool (as_int (go arg) <= 0)
    | Term.Arith { coeffs; const } ->
      let sum =
        Iarr.fold
          (fun acc (ti, ci) -> add_ovf acc (mul_ovf ci (as_int (go ti))))
          const
          coeffs
      in
      Value.Int sum
    | Term.App (sym, args) -> eval_app sym args
  and eval_app sym args =
    let name = Symbol.name sym in
    match name, Iarr.length args with
    | "div", 2 ->
      Value.Int
        (euclid_quot (as_int (go (Iarr.get args 0))) (as_int (go (Iarr.get args 1))))
    | "mod", 2 ->
      Value.Int
        (euclid_rem (as_int (go (Iarr.get args 0))) (as_int (go (Iarr.get args 1))))
    | _, 0 ->
      (match consts sym with
       | Some v -> v
       | None -> raise (Eval_error ("model does not define constant " ^ name)))
    | _, _ ->
      (match funs sym with
       | None -> raise (Eval_error ("model does not define function " ^ name))
       | Some (table : Eval_model.fun_table) ->
         let argv = List.map go (Iarr.to_list args) in
         let matches (case_args, _) =
           List.length case_args = List.length argv
           && List.for_all2 Value.equal case_args argv
         in
         (match List.find_opt matches table.cases with
          | Some (_, res) -> res
          | None -> table.default))
  in
  go root
;;

let eval (model : Eval_model.t) (t : Term.t) : Value.t =
  eval_general
    ~consts:(Eval_model.lookup_const model)
    ~funs:(Eval_model.lookup_fun model)
    t
;;

let eval_term ~env (t : Term.t) : Value.t =
  eval_general ~consts:env ~funs:(fun _ -> None) t
;;

(* --- Failing-path rendering --------------------------------------------------------- *)

let head_and_children (t : Term.t) : string * Term.t list =
  match t.node with
  | Term.Bool_const b -> Bool.to_string b, []
  | Term.Int_const n -> Int.to_string n, []
  | Term.Not a -> "(not _)", [ a ]
  | Term.And xs -> "(and ...)", Iarr.to_list xs
  | Term.Or xs -> "(or ...)", Iarr.to_list xs
  | Term.Ite (c, a, b) -> "(ite _ _ _)", [ c; a; b ]
  | Term.Eq (a, b) -> "(= _ _)", [ a; b ]
  | Term.Le a -> "(<= _ 0)", [ a ]
  | Term.Arith { coeffs; const } ->
    Printf.sprintf "(linear +%d ...)" const, List.map fst (Iarr.to_list coeffs)
  | Term.App (sym, args) ->
    let name = Symbol.name sym in
    ( (if Iarr.length args = 0 then name else Printf.sprintf "(%s ...)" name)
    , Iarr.to_list args )
;;

let explain (model : Eval_model.t) (t : Term.t) : string =
  let buf = Buffer.create 256 in
  let rec go depth indent (node : Term.t) =
    let head, children = head_and_children node in
    let vstr =
      match eval model node with
      | v -> Value.to_string v
      | exception Eval_error msg -> "<error: " ^ msg ^ ">"
    in
    Buffer.add_string buf (Printf.sprintf "%s%s => %s\n" indent head vstr);
    if depth > 0 then List.iter (go (depth - 1) (indent ^ "  ")) children
  in
  go 4 "" t;
  Buffer.contents buf
;;

type outcome =
  | Satisfies
  | Fails of
      { index : int
      ; trace : string
      }

(* Formula-completeness (UF-models ADR §4 R7). The emitted model must be FORMULA-complete:
   it must bind every symbol that SYNTACTICALLY appears in the assertions and supply a
   [(sort S k)] cardinality entry for every uninterpreted sort any subterm has. Two points
   are deliberate:
   - Syntactic, not semantic: a symbol under an UNTAKEN [ite] branch still counts, so this
     is stricter than lazy evaluation would be. Rejecting a model that omits it is
     fail-closed — a completeness loss (abstain), never a wrong [Satisfies].
   - Declared-but-UNUSED symbols/sorts are NOT required (that is signature-completeness,
     which the ADR rejects for the emitted contract). The reserved [div]/[mod] built-ins
     (name + arity 2) are not model-bound and are skipped, mirroring {!eval_app}. Missing
     data is a loud {!Eval_model.Malformed}. *)
let require_formula_complete (model : Eval_model.t) (assertions : Term.t list) : unit =
  let seen : (int, unit) Hashtbl.t = Hashtbl.create 256 in
  let missing = ref [] in
  let note m = if not (List.mem m !missing) then missing := m :: !missing in
  let check_sort (sort : Sort.t) =
    match sort with
    | Sort.Uninterpreted sym ->
      let name = Symbol.name sym in
      if Option.is_none (Eval_model.sort_card model name)
      then note (Printf.sprintf "cardinality entry for sort %s" name)
    (* No datatype-sorted model completeness obligation yet: the datatype theory emits no
       model values, so a datatype query never reaches [Sat]-with-model here. *)
    | Sort.Bool | Sort.Int _ | Sort.Datatype _ -> ()
  in
  let check_app sym arity =
    let name = Symbol.name sym in
    if not ((name = "div" || name = "mod") && arity = 2)
    then (
      let bound =
        if arity = 0
        then Option.is_some (Eval_model.lookup_const model sym)
        else Option.is_some (Eval_model.lookup_fun model sym)
      in
      if not bound then note (Printf.sprintf "binding for symbol %s" name))
  in
  let rec walk (t : Term.t) =
    if not (Hashtbl.mem seen t.tag)
    then (
      Hashtbl.add seen t.tag ();
      check_sort t.sort;
      match t.node with
      | Term.Bool_const _ | Term.Int_const _ -> ()
      | Term.Not a -> walk a
      | Term.And xs | Term.Or xs -> Iarr.iter walk xs
      | Term.Ite (c, a, b) ->
        walk c;
        walk a;
        walk b
      | Term.Eq (a, b) ->
        walk a;
        walk b
      | Term.Le a -> walk a
      | Term.Arith { coeffs; _ } -> Iarr.iter (fun (ti, _) -> walk ti) coeffs
      | Term.App (sym, args) ->
        check_app sym (Iarr.length args);
        Iarr.iter walk args)
  in
  List.iter walk assertions;
  match List.rev !missing with
  | [] -> ()
  | ms ->
    raise
      (Eval_model.Malformed
         ("model is not formula-complete; missing " ^ String.concat ", " ms))
;;

let check (model : Eval_model.t) (assertions : Term.t list) : outcome =
  require_formula_complete model assertions;
  let rec loop i = function
    | [] -> Satisfies
    | t :: tl ->
      (match eval model t with
       | Value.Bool true -> loop (i + 1) tl
       | Value.Bool false -> Fails { index = i; trace = explain model t }
       | v ->
         raise (Eval_error ("assertion did not evaluate to Bool: " ^ Value.to_string v)))
  in
  loop 0 assertions
;;
