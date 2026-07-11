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

(* Euclidean division/remainder (ADR-0003 Decision 5): 0 <= r < |d|, x = d*q + r. *)
let euclid x d =
  if d = 0 then raise (Eval_error "division by zero");
  if d = min_int then raise (Eval_error "integer overflow (divisor = min_int)");
  let ad = abs d in
  let r = ((x mod ad) + ad) mod ad in
  let q = sub_ovf x r / d in
  q, r
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
      let q, _ = euclid (as_int (go (Iarr.get args 0))) (as_int (go (Iarr.get args 1))) in
      Value.Int q
    | "mod", 2 ->
      let _, r = euclid (as_int (go (Iarr.get args 0))) (as_int (go (Iarr.get args 1))) in
      Value.Int r
    | _, 0 ->
      (match consts sym with
       | Some v -> v
       | None -> raise (Eval_error ("model does not define constant " ^ name)))
    | _, _ ->
      (match funs sym with
       | None -> raise (Eval_error ("model does not define function " ^ name))
       | Some (table : Model.fun_table) ->
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

let eval (model : Model.t) (t : Term.t) : Value.t =
  eval_general ~consts:(Model.lookup_const model) ~funs:(Model.lookup_fun model) t
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

let explain (model : Model.t) (t : Term.t) : string =
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

let check (model : Model.t) (assertions : Term.t list) : outcome =
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
