open Vox_smt

external solve : string -> int = "caml_vox_browser_solve"
external event : string -> unit = "caml_vox_browser_event"

let installed = ref false
let count = ref 0

let json s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (function
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c when Char.code c < 32 ->
          Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let array f xs = "[" ^ String.concat "," (List.map f xs) ^ "]"

let kind = function
  | Vox_smt.Bool -> "bool"
  | Int63 -> "int63"
  | Int -> "bigint"
  | Opaque _ -> "opaque"
  | Datatype _ -> "datatype"

let describe (query : Vox_smt.query) =
  let names label prefix items =
    List.mapi
      (fun i item ->
        let name = label item in
        let duplicate =
          List.length (List.filter (fun x -> label x = name) items) > 1
        in
        ( item,
          if duplicate then name ^ "[" ^ prefix ^ string_of_int i ^ "]"
          else name ))
      items
  in
  let symbols = names Vox_smt.Symbol.label "v" query.symbols in
  let functions = names Vox_smt.Function.label "f" query.functions in
  let definitions =
    List.filter_map
      (fun (fact : Vox_smt.labelled_term) ->
        match fact.term with
        | App (Eq, [ Var v; rhs ])
          when (fact.label = "value" || fact.label = "reachable")
               && (Symbol.label v = "value" || Symbol.label v = "reachable") ->
            Some (v, rhs)
        | _ -> None)
      query.facts
  in
  let rec variables = function
    | Var v -> [ v ]
    | App (_, args) | Call (_, args) | Construct (_, args) ->
        List.concat_map variables args
    | Is (_, t) | Select (_, _, t) -> variables t
    | _ -> []
  in
  let cycle_cache = Hashtbl.create 32 in
  let rec acyclic seen v =
    if List.mem v seen then false
    else
      match Hashtbl.find_opt cycle_cache v with
      | Some result -> result
      | None ->
          let result =
            match List.assoc_opt v definitions with
            | None -> true
            | Some rhs -> List.for_all (acyclic (v :: seen)) (variables rhs)
          in
          Hashtbl.replace cycle_cache v result;
          result
  in
  let definitions =
    List.filter
      (fun (v, _) ->
        List.length (List.filter (fun (w, _) -> v = w) definitions) = 1
        && acyclic [] v)
      definitions
  in
  let parts op = function
    | Vox_smt.App (op', xs) when op = op' -> xs
    | x -> [ x ]
  in
  let join op xs =
    let identity = op = Vox_smt.And in
    let xs = List.concat_map (parts op) xs |> List.sort_uniq compare in
    if List.mem (Vox_smt.Boolean (not identity)) xs then
      Vox_smt.Boolean (not identity)
    else
      match List.filter (fun x -> x <> Vox_smt.Boolean identity) xs with
      | [] -> Boolean identity
      | [ x ] -> x
      | xs -> App (op, xs)
  in
  let memo = Hashtbl.create 32 in
  let rec expand seen t =
    match t with
    | Vox_smt.Var v when not (List.mem v seen) -> (
        match Hashtbl.find_opt memo v with
        | Some t -> t
        | None -> (
            match List.assoc_opt v definitions with
            | None -> t
            | Some rhs ->
                let t = expand (v :: seen) rhs in
                Hashtbl.replace memo v t;
                t))
    | App (op, args) -> (
        let args = List.map (expand seen) args in
        match (op, args) with
        | Not, [ Boolean b ] -> Boolean (not b)
        | Not, [ App (Not, [ x ]) ] -> x
        | Eq, [ a; b ] when a = b -> Boolean true
        | Ite, [ Boolean b; a; c ] -> if b then a else c
        | Ite, [ c; a; Boolean false ] -> join And [ c; a ]
        | Ite, [ c; Boolean true; a ] -> join Or [ c; a ]
        | And, xs -> join And xs
        | Or, [ a; b ] ->
            let aa = parts And a and bb = parts And b in
            let common = List.filter (fun x -> List.mem x bb) aa in
            let left =
              join And (List.filter (fun x -> not (List.mem x common)) aa)
            in
            let right =
              join And (List.filter (fun x -> not (List.mem x common)) bb)
            in
            let rest =
              if left = App (Not, [ right ]) || right = App (Not, [ left ]) then
                Boolean true
              else join Or [ left; right ]
            in
            join And [ join And common; rest ]
        | Or, xs -> join Or xs
        | _ -> App (op, args))
    | Call (f, args) -> Call (f, List.map (expand seen) args)
    | Construct (c, args) -> Construct (c, List.map (expand seen) args)
    | Is (c, t) -> Is (c, expand seen t)
    | Select (c, i, t) -> Select (c, i, expand seen t)
    | _ -> t
  in
  let expanded_goal = expand [] query.goal.term in
  let conditions, conclusion =
    match expanded_goal with
    | App (Implies, [ condition; conclusion ]) ->
        (parts And condition, conclusion)
    | goal -> ([], goal)
  in
  let assumptions =
    List.filter_map
      (fun (fact : Vox_smt.labelled_term) ->
        match fact.term with
        | App (Eq, [ Var v; _ ]) when List.mem_assoc v definitions -> None
        | _ -> Some { fact with term = expand [] fact.term })
      query.facts
  in
  let assumptions =
    assumptions
    @ List.filter_map
        (function
          | Vox_smt.Boolean true -> None
          | term -> Some { Vox_smt.label = "path condition"; term })
        conditions
  in
  let rec term = function
    | Vox_smt.Boolean b -> string_of_bool b
    | Integer n -> Int64.to_string n
    | Big_integer n -> n ^ "Z"
    | Var s -> List.assoc s symbols
    | Call (f, args) -> application (List.assoc f functions) args
    | Construct (c, args) -> application (Vox_smt.Constructor.label c) args
    | Is (c, t) -> application ("is " ^ Vox_smt.Constructor.label c) [ t ]
    | Select (c, index, t) ->
        term t ^ "." ^ fst (List.nth (Vox_smt.Constructor.fields c) index)
    | App (op, args) -> (
        let name =
          match op with
          | Add | Int_add -> "+"
          | Sub | Int_sub -> "-"
          | Mul | Int_mul -> "*"
          | Div | Int_div -> "/"
          | Rem | Int_mod -> "mod"
          | Neg | Int_neg -> "negate"
          | Eq -> "="
          | Ne -> "<>"
          | Lt | Int_lt -> "<"
          | Le | Int_le -> "<="
          | Gt | Int_gt -> ">"
          | Ge | Int_ge -> ">="
          | Not -> "not"
          | And -> "&&"
          | Or -> "||"
          | Implies -> "implies"
          | Ite -> "if"
          | Bit_and -> "land"
          | Bit_or -> "lor"
          | Bit_xor -> "lxor"
          | Shift_right_logical -> "lsr"
          | Int_of_int63 -> "Bigint.of_int"
        in
        match args with
        | _ :: _ :: _ when op = And || op = Or ->
            "(" ^ String.concat (" " ^ name ^ " ") (List.map term args) ^ ")"
        | [ left; right ] ->
            "(" ^ term left ^ " " ^ name ^ " " ^ term right ^ ")"
        | _ -> application name args)
  and application name args =
    name ^ "(" ^ String.concat ", " (List.map term args) ^ ")"
  in
  let labelled (t : Vox_smt.labelled_term) =
    Printf.sprintf "{\"label\":%s,\"text\":%s}" (json t.label)
      (json (term t.term))
  in
  let symbol_json =
    List.mapi
      (fun i (s, name) ->
        Printf.sprintf
          "{\"id\":\"v%d\",\"name\":%s,\"kind\":%s,\"internal\":%b}" i
          (json name)
          (json (kind (Vox_smt.Symbol.sort s)))
          (List.mem_assoc s definitions))
      symbols
  in
  let constructors =
    List.concat_map
      (fun (d : Vox_smt.datatype_declaration) -> d.constructors)
      query.datatypes
  in
  let constructors =
    List.mapi
      (fun i c ->
        Printf.sprintf "{\"id\":\"c%d\",\"name\":%s,\"fields\":%s}" i
          (json (Vox_smt.Constructor.label c))
          (array
             (fun (name, sort) ->
               Printf.sprintf "{\"name\":%s,\"kind\":%s}" (json name)
                 (json (kind sort)))
             (Vox_smt.Constructor.fields c)))
      constructors
  in
  let rec calls = function
    | Call (f, args) -> f :: List.concat_map calls args
    | App (_, args) | Construct (_, args) -> List.concat_map calls args
    | Select (_, _, t) | Is (_, t) -> calls t
    | _ -> []
  in
  let used_functions =
    calls conclusion
    @ List.concat_map (fun (f : labelled_term) -> calls f.term) assumptions
  in
  let functions =
    List.filter (fun (f, _) -> List.mem f used_functions) functions
  in
  Printf.sprintf
    "\"goal\":%s,\"assumptions\":%s,\"symbols\":[%s],\"constructors\":[%s],\"opaqueFunctions\":%s"
    (labelled { query.goal with term = conclusion })
    (array labelled assumptions)
    (String.concat "," symbol_json)
    (String.concat "," constructors)
    (array (fun (_, name) -> json name) functions)

let prove loc query =
  incr count;
  event
    (Printf.sprintf "{\"type\":\"proof\",\"index\":%d,\"from\":%d,\"to\":%d,%s}"
       !count loc.Location.loc_start.Lexing.pos_cnum
       loc.Location.loc_end.Lexing.pos_cnum (describe query));
  let script = Vox_smt.to_smtlib ~int_width:63 ~timeout_ms:3000 query in
  let status = solve script in
  event
    (Printf.sprintf "{\"type\":\"proof-status\",\"index\":%d,\"status\":%d}"
       !count status);
  match status with
  | 0 -> ()
  | 1 ->
      Location.raise_errorf ~loc
        "Refinement could not be proved (counterexample)."
  | 2 ->
      Location.raise_errorf ~loc
        "Verification timed out; this obligation is not proved."
  | 3 ->
      Location.raise_errorf ~loc
        "Z3 returned unknown; this obligation is not proved."
  | _ ->
      Location.raise_errorf ~loc
        "Browser solver failed; this obligation is not proved."

let install () =
  if not !installed then begin
    installed := true;
    Verification.install (fun structure -> Vox_vc.generate ~prove structure);
    Verification.install_termination (fun ~self ~fn ~measure ->
        Vox_vc.check_termination ~prove ~self ~fn ~measure)
  end
