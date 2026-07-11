(* Lean 4 encoder: the trusted translation of QF_UFLIA into Lean (DESIGN.md §10). This is
   the highest-trust code in the gate; it is deliberately simple and structural. Every
   compound term is fully parenthesised so precedence is never load-bearing. See NOTES.md
   for the experiments that fixed these choices.

   SMT -> Lean sort mapping: Int -> Int (mathematical ℤ, DESIGN.md §1), Bool -> Prop,
   uninterpreted sort -> a Type binder (unsat) or Fin n (sat). *)

open Ast

exception Encode_error of string

let errf fmt = Printf.ksprintf (fun s -> raise (Encode_error s)) fmt

(* Bump on ANY change to the emitted Lean (preamble, tactic, term mapping). The cache is
   keyed on this, so a bump invalidates every prior certification. *)
let encoding_version = "enc-v1"

(* Centralised grind invocation; part of the cache key via [grind_config]. *)
let grind_tactic = "grind"
let grind_config = "grind:default"

(* ---- name assignment (fresh, collision-free Lean identifiers) ---- *)

type names =
  { sort : (string, string) Hashtbl.t
  ; sym : (string, string) Hashtbl.t
  }

let make_names (q : query) : names =
  let names = { sort = Hashtbl.create 16; sym = Hashtbl.create 16 } in
  List.iteri
    (fun i s -> Hashtbl.replace names.sort s (Printf.sprintf "S%d" i))
    q.sort_decls;
  List.iteri
    (fun i (n, _, _) -> Hashtbl.replace names.sym n (Printf.sprintf "u%d" i))
    q.fun_decls;
  names
;;

let sort_ident names s =
  match Hashtbl.find_opt names.sort s with
  | Some i -> i
  | None -> errf "internal: no lean name for sort %s" s
;;

let sym_ident names s =
  match Hashtbl.find_opt names.sym s with
  | Some i -> i
  | None -> errf "internal: no lean name for symbol %s" s
;;

(* Lean type of an SMT sort. [usort] renders uninterpreted sorts (a Type ident for the
   unsat direction, "Fin n" for the sat direction). *)
let lean_type ~usort names = function
  | Int -> "Int"
  | Bool -> "Prop"
  | Usort s -> usort names s
;;

(* ---- shared term printer ---- *)

let rec enc names (t : term) : string =
  match t with
  | True -> "True"
  | False -> "False"
  | Int_lit n -> Printf.sprintf "(%s : Int)" n
  | Const name -> sym_ident names name
  | App (name, args) ->
    Printf.sprintf
      "(%s %s)"
      (sym_ident names name)
      (String.concat " " (List.map (enc names) args))
  | Not a -> Printf.sprintf "(¬ %s)" (enc names a)
  | And xs -> join names " ∧ " xs
  | Or xs -> join names " ∨ " xs
  | Implies (a, b) -> Printf.sprintf "(%s → %s)" (enc names a) (enc names b)
  | Ite (c, th, el) ->
    Printf.sprintf "(if %s then %s else %s)" (enc names c) (enc names th) (enc names el)
  | Eq (a, b) -> Printf.sprintf "(%s = %s)" (enc names a) (enc names b)
  | Iff (a, b) -> Printf.sprintf "(%s ↔ %s)" (enc names a) (enc names b)
  | Distinct xs -> enc_distinct names xs
  | Le (a, b) -> Printf.sprintf "(%s ≤ %s)" (enc names a) (enc names b)
  | Lt (a, b) -> Printf.sprintf "(%s < %s)" (enc names a) (enc names b)
  | Ge (a, b) -> Printf.sprintf "(%s ≥ %s)" (enc names a) (enc names b)
  | Gt (a, b) -> Printf.sprintf "(%s > %s)" (enc names a) (enc names b)
  | Add xs -> join names " + " xs
  | Mul xs -> join names " * " xs
  | Neg a -> Printf.sprintf "(-(%s))" (enc names a)
  | Sub xs ->
    (match xs with
     | [] -> errf "internal: empty subtraction"
     | hd :: tl ->
       let s =
         List.fold_left
           (fun acc x -> Printf.sprintf "(%s - %s)" acc (enc names x))
           (enc names hd)
           tl
       in
       s)

and join names op xs =
  match xs with
  | [] -> errf "internal: empty %s" op
  | [ x ] -> enc names x
  | _ -> "(" ^ String.concat op (List.map (enc names) xs) ^ ")"

and enc_distinct names xs =
  let arr = Array.of_list (List.map (enc names) xs) in
  let n = Array.length arr in
  let pairs = ref [] in
  for i = n - 1 downto 0 do
    for j = n - 1 downto i + 1 do
      pairs := Printf.sprintf "%s ≠ %s" arr.(i) arr.(j) :: !pairs
    done
  done;
  match !pairs with
  | [] -> errf "internal: distinct with < 2 args"
  | [ one ] -> "(" ^ one ^ ")"
  | many -> "(" ^ String.concat " ∧ " many ^ ")"
;;

(* ---- unsat direction: assertions as hyps, goal False, discharged by grind ---- *)

let binders_for_decls names (q : query) : string list =
  let sort_binders =
    List.map (fun s -> Printf.sprintf "(%s : Type)" (sort_ident names s)) q.sort_decls
  in
  let usort names s = sort_ident names s in
  let sym_binders =
    List.map
      (fun (n, params, ret) ->
         let ty =
           match params with
           | [] -> lean_type ~usort names ret
           | _ ->
             String.concat " → " (List.map (lean_type ~usort names) params)
             ^ " → "
             ^ lean_type ~usort names ret
         in
         Printf.sprintf "(%s : %s)" (sym_ident names n) ty)
      q.fun_decls
  in
  sort_binders @ sym_binders
;;

let encode_unsat (q : query) : string =
  let names = make_names q in
  let buf = Buffer.create 512 in
  Buffer.add_string buf "set_option linter.unusedVariables false\n";
  Buffer.add_string buf "open Classical\n\n";
  Buffer.add_string buf "theorem gate_query\n";
  List.iter
    (fun b -> Buffer.add_string buf ("    " ^ b ^ "\n"))
    (binders_for_decls names q);
  List.iteri
    (fun i a -> Buffer.add_string buf (Printf.sprintf "    (h%d : %s)\n" i (enc names a)))
    q.asserts;
  Buffer.add_string buf (Printf.sprintf "    : False := by %s\n" grind_tactic);
  Buffer.contents buf
;;

(* ---- sat direction: model substituted, conjunction of assertions by decide ---- *)

let enc_value (model : Model.t) (sort : sort) (r : Model.raw) : string =
  match Model.coerce model sort r with
  | Model.Vint s -> Printf.sprintf "(%s : Int)" s
  | Model.Vbool true -> "True"
  | Model.Vbool false -> "False"
  | Model.Vidx i ->
    (match sort with
     | Usort s -> Printf.sprintf "(%d : Fin %d)" i (Model.sort_card_of model s)
     | _ -> errf "internal: index value for non-uninterpreted sort")
;;

let sat_sort_defs names (q : query) (model : Model.t) : string list =
  List.map
    (fun s ->
       let card = Model.sort_card_of model s in
       Printf.sprintf "abbrev %s := Fin %d" (sort_ident names s) card)
    q.sort_decls
;;

let sat_sym_defs names (q : query) (model : Model.t) : string list =
  let usort names s = sort_ident names s in
  List.map
    (fun (n, params, ret) ->
       match params with
       | [] ->
         (* a constant *)
         (match List.assoc_opt n model.consts with
          | None -> errf "sat model does not assign const %s" n
          | Some r ->
            Printf.sprintf
              "abbrev %s : %s := %s"
              (sym_ident names n)
              (lean_type ~usort names ret)
              (enc_value model ret r))
       | _ ->
         (* a function: total lambda, nested if over cases, ending in default *)
         (match List.assoc_opt n model.funs with
          | None -> errf "sat model does not assign function %s" n
          | Some fd ->
            let arg_names = List.mapi (fun i _ -> Printf.sprintf "a%d" i) params in
            let binders =
              String.concat
                " "
                (List.map2
                   (fun a p -> Printf.sprintf "(%s : %s)" a (lean_type ~usort names p))
                   arg_names
                   params)
            in
            let default = enc_value model ret fd.default in
            let body =
              List.fold_right
                (fun (cargs, cres) acc ->
                   if List.length cargs <> List.length params
                   then errf "function %s case has wrong arity" n;
                   let guard =
                     String.concat
                       " ∧ "
                       (List.map2
                          (fun a (cv, p) ->
                             Printf.sprintf "%s = %s" a (enc_value model p cv))
                          arg_names
                          (List.combine cargs params))
                   in
                   Printf.sprintf
                     "(if %s then %s else %s)"
                     guard
                     (enc_value model ret cres)
                     acc)
                fd.cases
                default
            in
            let ty =
              String.concat " → " (List.map (lean_type ~usort names) params)
              ^ " → "
              ^ lean_type ~usort names ret
            in
            Printf.sprintf
              "abbrev %s : %s := fun %s => %s"
              (sym_ident names n)
              ty
              binders
              body))
    q.fun_decls
;;

let encode_sat (q : query) (model : Model.t) ~(tactic : string) : string =
  let names = make_names q in
  let buf = Buffer.create 512 in
  Buffer.add_string buf "set_option linter.unusedVariables false\n\n";
  List.iter (fun d -> Buffer.add_string buf (d ^ "\n")) (sat_sort_defs names q model);
  List.iter (fun d -> Buffer.add_string buf (d ^ "\n")) (sat_sym_defs names q model);
  Buffer.add_char buf '\n';
  let goal =
    match q.asserts with
    | [] -> "True"
    | xs -> String.concat " ∧ " (List.map (fun a -> "(" ^ enc names a ^ ")") xs)
  in
  Buffer.add_string
    buf
    (Printf.sprintf "theorem gate_sat_model : %s := by %s\n" goal tactic);
  Buffer.contents buf
;;
