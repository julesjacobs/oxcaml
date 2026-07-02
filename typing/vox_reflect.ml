(* vox: partial translation of pure typedtree expressions into
   refinement predicates -- the compiler-attached logical meaning of the
   built-in int/bool operations.

   [translate e] returns the logic term denoting [e]'s value when [e]
   is built from variables, int/bool literals, and the primitive
   operations the predicate language models (+ - * ~- succ pred on int;
   && || not; comparisons at int or bool); [None] otherwise.
   Recognition is keyed on the PRIMITIVE ([Val_prim]), never the source
   name, so shadowing [(+)] cannot be mistaken for integer addition.
   Comparisons are polymorphic primitives and are translated only at
   int or bool operands: at other types the logic's equality is
   uninterpreted, and [compare] does not denote it.

   The translatable fragment is pure, so naming a subexpression never
   duplicates or reorders effects.

   CAVEAT (DESIGN.md): the logic's ints are unbounded while the
   machine's wrap, so the translation of + - * equates modular with
   ideal arithmetic; overflow is outside the model. *)

open Typedtree

let is_int_or_bool env ty =
  let ty =
    (* Expansion can fail on exotic types; falling back to no expansion
       just makes the translation more conservative. *)
    match Ctype.expand_head env ty with
    | ty -> ty
    | exception _ -> ty
  in
  match Types.get_desc ty with
  | Tconstr (p, [], _) ->
    Path.same p Predef.path_int || Path.same p Predef.path_bool
  | _ -> false
;;

let rec translate (e : expression) : Refinement.pred option =
  match e.exp_desc with
  | Texp_ident { path = Path.Pident id; _ } -> Some (Refinement.Pvar id)
  | Texp_constant (Const_int n) -> Some (Refinement.Pint n)
  | Texp_construct ({ txt = Longident.Lident "true"; _ }, _, _, [], _) ->
    Some (Refinement.Pbool true)
  | Texp_construct ({ txt = Longident.Lident "false"; _ }, _, _, [], _) ->
    Some (Refinement.Pbool false)
  | Texp_apply
      ( { exp_desc = Texp_ident { desc = { val_kind = Val_prim prim; _ }; _ }
        ; _
        }
      , args
      , _
      , _
      , _ ) ->
    let args =
      List.map
        (fun (lbl, arg) ->
          match (lbl : Types.arg_label), arg with
          | Nolabel, Arg (a, _) -> Some a
          | _ -> None)
        args
    in
    let unary k =
      match args with
      | [ Some a ] -> Option.map k (translate a)
      | _ -> None
    in
    let binary k =
      match args with
      | [ Some a; Some b ] ->
        (match translate a, translate b with
         | Some pa, Some pb -> Some (k pa pb)
         | _ -> None)
      | _ -> None
    in
    let intop op = binary (fun a b -> Refinement.Pbinop (op, a, b)) in
    let cmp op =
      (* Both operands have the same type; checking one suffices. *)
      match args with
      | Some a :: _ when is_int_or_bool a.exp_env a.exp_type ->
        binary (fun a b -> Refinement.Pbinop (op, a, b))
      | _ -> None
    in
    (match prim.prim_name with
     | "%addint" -> intop Refinement.Add
     | "%subint" -> intop Refinement.Sub
     | "%mulint" -> intop Refinement.Mul
     | "%negint" ->
       unary (fun a -> Refinement.Pbinop (Sub, Refinement.Pint 0, a))
     | "%succint" ->
       unary (fun a -> Refinement.Pbinop (Add, a, Refinement.Pint 1))
     | "%predint" ->
       unary (fun a -> Refinement.Pbinop (Sub, a, Refinement.Pint 1))
     | "%sequand" -> binary (fun a b -> Refinement.Pand (a, b))
     | "%sequor" -> binary (fun a b -> Refinement.Por (a, b))
     | "%boolnot" -> unary (fun a -> Refinement.Pnot a)
     | "%equal" -> cmp Refinement.Eq
     | "%notequal" -> cmp Refinement.Neq
     | "%lessthan" -> cmp Refinement.Lt
     | "%lessequal" -> cmp Refinement.Le
     | "%greaterthan" -> cmp Refinement.Gt
     | "%greaterequal" -> cmp Refinement.Ge
     | _ -> None)
  | _ -> None
;;
