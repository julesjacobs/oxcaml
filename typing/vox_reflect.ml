(* vox: partial translation of pure typedtree expressions into
   refinement predicates -- the compiler-attached logical meaning of the
   built-in int/bool operations.

   [translate e] returns the logic term denoting [e]'s value when [e]
   is built from variables, int/bool literals, immutable field reads
   of simple records, and the primitive operations the predicate
   language models (+ - * / mod ~- succ pred on int; && || not;
   comparisons at int or bool); [None] otherwise.
   Recognition is keyed on the PRIMITIVE ([Val_prim]), never the source
   name, so shadowing [(+)] cannot be mistaken for integer addition.
   Comparisons are polymorphic primitives and are translated only at
   int or bool operands: at other types the logic's equality is
   uninterpreted, and [compare] does not denote it.

   The translatable fragment is pure up to Division_by_zero: [/] and
   [mod] can raise where the logic's T-division is total ([tdiv x 0 =
   0]).  Sound under partial correctness -- a raised exception aborts
   the continuation, so facts recorded about the never-bound result
   hold vacuously -- and naming a subexpression still never duplicates
   or reorders effects (the LOGIC term is a value either way).  A
   short-circuit condition can also leave a division UNEVALUATED
   ([a && b / c > 0] with [a] false): the path fact still holds,
   because reaching a branch means the evaluated prefix already
   determines the condition's value -- [false && X] is false for
   EVERY [X] -- so the totalized value of the unevaluated suffix
   cannot flip the recorded fact, and every division that did
   evaluate returned (no raise) with exactly the logic's value.

   CAVEAT (DESIGN.md): the logic's ints are unbounded while the
   machine's wrap, so the translation of + - * equates modular with
   ideal arithmetic; overflow is outside the model.

   THE TRANSLATOR TOWER -- what turns a term into a logic term, and
   why there are several entry points rather than one:
   - [translate_surface]: Parsetree, for DEPENDENT ARGUMENTS -- runs
     before the argument is typed, so admission is keyed on resolution
     and declared types only;
   - [translate]: the typed superset of the surface fragment (same
     [prim_pred] table, plus type-gated comparisons/projections and
     field reads) -- names arguments in the VC walker, if-conditions,
     and exact synthesis;
   - [translate_rhs]: [translate] plus applications of simple-variant
     constructors, for reflected definition bodies (where an
     untranslatable term is an ERROR, never a fresh unknown);
   - [Vox_verify.name_of_expr]: [translate] made TOTAL -- adds
     registration-aware constructor/record/tuple naming and degrades
     everything else to a fresh unknown;
   - [Typetexp.elab_vox_pred]: the PREDICATE language, deliberately
     separate -- a different grammar ([_], named binders, spec
     functions, implication, quantifiers) whose operators are spelled,
     not resolved.
   The primitive fragment lives once, in [prim_pred]; the
   [@@vox.decreases] metric reuses [translate_surface]. *)

open Typedtree

let is_int_or_bool env ty =
  match Types.get_desc (Ctype.vox_expand_head env ty) with
  | Tconstr (p, [], _) ->
    Path.same p Predef.path_int || Path.same p Predef.path_bool
  | _ -> false
;;

(* An UNLABELED pair type: the shape [fst]/[snd] project out of.  The
   projection primitives ([%field0_immut]/[%field1_immut]) are generic
   block reads, so the argument's type is the gate -- a user [external]
   with the same primitive at another type must not be mistaken for a
   tuple projection. *)
let is_unlabeled_pair env ty =
  let ty = Ctype.vox_expand_head env ty in
  match Types.get_desc ty with
  | Ttuple [ (None, _); (None, _) ] -> true
  | _ -> false
;;

(* The surface twin of the gate above, usable BEFORE the argument is
   typed: the resolved value's DECLARED domain is an unlabeled pair
   (as [Stdlib.fst]'s ['a * 'b -> 'a] is), so any application that
   typechecks has its argument at a pair type -- exactly when
   [translate] will admit the projection.  A user [external] carrying
   the same primitive at a non-pair or fully polymorphic type is
   refused here (and [translate], gated on the actual argument type,
   stays a superset). *)
let declared_domain_is_unlabeled_pair env (desc : Types.value_description) =
  let ty =
    match Ctype.expand_head env desc.val_type with
    | ty -> ty
    | exception _ -> desc.val_type
  in
  match Types.get_desc ty with
  | Tarrow (_, dom, _, _) ->
    (* Arrow domains wrap their type in [Tpoly]; a trivial one is
       transparent, a genuinely polymorphic parameter is refused. *)
    let dom =
      match Types.get_desc dom with
      | Tpoly (t, []) -> t
      | _ -> dom
    in
    is_unlabeled_pair env dom
  | _ -> false
;;

(* TOTAL (reflected) functions ([let rec total_ f ... = ...]): program
   functions whose definitions are translated into the logic
   (translate_def below) and emitted as solver-side definitions
   (vox_verify).  Registered by stamp when the binding is typed
   (Typecore.type_let), so a SATURATED application of a reflected
   function translates like a primitive: [Pfun (name, args)].  Stamps
   are process-unique, so entries from other units in the same process
   can never be confused with the current unit's identifiers. *)
let reflected : (Ident.t, string * int) Hashtbl.t = Hashtbl.create 16

let register_reflected id ~arity =
  Hashtbl.replace reflected id (Ident.name id, arity)
;;

let find_attr name (attrs : Parsetree.attributes) =
  List.find_opt
    (fun (a : Parsetree.attribute) -> String.equal a.attr_name.txt name)
    attrs
;;

let has_total_attr attrs = find_attr "vox.total" attrs <> None

(* [Some (name, arity)] when [path] denotes a reflected function: a
   local one (the typing-time table), or any value carrying the
   [vox.total] marker in its val_attributes -- the marker rides the
   binder pattern into the value description and hence the .cmi, so
   IMPORTED reflected functions are recognized too (their definitions
   ride the exporting unit's spec blocks; a unit with an .mli exports
   no marker and no definition, consistently).  The definition's
   solver-side name is the source name, and the arity is the type's
   arrow count: reflected functions are first-order, so it is exact. *)
let reflected_call_info env path (desc : Types.value_description) =
  let table =
    match path with
    | Path.Pident id -> Hashtbl.find_opt reflected id
    | _ -> None
  in
  match table with
  | Some _ -> table
  | None ->
    if has_total_attr desc.val_attributes
    then (
      let rec arity ty acc =
        match Types.get_desc (Ctype.vox_expand_head env ty) with
        | Tarrow (_, _, ret, _) -> arity ret (acc + 1)
        | _ -> acc
      in
      Some (Path.last path, arity desc.val_type 0))
    else None
;;

(* The compiler-attached logical meaning of a PRIMITIVE application:
   ONE table, shared by the typed translation ([translate]) and its
   surface twin ([translate_surface]) so the two fragments cannot
   drift -- a primitive admitted in one is admitted in both.
   Admissions that need TYPE information are the callers' gates:
   [cmp_ok] (the polymorphic comparisons, at int/bool operands only)
   and [proj_ok] ([fst]/[snd], at a pair argument only) -- the typed
   side inspects the argument's type; the surface side passes what it
   can know before typing (no comparisons; the resolved declaration's
   domain for projections). *)
let prim_pred prim_name ~cmp_ok ~proj_ok
      (args : Refinement.pred option list) : Refinement.pred option =
  match
    if List.for_all Option.is_some args
    then Some (List.map Option.get args)
    else None
  with
  | None -> None
  | Some args ->
    let unary k =
      match args with
      | [ a ] -> Some (k a)
      | _ -> None
    in
    let binary k =
      match args with
      | [ a; b ] -> Some (k a b)
      | _ -> None
    in
    let intop op = binary (fun a b -> Refinement.Pbinop (op, a, b)) in
    let proj i =
      if proj_ok then unary (fun a -> Refinement.Pproj (2, i, a)) else None
    in
    let cmp op = if cmp_ok then intop op else None in
    (match prim_name with
     | "%addint" -> intop Refinement.Add
     | "%subint" -> intop Refinement.Sub
     | "%mulint" -> intop Refinement.Mul
     | "%divint" -> intop Refinement.Div
     | "%modint" -> intop Refinement.Mod
     | "%negint" ->
       unary (fun a -> Refinement.Pbinop (Sub, Refinement.Pint 0, a))
     | "%succint" ->
       unary (fun a -> Refinement.Pbinop (Add, a, Refinement.Pint 1))
     | "%predint" ->
       unary (fun a -> Refinement.Pbinop (Sub, a, Refinement.Pint 1))
     | "%field0_immut" -> proj 0
     | "%field1_immut" -> proj 1
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
;;

(* SURFACE translation, for dependent application: the logic term
   denoting an argument expression that has not been typed yet (the
   binder must be substituted throughout the remaining type BEFORE
   later arguments are typechecked, so [translate] below cannot be
   used).  Sound despite being syntactic because every construct is
   keyed on what the identifier RESOLVES to in [env] -- the same
   resolution the later typing will perform: a primitive recognized by
   its name cannot be a shadowing impostor, and the admitted int/bool
   primitives are monomorphic, so if the program typechecks their
   operands are ints/bools.  Unlabeled tuples need no type gate (the
   product model is per-arity, polymorphic); [fst]/[snd] are gated on
   the resolved value's DECLARED pair domain, which any typechecking
   application's argument then has.  The POLYMORPHIC comparisons are
   excluded: their operand sort is unknown before typing, and the
   logic's equality disagrees with the program's at floats (nan) and
   functions.
   Mutable variables are rejected as everywhere (a stamp names one
   value; a cell has many).  The fragment is pure up to
   Division_by_zero, whose raise makes downstream facts vacuous
   (partial correctness). *)
let rec translate_surface env (e : Parsetree.expression)
  : Refinement.pred option
  =
  match e.pexp_desc with
  | Pexp_constant { pconst_desc = Pconst_integer (s, None); _ } ->
    Option.map (fun n -> Refinement.Pint n) (int_of_string_opt s)
  | Pexp_construct ({ txt = Longident.Lident "true"; _ }, None) ->
    Some (Refinement.Pbool true)
  | Pexp_construct ({ txt = Longident.Lident "false"; _ }, None) ->
    Some (Refinement.Pbool false)
  | Pexp_tuple comps
    when List.length comps >= 2
         && List.for_all (fun (lbl, _) -> Option.is_none lbl) comps ->
    (* An unlabeled tuple is a product term regardless of its type
       (the per-arity product datatype is polymorphic), so it needs no
       type-based gate.  Labeled tuples are not modelled. *)
    let args = List.map (fun (_, a) -> translate_surface env a) comps in
    if List.for_all Option.is_some args
    then Some (Refinement.Ptuple (List.map Option.get args))
    else None
  | Pexp_ident lid ->
    (match Env.lookup_value ~use:false ~loc:e.pexp_loc lid.txt env with
     | Path.Pident id, { val_kind = Val_reg _; _ }, _ ->
       Some (Refinement.Pvar id)
     | _ -> None
     | exception _ -> None)
  | Pexp_apply ({ pexp_desc = Pexp_ident lid; pexp_loc; _ }, sargs) ->
    let args =
      List.map
        (fun (lbl, a) ->
          match (lbl : Asttypes.arg_label) with
          | Nolabel -> translate_surface env a
          | _ -> None)
        sargs
    in
    (match Env.lookup_value ~use:false ~loc:pexp_loc lid.txt env with
     | _, ({ val_kind = Val_prim prim; _ } as desc), _ ->
       prim_pred prim.prim_name ~cmp_ok:false
         ~proj_ok:(declared_domain_is_unlabeled_pair env desc)
         args
     | path, desc, _ ->
       (match reflected_call_info env path desc with
        | Some (name, arity)
          when List.length args = arity && List.for_all Option.is_some args ->
          Some (Refinement.Pfun (name, List.map Option.get args))
        | _ -> None)
     | exception _ -> None)
  | _ -> None
;;

(* [mutvar] names reads of mutable variables ([Texp_mutvar]): the VC
   walker passes its current-version lookup, so reflected expressions
   read the SSA version in force at this program point; every other
   caller keeps the default, under which mutable reads stay opaque. *)
let translate ?(mutvar = fun _ -> None) (e : expression)
  : Refinement.pred option
  =
  let rec go (e : expression) : Refinement.pred option =
    match e.exp_desc with
    | Texp_mutvar { txt = id; _ } -> mutvar id
    | Texp_ident { path = Path.Pident id; _ } -> Some (Refinement.Pvar id)
  | Texp_constant (Const_int n) -> Some (Refinement.Pint n)
  | Texp_construct ({ txt = Longident.Lident "true"; _ }, _, _, [], _) ->
    Some (Refinement.Pbool true)
  | Texp_construct ({ txt = Longident.Lident "false"; _ }, _, _, [], _) ->
    Some (Refinement.Pbool false)
  | Texp_tuple (comps, _)
    when List.length comps >= 2
         && List.for_all (fun (lbl, _) -> Option.is_none lbl) comps ->
    (* An unlabeled tuple is the product term the predicate language
       writes as [(p1, ..., pn)]; labeled tuples are not modelled. *)
    let args = List.map (fun (_, a) -> go a) comps in
    if List.for_all Option.is_some args
    then Some (Refinement.Ptuple (List.map Option.get args))
    else None
  | Texp_field { record; label; _ } ->
    (* A field read of a simple record is the structure projection the
       predicate language writes as [_.px].  [vox_simple_record]
       requires every field immutable, so the read is pure and its
       value is stable -- a mutable field (which disqualifies the whole
       record) must stay a fresh unknown at each read. *)
    let path = Data_types.lbl_res_type_path label in
    (match Ctype.vox_simple_record record.exp_env path with
     | Some _ ->
       Option.map
         (fun base -> Refinement.Pfield (path, label.lbl_name, base))
         (go record)
     | None -> None)
  | Texp_apply
      ({ exp_desc = Texp_ident { path; desc; _ }; _ }, args, _, _, _)
    when reflected_call_info e.exp_env path desc <> None ->
    let name, arity =
      match reflected_call_info e.exp_env path desc with
      | Some info -> info
      | None -> assert false
    in
    let args =
      List.map
        (fun (lbl, arg) ->
          match (lbl : Types.arg_label), arg with
          | Nolabel, Arg (a, _) -> go a
          | _ -> None)
        args
    in
    if List.length args = arity && List.for_all Option.is_some args
    then Some (Refinement.Pfun (name, List.map Option.get args))
    else None
  | Texp_apply
      ( { exp_desc = Texp_ident { desc = { val_kind = Val_prim prim; _ }; _ }
        ; _
        }
      , args
      , _
      , _
      , _ ) ->
    let sargs =
      List.map
        (fun (lbl, arg) ->
          match (lbl : Types.arg_label), arg with
          | Nolabel, Arg (a, _) -> Some a
          | _ -> None)
        args
    in
    (* The type-dependent gates: comparisons at int/bool operands
       (both operands have the same type; checking one suffices), and
       fst/snd at an unlabeled-pair argument (the primitive itself is
       a generic block read). *)
    let cmp_ok =
      match sargs with
      | Some a :: _ -> is_int_or_bool a.exp_env a.exp_type
      | _ -> false
    in
    let proj_ok =
      match sargs with
      | [ Some a ] -> is_unlabeled_pair a.exp_env a.exp_type
      | _ -> false
    in
    prim_pred prim.prim_name ~cmp_ok ~proj_ok
      (List.map (fun a -> Option.bind a go) sargs)
    | _ -> None
  in
  go e
;;

(* ------------------------------------------------------------------ *)
(* Reflected DEFINITIONS: the translation of a [total_] binding's
   body into an equation-style logical definition.

   The reflectable fragment is deliberately small (sharp edges, not
   bugs): a function whose parameters are plain variables of int, bool
   or simple-variant sort, whose body is built from [if] on translatable
   conditions, exhaustive one-level [match] on a variable in scope
   (constructor patterns over variables or wildcards, simple variants
   only), and right-hand sides in the [translate] fragment extended
   with constructor applications and saturated calls to reflected
   functions (self-calls included).  The definition must be CLOSED:
   every variable it mentions is one of its own parameters or match
   fields -- a local function capturing enclosing variables would
   otherwise smuggle activation-local stamps into a global definition.

   Termination is checked by the solver (the Lean backend emits an
   honest [def]); int-indexed recursion carries a [@@vox.decreases e]
   metric from which [termination_by (e).toNat] is synthesized. *)

type rsort =
  | Rint
  | Rbool
  | Rdata of Path.t

type def_body =
  | Bpred of Refinement.pred
  | Bite of Refinement.pred * def_body * def_body
  | Bcase of Ident.t * def_clause list

and def_clause =
  { dc_path : Path.t (* the scrutinee's datatype *)
  ; dc_cstr : string
  ; dc_fields : Ident.t list (* wildcards get fresh idents *)
  ; dc_rhs : def_body
  }

type spec_def =
  { sd_name : string (* solver-side name; the source name in v0 *)
  ; sd_id : Ident.t
  ; sd_params : (Ident.t * rsort) list
  ; sd_ret : rsort
  ; sd_body : def_body
  ; sd_decreases : Refinement.pred option
  ; sd_loc : Location.t
  }

let rsort_of_type env ~loc ~what ty =
  match Types.get_desc (Ctype.vox_expand_head env ty) with
  | Tconstr (p, [], _) ->
    if Path.same p Predef.path_int
    then Rint
    else if Path.same p Predef.path_bool
    then Rbool
    else (
      match Ctype.vox_simple_variant env p with
      | Some _ -> Rdata p
      | None ->
        Location.raise_errorf ~loc
          "vox: %s of a reflected function must be int, bool, or a simple \
           variant"
          what)
  | _ ->
    Location.raise_errorf ~loc
      "vox: %s of a reflected function must be int, bool, or a simple variant"
      what
;;

(* Right-hand sides: the [translate] fragment, extended with
   applications of simple-variant constructors. *)
let rec translate_rhs (e : expression) : Refinement.pred option =
  match translate e with
  | Some p -> Some p
  | None ->
    (match e.exp_desc with
     | Texp_construct (_, cstr, _, args, _) ->
       let path = Data_types.cstr_res_type_path cstr in
       (match Ctype.vox_simple_variant e.exp_env path with
        | None -> None
        | Some _ ->
          let args = List.map (fun (_, a) -> translate_rhs a) args in
          if List.for_all Option.is_some args
          then
            Some
              (Refinement.Pconstr
                 (path, cstr.cstr_name, List.map Option.get args))
          else None)
     | _ -> None)
;;

(* The NAMEABLE fragment: [translate] extended with constructor terms,
   record literals ([mk]), unlabeled tuples, and immutable field reads
   of nameable bases -- everything that names a value DETERMINISTICALLY,
   with no fresh-unknown fallback.  Used by [refine_]'s synthesis mode;
   [Vox_verify.name_of_expr] is the same fragment plus fresh unknowns
   (and solver-side datatype registration). *)
let rec translate_nameable (e : expression) : Refinement.pred option =
  match translate e with
  | Some p -> Some p
  | None ->
    let all_nameable args =
      let args = List.map translate_nameable args in
      if List.for_all Option.is_some args
      then Some (List.map Option.get args)
      else None
    in
    (match e.exp_desc with
     | Texp_construct (_, cstr, _, args, _) ->
       let path = Data_types.cstr_res_type_path cstr in
       (match Ctype.vox_simple_variant e.exp_env path with
        | None -> None
        | Some _ ->
          Option.map
            (fun ns -> Refinement.Pconstr (path, cstr.cstr_name, ns))
            (all_nameable (List.map snd args)))
     | Texp_record { fields; extended_expression; _ }
       when Array.length fields > 0 ->
       let path =
         Data_types.lbl_res_type_path (match fields.(0) with lbl, _, _ -> lbl)
       in
       (match Ctype.vox_simple_record e.exp_env path with
        | None -> None
        | Some _ ->
          let base =
            match extended_expression with
            | None -> Some None
            | Some (be, _, _) ->
              Option.map Option.some (translate_nameable be)
          in
          (match base with
           | None -> None
           | Some base ->
             let arg_of (lbl, _, def) =
               match def, base with
               | Typedtree.Overridden (_, ex), _ -> translate_nameable ex
               | Kept _, Some b ->
                 Some (Refinement.Pfield (path, lbl.Data_types.lbl_name, b))
               | Kept _, None -> None
             in
             let args = List.map arg_of (Array.to_list fields) in
             if List.for_all Option.is_some args
             then
               Some
                 (Refinement.Pconstr (path, "mk", List.map Option.get args))
             else None))
     | Texp_tuple (comps, _)
       when List.length comps >= 2
            && List.for_all (fun (lbl, _) -> Option.is_none lbl) comps ->
       Option.map
         (fun ns -> Refinement.Ptuple ns)
         (all_nameable (List.map snd comps))
     | Texp_field { record; label; _ } ->
       let path = Data_types.lbl_res_type_path label in
       (match label.lbl_mut, Ctype.vox_simple_record e.exp_env path with
        | Types.Immutable, Some _ ->
          Option.map
            (fun b -> Refinement.Pfield (path, label.lbl_name, b))
            (translate_nameable record)
        | _ -> None)
     | _ -> None)
;;

let def_unsupported loc =
  Location.raise_errorf ~loc
    "vox: this expression cannot be reflected into the logic (reflected \
     bodies are built from int/bool operations, constructors of simple \
     variants, saturated calls to reflected functions, [if] on \
     translatable conditions, and exhaustive one-level [match] on a \
     variable)"
;;

let rec translate_body (e : expression) : def_body =
  match e.exp_desc with
  | Texp_ifthenelse (c, e_then, Some e_else) ->
    (match translate c with
     | Some p -> Bite (p, translate_body e_then, translate_body e_else)
     | None -> def_unsupported c.exp_loc)
  | Texp_match (scrut, _sort, comp_cases, val_cases, partial) ->
    let scrut_id =
      match scrut.exp_desc with
      | Texp_ident { path = Path.Pident id; _ } -> id
      | _ ->
        Location.raise_errorf ~loc:scrut.exp_loc
          "vox: a reflected [match] must scrutinize a variable"
    in
    (* [bool] passes the simple-variant test, but its solver-side sort
       is a proposition, not a datatype: [match (b : Prop) with | True]
       is a Lean type error.  Reject it here with the fix spelled out
       rather than blaming the definition on an opaque solver error. *)
    (match Types.get_desc (Ctype.vox_expand_head scrut.exp_env scrut.exp_type) with
     | Tconstr (p, [], _) when Path.same p Predef.path_bool ->
       Location.raise_errorf ~loc:scrut.exp_loc
         "vox: a reflected function cannot [match] on bool (the solver \
          models bool as a proposition); use [if] instead"
     | _ -> ());
    (* Ordinary clauses arrive as COMPUTATION cases (value patterns
       wrapped in [Tpat_value]); [val_cases] holds effect handlers. *)
    if val_cases <> [] || partial <> Total
    then
      Location.raise_errorf ~loc:e.exp_loc
        "vox: a reflected [match] must be exhaustive, without effect \
         handlers";
    Bcase (scrut_id, List.map translate_clause comp_cases)
  | _ ->
    (match translate_rhs e with
     | Some p -> Bpred p
     | None -> def_unsupported e.exp_loc)

and translate_clause : type k. k case -> def_clause =
  fun c ->
  (match c.c_guard with
   | None -> ()
   | Some g ->
     Location.raise_errorf ~loc:g.exp_loc
       "vox: a reflected [match] cannot have when-guards");
  let rec constructor_clause : type k2. k2 general_pattern -> def_clause =
    fun pat ->
    match pat.pat_desc with
    | Tpat_value p -> constructor_clause (p :> value general_pattern)
    | Tpat_construct (_, cstr, _, args, _) ->
      let path = Data_types.cstr_res_type_path cstr in
      (match Ctype.vox_simple_variant pat.pat_env path with
       | Some _ -> ()
       | None ->
         Location.raise_errorf ~loc:pat.pat_loc
           "vox: a reflected [match] is limited to simple variants");
      let field (_, (p : value general_pattern)) =
        match p.pat_desc with
        | Tpat_var { id; _ } -> id
        | Tpat_any -> Ident.create_local "*vox-reflect-wild*"
        | _ ->
          Location.raise_errorf ~loc:p.pat_loc
            "vox: a reflected [match] is limited to one-level constructor \
             patterns over variables or wildcards"
      in
      { dc_path = path
      ; dc_cstr = cstr.cstr_name
      ; dc_fields = List.map field args
      ; dc_rhs = translate_body c.c_rhs
      }
    | _ ->
      Location.raise_errorf ~loc:pat.pat_loc
        "vox: a reflected [match] is limited to one-level constructor \
         patterns over variables or wildcards"
  in
  constructor_clause c.c_lhs
;;

(* The [@@vox.decreases e] metric: an int-valued expression over the
   parameters, in the SAME surface fragment dependent arguments use
   ([translate_surface] -- one fragment, not a bespoke third), plus a
   parameters-only restriction ([termination_by] quantifies over the
   definition's parameters, nothing else). *)
let translate_metric env params (e : Parsetree.expression) : Refinement.pred =
  match translate_surface env e with
  | None ->
    Location.raise_errorf ~loc:e.pexp_loc
      "vox: a [@@vox.decreases] metric must be a pure expression the logic \
       can name, over the function's parameters"
  | Some p ->
    List.iter
      (fun v ->
        if not (List.exists (fun (id, _) -> Ident.same v id) params)
        then
          Location.raise_errorf ~loc:e.pexp_loc
            "vox: a [@@vox.decreases] metric may mention only the function's \
             parameters")
      (Refinement.free_vars p);
    p
;;

(* The [total_] marker rides the binder pattern (parser); the
   [@@vox.total] attribute spelling on the binding also works. *)
let is_total_binding (vb : Typedtree.value_binding) =
  has_total_attr vb.vb_attributes || has_total_attr vb.vb_pat.pat_attributes
;;

let rec body_preds acc = function
  | Bpred p -> p :: acc
  | Bite (c, a, b) -> body_preds (body_preds (c :: acc) a) b
  | Bcase (_, clauses) ->
    List.fold_left (fun acc cl -> body_preds acc cl.dc_rhs) acc clauses
;;

let rec body_bound acc = function
  | Bpred _ -> acc
  | Bite (_, a, b) -> body_bound (body_bound acc a) b
  | Bcase (_, clauses) ->
    List.fold_left
      (fun acc cl -> body_bound (cl.dc_fields @ acc) cl.dc_rhs)
      acc
      clauses
;;

let rec body_scrutinees acc = function
  | Bpred _ -> acc
  | Bite (_, a, b) -> body_scrutinees (body_scrutinees acc a) b
  | Bcase (x, clauses) ->
    List.fold_left
      (fun acc cl -> body_scrutinees acc cl.dc_rhs)
      (x :: acc)
      clauses
;;

(* Datatype paths a definition depends on (for solver-side registration):
   scrutinized datatypes plus constructor applications in the preds. *)
let def_datatype_paths (d : spec_def) =
  let clause_paths =
    let rec go acc = function
      | Bpred _ -> acc
      | Bite (_, a, b) -> go (go acc a) b
      | Bcase (_, clauses) ->
        List.fold_left (fun acc cl -> go (cl.dc_path :: acc) cl.dc_rhs) acc clauses
    in
    go [] d.sd_body
  in
  let sort_paths =
    List.filter_map
      (fun s ->
        match s with
        | Rdata p -> Some p
        | Rint | Rbool -> None)
      (d.sd_ret :: List.map snd d.sd_params)
  in
  clause_paths
  @ sort_paths
  @ List.concat_map Refinement.constr_paths (body_preds [] d.sd_body)
;;

(* Translate a typed [total_] binding into a definition.  Raises
   with a source location on anything outside the fragment. *)
let translate_def (vb : Typedtree.value_binding) : spec_def =
  let loc = vb.vb_loc in
  let id =
    match vb.vb_pat.pat_desc with
    | Tpat_var { id; _ } -> id
    | _ ->
      Location.raise_errorf ~loc
        "vox: total_ requires a binding of a single variable"
  in
  match vb.vb_expr.exp_desc with
  | Texp_function { params; body; _ } ->
    let env = vb.vb_expr.exp_env in
    let param (fp : function_param) =
      match fp.fp_arg_label, fp.fp_kind with
      | Nolabel, Tparam_pat ({ pat_desc = Tpat_var { id; _ }; _ } as pat) ->
        ( id
        , rsort_of_type env ~loc:pat.pat_loc ~what:"each parameter"
            pat.pat_type )
      | _ ->
        Location.raise_errorf ~loc:fp.fp_loc
          "vox: a reflected function's parameters must be plain variables"
    in
    let params = List.map param params in
    let params, def_body, ret_ty, metric_env =
      match body with
      | Tfunction_body e -> params, translate_body e, e.exp_type, e.exp_env
      | Tfunction_cases fc ->
        (match fc.fc_cases with
         | [] -> def_unsupported loc
         | c0 :: _ ->
           if fc.fc_partial <> Total
           then
             Location.raise_errorf ~loc
               "vox: a reflected [function] must be exhaustive";
           let scrut_sort =
             rsort_of_type env ~loc ~what:"each parameter" c0.c_lhs.pat_type
           in
           ( params @ [ fc.fc_param, scrut_sort ]
           , Bcase (fc.fc_param, List.map translate_clause fc.fc_cases)
           , c0.c_rhs.exp_type
             (* the pattern's env: the parameters are in scope, the
                case's own binders are not *)
           , c0.c_lhs.pat_env ))
    in
    if params = [] then def_unsupported loc;
    let ret = rsort_of_type env ~loc ~what:"the result" ret_ty in
    let decreases =
      match find_attr "vox.decreases" vb.vb_attributes with
      | None -> None
      | Some { attr_payload = PStr [ { pstr_desc = Pstr_eval (e, _); _ } ]; _ }
        -> Some (translate_metric metric_env params e)
      | Some a ->
        Location.raise_errorf ~loc:a.attr_loc
          "vox: [@@vox.decreases] expects an expression payload"
    in
    (* Closedness: every variable in the definition -- in a
       right-hand side, an [if] condition, the metric, or as a [match]
       scrutinee -- is a parameter or a match field.  (A local function
       could otherwise capture an enclosing activation's variable in a
       global definition.) *)
    let bound = List.map fst params @ body_bound [] def_body in
    let check_closed v =
      if not (List.exists (Ident.same v) bound)
      then
        Location.raise_errorf ~loc
          "vox: a reflected definition must be closed, but this one mentions \
           %s; reflected functions may only be defined at the module level, \
           over their own parameters"
          (Ident.name v)
    in
    List.iter
      (fun p -> List.iter check_closed (Refinement.free_vars p))
      (Option.to_list decreases @ body_preds [] def_body);
    List.iter check_closed (body_scrutinees [] def_body);
    { sd_name = Ident.name id
    ; sd_id = id
    ; sd_params = params
    ; sd_ret = ret
    ; sd_body = def_body
    ; sd_decreases = decreases
    ; sd_loc = loc
    }
  | _ ->
    Location.raise_errorf ~loc
      "vox: total_ requires a function binding"
;;
