(* Refinement predicates for vox refined types [{v:int | p}].

   Predicates are untyped logical terms: the compiler never type checks them; ill-sorted
   predicates surface as solver errors at VC time, which count as verification failures.
   The bound value variable [v] is [Pbound]; program variables are identified by [Ident.t]
   stamp.  Dependent-arrow parameters are ordinary [Pvar]s whose binding ident is stored
   on the arrow ([Types.arrow_desc]), mirroring how [Tpoly] binds its univars: opening a
   binder substitutes the stamp; comparing two independently written (hence
   differently-stamped) dependent signatures runs under a binder pairing pushed at each
   arrow, the analogue of [Ctype.univar_pairs]. *)

type binop =
  | Add
  | Sub
  | Mul
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  | Div (* T-division, truncating toward zero: OCaml's [/], Lean's [Int.tdiv] *)
  | Mod (* remainder with the dividend's sign: OCaml's [mod], Lean's [Int.tmod] *)

type pred =
  | Pbound (* the bound value variable v *)
  | Pvar of Ident.t (* logical value of a program variable or dependent-arrow binder *)
  | Pglobal of Path.t
    (* logical value of a MODULE-LEVEL (immutable) value, identified
       by path -- the global counterpart of [Pvar].  Stamp-free and
       .cmi-stable, like the type paths in [Pconstr]/[Pfield]/[Pis];
       two distinct paths to one value are distinct names (both facts
       true, equality not assumed). *)
  | Pint of int
  | Pbool of bool
  | Pconstr of Path.t * string * pred list
    (* application of a variant constructor, identified by its type's
       path and the constructor's name.  Only constructors of "simple"
       variants (monomorphic, non-GADT, tuple arguments) are admitted
       at elaboration; the solver models them with the datatype theory
       (free, injective, pairwise-distinct constructors). *)
  | Pfun of string * pred list
    (* application of a SPEC function: a logical function (measure,
       predicate, ...) defined on the solver side -- by a prelude (the
       [-vox-prelude] file, an embedded [%%vox.lean] block, or an
       imported spec export) or by a [total_] definition.  Purely a name -- the compiler neither resolves
       nor sorts it; an undefined or ill-sorted application is a solver
       error at VC time, i.e. a verification failure. *)
  | Pfield of Path.t * string * pred
    (* projection of a field out of a record term.  Carries the record
       type's path (selector symbols are per-type, and predicates are
       untyped, so the label resolves at elaboration like constructors
       do).  Only fields of "simple" records (monomorphic, all fields
       immutable) are admitted; the solver models such records as
       single-constructor datatypes with named selectors. *)
  | Ptuple of pred list
    (* an unlabeled tuple term [(p1, ..., pn)], n >= 2.  Tuples are
       structural (no type path): the solver models each ARITY with one
       polymorphic product datatype, so construction and projection
       need no instantiation info and predicates stay untyped. *)
  | Pproj of int * int * pred
    (* [Pproj (arity, i, t)]: the [i]th component (0-based) of tuple
       term [t] at the given arity -- [fst]/[snd] in the surface
       syntax.  The arity picks the product datatype's selector. *)
  | Pis of Path.t * string * pred
    (* constructor tester: "the term is an application of THIS
       constructor".  INTERNAL ONLY -- not expressible in surface
       predicates; minted by the VC pass as the negative match fact
       [not (s is C)] for arms below a guard-free simple arm.  Lean
       encodes it existentially, with an exhaustiveness hypothesis
       supplied per tester subject. *)
  | Pbinop of binop * pred * pred
  | Pand of pred * pred
  | Por of pred * pred
  | Pnot of pred
  | Pimp of pred * pred
    (* implication [p -> q].  NATIVE, not sugar for [not p || q]: the
       two spellings are structurally distinct types (bridged by
       subsumption at binders, like any respelling), diagnostics show
       what the user wrote, and Lean receives a genuine arrow. *)
  | Pquant of quant * Ident.t * pred
    (* quantifier [forall_ x. p] / [exists_ x. p].  The binder is a
       fresh [Scoped] ident, like a dependent-arrow binder: [Scoped]
       stamps marshalled through a .cmi can never collide with a
       consuming unit's [Local] variables, and freshness makes
       substitution capture-free.  The binder is UNSORTED (predicates
       are untyped): the Lean side emits it unannotated and lets
       elaboration infer, exactly as the existential encoding of [Pis]
       already does; an uninferable binder is a solver error, i.e. a
       verification failure. *)

and quant =
  | Qforall
  | Qexists

let binop_name = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Eq -> "="
  | Neq -> "<>"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | Div -> "/"
  | Mod -> "mod"
;;

(* Alpha-equivalence support: while two arrow types are being compared, their binders are
   paired here (innermost first), and [Pvar]s are compared modulo the pairing.  A stamp
   paired at some level must correspond exactly to its partner there; unpaired stamps
   compare by identity. *)
let binder_pairs : (Ident.t * Ident.t) list ref = ref []

let with_binder_pair id1 id2 f =
  binder_pairs := (id1, id2) :: !binder_pairs;
  Fun.protect ~finally:(fun () -> binder_pairs := List.tl !binder_pairs) f
;;

let equal_var id1 id2 =
  (* Orientation-insensitive: unification can swap its two sides (e.g.
     when only one expands an abbreviation), so a pair must match with
     the ids in either order.  Innermost involvement wins, which keeps
     shadowed binders distinct. *)
  let involved x (a, b) = Ident.same x a || Ident.same x b in
  let partners (a, b) =
    (Ident.same id1 a && Ident.same id2 b)
    || (Ident.same id1 b && Ident.same id2 a)
  in
  let rec find = function
    | [] -> Ident.same id1 id2
    | pair :: rest ->
      if involved id1 pair || involved id2 pair
      then partners pair
      else find rest
  in
  find !binder_pairs
;;

(* Structural equality; program variables compare by stamp, modulo the binder pairing in
   effect. This is the equality unification uses: "v > 0" and "0 < v" are NOT equal, by
   design. *)
let rec equal p1 p2 =
  match p1, p2 with
  | Pbound, Pbound -> true
  | Pvar id1, Pvar id2 -> equal_var id1 id2
  | Pglobal p1, Pglobal p2 -> Path.same p1 p2
  | Pint n1, Pint n2 -> Int.equal n1 n2
  | Pbool b1, Pbool b2 -> Bool.equal b1 b2
  | Pbinop (op1, a1, b1), Pbinop (op2, a2, b2) -> op1 = op2 && equal a1 a2 && equal b1 b2
  | Pand (a1, b1), Pand (a2, b2) | Por (a1, b1), Por (a2, b2) ->
    equal a1 a2 && equal b1 b2
  | Pconstr (p1, c1, args1), Pconstr (p2, c2, args2) ->
    (* The type path compares with [Path.same]: two ways of naming the
       same type through different module aliases are DIFFERENT, like
       every other structural discrepancy.  Sharp edges, not bugs. *)
    Path.same p1 p2
    && String.equal c1 c2
    && List.length args1 = List.length args2
    && List.for_all2 equal args1 args2
  | Pfun (f1, args1), Pfun (f2, args2) ->
    String.equal f1 f2
    && List.length args1 = List.length args2
    && List.for_all2 equal args1 args2
  | Pfield (p1, l1, a1), Pfield (p2, l2, a2) ->
    Path.same p1 p2 && String.equal l1 l2 && equal a1 a2
  | Ptuple args1, Ptuple args2 ->
    List.length args1 = List.length args2 && List.for_all2 equal args1 args2
  | Pproj (n1, i1, a1), Pproj (n2, i2, a2) ->
    Int.equal n1 n2 && Int.equal i1 i2 && equal a1 a2
  | Pis (p1, c1, a1), Pis (p2, c2, a2) ->
    Path.same p1 p2 && String.equal c1 c2 && equal a1 a2
  | Pnot a1, Pnot a2 -> equal a1 a2
  | Pimp (a1, b1), Pimp (a2, b2) -> equal a1 a2 && equal b1 b2
  | Pquant (q1, id1, a1), Pquant (q2, id2, a2) ->
    (* Alpha-equivalence, by the same binder pairing dependent arrows
       use: two independently written (hence differently-stamped)
       quantifiers compare with their binders paired. *)
    q1 = q2 && with_binder_pair id1 id2 (fun () -> equal a1 a2)
  | ( ( Pbound | Pvar _ | Pglobal _ | Pint _ | Pbool _ | Pconstr _ | Pfun _
      | Pfield _ | Ptuple _ | Pproj _
      | Pis _ | Pbinop _ | Pand _ | Por _ | Pnot _ | Pimp _ | Pquant _ ),
      _ ) -> false
;;

(* Substitute program variable [id] by predicate [by] (dependent application and lambda
   opening: [by] is always a [Pvar]). *)
let rec subst_var id ~by p =
  match p with
  | Pvar id' when Ident.same id id' -> by
  | Pbound | Pvar _ | Pglobal _ | Pint _ | Pbool _ -> p
  | Pconstr (path, c, args) -> Pconstr (path, c, List.map (subst_var id ~by) args)
  | Pfun (f, args) -> Pfun (f, List.map (subst_var id ~by) args)
  | Pfield (path, l, a) -> Pfield (path, l, subst_var id ~by a)
  | Ptuple args -> Ptuple (List.map (subst_var id ~by) args)
  | Pproj (n, i, a) -> Pproj (n, i, subst_var id ~by a)
  | Pis (path, c, a) -> Pis (path, c, subst_var id ~by a)
  | Pbinop (op, a, b) -> Pbinop (op, subst_var id ~by a, subst_var id ~by b)
  | Pand (a, b) -> Pand (subst_var id ~by a, subst_var id ~by b)
  | Por (a, b) -> Por (subst_var id ~by a, subst_var id ~by b)
  | Pnot a -> Pnot (subst_var id ~by a)
  | Pimp (a, b) -> Pimp (subst_var id ~by a, subst_var id ~by b)
  | Pquant (q, b, a) ->
    (* No capture and no shadowing: quantifier binders are fresh
       stamps, distinct from every substitutable variable. *)
    Pquant (q, b, subst_var id ~by a)
;;

(* Substitute the bound variable [v] (used when instantiating a refinement at a logical
   name). *)
let rec subst_bound ~by p =
  match p with
  | Pbound -> by
  | Pvar _ | Pglobal _ | Pint _ | Pbool _ -> p
  | Pconstr (path, c, args) -> Pconstr (path, c, List.map (subst_bound ~by) args)
  | Pfun (f, args) -> Pfun (f, List.map (subst_bound ~by) args)
  | Pfield (path, l, a) -> Pfield (path, l, subst_bound ~by a)
  | Ptuple args -> Ptuple (List.map (subst_bound ~by) args)
  | Pproj (n, i, a) -> Pproj (n, i, subst_bound ~by a)
  | Pis (path, c, a) -> Pis (path, c, subst_bound ~by a)
  | Pbinop (op, a, b) -> Pbinop (op, subst_bound ~by a, subst_bound ~by b)
  | Pand (a, b) -> Pand (subst_bound ~by a, subst_bound ~by b)
  | Por (a, b) -> Por (subst_bound ~by a, subst_bound ~by b)
  | Pnot a -> Pnot (subst_bound ~by a)
  | Pimp (a, b) -> Pimp (subst_bound ~by a, subst_bound ~by b)
  | Pquant (q, b, a) -> Pquant (q, b, subst_bound ~by a)
;;

let rec free_vars acc p =
  match p with
  | Pvar id -> id :: acc
  | Pbound | Pglobal _ | Pint _ | Pbool _ -> acc
  | Pconstr (_, _, args) | Pfun (_, args) | Ptuple args ->
    List.fold_left free_vars acc args
  | Pfield (_, _, a) | Pis (_, _, a) | Pproj (_, _, a) -> free_vars acc a
  | Pbinop (_, a, b) | Pand (a, b) | Por (a, b) | Pimp (a, b) ->
    free_vars (free_vars acc a) b
  | Pnot a -> free_vars acc a
  | Pquant (_, b, a) ->
    (* The binder is bound, not free.  Filtering the body's variables
       suffices: binder stamps are fresh, so [b] cannot also occur in
       [acc]. *)
    List.filter (fun id -> not (Ident.same id b)) (free_vars acc a)
;;

let free_vars p = free_vars [] p

(* The module-level values a predicate mentions, by path. *)
let rec free_globals acc p =
  match p with
  | Pglobal path -> path :: acc
  | Pbound | Pvar _ | Pint _ | Pbool _ -> acc
  | Pconstr (_, _, args) | Pfun (_, args) | Ptuple args ->
    List.fold_left free_globals acc args
  | Pfield (_, _, a) | Pis (_, _, a) | Pproj (_, _, a) -> free_globals acc a
  | Pbinop (_, a, b) | Pand (a, b) | Por (a, b) | Pimp (a, b) ->
    free_globals (free_globals acc a) b
  | Pnot a -> free_globals acc a
  | Pquant (_, _, a) -> free_globals acc a
;;

let free_globals p = free_globals [] p

let rec mem_var id p =
  match p with
  | Pvar id' -> Ident.same id id'
  | Pbound | Pglobal _ | Pint _ | Pbool _ -> false
  | Pconstr (_, _, args) | Pfun (_, args) | Ptuple args ->
    List.exists (mem_var id) args
  | Pfield (_, _, a) | Pis (_, _, a) | Pproj (_, _, a) -> mem_var id a
  | Pbinop (_, a, b) | Pand (a, b) | Por (a, b) | Pimp (a, b) ->
    mem_var id a || mem_var id b
  | Pnot a -> mem_var id a
  | Pquant (_, b, a) -> (not (Ident.same id b)) && mem_var id a
;;

(* Remap the paths of a predicate (used by [Subst] when a predicate
   crosses a module boundary): [f] rewrites TYPE paths (constructor
   applications, fields, testers) exactly as [Tconstr] paths are
   rewritten; [value] rewrites the VALUE paths of [Pglobal]s. *)
let rec map_paths_impl value f p =
  let map_paths = map_paths_impl value in
  match p with
  | Pglobal q -> Pglobal (value q)
  | Pbound | Pvar _ | Pint _ | Pbool _ -> p
  | Pconstr (path, c, args) -> Pconstr (f path, c, List.map (map_paths f) args)
  | Pfun (g, args) -> Pfun (g, List.map (map_paths f) args)
  | Pfield (path, l, a) -> Pfield (f path, l, map_paths f a)
  | Ptuple args -> Ptuple (List.map (map_paths f) args)
  | Pproj (n, i, a) -> Pproj (n, i, map_paths f a)
  | Pis (path, c, a) -> Pis (f path, c, map_paths f a)
  | Pbinop (op, a, b) -> Pbinop (op, map_paths f a, map_paths f b)
  | Pand (a, b) -> Pand (map_paths f a, map_paths f b)
  | Por (a, b) -> Por (map_paths f a, map_paths f b)
  | Pnot a -> Pnot (map_paths f a)
  | Pimp (a, b) -> Pimp (map_paths f a, map_paths f b)
  | Pquant (q, b, a) -> Pquant (q, b, map_paths f a)
;;

let map_paths ?(value = fun (q : Path.t) -> q) f p = map_paths_impl value f p

let rec constr_paths acc p =
  match p with
  | Pbound | Pvar _ | Pglobal _ | Pint _ | Pbool _ -> acc
  | Pconstr (path, _, args) -> List.fold_left constr_paths (path :: acc) args
  | Pfun (_, args) | Ptuple args -> List.fold_left constr_paths acc args
  | Pfield (path, _, a) | Pis (path, _, a) -> constr_paths (path :: acc) a
  | Pproj (_, _, a) -> constr_paths acc a
  | Pbinop (_, a, b) | Pand (a, b) | Por (a, b) | Pimp (a, b) ->
    constr_paths (constr_paths acc a) b
  | Pnot a -> constr_paths acc a
  | Pquant (_, _, a) -> constr_paths acc a
;;

let constr_paths p = constr_paths [] p

(* Tuple arities used by a predicate (construction and projection): the
   solver input must declare one product datatype per arity. *)
let rec tuple_arities acc p =
  match p with
  | Pbound | Pvar _ | Pglobal _ | Pint _ | Pbool _ -> acc
  | Ptuple args -> List.fold_left tuple_arities (List.length args :: acc) args
  | Pproj (n, _, a) -> tuple_arities (n :: acc) a
  | Pconstr (_, _, args) | Pfun (_, args) -> List.fold_left tuple_arities acc args
  | Pfield (_, _, a) | Pis (_, _, a) -> tuple_arities acc a
  | Pbinop (_, a, b) | Pand (a, b) | Por (a, b) | Pimp (a, b) ->
    tuple_arities (tuple_arities acc a) b
  | Pnot a -> tuple_arities acc a
  | Pquant (_, _, a) -> tuple_arities acc a
;;

let tuple_arities p = tuple_arities [] p

(* Does [p] apply any spec function?  Spec text (which defines
   them) is injected only into solver inputs that need it: it may
   reference datatypes of one module, which do not exist in another
   module's input. *)
let rec mentions_spec_fun p =
  match p with
  | Pbound | Pvar _ | Pglobal _ | Pint _ | Pbool _ -> false
  | Pfun _ -> true
  | Pconstr (_, _, args) | Ptuple args -> List.exists mentions_spec_fun args
  | Pfield (_, _, a) | Pis (_, _, a) | Pproj (_, _, a) | Pnot a ->
    mentions_spec_fun a
  | Pbinop (_, a, b) | Pand (a, b) | Por (a, b) | Pimp (a, b) ->
    mentions_spec_fun a || mentions_spec_fun b
  | Pquant (_, _, a) -> mentions_spec_fun a
;;

(* Printing, in the compact surface format: the bound value variable prints as [_];
   program variables print through [var_display] -- the source name by default, but
   diagnostics that show several predicates together disambiguate same-named
   different-stamp variables (shadowing) via [with_var_display]. *)
let var_display : (Ident.t -> string) ref = ref Ident.name

let with_var_display f k =
  let saved = !var_display in
  var_display := f;
  Fun.protect ~finally:(fun () -> var_display := saved) k
;;

let rec print ppf p =
  let open Format in
  match p with
  | Pbound -> pp_print_string ppf "_"
  | Pvar id -> pp_print_string ppf (!var_display id)
  | Pglobal p -> pp_print_string ppf (Path.name p)
  | Pint n -> pp_print_int ppf n
  | Pbool b -> pp_print_bool ppf b
  | Pconstr (_, c, []) -> pp_print_string ppf c
  | Pconstr (_, c, [ a ]) -> fprintf ppf "@[%s %a@]" c print_atom a
  | Pconstr (_, c, a :: args) ->
    fprintf ppf "@[%s (%a" c print a;
    List.iter (fun x -> fprintf ppf ",@ %a" print x) args;
    fprintf ppf ")@]"
  | Pfun (f, []) -> pp_print_string ppf f
  | Pfun (f, [ a ]) when String.equal f "Vox_ia_len" ->
    fprintf ppf "@[Iarray.length %a@]" print_atom a
  | Pfun (f, [ a; i ]) when String.equal f "Vox_ia_get" ->
    fprintf ppf "%a.(%a)" print_atom a print i
  | Pfun (f, args) ->
    fprintf ppf "@[%s" f;
    List.iter (fun x -> fprintf ppf "@ %a" print_atom x) args;
    fprintf ppf "@]"
  | Pfield (_, l, a) -> fprintf ppf "%a.%s" print_atom a l
  | Ptuple (a :: args) ->
    fprintf ppf "@[(%a" print a;
    List.iter (fun x -> fprintf ppf ",@ %a" print x) args;
    fprintf ppf ")@]"
  | Ptuple [] ->
    (* unreachable (arity >= 2 by construction), but diagnostics must
       never crash *)
    pp_print_string ppf "()"
  | Pproj (2, 0, a) -> fprintf ppf "@[fst %a@]" print_atom a
  | Pproj (2, 1, a) -> fprintf ppf "@[snd %a@]" print_atom a
  | Pproj (_, i, a) ->
    (* diagnostics only: projections beyond pairs arise from match
       facts, never from surface predicates (1-based, as in Lean) *)
    fprintf ppf "%a.%d" print_atom a (i + 1)
  | Pis (_, c, a) -> fprintf ppf "@[%a is@ %s@]" print_atom a c
  | Pbinop (op, a, b) ->
    fprintf ppf "@[%a %s@ %a@]" print_atom a (binop_name op) print_atom b
  | Pand (a, b) -> fprintf ppf "@[%a &&@ %a@]" print_atom a print_atom b
  | Por (a, b) -> fprintf ppf "@[%a ||@ %a@]" print_atom a print_atom b
  | Pnot a -> fprintf ppf "@[not %a@]" print_atom a
  | Pimp (a, b) ->
    (* Right-associative, weakest after quantifiers: the right operand
       prints unparenthesized so chains reparse as written. *)
    (match b with
     | Pimp _ | Pquant _ -> fprintf ppf "@[%a ->@ %a@]" print_atom a print b
     | _ -> fprintf ppf "@[%a ->@ %a@]" print_atom a print_atom b)
  | Pquant (q, b, a) ->
    (* Reparses: [forall_ x. p] is the surface syntax.  The binder
       prints through [var_display] so shadowing diagnostics
       disambiguate it like any other variable. *)
    fprintf ppf "@[%s %s.@ %a@]"
      (match q with Qforall -> "forall_" | Qexists -> "exists_")
      (!var_display b)
      print a

and print_atom ppf p =
  match p with
  | Pbound | Pvar _ | Pglobal _ | Pint _ | Pbool _ | Pconstr (_, _, [])
  | Pfun (_, [])
  | Pfield _ | Ptuple _ -> print ppf p
  | Pproj (n, _, _) ->
    (* [fst]/[snd] print as applications; the [.i] form is atomic *)
    if n = 2 then Format.fprintf ppf "(%a)" print p else print ppf p
  | Pconstr (_, _, _ :: _) | Pfun (_, _ :: _) | Pis _ | Pbinop _
  | Pand _ | Por _ | Pnot _ | Pimp _ | Pquant _ ->
    Format.fprintf ppf "(%a)" print p
;;

let to_string p = Format.asprintf "%a" print p

(* Built-in iarray theory: [Iarray.length a] and [a.(i)] in predicates
   (and the reflected [Iarray.length]/[Iarray.get] in expressions)
   denote these reserved spec functions.  Capitalized, so no
   total_/prelude lowercase name can collide from OCaml source; the
   theory declarations are emitted by Vox_verify when used.  [get] is
   TOTAL in the logic, like division: the safe program [get] raises
   out of bounds, so no value flows there and the unconstrained fact
   is vacuous -- sound under partial correctness.  Bounds SAFETY is
   an opt-in contract, not a forced obligation. *)
let ia_len = "Vox_ia_len"
let ia_get = "Vox_ia_get"

(* Does [p] apply the spec function called [name]?  (Emission gates
   the built-in theories on use.) *)
let rec mentions_fun name p =
  match p with
  | Pbound | Pvar _ | Pglobal _ | Pint _ | Pbool _ -> false
  | Pfun (f, args) ->
    String.equal f name || List.exists (mentions_fun name) args
  | Pconstr (_, _, args) | Ptuple args -> List.exists (mentions_fun name) args
  | Pfield (_, _, a) | Pis (_, _, a) | Pproj (_, _, a) | Pnot a
  | Pquant (_, _, a) -> mentions_fun name a
  | Pbinop (_, a, b) | Pand (a, b) | Por (a, b) | Pimp (a, b) ->
    mentions_fun name a || mentions_fun name b
;;

(* The constructs a compiled runtime check cannot evaluate faithfully,
   for diagnostics.  Owned here so the two rejection messages
   (Vox_verify's gate and Translcore's backstop) cannot drift from the
   [pred] type or from each other; keep in sync with
   [Vox_verify.runtime_check_gate]. *)
let unreflectable_what = "a tuple, projection, record field, or quantifier"
