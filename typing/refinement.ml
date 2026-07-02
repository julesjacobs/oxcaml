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

type pred =
  | Pbound (* the bound value variable v *)
  | Pvar of Ident.t (* logical value of a program variable or dependent-arrow binder *)
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
       predicate, ...) that the user defines on the solver side via
       [-vox-prelude].  Purely a name -- the compiler neither resolves
       nor sorts it; an undefined or ill-sorted application is a solver
       error at VC time, i.e. a verification failure. *)
  | Pfield of Path.t * string * pred
    (* projection of a field out of a record term.  Carries the record
       type's path (selector symbols are per-type, and predicates are
       untyped, so the label resolves at elaboration like constructors
       do).  Only fields of "simple" records (monomorphic, all fields
       immutable) are admitted; the solver models such records as
       single-constructor datatypes with named selectors. *)
  | Pis of Path.t * string * pred
    (* constructor tester: "the term is an application of THIS
       constructor".  INTERNAL ONLY -- not expressible in surface
       predicates; minted by the VC pass as the negative match fact
       [not (s is C)] for arms below a guard-free simple arm.  Z3 has
       native testers; Lean encodes it existentially, with an
       exhaustiveness hypothesis supplied per tester subject. *)
  | Pbinop of binop * pred * pred
  | Pand of pred * pred
  | Por of pred * pred
  | Pnot of pred

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
  | Pis (p1, c1, a1), Pis (p2, c2, a2) ->
    Path.same p1 p2 && String.equal c1 c2 && equal a1 a2
  | Pnot a1, Pnot a2 -> equal a1 a2
  | ( ( Pbound | Pvar _ | Pint _ | Pbool _ | Pconstr _ | Pfun _ | Pfield _
      | Pis _ | Pbinop _ | Pand _ | Por _ | Pnot _ ),
      _ ) -> false
;;

(* Substitute program variable [id] by predicate [by] (dependent application and lambda
   opening: [by] is always a [Pvar]). *)
let rec subst_var id ~by p =
  match p with
  | Pvar id' when Ident.same id id' -> by
  | Pbound | Pvar _ | Pint _ | Pbool _ -> p
  | Pconstr (path, c, args) -> Pconstr (path, c, List.map (subst_var id ~by) args)
  | Pfun (f, args) -> Pfun (f, List.map (subst_var id ~by) args)
  | Pfield (path, l, a) -> Pfield (path, l, subst_var id ~by a)
  | Pis (path, c, a) -> Pis (path, c, subst_var id ~by a)
  | Pbinop (op, a, b) -> Pbinop (op, subst_var id ~by a, subst_var id ~by b)
  | Pand (a, b) -> Pand (subst_var id ~by a, subst_var id ~by b)
  | Por (a, b) -> Por (subst_var id ~by a, subst_var id ~by b)
  | Pnot a -> Pnot (subst_var id ~by a)
;;

(* Substitute the bound variable [v] (used when instantiating a refinement at a logical
   name). *)
let rec subst_bound ~by p =
  match p with
  | Pbound -> by
  | Pvar _ | Pint _ | Pbool _ -> p
  | Pconstr (path, c, args) -> Pconstr (path, c, List.map (subst_bound ~by) args)
  | Pfun (f, args) -> Pfun (f, List.map (subst_bound ~by) args)
  | Pfield (path, l, a) -> Pfield (path, l, subst_bound ~by a)
  | Pis (path, c, a) -> Pis (path, c, subst_bound ~by a)
  | Pbinop (op, a, b) -> Pbinop (op, subst_bound ~by a, subst_bound ~by b)
  | Pand (a, b) -> Pand (subst_bound ~by a, subst_bound ~by b)
  | Por (a, b) -> Por (subst_bound ~by a, subst_bound ~by b)
  | Pnot a -> Pnot (subst_bound ~by a)
;;

let rec free_vars acc p =
  match p with
  | Pvar id -> id :: acc
  | Pbound | Pint _ | Pbool _ -> acc
  | Pconstr (_, _, args) | Pfun (_, args) -> List.fold_left free_vars acc args
  | Pfield (_, _, a) | Pis (_, _, a) -> free_vars acc a
  | Pbinop (_, a, b) | Pand (a, b) | Por (a, b) -> free_vars (free_vars acc a) b
  | Pnot a -> free_vars acc a
;;

let free_vars p = free_vars [] p

let rec mem_var id p =
  match p with
  | Pvar id' -> Ident.same id id'
  | Pbound | Pint _ | Pbool _ -> false
  | Pconstr (_, _, args) | Pfun (_, args) -> List.exists (mem_var id) args
  | Pfield (_, _, a) | Pis (_, _, a) -> mem_var id a
  | Pbinop (_, a, b) | Pand (a, b) | Por (a, b) -> mem_var id a || mem_var id b
  | Pnot a -> mem_var id a
;;

(* Remap the type paths of constructor applications (used by [Subst] when a
   predicate crosses a module boundary, exactly as [Tconstr] paths do). *)
let rec map_paths f p =
  match p with
  | Pbound | Pvar _ | Pint _ | Pbool _ -> p
  | Pconstr (path, c, args) -> Pconstr (f path, c, List.map (map_paths f) args)
  | Pfun (g, args) -> Pfun (g, List.map (map_paths f) args)
  | Pfield (path, l, a) -> Pfield (f path, l, map_paths f a)
  | Pis (path, c, a) -> Pis (f path, c, map_paths f a)
  | Pbinop (op, a, b) -> Pbinop (op, map_paths f a, map_paths f b)
  | Pand (a, b) -> Pand (map_paths f a, map_paths f b)
  | Por (a, b) -> Por (map_paths f a, map_paths f b)
  | Pnot a -> Pnot (map_paths f a)
;;

let rec constr_paths acc p =
  match p with
  | Pbound | Pvar _ | Pint _ | Pbool _ -> acc
  | Pconstr (path, _, args) -> List.fold_left constr_paths (path :: acc) args
  | Pfun (_, args) -> List.fold_left constr_paths acc args
  | Pfield (path, _, a) | Pis (path, _, a) -> constr_paths (path :: acc) a
  | Pbinop (_, a, b) | Pand (a, b) | Por (a, b) -> constr_paths (constr_paths acc a) b
  | Pnot a -> constr_paths acc a
;;

let constr_paths p = constr_paths [] p

(* Does [p] apply any spec function?  The [-vox-prelude] (which defines
   them) is injected only into solver inputs that need it: it may
   reference datatypes of one module, which do not exist in another
   module's input. *)
let rec mentions_spec_fun p =
  match p with
  | Pbound | Pvar _ | Pint _ | Pbool _ -> false
  | Pfun _ -> true
  | Pconstr (_, _, args) -> List.exists mentions_spec_fun args
  | Pfield (_, _, a) | Pis (_, _, a) | Pnot a -> mentions_spec_fun a
  | Pbinop (_, a, b) | Pand (a, b) | Por (a, b) ->
    mentions_spec_fun a || mentions_spec_fun b
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
  | Pint n -> pp_print_int ppf n
  | Pbool b -> pp_print_bool ppf b
  | Pconstr (_, c, []) -> pp_print_string ppf c
  | Pconstr (_, c, [ a ]) -> fprintf ppf "@[%s %a@]" c print_atom a
  | Pconstr (_, c, a :: args) ->
    fprintf ppf "@[%s (%a" c print a;
    List.iter (fun x -> fprintf ppf ",@ %a" print x) args;
    fprintf ppf ")@]"
  | Pfun (f, []) -> pp_print_string ppf f
  | Pfun (f, args) ->
    fprintf ppf "@[%s" f;
    List.iter (fun x -> fprintf ppf "@ %a" print_atom x) args;
    fprintf ppf "@]"
  | Pfield (_, l, a) -> fprintf ppf "%a.%s" print_atom a l
  | Pis (_, c, a) -> fprintf ppf "@[%a is@ %s@]" print_atom a c
  | Pbinop (op, a, b) ->
    fprintf ppf "@[%a %s@ %a@]" print_atom a (binop_name op) print_atom b
  | Pand (a, b) -> fprintf ppf "@[%a &&@ %a@]" print_atom a print_atom b
  | Por (a, b) -> fprintf ppf "@[%a ||@ %a@]" print_atom a print_atom b
  | Pnot a -> fprintf ppf "@[not %a@]" print_atom a

and print_atom ppf p =
  match p with
  | Pbound | Pvar _ | Pint _ | Pbool _ | Pconstr (_, _, []) | Pfun (_, [])
  | Pfield _ -> print ppf p
  | Pconstr (_, _, _ :: _) | Pfun (_, _ :: _) | Pis _ | Pbinop _ | Pand _
  | Por _ | Pnot _ -> Format.fprintf ppf "(%a)" print p
;;

let to_string p = Format.asprintf "%a" print p
