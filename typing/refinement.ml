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
  | Pnot a1, Pnot a2 -> equal a1 a2
  | (Pbound | Pvar _ | Pint _ | Pbool _ | Pbinop _ | Pand _ | Por _ | Pnot _), _ -> false
;;

(* Substitute program variable [id] by predicate [by] (dependent application and lambda
   opening: [by] is always a [Pvar]). *)
let rec subst_var id ~by p =
  match p with
  | Pvar id' when Ident.same id id' -> by
  | Pbound | Pvar _ | Pint _ | Pbool _ -> p
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
  | Pbinop (op, a, b) -> Pbinop (op, subst_bound ~by a, subst_bound ~by b)
  | Pand (a, b) -> Pand (subst_bound ~by a, subst_bound ~by b)
  | Por (a, b) -> Por (subst_bound ~by a, subst_bound ~by b)
  | Pnot a -> Pnot (subst_bound ~by a)
;;

let rec free_vars acc p =
  match p with
  | Pvar id -> id :: acc
  | Pbound | Pint _ | Pbool _ -> acc
  | Pbinop (_, a, b) | Pand (a, b) | Por (a, b) -> free_vars (free_vars acc a) b
  | Pnot a -> free_vars acc a
;;

let free_vars p = free_vars [] p

let rec mem_var id p =
  match p with
  | Pvar id' -> Ident.same id id'
  | Pbound | Pint _ | Pbool _ -> false
  | Pbinop (_, a, b) | Pand (a, b) | Por (a, b) -> mem_var id a || mem_var id b
  | Pnot a -> mem_var id a
;;

(* Printing, in the compact surface format: the bound value variable prints as [_];
   program variables print with their source name (unique enough for diagnostics). *)
let rec print ppf p =
  let open Format in
  match p with
  | Pbound -> pp_print_string ppf "_"
  | Pvar id -> pp_print_string ppf (Ident.name id)
  | Pint n -> pp_print_int ppf n
  | Pbool b -> pp_print_bool ppf b
  | Pbinop (op, a, b) ->
    fprintf ppf "@[%a %s@ %a@]" print_atom a (binop_name op) print_atom b
  | Pand (a, b) -> fprintf ppf "@[%a &&@ %a@]" print_atom a print_atom b
  | Por (a, b) -> fprintf ppf "@[%a ||@ %a@]" print_atom a print_atom b
  | Pnot a -> fprintf ppf "@[not %a@]" print_atom a

and print_atom ppf p =
  match p with
  | Pbound | Pvar _ | Pint _ | Pbool _ -> print ppf p
  | Pbinop _ | Pand _ | Por _ | Pnot _ -> Format.fprintf ppf "(%a)" print p
;;

let to_string p = Format.asprintf "%a" print p
