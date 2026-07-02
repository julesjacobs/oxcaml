(* Refinement predicates for vox refined types [{v:int | p}].

   Predicates are untyped logical terms: the compiler never type checks them; ill-sorted
   predicates surface as solver errors at VC time, which count as verification failures.
   The bound value variable [v] is [Pbound]; program variables are identified by [Ident.t]
   stamp. *)

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
  | Pvar of Ident.t (* logical value of a program variable *)
  | Pparam of int * string
    (* parameter of an enclosing dependent arrow, as a de Bruijn index
       counting arrows outward from the predicate's position (innermost
       enclosing arrow = 0).  The string is the source name, used for
       printing ONLY: equality compares the index, so alpha-equivalent
       dependent signatures are equal.  Substituted with the argument's
       [Pvar] stamp at application (and with the parameter's stamp when
       a lambda is checked against a dependent arrow). *)
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

(* Structural equality; program variables compare by stamp. This is the equality
   unification uses: "v > 0" and "0 < v" are NOT equal, by design. *)
let rec equal p1 p2 =
  match p1, p2 with
  | Pbound, Pbound -> true
  | Pvar id1, Pvar id2 -> Ident.same id1 id2
  | Pparam (i1, _), Pparam (i2, _) -> Int.equal i1 i2
  | Pint n1, Pint n2 -> Int.equal n1 n2
  | Pbool b1, Pbool b2 -> Bool.equal b1 b2
  | Pbinop (op1, a1, b1), Pbinop (op2, a2, b2) -> op1 = op2 && equal a1 a2 && equal b1 b2
  | Pand (a1, b1), Pand (a2, b2) | Por (a1, b1), Por (a2, b2) ->
    equal a1 a2 && equal b1 b2
  | Pnot a1, Pnot a2 -> equal a1 a2
  | ( ( Pbound | Pvar _ | Pparam _ | Pint _ | Pbool _ | Pbinop _ | Pand _
      | Por _ | Pnot _ ),
      _ ) -> false
;;

(* Substitute program variable [id] by predicate [by] (used by dependent application: [by]
   is always a [Pvar]). *)
let rec subst_var id ~by p =
  match p with
  | Pvar id' when Ident.same id id' -> by
  | Pbound | Pvar _ | Pparam _ | Pint _ | Pbool _ -> p
  | Pbinop (op, a, b) -> Pbinop (op, subst_var id ~by a, subst_var id ~by b)
  | Pand (a, b) -> Pand (subst_var id ~by a, subst_var id ~by b)
  | Por (a, b) -> Por (subst_var id ~by a, subst_var id ~by b)
  | Pnot a -> Pnot (subst_var id ~by a)
;;

(* Substitute the dependent-arrow parameter at de Bruijn index [index]
   (relative to the predicate's position) by [by].  Predicates contain
   no internal binders, so no shifting is needed; indices other than
   [index] are left alone (smaller ones refer to inner arrows, larger
   ones are escapes caught elsewhere). *)
let rec subst_param ~index ~by p =
  match p with
  | Pparam (i, _) when Int.equal i index -> by
  | Pbound | Pvar _ | Pparam _ | Pint _ | Pbool _ -> p
  | Pbinop (op, a, b) ->
    Pbinop (op, subst_param ~index ~by a, subst_param ~index ~by b)
  | Pand (a, b) -> Pand (subst_param ~index ~by a, subst_param ~index ~by b)
  | Por (a, b) -> Por (subst_param ~index ~by a, subst_param ~index ~by b)
  | Pnot a -> Pnot (subst_param ~index ~by a)
;;

let rec mem_param index p =
  match p with
  | Pparam (i, _) -> Int.equal i index
  | Pbound | Pvar _ | Pint _ | Pbool _ -> false
  | Pbinop (_, a, b) | Pand (a, b) | Por (a, b) ->
    mem_param index a || mem_param index b
  | Pnot a -> mem_param index a
;;

(* Largest parameter index in [p], or -1 if none: an index >= the
   number of enclosing dependent arrows is an escaped dependency. *)
let rec max_param p =
  match p with
  | Pparam (i, _) -> i
  | Pbound | Pvar _ | Pint _ | Pbool _ -> -1
  | Pbinop (_, a, b) | Pand (a, b) | Por (a, b) ->
    Int.max (max_param a) (max_param b)
  | Pnot a -> max_param a
;;

(* Substitute the bound variable [v] (used when instantiating a refinement at a logical
   name). *)
let rec subst_bound ~by p =
  match p with
  | Pbound -> by
  | Pvar _ | Pparam _ | Pint _ | Pbool _ -> p
  | Pbinop (op, a, b) -> Pbinop (op, subst_bound ~by a, subst_bound ~by b)
  | Pand (a, b) -> Pand (subst_bound ~by a, subst_bound ~by b)
  | Por (a, b) -> Por (subst_bound ~by a, subst_bound ~by b)
  | Pnot a -> Pnot (subst_bound ~by a)
;;

let rec free_vars acc p =
  match p with
  | Pvar id -> id :: acc
  | Pbound | Pparam _ | Pint _ | Pbool _ -> acc
  | Pbinop (_, a, b) | Pand (a, b) | Por (a, b) -> free_vars (free_vars acc a) b
  | Pnot a -> free_vars acc a
;;

let free_vars p = free_vars [] p

let rec mem_var id p =
  match p with
  | Pvar id' -> Ident.same id id'
  | Pbound | Pparam _ | Pint _ | Pbool _ -> false
  | Pbinop (_, a, b) | Pand (a, b) | Por (a, b) -> mem_var id a || mem_var id b
  | Pnot a -> mem_var id a
;;

(* Printing. [v] prints as "v"; program variables print with their source name (unique
   enough for diagnostics; stamps available via [print_raw]). *)
let rec print ppf p =
  let open Format in
  match p with
  | Pbound -> pp_print_string ppf "v"
  | Pvar id -> pp_print_string ppf (Ident.name id)
  | Pparam (i, name) ->
    if String.equal name ""
    then fprintf ppf "<param.%d>" i
    else pp_print_string ppf name
  | Pint n -> pp_print_int ppf n
  | Pbool b -> pp_print_bool ppf b
  | Pbinop (op, a, b) ->
    fprintf ppf "@[%a %s@ %a@]" print_atom a (binop_name op) print_atom b
  | Pand (a, b) -> fprintf ppf "@[%a &&@ %a@]" print_atom a print_atom b
  | Por (a, b) -> fprintf ppf "@[%a ||@ %a@]" print_atom a print_atom b
  | Pnot a -> fprintf ppf "@[not %a@]" print_atom a

and print_atom ppf p =
  match p with
  | Pbound | Pvar _ | Pparam _ | Pint _ | Pbool _ -> print ppf p
  | Pbinop _ | Pand _ | Por _ | Pnot _ -> Format.fprintf ppf "(%a)" print p
;;

let to_string p = Format.asprintf "%a" print p
