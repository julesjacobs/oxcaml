(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Task #53 (finding C1) TIER 1: [f expr] works whenever [expr] can be
   reflected into the logic, so a dependent argument may be any
   reflectable expression -- not only a variable or a literal.  Each
   [let] below forces the argument's term to instantiate the callee's
   dependent contract; the goal reduces to a definitional identity and
   discharges. *)

type ilist = INil | ICons of int * ilist
type vopt = Vnone | Vsome of int

let rec total_ len (l : ilist) : int =
  match l with
  | INil -> 0
  | ICons (_, t) -> 1 + len t

let rec total_ is_some (o : vopt) : int =
  match o with
  | Vnone -> 0
  | Vsome _ -> 1

let f_l : (l : ilist) -> int{ _ = len l } = fun l -> len l
let f_o : (o : vopt) -> int{ _ = is_some o } = fun o -> is_some o
let f_i : (n : int) -> int{ _ = n } = fun n -> n

(* CONSTRUCTOR argument (single-, multi-arity, and nullary). *)
let c_single (x : int) : int{ _ = is_some (Vsome x) } = f_o (Vsome x)
let c_multi (x : int) (t : ilist) : int{ _ = len (ICons (x, t)) } =
  f_l (ICons (x, t))
let c_nullary : int{ _ = len INil } = f_l INil

(* FACT THREADING: the declared result differs syntactically from the
   body's [len (ICons (x, t))], so the constructor-substituted result
   fact must reach the walker (via [translate_nameable] in
   [stable_arg_name]) for grind to unfold [len]. *)
let c_thread (x : int) (t : ilist) : int{ _ = 1 + len t } = f_l (ICons (x, t))

(* NESTED reflectable: constructor over arithmetic. *)
let c_nested (x : int) : int{ _ = is_some (Vsome (x + 1)) } =
  f_o (Vsome (x + 1))

type pt = { px : int; py : int }

(* IMMUTABLE FIELD READ argument. *)
let c_field (p : pt) : int{ _ = p.px } = f_i p.px

(* ARITHMETIC argument (already supported; pinned for consistency). *)
let a_arith (x : int) : int{ _ = x + 1 } = f_i (x + 1)

(* DIVISION argument: the logic's total T-division names [x / y]
   directly (OCaml's [/] raises only at [y = 0], which aborts the call
   before the result is bound, so the substituted fact holds under
   partial correctness -- see the DESIGN note in vox_reflect.ml). *)
let a_div (x : int) (y : int) : int{ _ = x / y } = f_i (x / y)
