(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Task #67: a boolean CONNECTIVE (&&/||/not) or a CONSTRUCTOR wrapping a
   tier-2 call, in result position, must thread its operand / sub-call
   result facts -- not collapse to an opaque unknown, which spuriously
   DISPROVES a TRUE goal.  Connectives route through [decompose_bool]
   (operand facts, short-circuit-guarded); constructor sub-positions
   route through [call_result_name] ([translate_nameable] recursion).
   Before the fix each of these verified as a single leaf but DISPROVED
   once composed. *)

type vopt = Vnone | Vsome of int

let rec total_ has (o : vopt) : bool =
  match o with
  | Vnone -> false
  | Vsome _ -> true

let rec total_ cnt (o : vopt) : int =
  match o with
  | Vnone -> 0
  | Vsome _ -> 1

let hasb : (o : vopt) -> bool{ _ = has o } = fun o -> has o
let cntf : (o : vopt) -> int{ _ = cnt o } = fun o -> cnt o
let succ1 : (n : int) -> int{ _ = n + 1 } = fun n -> n + 1

(* && / || / not of dependent-call (constructor-arg) operands. *)
let both (x : int) : bool{ _ = true } = hasb (Vsome x) && hasb (Vsome x)
let either (x : int) : bool{ _ = true } = hasb Vnone || hasb (Vsome x)
let neg (x : int) : bool{ _ = false } = not (hasb (Vsome x))

(* Constructor WRAPPING a tier-2 call, differing declared result (needs
   the sub-call named [succ1 x = x + 1] under the constructor). *)
let wrap (x : int) : int{ _ = 1 } = cntf (Vsome (succ1 x))
