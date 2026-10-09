(* Thin session wrapper: forwards the Env-free constructors to {!Node} with the context's
   intern state, and owns the two constructor families that consult the {!Env} (App/const
   rank checks; the reserved div/mod symbols). *)

type t =
  { env : Env.t
  ; state : Node.state
  }

let create env = { env; state = Node.create_state () }
let term_count t = Node.term_count t.state

(* The native-[int] arithmetic API is a thin wrapper over the arbitrary-precision core:
   every [int] argument widens through [Bigint.of_int]. The [_big] variants take a
   [Bigint.t] directly and are the path for coefficients/literals that exceed int63 (the
   parser's big numerals, the presolve substitution rebuild). *)
let int_const_big t n = Node.int_const t.state n
let int_const t n = Node.int_const t.state (Bigint.of_int n)
let bool_const t b = Node.bool_const t.state b
let add t a b = Node.add t.state a b
let sub t a b = Node.sub t.state a b
let neg t a = Node.neg t.state a
let mul_const_big t c a = Node.mul_const t.state c a
let mul_const t c a = Node.mul_const t.state (Bigint.of_int c) a
let linear_combination_big t pairs const = Node.linear_combination t.state pairs const

let linear_combination t pairs const =
  Node.linear_combination
    t.state
    (List.map (fun (c, tm) -> Bigint.of_int c, tm) pairs)
    (Bigint.of_int const)
;;

let eq t a b = Node.eq t.state a b
let le t a b = Node.le t.state a b
let lt t a b = Node.lt t.state a b
let ge t a b = Node.ge t.state a b
let gt t a b = Node.gt t.state a b
let not_ t a = Node.not_ t.state a
let and_ t xs = Node.and_ t.state xs
let or_ t xs = Node.or_ t.state xs
let implies t a b = Node.implies t.state a b
let iff t a b = Node.iff t.state a b
let ite t c a b = Node.ite t.state c a b
let distinct t xs = Node.distinct t.state xs
let abs t a = Node.abs t.state a

(* App with rank check; the only place ranks are consulted (ADR-0003 Decision 6). Unknown
   symbol / arity / argument-sort mismatch is a [Sort_error]. *)
let app t sym args =
  let rank =
    match Env.rank t.env sym with
    | r -> r
    | exception Not_found ->
      raise
        (Node.Sort_error (Printf.sprintf "symbol %s is not declared" (Symbol.name sym)))
  in
  let arity = Iarr.length rank.Rank.domain in
  if List.length args <> arity
  then
    raise
      (Node.Sort_error
         (Printf.sprintf
            "%s applied to %d args, expected %d"
            (Symbol.name sym)
            (List.length args)
            arity));
  List.iteri
    (fun i a ->
       if not (Sort.equal a.Term.sort (Iarr.get rank.Rank.domain i))
       then
         raise
           (Node.Sort_error
              (Printf.sprintf "%s argument %d has the wrong sort" (Symbol.name sym) i)))
    args;
  Node.app t.state sym args rank.Rank.codomain
;;

let const t sym = app t sym []

let divmod t which x d =
  Node.require_int which x;
  Node.require_int which d;
  match d.Term.node with
  | Term.Int_const k when Bigint.is_zero k ->
    raise (Node.Unsupported (Printf.sprintf "%s by zero" which))
  | Term.Int_const _ ->
    let sym = if String.equal which "div" then Env.div_sym t.env else Env.mod_sym t.env in
    Node.app t.state sym [ x; d ] Sort.int
  | _ -> raise (Node.Unsupported (Printf.sprintf "%s by a non-constant divisor" which))
;;

let div t x d = divmod t "div" x d
let mod_ t x d = divmod t "mod" x d
