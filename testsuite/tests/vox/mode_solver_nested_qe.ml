(* TEST
 flags = "-extension refinement_types";
 has-z3;
 native;
*)

type term : immutable_data =
  | Const of bool
  | Var of int
  | Join of term * term
  | Meet of term * term
[@@inductive]

type qf : immutable_data =
  | Le of term * term
  | And of qf * qf
  | Or of qf * qf
  | Not of qf
[@@inductive]

type formula : immutable_data =
  | Plain of qf
  | Conj of formula * formula
  | Disj of formula * formula
  | Neg of formula
  | Exists of formula
  | Forall of formula
[@@inductive]

let[@def] rec (lookup @ total) env i =
  if i < 0 then false
  else match env with
  | [] -> false
  | b :: rest -> if i = 0 then b else lookup rest (i - 1)

let[@def] rec (eval_term @ total) env t =
  match t with
  | Const b -> b
  | Var i -> lookup env i
  | Join (a, b) -> eval_term env a || eval_term env b
  | Meet (a, b) -> eval_term env a && eval_term env b

let[@def] rec (eval_qf @ total) env q =
  match q with
  | Le (a, b) -> not (eval_term env a) || eval_term env b
  | And (a, b) -> eval_qf env a && eval_qf env b
  | Or (a, b) -> eval_qf env a || eval_qf env b
  | Not a -> not (eval_qf env a)

let[@def] rec (eval @ total) env f =
  match f with
  | Plain q -> eval_qf env q
  | Conj (a, b) -> eval env a && eval env b
  | Disj (a, b) -> eval env a || eval env b
  | Neg a -> not (eval env a)
  | Exists a -> eval (false :: env) a || eval (true :: env) a
  | Forall a -> eval (false :: env) a && eval (true :: env) a

let[@def] rec (subst_term @ total) value t =
  match t with
  | Const b -> Const b
  | Var i ->
    if i = 0 then Const value
    else if i > 0 then Var (i - 1)
    else Var i
  | Join (a, b) -> Join (subst_term value a, subst_term value b)
  | Meet (a, b) -> Meet (subst_term value a, subst_term value b)

let[@def] rec (subst_qf @ total) value q =
  match q with
  | Le (a, b) -> Le (subst_term value a, subst_term value b)
  | And (a, b) -> And (subst_qf value a, subst_qf value b)
  | Or (a, b) -> Or (subst_qf value a, subst_qf value b)
  | Not a -> Not (subst_qf value a)

let[@def] rec (eliminate @ total) f =
  match f with
  | Plain q -> q
  | Conj (a, b) -> And (eliminate a, eliminate b)
  | Disj (a, b) -> Or (eliminate a, eliminate b)
  | Neg a -> Not (eliminate a)
  | Exists a ->
    let q = eliminate a in
    Or (subst_qf false q, subst_qf true q)
  | Forall a ->
    let q = eliminate a in
    And (subst_qf false q, subst_qf true q)

let (lookup_shift @ total) :
    (env : bool list) -> (value : bool) -> (i : int) ->
    {u : unit |
      lookup (value :: env) i =
        if i = 0 then value
        else if i > 0 then lookup env (i - 1)
        else lookup env i} =
 fun env value i ->
  ghost_ (lookup_def (value :: env) i);
  if i < 0 then ghost_ (lookup_def env i);
  ()

let rec (subst_term_exact @ total) :
    (env : bool list) -> (value : bool) -> (t : term) ->
    {u : unit |
      eval_term env (subst_term value t) =
        eval_term (value :: env) t} =
 fun env value t ->
  ghost_ (subst_term_def value t);
  ghost_ (eval_term_def env (subst_term value t));
  ghost_ (eval_term_def (value :: env) t);
  match t with
  | Const _ -> ()
  | Var i ->
    ghost_ (lookup_shift env value i);
    (if i = 0 then ()
     else if i > 0 then ghost_ (lookup_def env (i - 1))
     else ghost_ (lookup_def env i));
    ()
  | Join (a, b) | Meet (a, b) ->
    ghost_ (subst_term_exact env value a);
    ghost_ (subst_term_exact env value b);
    ghost_ (eval_term_def env (subst_term value a));
    ghost_ (eval_term_def env (subst_term value b));
    ghost_ (eval_term_def (value :: env) a);
    ghost_ (eval_term_def (value :: env) b);
    ()

let rec (subst_qf_exact @ total) :
    (env : bool list) -> (value : bool) -> (q : qf) ->
    {u : unit |
      eval_qf env (subst_qf value q) =
        eval_qf (value :: env) q} =
 fun env value q ->
  ghost_ (subst_qf_def value q);
  ghost_ (eval_qf_def env (subst_qf value q));
  ghost_ (eval_qf_def (value :: env) q);
  match q with
  | Le (a, b) ->
    ghost_ (subst_term_exact env value a);
    ghost_ (subst_term_exact env value b);
    ()
  | And (a, b) | Or (a, b) ->
    ghost_ (subst_qf_exact env value a);
    ghost_ (subst_qf_exact env value b);
    ()
  | Not a ->
    ghost_ (subst_qf_exact env value a);
    ()

let rec (eliminate_exact @ total) :
    (env : bool list) -> (f : formula) ->
    {u : unit | eval_qf env (eliminate f) = eval env f} =
 fun env f ->
  ghost_ (eliminate_def f);
  ghost_ (eval_qf_def env (eliminate f));
  ghost_ (eval_def env f);
  match f with
  | Plain _ -> ()
  | Conj (a, b) | Disj (a, b) ->
    ghost_ (eliminate_exact env a);
    ghost_ (eliminate_exact env b);
    ()
  | Neg a ->
    ghost_ (eliminate_exact env a);
    ()
  | Exists a | Forall a ->
    ghost_ (eliminate_exact (false :: env) a);
    ghost_ (eliminate_exact (true :: env) a);
    ghost_ (subst_qf_exact env false (eliminate a));
    ghost_ (subst_qf_exact env true (eliminate a));
    ()

let equality a b =
  And (Le (a, b), Le (b, a))

let forall_exists_equal =
  Forall (Exists (Plain (equality (Var 1) (Var 0))))

let exists_forall_equal =
  Exists (Forall (Plain (equality (Var 1) (Var 0))))

let outer_example =
  Forall
    (Disj
       (Neg (Plain (Le (Var 0, Var 1))),
        Exists
          (Plain
             (And
                (Le (Var 0, Const false),
                 Le (Var 1, Var 0))))))

let guarded_disjunction =
  Forall
    (Disj
       (Neg
          (Plain
             (And (Le (Var 1, Var 0), Le (Var 2, Var 0)))),
        Exists
          (Plain
             (And
                (Le (Const true, Var 0), Le (Var 0, Var 1))))))

let () =
  assert (eval_qf [] (eliminate forall_exists_equal));
  assert (not (eval_qf [] (eliminate exists_forall_equal)));
  assert (eval_qf [false] (eliminate outer_example));
  assert (not (eval_qf [true] (eliminate outer_example)));
  assert (not (eval_qf [false; false] (eliminate guarded_disjunction)));
  assert (eval_qf [true; false] (eliminate guarded_disjunction));
  assert (eval_qf [false; true] (eliminate guarded_disjunction));
  assert (eval_qf [true; true] (eliminate guarded_disjunction))
