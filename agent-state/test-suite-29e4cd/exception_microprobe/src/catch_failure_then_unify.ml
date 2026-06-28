exception Unify

type term = Var of int | Prop of int * term list
type subst = Bind of int * term

let rec get_binding v = function
  | [] -> failwith "unbound"
  | Bind (w, t) :: rest -> if v = w then t else get_binding v rest

let rec unify1 term1 term2 subst =
  match term2 with
  | Var v ->
      (try
         if get_binding v subst = term1 then subst else raise_notrace Unify
       with Failure _ -> Bind (v, term1) :: subst)
  | Prop (head2, argl2) ->
      (match term1 with
       | Var _ -> raise_notrace Unify
       | Prop (head1, argl1) ->
           if head1 = head2 then unify1_lst argl1 argl2 subst else raise_notrace Unify)

and unify1_lst l1 l2 subst =
  match l1, l2 with
  | [], [] -> subst
  | h1 :: r1, h2 :: r2 -> unify1_lst r1 r2 (unify1 h1 h2 subst)
  | _ -> raise_notrace Unify

let rec rewrite_with_lemmas term lemmas =
  match lemmas with
  | [] -> term
  | (t1, t2) :: rest ->
      try
        let subst = unify1 term t1 [] in
        ignore subst;
        t2
      with Unify -> rewrite_with_lemmas term rest

let term = Prop (42, [Prop (1, [Var 0]); Prop (2, [Var 1]); Prop (3, [Var 2])])
let lemmas =
  [ Prop (1, [Var 0]), Var 0
  ; Prop (2, [Var 0]), Var 0
  ; Prop (3, [Var 0]), Var 0
  ; Prop (4, [Var 0]), Var 0
  ; Prop (5, [Var 0]), Var 0
  ; Prop (6, [Var 0]), Var 0
  ; Prop (7, [Var 0]), Var 0
  ; Prop (8, [Var 0]), Var 0
  ]

let n = 7_000_000

let run () =
  let acc = ref 0 in
  for i = 1 to n do
    match rewrite_with_lemmas term lemmas with
    | Var x -> acc := !acc + x
    | Prop (h, _) -> acc := !acc + h + (i land 1)
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
