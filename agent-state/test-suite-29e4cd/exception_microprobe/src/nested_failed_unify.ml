exception Unify

type term = Var of int | Prop of int * term list
type subst = Bind of int * term

let rec get_binding v = function
  | [] -> raise_notrace Unify
  | Bind (w, t) :: rest -> if v = w then t else get_binding v rest

let rec unify1 term1 term2 subst =
  match term2 with
  | Var v ->
      (try
         if get_binding v subst = term1 then subst else raise_notrace Unify
       with Unify -> Bind (v, term1) :: subst)
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

let rec try_rules term acc = function
  | [] -> acc
  | (t1, v) :: rest ->
      let acc =
        try
          let subst = unify1 term t1 [] in
          acc + List.length subst + v
        with Unify -> acc + 1
      in
      try_rules term acc rest

let term = Prop (100, [Prop (10, [Var 0; Var 1]); Prop (20, [Var 2; Var 3])])

let rules =
  [ Prop (1, [Var 0]), 1
  ; Prop (2, [Var 0]), 2
  ; Prop (3, [Var 0]), 3
  ; Prop (4, [Var 0]), 4
  ; Prop (5, [Var 0]), 5
  ; Prop (6, [Var 0]), 6
  ; Prop (7, [Var 0]), 7
  ; Prop (8, [Var 0]), 8
  ; Prop (9, [Var 0]), 9
  ; Prop (11, [Var 0]), 11
  ; Prop (12, [Var 0]), 12
  ; Prop (13, [Var 0]), 13
  ]

let n = 6_000_000

let run () =
  let acc = ref 0 in
  for i = 1 to n do
    acc := !acc + try_rules term (i land 1) rules
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
