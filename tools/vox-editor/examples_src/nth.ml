type ilist =
  | Nil
  | Cons of int * ilist

let rec total_ len l =
  match l with
  | Nil -> 0
  | Cons (_, t) -> 1 + len t

let rec append (a : ilist) (b : ilist) : ilist{ len _ = len a + len b } =
  match a with
  | Nil -> b
  | Cons (h, t) ->
    let r = append t b in
    Cons (h, r)

let rec unreachable_ (u : unit{ false }) : 'a = unreachable_ u

let rec nth (l : ilist) (i : int{ 0 <= _ && _ < len l }) : int =
  match l with
  | Nil -> unreachable_ ()
  | Cons (h, t) -> if i = 0 then h else nth t (i - 1)
