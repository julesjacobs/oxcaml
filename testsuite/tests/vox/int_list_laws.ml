type t = Nil | Cons of int * t [@@inductive]

let[@def] rec append xs ys =
  match xs with
  | Nil -> ys
  | Cons (head, tail) -> Cons (head, append tail ys)

let[@def] rec length xs =
  match xs with
  | Nil -> 0
  | Cons (_, tail) -> 1 + length tail

let[@def] rec sum xs =
  match xs with
  | Nil -> 0
  | Cons (head, tail) -> head + sum tail

module Laws = struct
  let append_nil_left ys : {u : unit | append Nil ys === ys} =
    append_def Nil ys

  let rec append_nil_right :
      (xs : t) -> {u : unit | append xs Nil === xs} =
    fun xs ->
    append_def xs Nil;
    match xs with
    | Nil -> ()
    | Cons (_, tail) -> append_nil_right tail

  let rec append_associative :
      (xs : t) -> (ys : t) -> (zs : t) ->
      {u : unit |
        append (append xs ys) zs === append xs (append ys zs)} =
    fun xs ys zs ->
    append_def xs ys;
    append_def (append xs ys) zs;
    append_def ys zs;
    append_def xs (append ys zs);
    match xs with
    | Nil -> ()
    | Cons (_, tail) -> append_associative tail ys zs

  let rec length_append :
      (xs : t) -> (ys : t) ->
      {u : unit | length (append xs ys) === length xs + length ys} =
    fun xs ys ->
    append_def xs ys;
    length_def (append xs ys);
    length_def xs;
    match xs with
    | Nil -> ()
    | Cons (_, tail) -> length_append tail ys

  let rec sum_append :
      (xs : t) -> (ys : t) ->
      {u : unit | sum (append xs ys) === sum xs + sum ys} =
    fun xs ys ->
    append_def xs ys;
    sum_def (append xs ys);
    sum_def xs;
    match xs with
    | Nil -> ()
    | Cons (_, tail) -> sum_append tail ys
end
