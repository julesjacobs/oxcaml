(* [@def] makes a total function's definition available to proofs: it adds
   an equation [length_def xs] stating [length xs === match xs with ...].

   A lemma is an ordinary total function whose result type is the theorem;
   its recursive call is the induction hypothesis. Clients call lemmas
   inside [ghost_], so the proof is checked but never run. *)

let[@def] rec (length @ total) (xs : int list) : int =
  match xs with [] -> 0 | _ :: rest -> 1 + length rest

let[@def] rec (append @ total) (xs : int list) (ys : int list) : int list =
  match xs with [] -> ys | x :: rest -> x :: append rest ys

let rec (length_append @ total) (xs : int list) (ys : int list) :
    {u : unit | length (append xs ys) = length xs + length ys} =
  append_def xs ys;
  length_def xs;
  match xs with
  | [] -> ()
  | _ :: rest ->
    length_def (append xs ys);
    length_append rest ys

let (concat_length @ total) (xs : int list) (ys : int list) :
    {n : int | n = length xs + length ys} =
  ghost_ (length_append xs ys);
  length (append xs ys)
