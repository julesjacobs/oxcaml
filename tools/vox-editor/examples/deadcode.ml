(* The write [m <- K 9] is a fact, so the [L] arm's hypotheses are
   contradictory: [unreachable_] proves it dead, and [K y] returns
   [y = 9] by constructor injectivity. *)

type kl =
  | K of int
  | L

let rec unreachable_ (u : unit{ false }) : 'a = unreachable_ u

let get () : int{ _ = 9 } =
  let mutable m = L in
  m <- K 9;
  match m with
  | K y -> y
  | L -> unreachable_ ()
