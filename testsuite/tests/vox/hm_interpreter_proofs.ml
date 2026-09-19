module D = Hm_declarative
open Hm_interpreter_typing

let rec (erase_typing @ total) :
    (n : D.index) @ immutable -> (g : D.context) @ immutable ->
    (e : D.term) @ immutable -> (t : D.mono) @ immutable ->
    (d : D.typing) @ immutable -> {u : unit | D.typed n g e t d} ->
    {u : unit | typed g e t d} @ ghost = fun n g e t d premise -> ghost_ (
  D.typed_def n g e t d; typed_def g e t d;
  (match d with
  | D.Variable _ | D.Constant -> ()
  | D.Abstraction (a, body) -> (match e, t with
    | D.Lambda e, D.Function (_, b) ->
      erase_typing n (D.Binding (D.Forall (D.Z, a), g)) e b body ()
    | _ -> ())
  | D.Application (a, left, right) -> (match e with
    | D.Apply (f, x) -> erase_typing n g f (D.Function (a, t)) left ();
      erase_typing n g x a right ()
    | _ -> ())
  | D.Recursion (a, b, body) -> (match e with
    | D.Recursive e -> erase_typing n
      (D.Binding (D.Forall (D.Z, a), D.Binding (D.Forall (D.Z, t), g)))
      e b body ()
    | _ -> ())
  | D.Let_binding (s, rhs, body) -> (match e, s with
    | D.Let (r, b), D.Forall (k, a) ->
      erase_typing (D.add k n) (D.weaken_context k g) r a rhs ();
      erase_typing n (D.Binding (s, g)) b t body ()
    | _ -> ()));
  ())
