module D = Hm_declarative
open Hm_interpreter_typing

let rec (erase_typing @ total) :
    (n : D.index) @ immutable -> (g : D.context) @ immutable ->
    (e : D.term) @ immutable -> (t : D.mono) @ immutable ->
    (d : D.typing) @ immutable -> {u : unit | D.typed n g e t d} ->
    {u : unit | typed g e t d} @ ghost = fun n g e t d premise -> ghost_ (
  D.typed_def n g e t d; typed_def g e t d;
  (match d with
  | D.Variable _ | D.Constant | D.Word_constant | D.Empty_list _ -> ()
  | D.List_cons (a, head, tail) -> (match e with
    | D.Cons (h, r) -> erase_typing n g h a head (); erase_typing n g r t tail ()
    | _ -> ())
  | D.List_case (a, scrutinee, empty, nonempty) -> (match e with
    | D.CaseList (s, l, r) -> erase_typing n g s (D.List_type a) scrutinee ();
      erase_typing n g l t empty ();
      erase_typing n (D.Binding (D.Forall (D.Z, a),
        D.Binding (D.Forall (D.Z, D.List_type a), g))) r t nonempty ()
    | _ -> ())
  | D.Conditional (condition, yes, no) -> (match e with
    | D.If (c, a, b) -> erase_typing n g c D.Boolean condition ();
      erase_typing n g a t yes (); erase_typing n g b t no ()
    | _ -> ())
  | D.Word_primitive (left, right) -> (match e with
    | D.Primitive (_, a, b) -> erase_typing n g a D.Word64 left ();
      erase_typing n g b D.Word64 right ()
    | _ -> ())
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
