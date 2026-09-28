module D = Hm_declarative
module If = Hm_conditional_constraints
module List_case = Hm_list_case_constraints
module Primitive = Hm_primitive_constraints

let (conditional @ total) : (n : D.index) @ immutable ghost -> (g : D.context) @ immutable ghost ->
    (condition : D.term) @ immutable ghost -> (yes : D.term) @ immutable ghost -> (no : D.term) @ immutable ghost ->
    (ty : D.mono) @ immutable ghost -> (d : D.typing) @ immutable ->
    {u : unit | D.typed n g (If.encoded condition yes no) ty d} ->
    {out : D.typing | D.typed n g (D.If (condition, yes, no)) ty out} @ immutable =
  fun n g condition yes no ty d premise ->
    ghost_ (If.encoded_def condition yes no;
      D.typed_def n g (If.encoded condition yes no) ty d);
    match d with
    | D.Application (b, dfy, dn) ->
      ghost_ (D.typed_def n g (D.Apply (D.Apply (If.selector (), If.condition condition), yes)) (D.Function (b, ty)) dfy);
      (match dfy with
      | D.Application (a, df, dy) ->
        ghost_ (D.typed_def n g (D.Apply (If.selector (), If.condition condition)) (D.Function (a, D.Function (b, ty))) df);
        (match df with
        | D.Application (first, ds, dc) ->
          ghost_ (If.selector_invert n g first a b ty ds ();
            let _proof = If.invert_condition n g condition first dc () in
            If.condition_def condition; D.typed_def n g (If.condition condition) first dc);
          (match dc with
          | D.List_cons (_, condition_proof, _) ->
            let out = D.Conditional (condition_proof, dy, dn) in
            ghost_ (D.typed_def n g (D.If (condition, yes, no)) ty out); out
          | _ -> unreachable_ ())
        | _ -> unreachable_ ())
      | _ -> unreachable_ ())
    | _ -> unreachable_ ()

let (list_case @ total) : (n : D.index) @ immutable ghost -> (g : D.context) @ immutable ghost ->
    (scrutinee : D.term) @ immutable ghost -> (empty : D.term) @ immutable ghost -> (nonempty : D.term) @ immutable ghost ->
    (ty : D.mono) @ immutable ghost -> (d : D.typing) @ immutable ->
    {u : unit | D.typed n g (List_case.encoded scrutinee empty nonempty) ty d} ->
    {out : D.typing | D.typed n g (D.CaseList (scrutinee, empty, nonempty)) ty out} @ immutable =
  fun n g scrutinee empty nonempty ty d premise ->
    ghost_ (List_case.encoded_def scrutinee empty nonempty;
      D.typed_def n g (List_case.encoded scrutinee empty nonempty) ty d);
    match d with
    | D.Application (b, dfe, db) ->
      ghost_ (D.typed_def n g (D.Apply (D.Apply (List_case.selector (), scrutinee), empty)) (D.Function (b, ty)) dfe);
      (match dfe with
      | D.Application (e, df, de) ->
        ghost_ (D.typed_def n g (D.Apply (List_case.selector (), scrutinee)) (D.Function (e, D.Function (b, ty))) df);
        (match df with
        | D.Application (s, ds, dc) ->
          let a = ghost_ (List_case.selector_invert n g s e b ty ds ()) in
          ghost_ (D.typed_def n g (D.Lambda (D.Lambda nonempty)) b db);
          (match db with
          | D.Abstraction (_, dh) ->
            ghost_ (D.typed_def n (D.Binding (D.Forall (D.Z, D.List_type a), g))
              (D.Lambda nonempty) (D.Function (a, ty)) dh);
            (match dh with
            | D.Abstraction (element, dn) ->
              let out = D.List_case (element, dc, de, dn) in
              ghost_ (D.typed_def n g (D.CaseList (scrutinee, empty, nonempty)) ty out); out
            | _ -> unreachable_ ())
          | _ -> unreachable_ ())
        | _ -> unreachable_ ())
      | _ -> unreachable_ ())
    | _ -> unreachable_ ()

let (primitive @ total) : (n : D.index) @ immutable ghost -> (g : D.context) @ immutable ghost ->
    (op : D.word_operation) @ immutable ghost -> (left : D.term) @ immutable ghost -> (right : D.term) @ immutable ghost ->
    (d : D.typing) @ immutable ->
    {u : unit | D.typed n g (Primitive.arguments left right) (D.List_type D.Word64) d} ->
    {out : D.typing | D.typed n g (D.Primitive (op, left, right)) (D.operation_type op) out} @ immutable =
  fun n g op left right d premise ->
    ghost_ (Primitive.arguments_def left right;
      D.typed_def n g (Primitive.arguments left right) (D.List_type D.Word64) d);
    match d with
    | D.List_cons (_, dl, tail) ->
      ghost_ (D.typed_def n g (D.Cons (right, D.Cons (D.Word {Hmc_word64.lo = 0; hi = 0}, D.Nil)))
        (D.List_type D.Word64) tail);
      (match tail with
      | D.List_cons (_, dr, _) ->
        let out = D.Word_primitive (dl, dr) in
        ghost_ (D.operation_type_def op; D.mono_wf_def n (D.operation_type op);
          D.typed_def n g (D.Primitive (op, left, right)) (D.operation_type op) out); out
      | _ -> unreachable_ ())
    | _ -> unreachable_ ()
