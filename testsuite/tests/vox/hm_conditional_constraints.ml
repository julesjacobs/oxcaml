module D = Hm_declarative

let[@def] (condition @ total) (term : D.term @ immutable) =
  D.Cons (term, D.Cons (D.Truth, D.Nil))
let[@def] (branches @ total) (yes : D.term @ immutable) (no : D.term @ immutable) =
  D.Cons (yes, D.Cons (no, D.Nil))

type branch_typings = {yes_typing : D.typing; no_typing : D.typing}

let (invert_condition @ total) : (n : D.index) @ immutable ->
    (g : D.context) @ immutable -> (term : D.term) @ immutable ->
    (ty : D.mono) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | D.typed n g (condition term) ty d} ->
    {p : D.typing | ty === D.List_type D.Boolean && D.typed n g term D.Boolean p}
      @ immutable ghost = fun n g term ty d premise -> ghost_ (
    condition_def term; D.typed_def n g (condition term) ty d;
    match d with
    | D.List_cons (a, dc, rest) ->
      D.typed_def n g (D.Cons (D.Truth, D.Nil)) ty rest;
      (match rest with D.List_cons (_, dt, _) ->
        D.typed_def n g D.Truth a dt; dc
      | _ -> unreachable_ ())
    | _ -> unreachable_ ())

let (construct_condition @ total) : (n : D.index) @ immutable ->
    (g : D.context) @ immutable -> (term : D.term) @ immutable ->
    (dc : D.typing) @ immutable -> {u : unit | D.typed n g term D.Boolean dc} ->
    {d : D.typing | D.typed n g (condition term) (D.List_type D.Boolean) d}
      @ immutable ghost = fun n g term dc premise -> ghost_ (
    D.typed_def n g term D.Boolean dc;
    let ty = D.List_type D.Boolean in
    D.mono_wf_def n ty;
    let dn = D.Empty_list D.Boolean in D.typed_def n g D.Nil ty dn;
    D.typed_def n g D.Truth D.Boolean D.Constant;
    let dr = D.List_cons (D.Boolean, D.Constant, dn) in
    D.typed_def n g (D.Cons (D.Truth, D.Nil)) ty dr;
    let d = D.List_cons (D.Boolean, dc, dr) in
    condition_def term; D.typed_def n g (condition term) ty d; d)

let (invert_branches @ total) : (n : D.index) @ immutable ->
    (g : D.context) @ immutable -> (yes : D.term) @ immutable -> (no : D.term) @ immutable ->
    (ty : D.mono) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | D.typed n g (branches yes no) (D.List_type ty) d} ->
    {p : branch_typings | D.typed n g yes ty p.yes_typing && D.typed n g no ty p.no_typing}
      @ immutable ghost = fun n g yes no ty d premise -> ghost_ (
    branches_def yes no; D.typed_def n g (branches yes no) (D.List_type ty) d;
    match d with
    | D.List_cons (_, dy, rest) ->
      D.typed_def n g (D.Cons (no, D.Nil)) (D.List_type ty) rest;
      (match rest with D.List_cons (_, dn, _) -> {yes_typing = dy; no_typing = dn}
      | _ -> unreachable_ ())
    | _ -> unreachable_ ())

let (construct_branches @ total) : (n : D.index) @ immutable ->
    (g : D.context) @ immutable -> (yes : D.term) @ immutable -> (no : D.term) @ immutable ->
    (ty : D.mono) @ immutable -> (dy : D.typing) @ immutable -> (dn : D.typing) @ immutable ->
    {u : unit | D.typed n g yes ty dy && D.typed n g no ty dn} ->
    {d : D.typing | D.typed n g (branches yes no) (D.List_type ty) d}
      @ immutable ghost = fun n g yes no ty dy dn premise -> ghost_ (
    D.typed_def n g yes ty dy;
    let list_ty = D.List_type ty in D.mono_wf_def n list_ty;
    let nil_d = D.Empty_list ty in D.typed_def n g D.Nil list_ty nil_d;
    let rest_d = D.List_cons (ty, dn, nil_d) in
    D.typed_def n g (D.Cons (no, D.Nil)) list_ty rest_d;
    let d = D.List_cons (ty, dy, rest_d) in
    branches_def yes no; D.typed_def n g (branches yes no) list_ty d; d)

let[@def] (selector @ total) (u : unit) =
  D.Lambda (D.Lambda (D.Lambda (
    D.Apply (D.Lambda (D.Bound (D.S (D.S D.Z))),
      D.Cons (D.Bound (D.S D.Z), D.Cons (D.Bound D.Z, D.Nil))))))

let[@def] (encoded @ total) (c : D.term @ immutable)
    (yes : D.term @ immutable) (no : D.term @ immutable) =
  D.Apply (D.Apply (D.Apply (selector (), condition c), yes), no)

let (binding_wf @ total) : (n : D.index) @ immutable -> (g : D.context) @ immutable ->
    (a : D.mono) @ immutable -> {u : unit | D.context_wf n g && D.mono_wf n a} ->
    {u : unit | D.context_wf n (D.Binding (D.Forall (D.Z, a), g))} @ ghost =
  fun n g a premise -> ghost_ (
    let s = D.Forall (D.Z, a) in D.add_def D.Z n;
    D.scheme_wf_def n s; D.context_wf_def n (D.Binding (s, g)); ())

let (variable_invert @ total) : (n : D.index) @ immutable -> (g : D.context) @ immutable ->
    (i : D.index) @ immutable -> (a : D.mono) @ immutable -> (b : D.mono) @ immutable ->
    (d : D.typing) @ immutable ->
    {u : unit | D.lookup g i === Some (D.Forall (D.Z, a)) && D.typed n g (D.Bound i) b d} ->
    {u : unit | b === a} @ ghost = fun n g i a b d premise -> ghost_ (
    D.typed_def n g (D.Bound i) b d;
    match d with D.Variable args ->
      let s = D.Forall (D.Z, a) in D.arity_def s; D.length_def args;
      (match args with D.No_arguments ->
        D.open_scheme_def s args; Hm_type_proofs.open_empty a; ()
      | _ -> ())
    | _ -> ())

let (variable_construct @ total) : (n : D.index) @ immutable -> (g : D.context) @ immutable ->
    (i : D.index) @ immutable -> (a : D.mono) @ immutable ->
    {u : unit | D.context_wf n g && D.mono_wf n a && D.lookup g i === Some (D.Forall (D.Z, a))} ->
    {d : D.typing | D.typed n g (D.Bound i) a d} @ immutable ghost =
  fun n g i a premise -> ghost_ (
    let args = D.No_arguments in let s = D.Forall (D.Z, a) in
    D.arguments_wf_def n args; D.length_def args; D.arity_def s;
    D.open_scheme_def s args; Hm_type_proofs.open_empty a;
    let d = D.Variable args in D.typed_def n g (D.Bound i) a d; d)

let (selector_construct @ total) : (n : D.index) @ immutable -> (g : D.context) @ immutable ->
    (first : D.mono) @ immutable -> (a : D.mono) @ immutable ->
    {u : unit | D.context_wf n g && D.mono_wf n first && D.mono_wf n a} ->
    {d : D.typing | D.typed n g (selector ())
      (D.Function (first, D.Function (a, D.Function (a, a)))) d} @ immutable ghost =
  fun n g first a premise -> ghost_ (
    D.mono_wf_def n (D.List_type a);
    let g1 = D.Binding (D.Forall (D.Z, first), g) in
    binding_wf n g (first) ();
    let g2 = D.Binding (D.Forall (D.Z, a), g1) in
    binding_wf n g1 (a) ();
    let g3 = D.Binding (D.Forall (D.Z, a), g2) in
    binding_wf n g2 (a) ();
    let g5 = D.Binding (D.Forall (D.Z, D.List_type a), g3) in
    binding_wf n g3 (D.List_type a) ();
    let e6 = D.Bound (D.S (D.S (D.Z))) in
    D.lookup_def g5 (D.S (D.S (D.Z)));
    D.lookup_def g3 (D.S (D.Z));
    D.lookup_def g2 (D.Z);
    let d6 = variable_construct n g5 (D.S (D.S (D.Z))) (a) () in
    let e5 = D.Lambda e6 in let d5 = D.Abstraction (D.List_type a, d6) in
    D.mono_wf_def n (D.Function (D.List_type a, a)); D.typed_def n g3 e5 (D.Function (D.List_type a, a)) d5;
    let e8 = D.Bound (D.S (D.Z)) in
    D.lookup_def g3 (D.S (D.Z));
    D.lookup_def g2 (D.Z);
    let d8 = variable_construct n g3 (D.S (D.Z)) (a) () in
    let e10 = D.Bound (D.Z) in
    D.lookup_def g3 (D.Z);
    let d10 = variable_construct n g3 (D.Z) (a) () in
    let e11 = D.Nil in let d11 = D.Empty_list a in
    D.typed_def n g3 e11 (D.List_type a) d11;
    let e9 = D.Cons (e10, e11) in let d9 = D.List_cons (a, d10, d11) in
    D.typed_def n g3 e9 (D.List_type a) d9;
    let e7 = D.Cons (e8, e9) in let d7 = D.List_cons (a, d8, d9) in
    D.typed_def n g3 e7 (D.List_type a) d7;
    let e4 = D.Apply (e5, e7) in let d4 = D.Application (D.List_type a, d5, d7) in
    D.typed_def n g3 e4 (a) d4;
    let e3 = D.Lambda e4 in let d3 = D.Abstraction (a, d4) in
    D.mono_wf_def n (D.Function (a, a)); D.typed_def n g2 e3 (D.Function (a, a)) d3;
    let e2 = D.Lambda e3 in let d2 = D.Abstraction (a, d3) in
    D.mono_wf_def n (D.Function (a, D.Function (a, a))); D.typed_def n g1 e2 (D.Function (a, D.Function (a, a))) d2;
    let e1 = D.Lambda e2 in let d1 = D.Abstraction (first, d2) in
    D.mono_wf_def n (D.Function (first, D.Function (a, D.Function (a, a)))); D.typed_def n g e1 (D.Function (first, D.Function (a, D.Function (a, a)))) d1;
    selector_def (); d1)

let (selector_invert @ total) : (n : D.index) @ immutable -> (g : D.context) @ immutable ->
    (first : D.mono) @ immutable -> (a : D.mono) @ immutable -> (b : D.mono) @ immutable ->
    (out : D.mono) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | D.typed n g (selector ()) (D.Function (first, D.Function (a, D.Function (b, out)))) d} ->
    {u : unit | a === b && out === a} @ ghost = fun n g first a b out d premise -> ghost_ (
    let z = D.Z in let one = D.S z in let two = D.S one in
    let ret = D.Bound two in let no = D.Bound z in let yes = D.Bound one in
    let tail = D.Cons (no, D.Nil) in let pairs = D.Cons (yes, tail) in
    let ignore = D.Lambda ret in let body = D.Apply (ignore, pairs) in
    let lno = D.Lambda body in let lyes = D.Lambda lno in
    selector_def ();
    D.typed_def n g (selector ()) (D.Function (first, D.Function (a, D.Function (b, out)))) d;
    match d with D.Abstraction (_, d1) ->
      let g1 = D.Binding (D.Forall (z, first), g) in
      D.typed_def n g1 lyes (D.Function (a, D.Function (b, out))) d1;
      (match d1 with D.Abstraction (_, d2) ->
        let g2 = D.Binding (D.Forall (z, a), g1) in
        D.typed_def n g2 lno (D.Function (b, out)) d2;
        (match d2 with D.Abstraction (_, d3) ->
          let g3 = D.Binding (D.Forall (z, b), g2) in
          D.typed_def n g3 body out d3;
          (match d3 with D.Application (list, df, dl) ->
            D.typed_def n g3 ignore (D.Function (list, out)) df;
            D.typed_def n g3 pairs list dl;
            (match df with D.Abstraction (_, dr) ->
              let g4 = D.Binding (D.Forall (z, list), g3) in
              D.lookup_def g4 two; D.lookup_def g3 one; D.lookup_def g2 z;
              variable_invert n g4 two a out dr ();
              (match dl with D.List_cons (elem, dy, dt) ->
                D.lookup_def g3 one; D.lookup_def g2 z;
                variable_invert n g3 one a elem dy ();
                D.typed_def n g3 tail list dt;
                (match dt with D.List_cons (_, dn, _) ->
                  D.lookup_def g3 z; variable_invert n g3 z b elem dn ()
                | _ -> ())
              | _ -> ())
            | _ -> ())
          | _ -> ())
        | _ -> ())
      | _ -> ())
    | _ -> ())

let (invert @ total) : (n : D.index) @ immutable -> (g : D.context) @ immutable ->
    (c : D.term) @ immutable -> (yes : D.term) @ immutable -> (no : D.term) @ immutable ->
    (ty : D.mono) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | D.typed n g (encoded c yes no) ty d} ->
    {d : D.typing | D.typed n g (D.If (c, yes, no)) ty d} @ immutable ghost =
  fun n g c yes no ty d premise -> ghost_ (
    encoded_def c yes no;
    let f = D.Apply (selector (), condition c) in let fy = D.Apply (f, yes) in
    D.typed_def n g (encoded c yes no) ty d;
    match d with D.Application (b, dfy, dn) ->
      D.typed_def n g fy (D.Function (b, ty)) dfy;
      (match dfy with D.Application (a, df, dy) ->
        D.typed_def n g f (D.Function (a, D.Function (b, ty))) df;
        (match df with D.Application (first, ds, dc) ->
          selector_invert n g first a b ty ds ();
          let dc = invert_condition n g c first dc () in
          let out = D.Conditional (dc, dy, dn) in
          D.typed_def n g (D.If (c, yes, no)) ty out; out
        | _ -> unreachable_ ())
      | _ -> unreachable_ ())
    | _ -> unreachable_ ())

let (construct @ total) : (n : D.index) @ immutable -> (g : D.context) @ immutable ->
    (c : D.term) @ immutable -> (yes : D.term) @ immutable -> (no : D.term) @ immutable ->
    (ty : D.mono) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | D.typed n g (D.If (c, yes, no)) ty d} ->
    {d : D.typing | D.typed n g (encoded c yes no) ty d} @ immutable ghost =
  fun n g c yes no ty d premise -> ghost_ (
    D.typed_def n g (D.If (c, yes, no)) ty d;
    match d with D.Conditional (dc, dy, dn) ->
      let dc = construct_condition n g c dc () in
      let first = D.List_type D.Boolean in D.mono_wf_def n D.Boolean; D.mono_wf_def n first;
      let ds = selector_construct n g first ty () in
      let f = D.Apply (selector (), condition c) in let ft = D.Function (ty, D.Function (ty, ty)) in
      let df = D.Application (first, ds, dc) in
      D.mono_wf_def n (D.Function (ty, ty)); D.mono_wf_def n ft;
      D.typed_def n g f ft df;
      let fy = D.Apply (f, yes) in let dfy = D.Application (ty, df, dy) in
      D.typed_def n g fy (D.Function (ty, ty)) dfy;
      let out = D.Application (ty, dfy, dn) in encoded_def c yes no;
      D.typed_def n g (encoded c yes no) ty out; out
    | _ -> unreachable_ ())
