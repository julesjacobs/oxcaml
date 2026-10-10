module D = Hm_declarative
module C = Hm_conditional_constraints

let[@def] (selector @ total) (u : unit) =
  D.Lambda (D.Lambda (D.Lambda (D.Apply (D.Lambda (D.Bound (D.S (D.S (D.Z)))), D.Lambda (D.Apply (D.Lambda (D.Cons (D.Bound (D.S (D.S (D.S (D.Z)))), D.Cons (D.Apply (D.Apply (D.Bound (D.S (D.S (D.Z))), D.Bound (D.S (D.S (D.S (D.S (D.Z)))))), D.Bound (D.S (D.Z))), D.Nil))), D.Cons (D.Bound (D.S (D.S (D.S (D.Z)))), D.Cons (D.Cons (D.Bound (D.Z), D.Nil), D.Nil))))))))

let[@def] (encoded @ total) (scrutinee : D.term @ immutable)
    (empty : D.term @ immutable) (nonempty : D.term @ immutable) =
  D.Apply (D.Apply (D.Apply (selector (), scrutinee), empty), D.Lambda (D.Lambda nonempty))

let (selector_construct @ total) : (n : D.index) @ immutable -> (g : D.context) @ immutable ->
    (a : D.mono) @ immutable -> (b : D.mono) @ immutable ->
    {u : unit | D.context_wf n g && D.mono_wf n a && D.mono_wf n b} ->
    {d : D.typing | D.typed n g (selector ())
      (D.Function (D.List_type a, D.Function (b, D.Function (
        D.Function (D.List_type a, D.Function (a, b)), b)))) d} @ immutable ghost =
  fun n g a b premise -> ghost_ (
    D.mono_wf_def n (b);
    D.mono_wf_def n (a);
    D.mono_wf_def n (D.List_type a);
    D.mono_wf_def n (D.List_type b);
    D.mono_wf_def n (D.List_type (b));
    D.mono_wf_def n (D.List_type (a));
    D.mono_wf_def n (D.Function (a, b));
    D.mono_wf_def n (D.List_type (D.List_type a));
    D.mono_wf_def n (D.Function (a, D.List_type b));
    D.mono_wf_def n (D.Function (D.Function (a, D.List_type b), b));
    D.mono_wf_def n (D.Function (D.List_type a, D.Function (a, b)));
    D.mono_wf_def n (D.Function (D.List_type (D.List_type a), D.List_type (b)));
    D.mono_wf_def n (D.Function (D.Function (D.List_type a, D.Function (a, b)), b));
    D.mono_wf_def n (D.Function (b, D.Function (D.Function (D.List_type a, D.Function (a, b)), b)));
    D.mono_wf_def n (D.Function (D.List_type a, D.Function (b, D.Function (D.Function (D.List_type a, D.Function (a, b)), b))));
    let g1 = D.Binding (D.Forall (D.Z, D.List_type a), g) in
    C.binding_wf n g (D.List_type a) ();
    let g2 = D.Binding (D.Forall (D.Z, b), g1) in
    C.binding_wf n g1 (b) ();
    let g3 = D.Binding (D.Forall (D.Z, D.Function (D.List_type a, D.Function (a, b))), g2) in
    C.binding_wf n g2 (D.Function (D.List_type a, D.Function (a, b))) ();
    let g5 = D.Binding (D.Forall (D.Z, D.Function (a, D.List_type b)), g3) in
    C.binding_wf n g3 (D.Function (a, D.List_type b)) ();
    let e6 = D.Bound (D.S (D.S (D.Z))) in
    D.lookup_def g5 (D.S (D.S (D.Z)));
    D.lookup_def g3 (D.S (D.Z));
    D.lookup_def g2 (D.Z);
    let d6 = C.variable_construct n g5 (D.S (D.S (D.Z))) (b) () in
    D.typed_def n g5 e6 (b) d6;
    let e5 = D.Lambda e6 in let d5 = D.Abstraction (D.Function (a, D.List_type b), d6) in
    D.typed_def n g3 e5 (D.Function (D.Function (a, D.List_type b), b)) d5;
    let g7 = D.Binding (D.Forall (D.Z, a), g3) in
    C.binding_wf n g3 (a) ();
    let g9 = D.Binding (D.Forall (D.Z, D.List_type (D.List_type a)), g7) in
    C.binding_wf n g7 (D.List_type (D.List_type a)) ();
    let e11 = D.Bound (D.S (D.S (D.S (D.Z)))) in
    D.lookup_def g9 (D.S (D.S (D.S (D.Z))));
    D.lookup_def g7 (D.S (D.S (D.Z)));
    D.lookup_def g3 (D.S (D.Z));
    D.lookup_def g2 (D.Z);
    let d11 = C.variable_construct n g9 (D.S (D.S (D.S (D.Z)))) (b) () in
    D.typed_def n g9 e11 (b) d11;
    let e15 = D.Bound (D.S (D.S (D.Z))) in
    D.lookup_def g9 (D.S (D.S (D.Z)));
    D.lookup_def g7 (D.S (D.Z));
    D.lookup_def g3 (D.Z);
    let d15 = C.variable_construct n g9 (D.S (D.S (D.Z))) (D.Function (D.List_type a, D.Function (a, b))) () in
    D.typed_def n g9 e15 (D.Function (D.List_type a, D.Function (a, b))) d15;
    let e16 = D.Bound (D.S (D.S (D.S (D.S (D.Z))))) in
    D.lookup_def g9 (D.S (D.S (D.S (D.S (D.Z)))));
    D.lookup_def g7 (D.S (D.S (D.S (D.Z))));
    D.lookup_def g3 (D.S (D.S (D.Z)));
    D.lookup_def g2 (D.S (D.Z));
    D.lookup_def g1 (D.Z);
    let d16 = C.variable_construct n g9 (D.S (D.S (D.S (D.S (D.Z))))) (D.List_type a) () in
    D.typed_def n g9 e16 (D.List_type a) d16;
    let e14 = D.Apply (e15, e16) in
    let d14 = D.Application (D.List_type a, d15, d16) in
    D.typed_def n g9 e14 (D.Function (a, b)) d14;
    let e17 = D.Bound (D.S (D.Z)) in
    D.lookup_def g9 (D.S (D.Z));
    D.lookup_def g7 (D.Z);
    let d17 = C.variable_construct n g9 (D.S (D.Z)) (a) () in
    D.typed_def n g9 e17 (a) d17;
    let e13 = D.Apply (e14, e17) in
    let d13 = D.Application (a, d14, d17) in
    D.typed_def n g9 e13 (b) d13;
    let e18 = D.Nil in let d18 = D.Empty_list (b) in
    D.typed_def n g9 e18 (D.List_type (b)) d18;
    let e12 = D.Cons (e13, e18) in
    let d12 = D.List_cons (b, d13, d18) in
    D.typed_def n g9 e12 (D.List_type (b)) d12;
    let e10 = D.Cons (e11, e12) in
    let d10 = D.List_cons (b, d11, d12) in
    D.typed_def n g9 e10 (D.List_type (b)) d10;
    let e9 = D.Lambda e10 in let d9 = D.Abstraction (D.List_type (D.List_type a), d10) in
    D.typed_def n g7 e9 (D.Function (D.List_type (D.List_type a), D.List_type (b))) d9;
    let e20 = D.Bound (D.S (D.S (D.S (D.Z)))) in
    D.lookup_def g7 (D.S (D.S (D.S (D.Z))));
    D.lookup_def g3 (D.S (D.S (D.Z)));
    D.lookup_def g2 (D.S (D.Z));
    D.lookup_def g1 (D.Z);
    let d20 = C.variable_construct n g7 (D.S (D.S (D.S (D.Z)))) (D.List_type a) () in
    D.typed_def n g7 e20 (D.List_type a) d20;
    let e23 = D.Bound (D.Z) in
    D.lookup_def g7 (D.Z);
    let d23 = C.variable_construct n g7 (D.Z) (a) () in
    D.typed_def n g7 e23 (a) d23;
    let e24 = D.Nil in let d24 = D.Empty_list (a) in
    D.typed_def n g7 e24 (D.List_type (a)) d24;
    let e22 = D.Cons (e23, e24) in
    let d22 = D.List_cons (a, d23, d24) in
    D.typed_def n g7 e22 (D.List_type (a)) d22;
    let e25 = D.Nil in let d25 = D.Empty_list (D.List_type a) in
    D.typed_def n g7 e25 (D.List_type (D.List_type a)) d25;
    let e21 = D.Cons (e22, e25) in
    let d21 = D.List_cons (D.List_type a, d22, d25) in
    D.typed_def n g7 e21 (D.List_type (D.List_type a)) d21;
    let e19 = D.Cons (e20, e21) in
    let d19 = D.List_cons (D.List_type a, d20, d21) in
    D.typed_def n g7 e19 (D.List_type (D.List_type a)) d19;
    let e8 = D.Apply (e9, e19) in
    let d8 = D.Application (D.List_type (D.List_type a), d9, d19) in
    D.typed_def n g7 e8 (D.List_type b) d8;
    let e7 = D.Lambda e8 in let d7 = D.Abstraction (a, d8) in
    D.typed_def n g3 e7 (D.Function (a, D.List_type b)) d7;
    let e4 = D.Apply (e5, e7) in
    let d4 = D.Application (D.Function (a, D.List_type b), d5, d7) in
    D.typed_def n g3 e4 (b) d4;
    let e3 = D.Lambda e4 in let d3 = D.Abstraction (D.Function (D.List_type a, D.Function (a, b)), d4) in
    D.typed_def n g2 e3 (D.Function (D.Function (D.List_type a, D.Function (a, b)), b)) d3;
    let e2 = D.Lambda e3 in let d2 = D.Abstraction (b, d3) in
    D.typed_def n g1 e2 (D.Function (b, D.Function (D.Function (D.List_type a, D.Function (a, b)), b))) d2;
    let e1 = D.Lambda e2 in let d1 = D.Abstraction (D.List_type a, d2) in
    D.typed_def n g e1 (D.Function (D.List_type a, D.Function (b, D.Function (D.Function (D.List_type a, D.Function (a, b)), b)))) d1;
    selector_def (); d1)

let (selector_invert @ total) : (n : D.index) @ immutable -> (g : D.context) @ immutable ->
    (scrutinee : D.mono) @ immutable -> (empty : D.mono) @ immutable ->
    (branch : D.mono) @ immutable -> (out : D.mono) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | D.typed n g (selector ())
      (D.Function (scrutinee, D.Function (empty, D.Function (branch, out)))) d} ->
    {a : D.mono | scrutinee === D.List_type a && empty === out
      && branch === D.Function (D.List_type a, D.Function (a, out)) && D.mono_wf n a}
      @ immutable ghost = fun n g scrutinee empty branch out d premise -> ghost_ (
    selector_def ();
    let e1 = D.Lambda (D.Lambda (D.Lambda (D.Apply (D.Lambda (D.Bound (D.S (D.S (D.Z)))), D.Lambda (D.Apply (D.Lambda (D.Cons (D.Bound (D.S (D.S (D.S (D.Z)))), D.Cons (D.Apply (D.Apply (D.Bound (D.S (D.S (D.Z))), D.Bound (D.S (D.S (D.S (D.S (D.Z)))))), D.Bound (D.S (D.Z))), D.Nil))), D.Cons (D.Bound (D.S (D.S (D.S (D.Z)))), D.Cons (D.Cons (D.Bound (D.Z), D.Nil), D.Nil)))))))) in
    D.typed_def n g e1 (D.Function (scrutinee, D.Function (empty, D.Function (branch, out)))) d;
    (match D.Function (scrutinee, D.Function (empty, D.Function (branch, out))) with D.Function (a1, b1) ->
    (match d with D.Abstraction (_, db1) ->
    let g1 = D.Binding (D.Forall (D.Z, a1), g) in
    let e2 = D.Lambda (D.Lambda (D.Apply (D.Lambda (D.Bound (D.S (D.S (D.Z)))), D.Lambda (D.Apply (D.Lambda (D.Cons (D.Bound (D.S (D.S (D.S (D.Z)))), D.Cons (D.Apply (D.Apply (D.Bound (D.S (D.S (D.Z))), D.Bound (D.S (D.S (D.S (D.S (D.Z)))))), D.Bound (D.S (D.Z))), D.Nil))), D.Cons (D.Bound (D.S (D.S (D.S (D.Z)))), D.Cons (D.Cons (D.Bound (D.Z), D.Nil), D.Nil))))))) in
    D.typed_def n g1 e2 (b1) db1;
    (match b1 with D.Function (a2, b2) ->
    (match db1 with D.Abstraction (_, db2) ->
    let g2 = D.Binding (D.Forall (D.Z, a2), g1) in
    let e3 = D.Lambda (D.Apply (D.Lambda (D.Bound (D.S (D.S (D.Z)))), D.Lambda (D.Apply (D.Lambda (D.Cons (D.Bound (D.S (D.S (D.S (D.Z)))), D.Cons (D.Apply (D.Apply (D.Bound (D.S (D.S (D.Z))), D.Bound (D.S (D.S (D.S (D.S (D.Z)))))), D.Bound (D.S (D.Z))), D.Nil))), D.Cons (D.Bound (D.S (D.S (D.S (D.Z)))), D.Cons (D.Cons (D.Bound (D.Z), D.Nil), D.Nil)))))) in
    D.typed_def n g2 e3 (b2) db2;
    (match b2 with D.Function (a3, b3) ->
    (match db2 with D.Abstraction (_, db3) ->
    let g3 = D.Binding (D.Forall (D.Z, a3), g2) in
    let e4 = D.Apply (D.Lambda (D.Bound (D.S (D.S (D.Z)))), D.Lambda (D.Apply (D.Lambda (D.Cons (D.Bound (D.S (D.S (D.S (D.Z)))), D.Cons (D.Apply (D.Apply (D.Bound (D.S (D.S (D.Z))), D.Bound (D.S (D.S (D.S (D.S (D.Z)))))), D.Bound (D.S (D.Z))), D.Nil))), D.Cons (D.Bound (D.S (D.S (D.S (D.Z)))), D.Cons (D.Cons (D.Bound (D.Z), D.Nil), D.Nil))))) in
    D.typed_def n g3 e4 (b3) db3;
    (match db3 with D.Application (a4, dl4, dr4) ->
    let e5 = D.Lambda (D.Bound (D.S (D.S (D.Z)))) in
    D.typed_def n g3 e5 (D.Function (a4, b3)) dl4;
    (match D.Function (a4, b3) with D.Function (a5, b5) ->
    (match dl4 with D.Abstraction (_, db5) ->
    let g5 = D.Binding (D.Forall (D.Z, a5), g3) in
    let e6 = D.Bound (D.S (D.S (D.Z))) in
    D.typed_def n g5 e6 (b5) db5;
    D.lookup_def g5 (D.S (D.S (D.Z)));
    D.lookup_def g3 (D.S (D.Z));
    D.lookup_def g2 (D.Z);
    C.variable_invert n g5 (D.S (D.S (D.Z))) (a2) (b5) db5 ();
    let e7 = D.Lambda (D.Apply (D.Lambda (D.Cons (D.Bound (D.S (D.S (D.S (D.Z)))), D.Cons (D.Apply (D.Apply (D.Bound (D.S (D.S (D.Z))), D.Bound (D.S (D.S (D.S (D.S (D.Z)))))), D.Bound (D.S (D.Z))), D.Nil))), D.Cons (D.Bound (D.S (D.S (D.S (D.Z)))), D.Cons (D.Cons (D.Bound (D.Z), D.Nil), D.Nil)))) in
    D.typed_def n g3 e7 (a4) dr4;
    (match a4 with D.Function (a7, b7) ->
    (match dr4 with D.Abstraction (_, db7) ->
    let g7 = D.Binding (D.Forall (D.Z, a7), g3) in
    let e8 = D.Apply (D.Lambda (D.Cons (D.Bound (D.S (D.S (D.S (D.Z)))), D.Cons (D.Apply (D.Apply (D.Bound (D.S (D.S (D.Z))), D.Bound (D.S (D.S (D.S (D.S (D.Z)))))), D.Bound (D.S (D.Z))), D.Nil))), D.Cons (D.Bound (D.S (D.S (D.S (D.Z)))), D.Cons (D.Cons (D.Bound (D.Z), D.Nil), D.Nil))) in
    D.typed_def n g7 e8 (b7) db7;
    (match db7 with D.Application (a8, dl8, dr8) ->
    let e9 = D.Lambda (D.Cons (D.Bound (D.S (D.S (D.S (D.Z)))), D.Cons (D.Apply (D.Apply (D.Bound (D.S (D.S (D.Z))), D.Bound (D.S (D.S (D.S (D.S (D.Z)))))), D.Bound (D.S (D.Z))), D.Nil))) in
    D.typed_def n g7 e9 (D.Function (a8, b7)) dl8;
    (match D.Function (a8, b7) with D.Function (a9, b9) ->
    (match dl8 with D.Abstraction (_, db9) ->
    let g9 = D.Binding (D.Forall (D.Z, a9), g7) in
    let e10 = D.Cons (D.Bound (D.S (D.S (D.S (D.Z)))), D.Cons (D.Apply (D.Apply (D.Bound (D.S (D.S (D.Z))), D.Bound (D.S (D.S (D.S (D.S (D.Z)))))), D.Bound (D.S (D.Z))), D.Nil)) in
    D.typed_def n g9 e10 (b9) db9;
    (match db9 with D.List_cons (a10, dl10, dr10) ->
    let e11 = D.Bound (D.S (D.S (D.S (D.Z)))) in
    D.typed_def n g9 e11 (a10) dl10;
    D.lookup_def g9 (D.S (D.S (D.S (D.Z))));
    D.lookup_def g7 (D.S (D.S (D.Z)));
    D.lookup_def g3 (D.S (D.Z));
    D.lookup_def g2 (D.Z);
    C.variable_invert n g9 (D.S (D.S (D.S (D.Z)))) (a2) (a10) dl10 ();
    let e12 = D.Cons (D.Apply (D.Apply (D.Bound (D.S (D.S (D.Z))), D.Bound (D.S (D.S (D.S (D.S (D.Z)))))), D.Bound (D.S (D.Z))), D.Nil) in
    D.typed_def n g9 e12 (b9) dr10;
    (match dr10 with D.List_cons (a12, dl12, dr12) ->
    let e13 = D.Apply (D.Apply (D.Bound (D.S (D.S (D.Z))), D.Bound (D.S (D.S (D.S (D.S (D.Z)))))), D.Bound (D.S (D.Z))) in
    D.typed_def n g9 e13 (a12) dl12;
    (match dl12 with D.Application (a13, dl13, dr13) ->
    let e14 = D.Apply (D.Bound (D.S (D.S (D.Z))), D.Bound (D.S (D.S (D.S (D.S (D.Z)))))) in
    D.typed_def n g9 e14 (D.Function (a13, a12)) dl13;
    (match dl13 with D.Application (a14, dl14, dr14) ->
    let e15 = D.Bound (D.S (D.S (D.Z))) in
    D.typed_def n g9 e15 (D.Function (a14, D.Function (a13, a12))) dl14;
    D.lookup_def g9 (D.S (D.S (D.Z)));
    D.lookup_def g7 (D.S (D.Z));
    D.lookup_def g3 (D.Z);
    C.variable_invert n g9 (D.S (D.S (D.Z))) (a3) (D.Function (a14, D.Function (a13, a12))) dl14 ();
    let e16 = D.Bound (D.S (D.S (D.S (D.S (D.Z))))) in
    D.typed_def n g9 e16 (a14) dr14;
    D.lookup_def g9 (D.S (D.S (D.S (D.S (D.Z)))));
    D.lookup_def g7 (D.S (D.S (D.S (D.Z))));
    D.lookup_def g3 (D.S (D.S (D.Z)));
    D.lookup_def g2 (D.S (D.Z));
    D.lookup_def g1 (D.Z);
    C.variable_invert n g9 (D.S (D.S (D.S (D.S (D.Z))))) (a1) (a14) dr14 ();
    let e17 = D.Bound (D.S (D.Z)) in
    D.typed_def n g9 e17 (a13) dr13;
    D.lookup_def g9 (D.S (D.Z));
    D.lookup_def g7 (D.Z);
    C.variable_invert n g9 (D.S (D.Z)) (a7) (a13) dr13 ();
    let e18 = D.Nil in
    D.typed_def n g9 e18 (b9) dr12;
    let e19 = D.Cons (D.Bound (D.S (D.S (D.S (D.Z)))), D.Cons (D.Cons (D.Bound (D.Z), D.Nil), D.Nil)) in
    D.typed_def n g7 e19 (a8) dr8;
    (match dr8 with D.List_cons (a19, dl19, dr19) ->
    let e20 = D.Bound (D.S (D.S (D.S (D.Z)))) in
    D.typed_def n g7 e20 (a19) dl19;
    D.lookup_def g7 (D.S (D.S (D.S (D.Z))));
    D.lookup_def g3 (D.S (D.S (D.Z)));
    D.lookup_def g2 (D.S (D.Z));
    D.lookup_def g1 (D.Z);
    C.variable_invert n g7 (D.S (D.S (D.S (D.Z)))) (a1) (a19) dl19 ();
    let e21 = D.Cons (D.Cons (D.Bound (D.Z), D.Nil), D.Nil) in
    D.typed_def n g7 e21 (a8) dr19;
    (match dr19 with D.List_cons (a21, dl21, dr21) ->
    let e22 = D.Cons (D.Bound (D.Z), D.Nil) in
    D.typed_def n g7 e22 (a21) dl21;
    (match dl21 with D.List_cons (a22, dl22, dr22) ->
    let e23 = D.Bound (D.Z) in
    D.typed_def n g7 e23 (a22) dl22;
    D.lookup_def g7 (D.Z);
    C.variable_invert n g7 (D.Z) (a7) (a22) dl22 ();
    let e24 = D.Nil in
    D.typed_def n g7 e24 (a21) dr22;
    let e25 = D.Nil in
    D.typed_def n g7 e25 (a8) dr21;
    a7
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
    | _ -> unreachable_ ())
)

let (invert @ total) : (n : D.index) @ immutable -> (g : D.context) @ immutable ->
    (scrutinee : D.term) @ immutable -> (empty : D.term) @ immutable -> (nonempty : D.term) @ immutable ->
    (ty : D.mono) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | D.typed n g (encoded scrutinee empty nonempty) ty d} ->
    {d : D.typing | D.typed n g (D.CaseList (scrutinee, empty, nonempty)) ty d} @ immutable ghost =
  fun n g scrutinee empty nonempty ty d premise -> ghost_ (
    encoded_def scrutinee empty nonempty;
    let f = D.Apply (selector (), scrutinee) in let fe = D.Apply (f, empty) in
    let branch = D.Lambda (D.Lambda nonempty) in
    D.typed_def n g (encoded scrutinee empty nonempty) ty d;
    match d with D.Application (b, dfe, db) ->
      D.typed_def n g fe (D.Function (b, ty)) dfe;
      (match dfe with D.Application (e, df, de) ->
        D.typed_def n g f (D.Function (e, D.Function (b, ty))) df;
        (match df with D.Application (s, ds, dc) ->
          let a = selector_invert n g s e b ty ds () in
          D.typed_def n g branch b db;
          (match db with D.Abstraction (_, dh) ->
            let gt = D.Binding (D.Forall (D.Z, D.List_type a), g) in
            D.typed_def n gt (D.Lambda nonempty) (D.Function (a, ty)) dh;
            (match dh with D.Abstraction (_, dn) ->
              let out = D.List_case (a, dc, de, dn) in
              D.typed_def n g (D.CaseList (scrutinee, empty, nonempty)) ty out; out
            | _ -> unreachable_ ())
          | _ -> unreachable_ ())
        | _ -> unreachable_ ())
      | _ -> unreachable_ ())
    | _ -> unreachable_ ())

let (construct @ total) : (n : D.index) @ immutable -> (g : D.context) @ immutable ->
    (scrutinee : D.term) @ immutable -> (empty : D.term) @ immutable -> (nonempty : D.term) @ immutable ->
    (ty : D.mono) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | D.typed n g (D.CaseList (scrutinee, empty, nonempty)) ty d} ->
    {d : D.typing | D.typed n g (encoded scrutinee empty nonempty) ty d} @ immutable ghost =
  fun n g scrutinee empty nonempty ty d premise -> ghost_ (
    D.typed_def n g (D.CaseList (scrutinee, empty, nonempty)) ty d;
    match d with D.List_case (a, dc, de, dn) ->
      let list = D.List_type a in D.mono_wf_def n list;
      let fn = D.Function (a, ty) in D.mono_wf_def n fn;
      let bt = D.Function (list, fn) in D.mono_wf_def n bt;
      let gt = D.Binding (D.Forall (D.Z, list), g) in C.binding_wf n g list ();
      let dh = D.Abstraction (a, dn) in D.typed_def n gt (D.Lambda nonempty) fn dh;
      let branch = D.Lambda (D.Lambda nonempty) in let db = D.Abstraction (list, dh) in
      D.typed_def n g branch bt db;
      let ds = selector_construct n g a ty () in
      let ft = D.Function (ty, D.Function (bt, ty)) in
      D.mono_wf_def n (D.Function (bt, ty)); D.mono_wf_def n ft;
      let f = D.Apply (selector (), scrutinee) in let df = D.Application (list, ds, dc) in
      D.typed_def n g f ft df;
      let fe = D.Apply (f, empty) in let dfe = D.Application (ty, df, de) in
      D.typed_def n g fe (D.Function (bt, ty)) dfe;
      let out = D.Application (bt, dfe, db) in encoded_def scrutinee empty nonempty;
      D.typed_def n g (encoded scrutinee empty nonempty) ty out; out
    | _ -> unreachable_ ())
