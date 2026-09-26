module D = Hm_declarative

let[@def] (arguments @ total) (left : D.term @ immutable) (right : D.term @ immutable) =
  D.Cons (left, D.Cons (right, D.Cons (D.Word {Hmc_word64.lo = 0; hi = 0}, D.Nil)))

type operands = {left_typing : D.typing; right_typing : D.typing}

let (invert @ total) : (n : D.index) @ immutable -> (g : D.context) @ immutable ->
    (left : D.term) @ immutable -> (right : D.term) @ immutable ->
    (ty : D.mono) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | D.typed n g (arguments left right) ty d} ->
    {p : operands | ty === D.List_type D.Word64
      && D.typed n g left D.Word64 p.left_typing
      && D.typed n g right D.Word64 p.right_typing} @ immutable ghost =
  fun n g left right ty d premise -> ghost_ (
    arguments_def left right;
    let word = D.Word {Hmc_word64.lo = 0; hi = 0} in
    let tail = D.Cons (word, D.Nil) in let rest = D.Cons (right, tail) in
    D.typed_def n g (arguments left right) ty d;
    match d with
    | D.List_cons (a, dl, dr) ->
      D.typed_def n g rest ty dr;
      (match dr with
      | D.List_cons (_, dr, dt) ->
        D.typed_def n g tail ty dt;
        (match dt with
        | D.List_cons (_, dw, _) ->
          D.typed_def n g word a dw;
          {left_typing = dl; right_typing = dr}
        | _ -> unreachable_ ())
      | _ -> unreachable_ ())
    | _ -> unreachable_ ())

let (construct @ total) : (n : D.index) @ immutable -> (g : D.context) @ immutable ->
    (left : D.term) @ immutable -> (right : D.term) @ immutable ->
    (dl : D.typing) @ immutable -> (dr : D.typing) @ immutable ->
    {u : unit | D.typed n g left D.Word64 dl && D.typed n g right D.Word64 dr} ->
    {d : D.typing | D.typed n g (arguments left right) (D.List_type D.Word64) d}
      @ immutable ghost = fun n g left right dl dr premise -> ghost_ (
    D.typed_def n g left D.Word64 dl;
    let word = D.Word {Hmc_word64.lo = 0; hi = 0} in
    let tail = D.Cons (word, D.Nil) in let rest = D.Cons (right, tail) in
    let ty = D.List_type D.Word64 in
    D.mono_wf_def n D.Word64; D.mono_wf_def n ty;
    let nil = D.Empty_list D.Word64 in D.typed_def n g D.Nil ty nil;
    D.typed_def n g word D.Word64 D.Word_constant;
    let dt = D.List_cons (D.Word64, D.Word_constant, nil) in D.typed_def n g tail ty dt;
    let rest_d = D.List_cons (D.Word64, dr, dt) in D.typed_def n g rest ty rest_d;
    let d = D.List_cons (D.Word64, dl, rest_d) in
    arguments_def left right; D.typed_def n g (arguments left right) ty d; d)
