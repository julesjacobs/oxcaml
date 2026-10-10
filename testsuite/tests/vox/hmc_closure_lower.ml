module D = Hm_declarative
module C = Hmc_monomorphic
module M = Hmc_manifest
module H = Hmc_monomorphic_typing
module K = Hmc_closure_ir
module E = Hmc_closure_extension
module G = Hmc_ground_type
module A = Hmc_ground_annotations

type result = {table : K.table; code : K.term}
let rec (lower @ total) : (globals : M.table) @ immutable -> (initial : K.table) @ immutable ->
    (locals : D.context) @ immutable -> (source : C.term) @ immutable ->
    (ty : D.mono) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | K.valid globals initial && H.typed globals locals source ty d
      && K.ground_context locals && G.ground ty && A.typing d} ->
    {r : result | K.valid globals r.table && K.extends r.table initial
      && K.typed globals r.table locals r.code ty d && K.related r.table source r.code} @ immutable =
  fun globals initial locals source ty d premise ->
    ghost_ (H.typed_def globals locals source ty d; A.typing_def d; G.ground_def ty);
    let out = match source, d with
    | C.Local i, D.Variable _ -> {table = initial; code = K.Local i}
    | C.Global (_, _, id), D.Variable _ -> {table = initial; code = K.Global id}
    | C.Truth, D.Constant -> {table = initial; code = K.Truth}
    | C.False, D.Constant -> {table = initial; code = K.False}
    | C.Word w, D.Word_constant -> {table = initial; code = K.Word w}
    | C.Nil, D.Empty_list _ -> {table = initial; code = K.Nil}
    | C.Lambda body, D.Abstraction (arg, db) -> (match ty with D.Function (_, ret) ->
      let next = D.Binding (D.Forall (D.Z, arg), locals) in
      ghost_ (K.ground_context_def next);
      let child = lower globals initial next body ret db () in
      let entry = {K.recursive = false; captured = locals; argument = arg; result = ret;
        source = body; derivation = db; body = child.code} in
      let table = K.Add (entry, child.table) in
      let id = K.size child.table in
      ghost_ (K.context_def entry; K.valid_def globals table; K.extends_def table initial;
        K.lookup_def table id; let _ = Hm_elaboration_check.index_equal id id in ());
      {table; code = K.Closure id}
      | _ -> unreachable_ ())
    | C.Recursive body, D.Recursion (arg, ret, db) ->
      let self = D.Binding (D.Forall (D.Z, D.Function (arg, ret)), locals) in
      let next = D.Binding (D.Forall (D.Z, arg), self) in
      ghost_ (K.ground_context_def self; K.ground_context_def next; G.ground_def (D.Function (arg, ret)));
      let child = lower globals initial next body ret db () in
      let entry = {K.recursive = true; captured = locals; argument = arg; result = ret;
        source = body; derivation = db; body = child.code} in
      let table = K.Add (entry, child.table) in
      let id = K.size child.table in
      ghost_ (K.context_def entry; K.valid_def globals table; K.extends_def table initial;
        K.lookup_def table id; let _ = Hm_elaboration_check.index_equal id id in ());
      {table; code = K.Closure id}
    | C.Apply (a, b), D.Application (arg, da, db) ->
      ghost_ (G.ground_def (D.Function (arg, ty)));
      let left = lower globals initial locals a (D.Function (arg, ty)) da () in
      let right = lower globals left.table locals b (arg) db () in
      ghost_ (E.transitive right.table left.table initial ();
        E.related right.table left.table a left.code ();
        E.typing globals right.table left.table locals left.code (D.Function (arg, ty)) da ());
      {table = right.table; code = K.Apply (left.code, right.code)}
    | C.Cons (a, b), D.List_cons (arg, da, db) ->
      ghost_ (G.ground_def D.Word64);
      let left = lower globals initial locals a (arg) da () in
      let right = lower globals left.table locals b (ty) db () in
      ghost_ (E.transitive right.table left.table initial ();
        E.related right.table left.table a left.code ();
        E.typing globals right.table left.table locals left.code (arg) da ());
      {table = right.table; code = K.Cons (left.code, right.code)}
    | C.Primitive (op, a, b), D.Word_primitive (da, db) ->
      ghost_ (G.ground_def D.Word64);
      let left = lower globals initial locals a (D.Word64) da () in
      let right = lower globals left.table locals b (D.Word64) db () in
      ghost_ (E.transitive right.table left.table initial ();
        E.related right.table left.table a left.code ();
        E.typing globals right.table left.table locals left.code (D.Word64) da ());
      {table = right.table; code = K.Primitive (op, left.code, right.code)}
    | C.Let (a, b), D.Let_binding (D.Forall (D.Z, arg), da, db) ->
      let next = D.Binding (D.Forall (D.Z, arg), locals) in
      ghost_ (K.ground_context_def next);
      let left = lower globals initial locals a arg da () in
      let right = lower globals left.table next b ty db () in
      ghost_ (E.transitive right.table left.table initial ();
        E.related right.table left.table a left.code ();
        E.typing globals right.table left.table locals left.code arg da ());
      {table = right.table; code = K.Let (left.code, right.code)}
    | C.If (a, b, c), D.Conditional (da, db, dc) ->
      ghost_ (G.ground_def D.Boolean);
      let first = lower globals initial locals a D.Boolean da () in
      let second = lower globals first.table locals b ty db () in
      let third = lower globals second.table locals c ty dc () in
      ghost_ (E.transitive third.table second.table first.table (); E.transitive third.table first.table initial ();
        E.related third.table first.table a first.code (); E.related third.table second.table b second.code ();
        E.typing globals third.table first.table locals first.code D.Boolean da ();
        E.typing globals third.table second.table locals second.code ty db ());
      {table = third.table; code = K.If (first.code, second.code, third.code)}
    | C.CaseList (s, a, b), D.List_case (element, ds, da, db) ->
      let tail = D.Binding (D.Forall (D.Z, D.List_type element), locals) in
      let next = D.Binding (D.Forall (D.Z, element), tail) in
      ghost_ (G.ground_def (D.List_type element); K.ground_context_def tail; K.ground_context_def next);
      let first = lower globals initial locals s (D.List_type element) ds () in
      let second = lower globals first.table locals a ty da () in
      let third = lower globals second.table next b ty db () in
      ghost_ (E.transitive third.table second.table first.table (); E.transitive third.table first.table initial ();
        E.related third.table first.table s first.code (); E.related third.table second.table a second.code ();
        E.typing globals third.table first.table locals first.code (D.List_type element) ds ();
        E.typing globals third.table second.table locals second.code ty da ());
      {table = third.table; code = K.CaseList (first.code, second.code, third.code)}
    | _ -> unreachable_ () in
    ghost_ (K.extends_def initial initial;
      K.typed_def globals out.table locals out.code ty d; K.related_def out.table source out.code);
    out
