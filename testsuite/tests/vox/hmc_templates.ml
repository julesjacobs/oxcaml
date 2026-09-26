module D = Hm_declarative
module A = Hmc_admission
module G = Hmc_grounding
module N = Hmc_no_free

type definition = {scheme : D.scheme; source : D.term; derivation : D.typing}
type catalog = Empty | Declare of definition * catalog [@@inductive]

let[@def] rec (context @ total) (catalog : catalog @ immutable) = match catalog with
  | Empty -> D.Empty_context
  | Declare (d, earlier) -> D.Binding (d.scheme, context earlier)

let[@def] rec (rank @ total) (catalog : catalog @ immutable) = match catalog with
  | Empty -> D.Z | Declare (_, earlier) -> D.S (rank earlier)

let[@def] (definition_valid @ total) (earlier : catalog @ immutable)
    (d : definition @ immutable) = ghost_ (
  N.scheme d.scheme && N.typing d.derivation && A.callable d.source && A.local d.source d.derivation
  && match d.scheme with D.Forall (k, ty) ->
    D.typed (D.add k D.Z) (D.weaken_context k (context earlier)) d.source ty d.derivation)

let[@def] rec (valid @ total) (catalog : catalog @ immutable) = ghost_ (match catalog with
  | Empty -> true
  | Declare (d, earlier) -> valid earlier && definition_valid earlier d)

let[@def] rec (rebuild @ total) (catalog : catalog @ immutable) (entry : D.term @ immutable) =
  match catalog with Empty -> entry
  | Declare (d, earlier) -> rebuild earlier (D.Let (d.source, entry))

let[@def] rec (rebuild_derivation @ total) (catalog : catalog @ immutable)
    (entry : D.typing @ immutable) = match catalog with
  | Empty -> entry
  | Declare (d, earlier) -> rebuild_derivation earlier (D.Let_binding (d.scheme, d.derivation, entry))

type program = {globals : catalog; entry : D.term; derivation : D.typing}

let[@def] (ready @ total) (p : program @ immutable) = ghost_ (
  valid p.globals && A.callable p.entry && A.local p.entry p.derivation && N.typing p.derivation
  && D.typed D.Z (context p.globals) p.entry (D.Function (D.Word64, D.Word64)) p.derivation)

let rec (split @ total) : (catalog : catalog) @ immutable ->
    (term : D.term) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | valid catalog && A.outer term d && N.typing d
      && D.typed D.Z (context catalog) term (D.Function (D.Word64, D.Word64)) d} ->
    {p : program | ready p && rebuild p.globals p.entry === rebuild catalog term
      && rebuild_derivation p.globals p.derivation === rebuild_derivation catalog d} @ immutable =
  fun catalog term d premise ->
    ghost_ (A.outer_def term d; N.typing_def d;
      D.typed_def D.Z (context catalog) term (D.Function (D.Word64, D.Word64)) d);
    match term, d with
    | D.Let (rhs, rest), D.Let_binding (scheme, dr, db) ->
      let definition = {scheme; source = rhs; derivation = dr} in
      let globals = Declare (definition, catalog) in
      ghost_ (definition_valid_def catalog definition; valid_def globals; context_def globals;
        rebuild_def globals rest; rebuild_derivation_def globals db);
      split globals rest db ()
    | _ ->
      let p = {globals = catalog; entry = term; derivation = d} in
      ghost_ (ready_def p); p

let (extract @ total) : (input : A.admitted) @ immutable ->
    {p : program | ready p && rebuild p.globals p.entry === input.G.term
      && rebuild_derivation p.globals p.derivation === input.G.proof} @ immutable = fun input ->
  ghost_ (valid_def Empty; context_def Empty; rebuild_def Empty input.G.term;
    rebuild_derivation_def Empty input.G.proof);
  split Empty input.G.term input.G.proof ()

type selected = {definition : definition; earlier : catalog}

let[@def] rec (selection @ total) (catalog : catalog @ immutable) (index : D.index @ immutable) =
  match catalog with
  | Empty -> None
  | Declare (definition, earlier) -> match index with
    | D.Z -> Some {definition; earlier} | D.S i -> selection earlier i

let rec (rank_self @ total) : (n : D.index) @ immutable ->
    {u : unit | D.present (D.S n) n} @ ghost = fun n -> ghost_ (
    D.present_def (D.S n) n; match n with D.Z -> () | D.S n -> rank_self n)

let rec (rank_weaken @ total) : (n : D.index) @ immutable -> (i : D.index) @ immutable ->
    {u : unit | D.present n i} -> {u : unit | D.present (D.S n) i} @ ghost =
  fun n i premise -> ghost_ (
    D.present_def n i; D.present_def (D.S n) i;
    match n, i with D.S n, D.S i -> rank_weaken n i () | _ -> ())

let rec (select @ total) : (catalog : catalog) @ immutable -> (index : D.index) @ immutable ->
    {u : unit | valid catalog} ->
    {r : selected option | r === selection catalog index && match r with
      | None -> D.lookup (context catalog) index === None
      | Some s -> valid s.earlier && definition_valid s.earlier s.definition
        && D.lookup (context catalog) index === Some s.definition.scheme
        && D.present (rank catalog) (rank s.earlier)} @ immutable = fun catalog index premise ->
  ghost_ (selection_def catalog index; valid_def catalog; context_def catalog; rank_def catalog; D.lookup_def (context catalog) index);
  match catalog with
  | Empty -> None
  | Declare (definition, earlier) -> match index with
    | D.Z -> ghost_ (rank_self (rank earlier)); Some {definition; earlier}
    | D.S rest ->
      let found = select earlier rest () in
      ghost_ (match found with None -> () | Some s -> rank_weaken (rank earlier) (rank s.earlier) ());
      found
