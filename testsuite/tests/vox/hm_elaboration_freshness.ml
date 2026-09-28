module Ty = Copy_spec
module H = Pref.Heap
module U = Level_unifier_spec
module E = Effective_level
module T = Effective_template
module F = Level_finite_spec
module D = Hm_declarative
module G = Hm_generalization

let[@def] (closed_at @ total) (heap : Ty.node Pref.heap @ immutable)
    (protected : (Ty.node Pref.t @ immutable total -> bool) @ total)
    (p : Ty.node Pref.t @ immutable) = ghost_ (
  not (H.mem heap p) || protected p || match U.observe heap p with
  | None | Some Ty.Var | Some Ty.Bool | Some Ty.Word -> true
  | Some (Ty.Link q) | Some (Ty.List q) -> not (protected q)
  | Some (Ty.Arrow (a, b)) -> not (protected a) && not (protected b))

let rec (readback_avoids @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
    (closed : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | closed_at heap protected p})) @ total ->
    (tree : F.tree) @ immutable -> (p : Ty.node Pref.t) @ immutable ->
    {u : unit | F.finite heap tree && not (protected (F.tree_root tree)) && protected p} ->
    {u : unit | not (G.occurs p (D.embed (F.readback tree)))} @ ghost =
  fun heap protected closed tree p premise -> ghost_ (
    F.finite_def heap tree; F.tree_root_def tree; F.readback_def tree;
    closed (F.tree_root tree); closed_at_def heap protected (F.tree_root tree);
    D.embed_def (F.readback tree); G.occurs_def p (D.embed (F.readback tree));
    match tree with
    | F.Free q -> let same = Pref.equal p q in if same then () else ()
    | F.Constant_tree _ | F.Word_tree _ -> ()
    | F.Alias_tree (_, child) | F.List_tree (_, child) ->
      readback_avoids heap protected closed child p ()
    | F.Branch (_, left, right) ->
      readback_avoids heap protected closed left p ();
      readback_avoids heap protected closed right p ())

let (initial @ total) : (heap : Ty.node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (valid : ((p : Ty.node Pref.t) @ immutable -> {u : unit | E.valid_head heap heads p})) @ total ->
    (protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
    (agrees : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | protected p = T.generic heap heads p})) @ total ->
    (p : Ty.node Pref.t) @ immutable ->
    {u : unit | E.effective_ordered heap heads p && (not (H.mem heap p) || Ty.source_ok heap p)} ->
    {u : unit | closed_at heap protected p} @ ghost =
  fun heap heads valid protected agrees p premise -> ghost_ (
    closed_at_def heap protected p; agrees p; T.generic_def heap heads p;
    E.effective_ordered_def heap heads p; U.observe_def heap p; Ty.source_ok_def heap p;
    if H.mem heap p then (
      valid p;
      match H.at heap p with
      | None -> ()
      | Some node -> match node.Ty.desc with
        | Ty.Var | Ty.Bool | Ty.Word -> ()
        | Ty.Link q ->
          valid q; E.link_level heap heads p q ();
          agrees q; T.generic_def heap heads q
        | Ty.List a ->
          U.terminal_def heap p; E.terminal_level heap heads p ();
          Level_spec.at_level_def heap p;
          agrees a; T.generic_def heap heads a;
          (match node.Ty.level with Ty.Generic -> () | Ty.Finite n ->
            E.effective_below_def heap heads a n)
        | Ty.Arrow (a, b) ->
          U.terminal_def heap p; E.terminal_level heap heads p ();
          Level_spec.at_level_def heap p;
          agrees a; agrees b; T.generic_def heap heads a; T.generic_def heap heads b;
          (match node.Ty.level with Ty.Generic -> () | Ty.Finite n ->
            E.effective_below_def heap heads a n; E.effective_below_def heap heads b n)); ())

let (frame @ total) : (heap : Ty.node Pref.heap) @ immutable -> (after : Ty.node Pref.heap) @ immutable ->
    (protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
    (p : Ty.node Pref.t) @ immutable ->
    {u : unit | closed_at heap protected p && H.mem heap p = H.mem after p
      && U.observe heap p === U.observe after p} ->
    {u : unit | closed_at after protected p} @ ghost =
  fun heap after protected p premise -> ghost_ (
    closed_at_def heap protected p; closed_at_def after protected p)

let (write @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
    (p : Ty.node Pref.t) @ immutable -> (node : Ty.node) @ immutable ->
    (x : Ty.node Pref.t) @ immutable ->
    {u : unit | closed_at heap protected x && (protected p || match node.Ty.desc with
      | Ty.Var | Ty.Bool | Ty.Word -> true
      | Ty.Link q | Ty.List q -> not (protected q)
      | Ty.Arrow (a, b) -> not (protected a) && not (protected b))} ->
    {u : unit | closed_at (H.put heap p node) protected x} @ ghost =
  fun heap protected p node x premise -> ghost_ (
    closed_at_def heap protected x;
    closed_at_def (H.put heap p node) protected x;
    U.observe_def heap x; U.observe_def (H.put heap p node) x;
    let same = Pref.equal p x in if same then () else ())

let (close @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
    (cut : int) -> (pool : Generalize_spec.pool) @ immutable ->
    (p : Ty.node Pref.t) @ immutable ->
    {u : unit | Generalize_spec.pool_scoped heap pool && closed_at heap protected p} ->
    {u : unit | closed_at (Representative_pool_spec.close_heap heap cut pool) protected p} @ ghost =
  fun heap protected cut pool p premise -> ghost_ (
    let after = Representative_pool_spec.close_heap heap cut pool in
    Representative_pool_spec.close_heap_def heap cut pool;
    let filtered = Representative_level.representatives heap pool in
    Representative_level.representatives_scoped heap pool ();
    Generalize_proofs.closed_observe heap cut filtered p ();
    Generalize_spec.closed_at_def heap after cut filtered p;
    U.observe_def heap p; U.observe_def after p;
    frame heap after protected p ())

let rec (resolution @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
    (closed : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | closed_at heap protected p})) @ total ->
    (p : Ty.node Pref.t) @ immutable -> (root : Ty.node Pref.t) @ immutable ->
    (path : U.resolution) @ immutable ->
    {u : unit | U.resolves heap p root path && not (protected p)} ->
    {u : unit | not (protected root)} @ ghost =
  fun heap protected closed p root path premise -> ghost_ (
    U.resolves_def heap p root path;
    match path with
    | U.Here -> ()
    | U.Via (q, rest) ->
      closed p; closed_at_def heap protected p;
      resolution heap protected closed q root rest ())

let rec (compression @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (after : Ty.node Pref.heap) @ immutable -> (edits : Compression_spec.edits) @ immutable ->
    (protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
    (closed : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | closed_at heap protected p})) @ total ->
    (x : Ty.node Pref.t) @ immutable ->
    {u : unit | Effective_compression_spec.effective_rewritten heap after edits} ->
    {u : unit | closed_at after protected x} @ ghost =
  fun heap after edits protected closed x premise -> ghost_ (
    Effective_compression_spec.effective_rewritten_def heap after edits;
    match edits with
    | Compression_spec.Done -> closed x
    | Compression_spec.Write (p, _, root, path, rest) ->
      if not (protected p) then resolution heap protected closed p root path ();
      let node = U.redirect heap p root in U.redirect_def heap p root;
      U.observe_def heap p;
      let middle = H.put heap p node in
      let middle_closed : ((y : Ty.node Pref.t) @ immutable ->
        {u : unit | closed_at middle protected y}) @ total = fun y ->
        closed y; write heap protected p node y () in
      compression middle after rest protected middle_closed x ())

module A = Hm_abstraction
module Names = Hm_freshness_proofs
module P = Hm_template_instance_proofs

let rec (readback_names @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
    (closed : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | closed_at heap protected p})) @ total ->
    (names : A.names) @ immutable ->
    (selected : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | A.position names p === None || protected p})) @ total ->
    (tree : F.tree) @ immutable ->
    {u : unit | F.finite heap tree && not (protected (F.tree_root tree))} ->
    {u : unit | A.avoids names (D.embed (F.readback tree))} @ ghost =
  fun heap protected closed names selected tree premise -> ghost_ (
    F.finite_def heap tree; F.tree_root_def tree; F.readback_def tree;
    closed (F.tree_root tree); closed_at_def heap protected (F.tree_root tree);
    D.embed_def (F.readback tree); A.avoids_def names (D.embed (F.readback tree));
    match tree with
    | F.Free p -> selected p
    | F.Constant_tree _ | F.Word_tree _ -> ()
    | F.Alias_tree (_, child) | F.List_tree (_, child) ->
      readback_names heap protected closed names selected child ()
    | F.Branch (_, left, right) ->
      readback_names heap protected closed names selected left ();
      readback_names heap protected closed names selected right ())

let rec (parameters_protected @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (heads : E.heads) @ total ->
    (protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
    (agrees : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | protected p = T.generic heap heads p})) @ total ->
    (schema : Ty.template) @ immutable -> (p : Ty.node Pref.t) @ immutable ->
    {u : unit | T.valid_template heap heads schema} ->
    {u : unit | A.position (Names.template_names schema) p === None || protected p} @ ghost =
  fun heap heads protected agrees schema p premise -> ghost_ (
    T.valid_template_def heap heads schema; Names.template_names_def schema;
    A.position_def A.No_names p;
    match schema with
    | Ty.Boundary _ | Ty.Constant _ | Ty.Word_constant _ -> ()
    | Ty.Parameter q ->
      A.position_def (A.Name (q, A.No_names)) p; agrees q
    | Ty.Indirect (_, child) | Ty.List_template (_, child) ->
      parameters_protected heap heads protected agrees child p ()
    | Ty.Product (_, left, right) ->
      Names.join_position (Names.template_names left) (Names.template_names right) p;
      parameters_protected heap heads protected agrees left p ();
      parameters_protected heap heads protected agrees right p ())

let rec (boundaries_after @ total) : (heap : Ty.node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (after : Ty.node Pref.heap) @ immutable ->
    (protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
    (agrees : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | protected p = T.generic heap heads p})) @ total ->
    (owned : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | not (H.mem heap p) || H.mem after p})) @ total ->
    (closed : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | closed_at after protected p})) @ total ->
    (trees : ((p : Ty.node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === p &&
        (if H.mem after p then F.finite after t else U.observe after p === None)} @ immutable)) @ total ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (values : ((p : Ty.node Pref.t) @ immutable -> {u : unit | rho p === F.readback (trees p)})) @ total ->
    (names : A.names) @ immutable ->
    (selected : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | A.position names p === None || protected p})) @ total ->
    (schema : Ty.template) @ immutable ->
    {u : unit | T.valid_template heap heads schema} ->
    {u : unit | P.boundaries_avoid names rho schema} @ ghost =
  fun heap heads after protected agrees owned closed trees rho values names selected schema premise -> ghost_ (
    T.valid_template_def heap heads schema; P.boundaries_avoid_def names rho schema;
    match schema with
    | Ty.Parameter _ | Ty.Constant _ | Ty.Word_constant _ -> ()
    | Ty.Boundary p ->
      agrees p; owned p; values p; T.finite_def heap heads p; T.generic_def heap heads p;
      readback_names after protected closed names selected (trees p) ()
    | Ty.Indirect (_, child) | Ty.List_template (_, child) ->
      boundaries_after heap heads after protected agrees owned closed trees rho values names selected child ()
    | Ty.Product (_, left, right) ->
      boundaries_after heap heads after protected agrees owned closed trees rho values names selected left ();
      boundaries_after heap heads after protected agrees owned closed trees rho values names selected right ())

module L = Level_spec
module R = Representative_level
module V = Effective_unifier_spec
module Heads = Effective_unifier_heads

let[@def] (generic_at @ total) (heap : Ty.node Pref.heap @ immutable)
    (p : Ty.node Pref.t @ immutable) (r : R.representative @ immutable) = ghost_ (
  U.resolves heap p r.R.root r.R.path && L.at_level heap r.R.root === Ty.Generic)

let (generic_unify @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (p : Ty.node Pref.t) @ immutable -> (q : Ty.node Pref.t) @ immutable ->
    (ok : bool) -> (after : Ty.node Pref.heap) @ immutable -> (derivation : V.derivation) @ immutable ->
    (x : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
    {u : unit | V.unified heap p q ok after derivation && generic_at heap x r} ->
    {r : R.representative | generic_at after x r} @ immutable ghost =
  fun heap p q ok after derivation x r premise -> ghost_ (
    generic_at_def heap x r;
    Hm_effective_generic.generic_path_def heap x r (U.observe heap x);
    let next = Hm_effective_generic.unify heap p q ok after derivation x r (U.observe heap x) () in
    Hm_effective_generic.generic_path_def after x next (U.observe heap x);
    generic_at_def after x next; next)

let (generic_frame @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (after : Ty.node Pref.heap) @ immutable ->
    (changes : ((p : Ty.node Pref.t) @ immutable -> {u : unit | L.lower_frame heap after p})) @ total ->
    (p : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
    {u : unit | generic_at heap p r} ->
    {u : unit | generic_at after p r} @ ghost =
  fun heap after changes p r premise -> ghost_ (
    generic_at_def heap p r;
    Heads.framed heap after changes p r.R.root r.R.path ();
    Heads.progress_def heap r.R.root after r.R.root;
    L.decreases_def Ty.Generic (L.at_level after r.R.root);
    generic_at_def after p r)

let (generic_compression @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (after : Ty.node Pref.heap) @ immutable -> (edits : Compression_spec.edits) @ immutable ->
    (p : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
    {u : unit | Effective_compression_spec.effective_rewritten heap after edits && generic_at heap p r} ->
    {s : R.representative | generic_at after p s} @ immutable ghost =
  fun heap after edits p r premise -> ghost_ (
    generic_at_def heap p r;
    let path = Effective_compression_proofs.resolution heap after edits p r.R.root r.R.path () in
    Effective_compression_proofs.frame heap after edits r.R.root ();
    let next = {R.root = r.R.root; path} in generic_at_def after p next; next)

let (terminal_unprotected @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
    (generic : ((p : Ty.node Pref.t) @ immutable ->
      {r : R.representative | not (protected p) || generic_at heap p r} @ immutable)) @ total ->
    (p : Ty.node Pref.t) @ immutable ->
    {u : unit | U.terminal heap p && L.active heap p} ->
    {u : unit | not (protected p)} @ ghost =
  fun heap protected generic p premise -> ghost_ (
    let r = generic p in
    if protected p then (
      generic_at_def heap p r;
      R.terminal_here heap p r.R.root r.R.path ();
      L.active_def heap p);
    ())

let rec (unification @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (p : Ty.node Pref.t) @ immutable -> (q : Ty.node Pref.t) @ immutable ->
    (ok : bool) -> (after : Ty.node Pref.heap) @ immutable -> (derivation : V.derivation) @ immutable ->
    (protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
    (closed : ((p : Ty.node Pref.t) @ immutable -> {u : unit | closed_at heap protected p})) @ total ->
    (generic : ((p : Ty.node Pref.t) @ immutable ->
      {r : R.representative | not (protected p) || generic_at heap p r} @ immutable)) @ total ->
    (x : Ty.node Pref.t) @ immutable ->
    {u : unit | V.unified heap p q ok after derivation} ->
    {u : unit | closed_at after protected x} @ ghost =
  fun heap p q ok after derivation protected closed generic x premise -> ghost_ (
    V.unified_def heap p q ok after derivation;
    match derivation with
    | V.Swap rest -> unification heap q p ok after rest protected closed generic x ()
    | V.Resolve (a, b, _, _, rest) | V.List_children (a, b, rest) ->
      unification heap a b ok after rest protected closed generic x ()
    | V.Base old ->
      U.unified_def heap p q ok after old; closed x;
      (match old with
      | U.Bind_left _ ->
        terminal_unprotected heap protected generic q ();
        U.redirect_def heap p q; Ty.cell_def (Ty.Link q) 0; write heap protected p (U.redirect heap p q) x ()
      | U.Bind_right _ ->
        terminal_unprotected heap protected generic p ();
        U.redirect_def heap q p; Ty.cell_def (Ty.Link p) 0; write heap protected q (U.redirect heap q p) x ()
      | _ -> ())
    | V.Scanned (needle, marks, rest) ->
      let middle = U.scan_heap heap marks in
      let changes : ((y : Ty.node Pref.t) @ immutable -> {u : unit | L.lower_frame heap middle y}) @ total = fun y ->
        Marked_occurs_proofs.scan_frame heap needle marks y () in
      let middle_closed : ((y : Ty.node Pref.t) @ immutable -> {u : unit | closed_at middle protected y}) @ total = fun y ->
        closed y; Marked_occurs_proofs.scan_observe heap needle marks y ();
        frame heap middle protected y () in
      let middle_generic : ((y : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected y) || generic_at middle y r} @ immutable) @ total = fun y ->
        let r = generic y in
        if protected y then generic_frame heap middle changes y r ();
        r in
      unification middle p q ok after rest protected middle_closed middle_generic x ()
    | V.Terminal_lower (bound, edits, tree, rest) ->
      let middle = L.lower_heap heap bound edits in
      Terminal_lower_spec.completed_def heap bound q middle edits tree;
      let changes : ((y : Ty.node Pref.t) @ immutable -> {u : unit | L.lower_frame heap middle y}) @ total = fun y ->
        Terminal_lower_proofs.lowering_at heap bound edits y () in
      let middle_closed : ((y : Ty.node Pref.t) @ immutable -> {u : unit | closed_at middle protected y}) @ total = fun y ->
        closed y; Effective_unifier_finite.lower_observe heap bound edits y ();
        frame heap middle protected y () in
      let middle_generic : ((y : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected y) || generic_at middle y r} @ immutable) @ total = fun y ->
        let r = generic y in
        if protected y then generic_frame heap middle changes y r ();
        r in
      unification middle p q ok after rest protected middle_closed middle_generic x ()
    | V.Pre_compress (middle, edits, rest) ->
      let middle_closed : ((y : Ty.node Pref.t) @ immutable -> {u : unit | closed_at middle protected y}) @ total = fun y ->
        compression heap middle edits protected closed y () in
      let middle_generic : ((y : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected y) || generic_at middle y r} @ immutable) @ total = fun y ->
        let r = generic y in
        if protected y then generic_compression heap middle edits y r () else r in
      unification middle p q ok after rest protected middle_closed middle_generic x ()
    | V.Children (a, b, c, d, middle, left_ok, left, right) ->
      let middle_closed : ((y : Ty.node Pref.t) @ immutable -> {u : unit | closed_at middle protected y}) @ total = fun y ->
        unification heap a c left_ok middle left protected closed generic y () in
      let middle_generic : ((y : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected y) || generic_at middle y r} @ immutable) @ total = fun y ->
        let r = generic y in
        if protected y then generic_unify heap a c left_ok middle left y r () else r in
      if left_ok then unification middle b d ok after right protected middle_closed middle_generic x ()
      else middle_closed x
    | V.Post_link (middle, rest, source, target) ->
      let middle_closed : ((y : Ty.node Pref.t) @ immutable -> {u : unit | closed_at middle protected y}) @ total = fun y ->
        unification heap p q true middle rest protected closed generic y () in
      let middle_generic : ((y : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected y) || generic_at middle y r} @ immutable) @ total = fun y ->
        let r = generic y in
        if protected y then generic_unify heap p q true middle rest y r () else r in
      Structure_spec.linkable_def middle source target;
      let a = F.tree_root source in let b = F.tree_root target in
      terminal_unprotected middle protected middle_generic b ();
      middle_closed x; U.redirect_def middle a b; Ty.cell_def (Ty.Link b) 0;
      write middle protected a (U.redirect middle a b) x ())

module Copy = Effective_copy_spec

let (copy_target @ total) : (heap : Ty.node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (valid : ((p : Ty.node Pref.t) @ immutable -> {u : unit | E.valid_head heap heads p})) @ total ->
    (protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
    (generic : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | not (protected p) || T.generic heap heads p})) @ total ->
    (epoch : Ty.node Pref.t) @ immutable -> (depth : int) -> (history : Ty.history) @ immutable ->
    (p : Ty.node Pref.t) @ immutable -> (q : Ty.node Pref.t) @ immutable ->
    {u : unit | Copy.effective_valid heap heads epoch depth history
      && Copy.effective_target_for heap heads history p q} ->
    {u : unit | not (protected q)} @ ghost =
  fun heap heads valid protected generic epoch depth history p q premise -> ghost_ (
    Copy.effective_target_for_def heap heads history p q;
    generic q; T.generic_def heap heads q;
    match E.level heap heads p with
    | Ty.Finite _ -> ()
    | Ty.Generic -> Effective_copy_heap_proofs.mapped_fresh heap heads valid epoch depth history p q ())

let rec (copy_history @ total) : (heap : Ty.node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (valid : ((p : Ty.node Pref.t) @ immutable -> {u : unit | E.valid_head heap heads p})) @ total ->
    (protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
    (closed : ((p : Ty.node Pref.t) @ immutable -> {u : unit | closed_at heap protected p})) @ total ->
    (generic : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | not (protected p) || T.generic heap heads p})) @ total ->
    (epoch : Ty.node Pref.t) @ immutable -> (depth : int) -> (history : Ty.history) @ immutable ->
    (x : Ty.node Pref.t) @ immutable ->
    {u : unit | Copy.effective_valid heap heads epoch depth history} ->
    {u : unit | closed_at (Ty.heap heap epoch depth history) protected x} @ ghost =
  fun heap heads valid protected closed generic epoch depth history x premise -> ghost_ (
    Copy.effective_valid_def heap heads epoch depth history;
    Ty.heap_def heap epoch depth history;
    match history with
    | Ty.Clean -> closed x
    | Ty.Start ->
      closed x; Ty.cell_def Ty.Bool depth;
      write heap protected epoch (Ty.cell Ty.Bool depth) x ()
    | Ty.Fresh (rest, p, q, old, desc) ->
      copy_history heap heads valid protected closed generic epoch depth rest x ();
      copy_history heap heads valid protected closed generic epoch depth rest p ();
      let before = Ty.heap heap epoch depth rest in
      closed_at_def before protected p; U.observe_def before p;
      Effective_copy_heap_proofs.history_at heap heads epoch depth rest p ();
      Copy.effective_ready_def heap heads rest old.Ty.desc desc;
      (match old.Ty.desc, desc with
      | Ty.List a, Ty.List b -> copy_target heap heads valid protected generic epoch depth rest a b ()
      | Ty.Arrow (a, b), Ty.Arrow (c, d) ->
        copy_target heap heads valid protected generic epoch depth rest a c ();
        copy_target heap heads valid protected generic epoch depth rest b d ()
      | _ -> ());
      let node = Ty.cell desc depth in Ty.cell_def desc depth;
      write before protected q node x ();
      let middle = H.put before q node in
      let mark = Ty.session_mark rest old epoch q in
      Ty.session_mark_def rest old epoch q; Ty.mark_def old epoch q;
      write middle protected p mark x ()
    | Ty.Alias (rest, p, q, old) ->
      copy_history heap heads valid protected closed generic epoch depth rest x ();
      copy_history heap heads valid protected closed generic epoch depth rest p ();
      let before = Ty.heap heap epoch depth rest in
      closed_at_def before protected p; U.observe_def before p;
      Effective_copy_heap_proofs.history_at heap heads epoch depth rest p ();
      let mark = Ty.session_mark rest old epoch q in
      Ty.session_mark_def rest old epoch q; Ty.mark_def old epoch q;
      write before protected p mark x ())

let (copy @ total) : (heap : Ty.node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (valid : ((p : Ty.node Pref.t) @ immutable -> {u : unit | E.valid_head heap heads p})) @ total ->
    (protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
    (closed : ((p : Ty.node Pref.t) @ immutable -> {u : unit | closed_at heap protected p})) @ total ->
    (generic : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | not (protected p) || T.generic heap heads p})) @ total ->
    (epoch : Ty.node Pref.t) @ immutable -> (depth : int) -> (history : Ty.history) @ immutable ->
    (x : Ty.node Pref.t) @ immutable ->
    {u : unit | Copy.effective_valid heap heads epoch depth history} ->
    {u : unit | closed_at (Hm_execution_spec.copy_heap heap epoch depth history) protected x} @ ghost =
  fun heap heads valid protected closed generic epoch depth history x premise -> ghost_ (
    copy_history heap heads valid protected closed generic epoch depth history x ();
    Effective_copy_metadata.result_at heap heads epoch depth history x ();
    let raw = Ty.heap heap epoch depth history in
    let after = Hm_execution_spec.copy_heap heap epoch depth history in
    let trail = Pooled_spec.touched history in
    Hm_execution_spec.copy_heap_def heap epoch depth history;
    Copy_cleanup_spec.swept_at_def raw after trail x;
    U.observe_def raw x; U.observe_def after x;
    frame raw after protected x ())

module Run = Hm_effective_execution_spec
module Env = Hm_environment_spec
module Pool = Generalize_spec
module Forest = Hm_effective_forest
module Result = Hm_effective_result

let (generic_run @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (depth : int) -> (pool : Pool.pool) @ immutable -> (env : Env.env) @ immutable ->
    (execution : Run.execution) @ immutable -> (after : Ty.node Pref.heap) @ immutable ->
    (final_pool : Pool.pool) @ immutable -> (p : Ty.node Pref.t) @ immutable ->
    (r : R.representative) @ immutable ->
    {u : unit | Run.ran heap depth pool env execution after final_pool && generic_at heap p r} ->
    {r : R.representative | generic_at after p r} @ immutable ghost =
  fun heap depth pool env execution after final_pool p r premise -> ghost_ (
    generic_at_def heap p r;
    Hm_effective_generic.generic_path_def heap p r (U.observe heap p);
    let next = Hm_effective_generic.run heap depth pool env execution after final_pool p r (U.observe heap p) () in
    Hm_effective_generic.generic_path_def after p next (U.observe heap p);
    generic_at_def after p next; next)

let (generic_allocate @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (p : Ty.node Pref.t) @ immutable -> (node : Ty.node) @ immutable ->
    (x : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
    {u : unit | not (H.mem heap p) && generic_at heap x r} ->
    {u : unit | generic_at (H.put heap p node) x r} @ ghost =
  fun heap p node x r premise -> ghost_ (
    generic_at_def heap x r;
    Hm_effective_generic.generic_path_def heap x r (U.observe heap x);
    Hm_effective_generic.allocate heap p node x r (U.observe heap x) ();
    Hm_effective_generic.generic_path_def (H.put heap p node) x r (U.observe heap x);
    generic_at_def (H.put heap p node) x r)

let (generic_close @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (cut : int) -> (pool : Pool.pool) @ immutable ->
    (p : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
    {u : unit | Pool.pool_scoped heap pool && generic_at heap p r} ->
    {u : unit | generic_at (Representative_pool_spec.close_heap heap cut pool) p r} @ ghost =
  fun heap cut pool p r premise -> ghost_ (
    generic_at_def heap p r;
    Hm_effective_generic.generic_path_def heap p r (U.observe heap p);
    Hm_effective_generic.close heap cut pool p r (U.observe heap p) ();
    let after = Representative_pool_spec.close_heap heap cut pool in
    Hm_effective_generic.generic_path_def after p r (U.observe heap p);
    generic_at_def after p r)

let (fresh_unprotected @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
    (generic : ((p : Ty.node Pref.t) @ immutable ->
      {r : R.representative | not (protected p) || generic_at heap p r} @ immutable)) @ total ->
    (p : Ty.node Pref.t) @ immutable -> {u : unit | not (H.mem heap p)} ->
    {u : unit | not (protected p)} @ ghost =
  fun heap protected generic p premise -> ghost_ (
    let r = generic p in
    if protected p then (generic_at_def heap p r; U.resolves_def heap p r.R.root r.R.path);
    ())

let (finite_unprotected @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
    (generic : ((p : Ty.node Pref.t) @ immutable ->
      {r : R.representative | not (protected p) || generic_at heap p r} @ immutable)) @ total ->
    (p : Ty.node Pref.t) @ immutable -> (finite : R.representative) @ immutable ->
    {u : unit | Result.finite_path heap p finite} ->
    {u : unit | not (protected p)} @ ghost =
  fun heap protected generic p finite premise -> ghost_ (
    Result.finite_path_def heap p finite;
    let r = generic p in
    if protected p then (
      generic_at_def heap p r;
      R.unique heap p r.R.root r.R.path finite.R.root finite.R.path ();
      Ty.finite_node_def heap finite.R.root; L.at_level_def heap finite.R.root);
    ())

let rec (run @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (trees : ((p : Ty.node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === p &&
        (if H.mem heap p then F.finite heap t else U.observe heap p === None)} @ immutable)) @ total ->
    (depth : int) -> (pool : Pool.pool) @ immutable -> (env : Env.env) @ immutable ->
    (execution : Run.execution) @ immutable -> (after : Ty.node Pref.heap) @ immutable ->
    (final_pool : Pool.pool) @ immutable ->
    (protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
    (closed : ((p : Ty.node Pref.t) @ immutable -> {u : unit | closed_at heap protected p})) @ total ->
    (generic : ((p : Ty.node Pref.t) @ immutable ->
      {r : R.representative | not (protected p) || generic_at heap p r} @ immutable)) @ total ->
    (x : Ty.node Pref.t) @ immutable ->
    {u : unit | Run.ran heap depth pool env execution after final_pool} ->
    {u : unit | closed_at after protected x} @ ghost =
  fun heap trees depth pool env execution after final_pool protected closed generic x premise -> ghost_ (
    Run.ran_def heap depth pool env execution after final_pool;
    match execution with
    | Run.RShared _ -> closed x
    | Run.RBool p | Run.RFalse p ->
      closed x; Ty.cell_def Ty.Bool depth;
      write heap protected p (Ty.cell Ty.Bool depth) x ()
    | Run.RWord (_, p) ->
      closed x; Ty.cell_def Ty.Word depth;
      write heap protected p (Ty.cell Ty.Word depth) x ()
    | Run.RVar (i, target, epoch, history, certificate) ->
      (match Env.lookup env i with
      | None -> ()
      | Some original ->
        let[@def] heads : E.heads = fun p -> Forest_heads.select heap trees p in
        let valid : ((p : Ty.node Pref.t) @ immutable -> {u : unit | E.valid_head heap heads p}) @ total = fun p ->
          heads_def p; let _selected = Forest_heads.select heap trees p in E.valid_head_def heap heads p in
        let protected_generic : ((p : Ty.node Pref.t) @ immutable ->
          {u : unit | not (protected p) || T.generic heap heads p}) @ total = fun p ->
          let r = generic p in
          if protected p then (
            generic_at_def heap p r; U.resolves_def heap p r.R.root r.R.path;
            valid p; E.valid_head_def heap heads p; E.level_def heap heads p;
            let selected = heads p in
            R.unique heap p r.R.root r.R.path selected.R.root selected.R.path ();
            T.generic_def heap heads p);
          () in
        Copy_certificate_proofs.replay heap certificate heads valid epoch depth history original target ();
        Run.copy_heap_def heap epoch depth history;
        Hm_execution_spec.copy_heap_def heap epoch depth history;
        copy heap heads valid protected closed protected_generic epoch depth history x ())
    | Run.RApp_left (child, _) | Run.RCons_left (child, _)
    | Run.RIf (_, _, _, child) | Run.RCaseList (_, _, _, child) ->
      run heap trees depth pool env child after final_pool protected closed generic x ()
    | Run.RLet_left (child, _) ->
      run heap trees (depth + 1) Pool.Empty env child after final_pool protected closed generic x ()
    | Run.RApp_right (left, right, middle, left_pool)
    | Run.RCons_right (left, right, middle, left_pool) ->

      let trees1 : ((y : Ty.node Pref.t) @ immutable ->
        {t : F.tree | F.tree_root t === y &&
          (if H.mem middle y then F.finite middle t else U.observe middle y === None)} @ immutable) @ total = fun y ->
        Forest.run_forest heap trees depth pool env left middle left_pool y () in
      let closed1 : ((y : Ty.node Pref.t) @ immutable -> {u : unit | closed_at middle protected y}) @ total = fun y ->
        run heap trees depth pool env left middle left_pool protected closed generic y () in
      let generic1 : ((y : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected y) || generic_at middle y r} @ immutable) @ total = fun y ->
        let r = generic y in
        if protected y then generic_run heap depth pool env left middle left_pool y r () else r in
      run middle trees1 depth left_pool env right after final_pool protected closed1 generic1 x ()
    | Run.RNil (arg, p) ->

      Run.allocated_def heap depth arg Ty.Var;
      fresh_unprotected heap protected generic arg ();
      let node1 = Ty.cell Ty.Var depth in Ty.cell_def Ty.Var depth;
      let h1 = H.put heap arg node1 in
      let closed1 : ((y : Ty.node Pref.t) @ immutable -> {u : unit | closed_at h1 protected y}) @ total = fun y ->
        closed y; write heap protected arg node1 y () in
      Ty.cell_def (Ty.List arg) depth; closed1 x;
      write h1 protected p (Ty.cell (Ty.List arg) depth) x ()
    | Run.RPrimitive (op, _, _, body, middle, body_pool, out) ->

      let closed1 : ((y : Ty.node Pref.t) @ immutable -> {u : unit | closed_at middle protected y}) @ total = fun y ->
        run heap trees depth pool env body middle body_pool protected closed generic y () in
      (match Run.result body with
      | None -> closed1 x
      | Some _ -> match out with None -> () | Some p ->
        let desc = Run.primitive_desc op in Run.primitive_desc_def op;
        Ty.cell_def desc depth; closed1 x;
        write middle protected p (Ty.cell desc depth) x ())
    | Run.RLam (arg, body, middle, body_pool, out) ->

      Run.allocated_def heap depth arg Ty.Var;
      fresh_unprotected heap protected generic arg ();
      let node1 = Ty.cell Ty.Var depth in Ty.cell_def Ty.Var depth;
      let start = H.put heap arg node1 in
      let allocated1 = Forest.allocated_forest heap trees depth arg Ty.Var () in
      let trees1 : ((y : Ty.node Pref.t) @ immutable ->
        {t : F.tree | F.tree_root t === y &&
          (if H.mem start y then F.finite start t else U.observe start y === None)} @ immutable) @ total = fun y -> allocated1 y in
      let closed1 : ((y : Ty.node Pref.t) @ immutable -> {u : unit | closed_at start protected y}) @ total = fun y ->
        closed y; write heap protected arg node1 y () in
      let generic1 : ((y : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected y) || generic_at start y r} @ immutable) @ total = fun y ->
        let r = generic y in
        if protected y then generic_allocate heap arg node1 y r ();
        r in
      let pool1 = Pool.Entry (arg, pool) in let env1 = Env.Bind (arg, env) in

      let closed2 : ((y : Ty.node Pref.t) @ immutable -> {u : unit | closed_at middle protected y}) @ total = fun y ->
        run start trees1 depth pool1 env1 body middle body_pool protected closed1 generic1 y () in
      let generic2 : ((y : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected y) || generic_at middle y r} @ immutable) @ total = fun y ->
        let r = generic1 y in
        if protected y then generic_run start depth pool1 env1 body middle body_pool y r () else r in
      (match Run.result body with None -> closed2 x | Some b ->
      match out with None -> () | Some p ->

      let finite = Result.run start trees1 depth pool1 env1 body middle body_pool b () in
      finite_unprotected middle protected generic2 b finite ();
      Ty.cell_def (Ty.Arrow (arg, b)) depth; closed2 x;
      write middle protected p (Ty.cell (Ty.Arrow (arg, b)) depth) x ())
    | Run.RApp (left, right, h1, pool1, h2, pool2, p, arrow, ok, d) ->

      let trees1 : ((y : Ty.node Pref.t) @ immutable ->
        {t : F.tree | F.tree_root t === y &&
          (if H.mem h1 y then F.finite h1 t else U.observe h1 y === None)} @ immutable) @ total = fun y ->
        Forest.run_forest heap trees depth pool env left h1 pool1 y () in
      let closed1 : ((y : Ty.node Pref.t) @ immutable -> {u : unit | closed_at h1 protected y}) @ total = fun y ->
        run heap trees depth pool env left h1 pool1 protected closed generic y () in
      let generic1 : ((y : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected y) || generic_at h1 y r} @ immutable) @ total = fun y ->
        let r = generic y in
        if protected y then generic_run heap depth pool env left h1 pool1 y r () else r in

      let closed2 : ((y : Ty.node Pref.t) @ immutable -> {u : unit | closed_at h2 protected y}) @ total = fun y ->
        run h1 trees1 depth pool1 env right h2 pool2 protected closed1 generic1 y () in
      let generic2 : ((y : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected y) || generic_at h2 y r} @ immutable) @ total = fun y ->
        let r = generic1 y in
        if protected y then generic_run h1 depth pool1 env right h2 pool2 y r () else r in
      (match Run.result left, Run.result right with
      | Some f, Some a ->

      let finite = Result.run h1 trees1 depth pool1 env right h2 pool2 a () in
      finite_unprotected h2 protected generic2 a finite ();

      Run.allocated_def h2 depth p Ty.Var;
      fresh_unprotected h2 protected generic2 p ();
      let node3 = Ty.cell Ty.Var depth in Ty.cell_def Ty.Var depth;
      let h3 = H.put h2 p node3 in
      let closed3 : ((y : Ty.node Pref.t) @ immutable -> {u : unit | closed_at h3 protected y}) @ total = fun y ->
        closed2 y; write h2 protected p node3 y () in
      let generic3 : ((y : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected y) || generic_at h3 y r} @ immutable) @ total = fun y ->
        let r = generic2 y in
        if protected y then generic_allocate h2 p node3 y r ();
        r in

      Run.allocated_def h3 depth arrow (Ty.Arrow (a, p));
      fresh_unprotected h3 protected generic3 arrow ();
      let node4 = Ty.cell (Ty.Arrow (a, p)) depth in Ty.cell_def (Ty.Arrow (a, p)) depth;
      let h4 = H.put h3 arrow node4 in
      let closed4 : ((y : Ty.node Pref.t) @ immutable -> {u : unit | closed_at h4 protected y}) @ total = fun y ->
        closed3 y; write h3 protected arrow node4 y () in
      let generic4 : ((y : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected y) || generic_at h4 y r} @ immutable) @ total = fun y ->
        let r = generic3 y in
        if protected y then generic_allocate h3 arrow node4 y r ();
        r in
      unification h4 f arrow ok after d protected closed4 generic4 x ()
      | _ -> ())
    | Run.RCons (left, right, h1, pool1, h2, pool2, p, ok, d) ->

      let trees1 : ((y : Ty.node Pref.t) @ immutable ->
        {t : F.tree | F.tree_root t === y &&
          (if H.mem h1 y then F.finite h1 t else U.observe h1 y === None)} @ immutable) @ total = fun y ->
        Forest.run_forest heap trees depth pool env left h1 pool1 y () in
      let closed1 : ((y : Ty.node Pref.t) @ immutable -> {u : unit | closed_at h1 protected y}) @ total = fun y ->
        run heap trees depth pool env left h1 pool1 protected closed generic y () in
      let generic1 : ((y : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected y) || generic_at h1 y r} @ immutable) @ total = fun y ->
        let r = generic y in
        if protected y then generic_run heap depth pool env left h1 pool1 y r () else r in

      let closed2 : ((y : Ty.node Pref.t) @ immutable -> {u : unit | closed_at h2 protected y}) @ total = fun y ->
        run h1 trees1 depth pool1 env right h2 pool2 protected closed1 generic1 y () in
      let generic2 : ((y : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected y) || generic_at h2 y r} @ immutable) @ total = fun y ->
        let r = generic1 y in
        if protected y then generic_run h1 depth pool1 env right h2 pool2 y r () else r in
      (match Run.result left, Run.result right with
      | Some f, Some a ->

      let finite = Result.run heap trees depth pool env left h1 pool1 f () in
      finite_unprotected h1 protected generic1 f finite ();

      Run.allocated_def h2 depth p (Ty.List f);
      fresh_unprotected h2 protected generic2 p ();
      let node3 = Ty.cell (Ty.List f) depth in Ty.cell_def (Ty.List f) depth;
      let h3 = H.put h2 p node3 in
      let closed3 : ((y : Ty.node Pref.t) @ immutable -> {u : unit | closed_at h3 protected y}) @ total = fun y ->
        closed2 y; write h2 protected p node3 y () in
      let generic3 : ((y : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected y) || generic_at h3 y r} @ immutable) @ total = fun y ->
        let r = generic2 y in
        if protected y then generic_allocate h2 p node3 y r ();
        r in
      unification h3 a p ok after d protected closed3 generic3 x ()
      | _ -> ())
    | Run.RRec (arg, res, self, body, middle, body_pool, finish) ->

      Run.allocated_def heap depth arg Ty.Var;
      fresh_unprotected heap protected generic arg ();
      let node1 = Ty.cell Ty.Var depth in Ty.cell_def Ty.Var depth;
      let h1 = H.put heap arg node1 in
      let allocated1 = Forest.allocated_forest heap trees depth arg Ty.Var () in
      let trees1 : ((y : Ty.node Pref.t) @ immutable ->
        {t : F.tree | F.tree_root t === y &&
          (if H.mem h1 y then F.finite h1 t else U.observe h1 y === None)} @ immutable) @ total = fun y -> allocated1 y in
      let closed1 : ((y : Ty.node Pref.t) @ immutable -> {u : unit | closed_at h1 protected y}) @ total = fun y ->
        closed y; write heap protected arg node1 y () in
      let generic1 : ((y : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected y) || generic_at h1 y r} @ immutable) @ total = fun y ->
        let r = generic y in
        if protected y then generic_allocate heap arg node1 y r ();
        r in

      Run.allocated_def h1 depth res Ty.Var;
      fresh_unprotected h1 protected generic1 res ();
      let node2 = Ty.cell Ty.Var depth in Ty.cell_def Ty.Var depth;
      let h2 = H.put h1 res node2 in
      let allocated2 = Forest.allocated_forest h1 trees1 depth res Ty.Var () in
      let trees2 : ((y : Ty.node Pref.t) @ immutable ->
        {t : F.tree | F.tree_root t === y &&
          (if H.mem h2 y then F.finite h2 t else U.observe h2 y === None)} @ immutable) @ total = fun y -> allocated2 y in
      let closed2 : ((y : Ty.node Pref.t) @ immutable -> {u : unit | closed_at h2 protected y}) @ total = fun y ->
        closed1 y; write h1 protected res node2 y () in
      let generic2 : ((y : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected y) || generic_at h2 y r} @ immutable) @ total = fun y ->
        let r = generic1 y in
        if protected y then generic_allocate h1 res node2 y r ();
        r in

      Run.allocated_def h2 depth self (Ty.Arrow (arg, res));
      fresh_unprotected h2 protected generic2 self ();
      let node3 = Ty.cell (Ty.Arrow (arg, res)) depth in Ty.cell_def (Ty.Arrow (arg, res)) depth;
      let h3 = H.put h2 self node3 in
      let allocated3 = Forest.allocated_forest h2 trees2 depth self (Ty.Arrow (arg, res)) () in
      let trees3 : ((y : Ty.node Pref.t) @ immutable ->
        {t : F.tree | F.tree_root t === y &&
          (if H.mem h3 y then F.finite h3 t else U.observe h3 y === None)} @ immutable) @ total = fun y -> allocated3 y in
      let closed3 : ((y : Ty.node Pref.t) @ immutable -> {u : unit | closed_at h3 protected y}) @ total = fun y ->
        closed2 y; write h2 protected self node3 y () in
      let generic3 : ((y : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected y) || generic_at h3 y r} @ immutable) @ total = fun y ->
        let r = generic2 y in
        if protected y then generic_allocate h2 self node3 y r ();
        r in
      let pool3 = Pool.Entry (self, Pool.Entry (res, Pool.Entry (arg, pool))) in
      let env3 = Env.Bind (arg, Env.Bind (self, env)) in

      let closed4 : ((y : Ty.node Pref.t) @ immutable -> {u : unit | closed_at middle protected y}) @ total = fun y ->
        run h3 trees3 depth pool3 env3 body middle body_pool protected closed3 generic3 y () in
      let generic4 : ((y : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected y) || generic_at middle y r} @ immutable) @ total = fun y ->
        let r = generic3 y in
        if protected y then generic_run h3 depth pool3 env3 body middle body_pool y r () else r in
      (match Run.result body with None -> closed4 x | Some b ->
      match finish with Run.Aborted -> () | Run.Unified (ok, d) ->
      unification middle b res ok after d protected closed4 generic4 x ())
    | Run.RLet (rhs, body, middle, child_pool) ->
      let child_depth = depth + 1 in let empty = Pool.Empty in

      let trees1 : ((y : Ty.node Pref.t) @ immutable ->
        {t : F.tree | F.tree_root t === y &&
          (if H.mem middle y then F.finite middle t else U.observe middle y === None)} @ immutable) @ total = fun y ->
        Forest.run_forest heap trees child_depth empty env rhs middle child_pool y () in
      let closed1 : ((y : Ty.node Pref.t) @ immutable -> {u : unit | closed_at middle protected y}) @ total = fun y ->
        run heap trees child_depth empty env rhs middle child_pool protected closed generic y () in
      let generic1 : ((y : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected y) || generic_at middle y r} @ immutable) @ total = fun y ->
        let r = generic y in
        if protected y then generic_run heap child_depth empty env rhs middle child_pool y r () else r in
      (match Run.result rhs with None -> () | Some p ->
      let start = Representative_pool_spec.close_heap middle depth child_pool in
      let trees2 : ((y : Ty.node Pref.t) @ immutable ->
        {t : F.tree | F.tree_root t === y &&
          (if H.mem start y then F.finite start t else U.observe start y === None)} @ immutable) @ total = fun y ->
        Forest.representative_closed_forest middle trees1 depth child_pool y () in
      let closed2 : ((y : Ty.node Pref.t) @ immutable -> {u : unit | closed_at start protected y}) @ total = fun y ->
        closed1 y; close middle protected depth child_pool y () in
      let generic2 : ((y : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected y) || generic_at start y r} @ immutable) @ total = fun y ->
        let r = generic1 y in
        if protected y then generic_close middle depth child_pool y r ();
        r in
      let next_pool = Representative_pool_spec.transfer_rep start child_pool pool in
      let next_env = Env.Bind (p, env) in
      run start trees2 depth next_pool next_env body after final_pool protected closed2 generic2 x ()))

let (run_boundaries @ total) : (heap : Ty.node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (safe : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | Hm_effective_runtime.safe heap heads p})) @ total ->
    (trees : ((p : Ty.node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === p &&
        (if H.mem heap p then F.finite heap t else U.observe heap p === None)} @ immutable)) @ total ->
    (depth : int) -> (pool : Pool.pool) @ immutable -> (env : Env.env) @ immutable ->
    (execution : Run.execution) @ immutable -> (after : Ty.node Pref.heap) @ immutable ->
    (final_pool : Pool.pool) @ immutable ->
    (final_trees : ((p : Ty.node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === p &&
        (if H.mem after p then F.finite after t else U.observe after p === None)} @ immutable)) @ total ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (values : ((p : Ty.node Pref.t) @ immutable -> {u : unit | rho p === F.readback (final_trees p)})) @ total ->
    (schema : Ty.template) @ immutable ->
    {u : unit | Run.ran heap depth pool env execution after final_pool && T.valid_template heap heads schema} ->
    {u : unit | P.boundaries_avoid (Names.template_names schema) rho schema} @ ghost =
  fun heap heads safe trees depth pool env execution after final_pool final_trees rho values schema premise -> ghost_ (
    let[@def] protected : Ty.node Pref.t @ immutable total -> bool = fun p -> T.generic heap heads p in
    let agrees : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | protected p = T.generic heap heads p}) @ total = fun p -> protected_def p in
    let valid : ((p : Ty.node Pref.t) @ immutable -> {u : unit | E.valid_head heap heads p}) @ total = fun p ->
      safe p; Hm_effective_runtime.safe_def heap heads p in
    let closed : ((p : Ty.node Pref.t) @ immutable -> {u : unit | closed_at heap protected p}) @ total = fun p ->
      safe p; Hm_effective_runtime.safe_def heap heads p;
      initial heap heads valid protected agrees p () in
    let generic : ((p : Ty.node Pref.t) @ immutable ->
      {r : R.representative | not (protected p) || generic_at heap p r} @ immutable) @ total = fun p ->
      let r = heads p in
      protected_def p; T.generic_def heap heads p;
      valid p; E.valid_head_def heap heads p; E.level_def heap heads p;
      generic_at_def heap p r; r in
    let owned : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | not (H.mem heap p) || H.mem after p}) @ total = fun p ->
      Hm_effective_membership.run_extends heap depth pool env execution after final_pool p () in
    let final_closed : ((p : Ty.node Pref.t) @ immutable -> {u : unit | closed_at after protected p}) @ total = fun p ->
      run heap trees depth pool env execution after final_pool protected closed generic p () in
    let selected : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | A.position (Names.template_names schema) p === None || protected p}) @ total = fun p ->
      parameters_protected heap heads protected agrees schema p () in
    boundaries_after heap heads after protected agrees owned final_closed final_trees rho values
      (Names.template_names schema) (fun p -> selected p) schema ())

let rec (avoids_occurrence @ total) : (names : A.names) @ immutable ->
    (p : Ty.node Pref.t) @ immutable -> (ty : D.mono) @ immutable ->
    {u : unit | A.avoids names ty && not (A.position names p === None)} ->
    {u : unit | not (G.occurs p ty)} @ ghost =
  fun names p ty premise -> ghost_ (
    A.avoids_def names ty; G.occurs_def p ty;
    match ty with
    | D.Free q -> let same = Pref.equal p q in if same then () else ()
    | D.List_type element -> avoids_occurrence names p element ()
    | D.Function (argument, result) ->
      avoids_occurrence names p argument (); avoids_occurrence names p result ()
    | D.Boolean | D.Word64 | D.Parameter _ -> ())

let rec (template_body_free @ total) : (quantifiers : A.names) @ immutable ->
    (excluded : A.names) @ immutable -> (p : Ty.node Pref.t) @ immutable ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (schema : Ty.template) @ immutable ->
    {u : unit | P.parameters_in quantifiers schema && P.boundaries_avoid excluded rho schema
      && not (A.position excluded p === None)} ->
    {u : unit | not (G.occurs p (P.body quantifiers rho schema))} @ ghost =
  fun quantifiers excluded p rho schema premise -> ghost_ (
    P.parameters_in_def quantifiers schema; P.boundaries_avoid_def excluded rho schema;
    P.body_def quantifiers rho schema; G.occurs_def p (P.body quantifiers rho schema);
    match schema with
    | Ty.Boundary q -> avoids_occurrence excluded p (D.embed (rho q)) ()
    | Ty.Parameter q ->
      A.abstract_free_def quantifiers D.Z q;
      (match A.position quantifiers q with None -> () | Some index ->
        G.occurs_def p (D.Parameter (D.add D.Z index)))
    | Ty.Constant _ | Ty.Word_constant _ -> ()
    | Ty.Indirect (_, child) | Ty.List_template (_, child) ->
      template_body_free quantifiers excluded p rho child ()
    | Ty.Product (_, left, right) ->
      template_body_free quantifiers excluded p rho left ();
      template_body_free quantifiers excluded p rho right ())

let rec (context_after @ total) : (heap : Ty.node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (after : Ty.node Pref.heap) @ immutable ->
    (protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
    (agrees : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | protected p = T.generic heap heads p})) @ total ->
    (owned : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | not (H.mem heap p) || H.mem after p})) @ total ->
    (closed : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | closed_at after protected p})) @ total ->
    (trees : ((p : Ty.node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === p &&
        (if H.mem after p then F.finite after t else U.observe after p === None)} @ immutable)) @ total ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (values : ((p : Ty.node Pref.t) @ immutable -> {u : unit | rho p === F.readback (trees p)})) @ total ->
    (depth : int) -> (env : Env.env) @ immutable -> (schemas : Env.templates) @ immutable ->
    (p : Ty.node Pref.t) @ immutable ->
    {u : unit | Hm_effective_environment.effective_env heap heads depth env schemas && protected p} ->
    {u : unit | not (G.in_context p (P.context rho schemas))} @ ghost =
  fun heap heads after protected agrees owned closed trees rho values depth env schemas p premise -> ghost_ (
    Hm_effective_environment.effective_env_def heap heads depth env schemas;
    P.context_def rho schemas; G.in_context_def p (P.context rho schemas);
    match env, schemas with
    | Env.Bind (_, tail), Env.Template_binding (schema, rest) ->
      let excluded = A.Name (p, A.No_names) in
      let selected : ((q : Ty.node Pref.t) @ immutable ->
        {u : unit | A.position excluded q === None || protected q}) @ total = fun q ->
        A.position_def excluded q; A.position_def A.No_names q in
      boundaries_after heap heads after protected agrees owned closed trees rho values excluded selected schema ();
      A.position_def excluded p;
      let quantifiers = Names.template_names schema in
      P.parameters_subset schema quantifiers (fun _q -> ());
      template_body_free quantifiers excluded p rho schema ();
      P.scheme_def rho schema;
      context_after heap heads after protected agrees owned closed trees rho values depth tail rest p ()
    | _ -> ())

module Scope = Hm_elaboration

let (scope_before @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (depth : int) -> (pool : Pool.pool) @ immutable -> (env : Env.env) @ immutable ->
    (execution : Run.execution) @ immutable -> (after : Ty.node Pref.heap) @ immutable ->
    (final_pool : Pool.pool) @ immutable -> (scope : Scope.scope) @ immutable ->
    (fresh : ((p : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
      {u : unit | generic_at after p r} -> {u : unit | Scope.parameter p scope === None})) @ total ->
    (p : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
    {u : unit | Run.ran heap depth pool env execution after final_pool && generic_at heap p r} ->
    {u : unit | Scope.parameter p scope === None} @ ghost =
  fun heap depth pool env execution after final_pool scope fresh p r premise -> ghost_ (
    let next = generic_run heap depth pool env execution after final_pool p r () in
    fresh p next ())

let (generalized_scope_at_result @ total) : (heap : Ty.node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (safe : ((p : Ty.node Pref.t) @ immutable -> {u : unit | Hm_effective_runtime.safe heap heads p})) @ total ->
    (after : Ty.node Pref.heap) @ immutable ->
    (owned : ((p : Ty.node Pref.t) @ immutable -> {u : unit | not (H.mem heap p) || H.mem after p})) @ total ->
    (future : ((protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
      (closed : ((p : Ty.node Pref.t) @ immutable -> {u : unit | closed_at heap protected p})) @ total ->
      (generic : ((p : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected p) || generic_at heap p r} @ immutable)) @ total ->
      (p : Ty.node Pref.t) @ immutable -> {u : unit | closed_at after protected p})) @ total ->
    (trees : ((p : Ty.node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === p &&
        (if H.mem after p then F.finite after t else U.observe after p === None)} @ immutable)) @ total ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (values : ((p : Ty.node Pref.t) @ immutable -> {u : unit | rho p === F.readback (trees p)})) @ total ->
    (scope : Scope.scope) @ immutable -> (context : D.context) @ immutable ->
    (root : Ty.node Pref.t) @ immutable -> (finite : R.representative) @ immutable ->
    (p : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
    {u : unit | Result.finite_path heap root finite && generic_at heap p r
      && Scope.parameter p scope === None} ->
    {u : unit | Scope.parameter p (Scope.Quantifiers
      ((G.generalize context (Scope.interpret scope (rho root))).G.variables, scope)) === None} @ ghost =
  fun heap heads safe after owned future trees rho values scope context root finite p r premise -> ghost_ (
    let[@def] protected : Ty.node Pref.t @ immutable total -> bool = fun q -> T.generic heap heads q in
    let agrees : ((q : Ty.node Pref.t) @ immutable -> {u : unit | protected q = T.generic heap heads q}) @ total =
      fun q -> protected_def q in
    let valid : ((q : Ty.node Pref.t) @ immutable -> {u : unit | E.valid_head heap heads q}) @ total = fun q ->
      safe q; Hm_effective_runtime.safe_def heap heads q in
    let closed : ((q : Ty.node Pref.t) @ immutable -> {u : unit | closed_at heap protected q}) @ total = fun q ->
      safe q; Hm_effective_runtime.safe_def heap heads q;
      initial heap heads valid protected agrees q () in
    let generic : ((q : Ty.node Pref.t) @ immutable ->
      {r : R.representative | not (protected q) || generic_at heap q r} @ immutable) @ total = fun q ->
      let r = heads q in
      protected_def q; T.generic_def heap heads q;
      valid q; E.valid_head_def heap heads q; E.level_def heap heads q;
      generic_at_def heap q r; r in
    let final_closed : ((q : Ty.node Pref.t) @ immutable -> {u : unit | closed_at after protected q}) @ total = fun q ->
      future protected closed generic q in
    finite_unprotected heap protected generic root finite ();
    Result.finite_path_def heap root finite; U.resolves_def heap root finite.R.root finite.R.path;
    owned root; values root;
    generic_at_def heap p r; U.resolves_def heap p r.R.root r.R.path;
    valid p; E.valid_head_def heap heads p; E.level_def heap heads p;
    let current = heads p in
    R.unique heap p r.R.root r.R.path current.R.root current.R.path ();
    protected_def p; T.generic_def heap heads p;
    readback_avoids after protected final_closed (trees root) p ();
    Hm_elaboration_instance_scope.interpret_free p scope (rho root);
    Hm_elaboration_instance_scope.generalized_scope_unbound p scope context (rho root) ())
