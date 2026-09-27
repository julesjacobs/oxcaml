(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml vox_big_credits.mli vox_big_credits.ml vox_ackermann.ml vox_union_find_potential.ml vox_union_find_levels.ml vox_union_find_path_cost.ml vox_union_find_model.ml vox_union_find_forest.ml vox_union_find_rank.ml vox_union_find_mass.ml vox_union_find_link.ml vox_union_find_worker.ml vox_union_find_amortized.ml vox_union_find_bank.ml vox_union_find_spec.ml vox_union_find_events.mli vox_union_find_events.ml vox_union_find.mli vox_union_find.ml vox_union_find_online.mli vox_union_find_online.ml vox_connectivity.mli vox_connectivity.ml vox_union_find_online_cost.ml";
 { bytecode; }
*)

module C = Vox_big_credits.Make ()
module U = Vox_connectivity.Make (C)
module K = Vox_ackermann
module Q = Vox_union_find_online_cost
module E = Vox_union_find_events

let (charged_work @ total)
    (state : U.t @ local immutable total ghost forkable unyielding) :
    {u : unit | Vox_union_find_events.total (U.events state) <= U.account state} @ ghost =
  ghost_ (U.event_cost state; U.account_bounds state; ())

type funded = #{ state : U.t; wallet : C.token @@ ghost total }
type result = #{ value : U.elem @@ aliased; owned : funded }
let[@def] budget (s : funded @ local immutable total ghost forkable unyielding) =
  ghost_ (Bigint.add (U.account s.#state) (C.credits s.#wallet))
let[@def] available (s : funded @ local immutable total ghost forkable unyielding) =
  ghost_ (C.credits s.#wallet)

let (fee_bounds @ total) :
    (state : U.t) @ local immutable total ghost forkable unyielding ->
    {u : unit | if U.size state <= 8Z then
      U.find_fee state <= 44Z && U.union_fee state <= 132Z else true} @ ghost =
    fun state -> ghost_ (
  let population = 8Z in
  let alpha = K.inverse population in
  U.fee_bounds (borrow_ state) population alpha;
  ())

let create : (fee : {b : C.token | C.credits b >= 1Z}) @ unique total ghost ->
    {s : funded | let fee = fee in U.size s.#state = 0Z && budget s = C.credits fee &&
      available s = Bigint.sub (C.credits fee) 1Z && U.ticks s.#state = 1Z} @ unique =
    fun fee ->
  let fee = fee in
  let split = C.split 1Z fee in
  let state = U.create split.C.left in
  ghost_ (U.event_cost (borrow_ state);
    E.total_def [E.Initialize]; E.total_def []; E.weight_def E.Initialize);
  let owned = #{state; wallet = split.C.right} in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  owned

let allocate :
    (owned : {s : funded | U.size s.#state < Bigint.of_int max_int && available s >= 11Z})
      @ unique read_write total ->
    {r : result | let owned = owned in U.size r.#owned.#state = Bigint.add (U.size owned.#state) 1Z && U.contains (U.snapshot r.#owned.#state) r.#value &&
      U.added (U.snapshot owned.#state) (U.snapshot r.#owned.#state) r.#value &&
      budget r.#owned = budget owned &&
      available r.#owned >= Bigint.sub (available owned) 11Z &&
      U.ticks r.#owned.#state = Bigint.add (U.ticks owned.#state) 3Z} @ unique =
    fun owned ->
  let owned = owned in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned);
    U.observations (borrow_ owned.#state); fee_bounds (borrow_ owned.#state);
    U.event_cost (borrow_ owned.#state));
  let history = ghost_ (U.events (borrow_ owned.#state)) in
  let amount = ghost_ (11Z) in
  let wallet = owned.#wallet in let state = owned.#state in
  let split = C.split amount wallet in
  let r = U.make_set state split.C.left in
  let #{U.value; state} = r in
  ghost_ (U.event_cost (borrow_ state);
    E.total_def (E.Allocate :: history); E.weight_def E.Allocate);
  let owned = #{state; wallet = split.C.right} in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let r = #{value; owned} in r

let join : (x : U.elem) @ immutable -> (y : U.elem) @ immutable ->
    (owned : {s : funded | U.size s.#state <= 8Z && U.contains (U.snapshot s.#state) x && U.contains (U.snapshot s.#state) y && available s >= 132Z})
      @ unique read_write total ->
    {r : result | let owned = owned in U.size r.#owned.#state = U.size owned.#state &&
      U.joined (U.snapshot owned.#state) (U.snapshot r.#owned.#state) x y r.#value &&
      budget r.#owned = budget owned &&
      available r.#owned >= Bigint.sub (available owned) 132Z &&
      (let p = U.snapshot owned.#state in
        U.ticks r.#owned.#state = Bigint.add (U.ticks owned.#state)
          (Bigint.add 12Z (Bigint.mul 4Z
            (Bigint.add (U.depth p x) (U.depth (U.compressed p x) y)))))} @ unique =
    fun x y owned ->
  let owned = owned in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned);
    U.observations (borrow_ owned.#state); fee_bounds (borrow_ owned.#state);
    U.event_cost (borrow_ owned.#state));
  let history = ghost_ (U.events (borrow_ owned.#state)) in
  let p = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let amount = ghost_ (U.union_fee (borrow_ owned.#state)) in
  let wallet = owned.#wallet in let state = owned.#state in
  let split = C.split amount wallet in
  let r = U.union x y state split.C.left in
  let #{U.value; state} = r in
  ghost_ (U.event_cost (borrow_ state);
    let first = E.Find (U.depth p x) in
    let second = E.Find (U.depth (U.compressed p x) y) in
    E.total_def (E.Union :: E.Link :: second :: first :: history);
    E.total_def (E.Link :: second :: first :: history);
    E.total_def (second :: first :: history);
    E.total_def (first :: history);
    E.weight_def E.Union; E.weight_def E.Link;
    E.weight_def second; E.weight_def first);
  let owned = #{state; wallet = split.C.right} in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let r = #{value; owned} in r

let find : (x : U.elem) @ immutable ->
    (owned : {s : funded | U.size s.#state <= 8Z && U.contains (U.snapshot s.#state) x && available s >= 44Z})
      @ unique read_write total ->
    {r : result | let owned = owned in U.size r.#owned.#state = U.size owned.#state && r.#value === U.root (U.snapshot owned.#state) x &&
      U.found (U.snapshot owned.#state) (U.snapshot r.#owned.#state) x &&
      budget r.#owned = budget owned &&
      available r.#owned >= Bigint.sub (available owned) 44Z &&
      U.ticks r.#owned.#state = Bigint.add (U.ticks owned.#state)
        (Bigint.add 2Z (Bigint.mul 4Z (U.depth (U.snapshot owned.#state) x)))}
      @ unique =
    fun x owned ->
  let owned = owned in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned);
    U.observations (borrow_ owned.#state); fee_bounds (borrow_ owned.#state);
    U.event_cost (borrow_ owned.#state));
  let history = ghost_ (U.events (borrow_ owned.#state)) in
  let p = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let amount = ghost_ (U.find_fee (borrow_ owned.#state)) in
  let wallet = owned.#wallet in let state = owned.#state in
  let split = C.split amount wallet in
  let r = U.find x state split.C.left in
  let #{U.value; state} = r in
  ghost_ (U.event_cost (borrow_ state);
    E.total_def (E.Find (U.depth p x) :: history); E.weight_def (E.Find (U.depth p x)));
  let owned = #{state; wallet = split.C.right} in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let r = #{value; owned} in r

let initially_empty (x : U.elem @ immutable) =
  let amount = ghost_ 1Z in
  let fee = C.Budget.create amount in
  let state = U.create fee in
  ghost_ (U.empty_law (borrow_ state) x);
  let proof : {u : unit | not (U.contains (U.snapshot state) x)} = () in
  let _ = proof in ()

let run () =
  if max_int >= 8 then (
  let initial = ghost_ 1000Z in
  let wallet = C.Budget.create initial in
  let owned = create wallet in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let r = allocate owned in
  let #{value = x0; owned} = r in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let account1 = ghost_ (U.account (borrow_ owned.#state)) in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.added_law before after x0 x0;
    ());
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let r = allocate owned in
  let #{value = x1; owned} = r in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let account2 = ghost_ (U.account (borrow_ owned.#state)) in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.added_law before after x1 x0;
    U.added_law before after x1 x1;
    ());
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let r = allocate owned in
  let #{value = x2; owned} = r in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let account3 = ghost_ (U.account (borrow_ owned.#state)) in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.added_law before after x2 x0;
    U.added_law before after x2 x1;
    U.added_law before after x2 x2;
    ());
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let r = allocate owned in
  let #{value = x3; owned} = r in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let account4 = ghost_ (U.account (borrow_ owned.#state)) in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.added_law before after x3 x0;
    U.added_law before after x3 x1;
    U.added_law before after x3 x2;
    U.added_law before after x3 x3;
    ());
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let r = allocate owned in
  let #{value = x4; owned} = r in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let account5 = ghost_ (U.account (borrow_ owned.#state)) in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.added_law before after x4 x0;
    U.added_law before after x4 x1;
    U.added_law before after x4 x2;
    U.added_law before after x4 x3;
    U.added_law before after x4 x4;
    ());
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let work6 = ghost_ (Bigint.add (U.depth before x0)
    (U.depth (U.compressed before x0) x1)) in
  ghost_ (U.depth_law before x0; U.depth_law (U.compressed before x0) x1);
  let r = join x0 x1 owned in
  let #{value = merged; owned} = r in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let account6 = ghost_ (U.account (borrow_ owned.#state)) in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.joined_law before after x0 x1 merged x0;
    U.joined_law before after x0 x1 merged x1;
    U.joined_law before after x0 x1 merged x2;
    U.joined_law before after x0 x1 merged x3;
    U.joined_law before after x0 x1 merged x4;
    ());
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let work7 = ghost_ (Bigint.add (U.depth before x2)
    (U.depth (U.compressed before x2) x3)) in
  ghost_ (U.depth_law before x2; U.depth_law (U.compressed before x2) x3);
  let r = join x2 x3 owned in
  let #{value = merged; owned} = r in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let account7 = ghost_ (U.account (borrow_ owned.#state)) in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.joined_law before after x2 x3 merged x0;
    U.joined_law before after x2 x3 merged x1;
    U.joined_law before after x2 x3 merged x2;
    U.joined_law before after x2 x3 merged x3;
    U.joined_law before after x2 x3 merged x4;
    ());
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let work8 = ghost_ (Bigint.add (U.depth before x1)
    (U.depth (U.compressed before x1) x2)) in
  ghost_ (U.depth_law before x1; U.depth_law (U.compressed before x1) x2);
  let r = join x1 x2 owned in
  let #{value = merged; owned} = r in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let account8 = ghost_ (U.account (borrow_ owned.#state)) in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.joined_law before after x1 x2 merged x0;
    U.joined_law before after x1 x2 merged x1;
    U.joined_law before after x1 x2 merged x2;
    U.joined_law before after x1 x2 merged x3;
    U.joined_law before after x1 x2 merged x4;
    ());
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let work9 = ghost_ (Bigint.add (U.depth before x0)
    (U.depth (U.compressed before x0) x3)) in
  ghost_ (U.depth_law before x0; U.depth_law (U.compressed before x0) x3);
  let r = join x0 x3 owned in
  let #{value = merged; owned} = r in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let account9 = ghost_ (U.account (borrow_ owned.#state)) in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.joined_law before after x0 x3 merged x0;
    U.joined_law before after x0 x3 merged x1;
    U.joined_law before after x0 x3 merged x2;
    U.joined_law before after x0 x3 merged x3;
    U.joined_law before after x0 x3 merged x4;
    ());
  ghost_ (U.connected_def (U.snapshot (borrow_ owned.#state)) x0 x3;
    U.connected_def (U.snapshot (borrow_ owned.#state)) x0 x4);
  let proof : {u : unit | U.connected (U.snapshot owned.#state) x0 x3 &&
    not (U.connected (U.snapshot owned.#state) x0 x4)} = () in
  let _ = proof in
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let work10 = ghost_ (U.depth before x0) in
  ghost_ (U.depth_law before x0);
  let r = find x0 owned in
  let #{value = root0; owned} = r in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let account10 = ghost_ (U.account (borrow_ owned.#state)) in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.found_law before after x0 x0;
    U.found_law before after x0 x3;
    U.found_law before after x0 x4;
    ());
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let work11 = ghost_ (U.depth before x3) in
  ghost_ (U.depth_law before x3);
  let r = find x3 owned in
  let #{value = root3; owned} = r in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let account11 = ghost_ (U.account (borrow_ owned.#state)) in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.found_law before after x3 x0;
    U.found_law before after x3 x3;
    U.found_law before after x3 x4;
    ());
  (* A returned representative is a member and its own root, so a find of it
     follows no links. *)
  ghost_ (U.root_law (borrow_ owned.#state) x0;
    U.root_law (borrow_ owned.#state) root0);
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let ticks11 = ghost_ (U.ticks (borrow_ owned.#state)) in
  let r = find root0 owned in
  let #{value = again; owned} = r in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let account12 = ghost_ (U.account (borrow_ owned.#state)) in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.found_law before after root0 x0;
    U.found_law before after root0 x3;
    U.found_law before after root0 x4;
    ());
  let proof : {u : unit | again === root0 &&
    U.ticks owned.#state = Bigint.add ticks11 2Z} = () in
  let _ = proof in
  (* The whole run: one creation, five allocations, four unions (each two
     finds, a link and its own step) and three finds cost 70 ticks plus four
     per parent link followed. *)
  let work = ghost_ (Bigint.add (Bigint.add (Bigint.add work6 work7)
    (Bigint.add work8 work9)) (Bigint.add work10 work11)) in
  let proof : {u : unit |
    U.ticks owned.#state = Bigint.add 70Z (Bigint.mul 4Z work) &&
    work >= 0Z} = () in
  let _ = proof in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned);
    C.nonnegative (borrow_ owned.#wallet); U.account_bounds (borrow_ owned.#state);
    U.connected_def (U.snapshot (borrow_ owned.#state)) x0 x3;
    U.connected_def (U.snapshot (borrow_ owned.#state)) x0 x4);
  let proof : {u : unit | root0 === root3 &&
    U.connected (U.snapshot owned.#state) x0 x3 &&
    not (U.connected (U.snapshot owned.#state) x0 x4) &&
    U.ticks owned.#state <= initial} = () in
  let _ = proof in
  ghost_ (
    let trace12 : Q.step list = [] in
    let trace11 = {Q.operation = Q.Find; account = account12} :: trace12 in
    let trace10 = {Q.operation = Q.Find; account = account11} :: trace11 in
    let trace9 = {Q.operation = Q.Find; account = account10} :: trace10 in
    let trace8 = {Q.operation = Q.Union; account = account9} :: trace9 in
    let trace7 = {Q.operation = Q.Union; account = account8} :: trace8 in
    let trace6 = {Q.operation = Q.Union; account = account7} :: trace7 in
    let trace5 = {Q.operation = Q.Union; account = account6} :: trace6 in
    let trace4 = {Q.operation = Q.Allocate; account = account5} :: trace5 in
    let trace3 = {Q.operation = Q.Allocate; account = account4} :: trace4 in
    let trace2 = {Q.operation = Q.Allocate; account = account3} :: trace3 in
    let trace1 = {Q.operation = Q.Allocate; account = account2} :: trace2 in
    let trace0 = {Q.operation = Q.Allocate; account = account1} :: trace1 in
    Q.trace_def 8Z 1Z trace0;
    Q.final_account_def 1Z trace0;
    Q.count_def Q.Allocate trace0;
    Q.count_def Q.Find trace0;
    Q.count_def Q.Union trace0;
    Q.trace_def 8Z account1 trace1;
    Q.final_account_def account1 trace1;
    Q.count_def Q.Allocate trace1;
    Q.count_def Q.Find trace1;
    Q.count_def Q.Union trace1;
    Q.trace_def 8Z account2 trace2;
    Q.final_account_def account2 trace2;
    Q.count_def Q.Allocate trace2;
    Q.count_def Q.Find trace2;
    Q.count_def Q.Union trace2;
    Q.trace_def 8Z account3 trace3;
    Q.final_account_def account3 trace3;
    Q.count_def Q.Allocate trace3;
    Q.count_def Q.Find trace3;
    Q.count_def Q.Union trace3;
    Q.trace_def 8Z account4 trace4;
    Q.final_account_def account4 trace4;
    Q.count_def Q.Allocate trace4;
    Q.count_def Q.Find trace4;
    Q.count_def Q.Union trace4;
    Q.trace_def 8Z account5 trace5;
    Q.final_account_def account5 trace5;
    Q.count_def Q.Allocate trace5;
    Q.count_def Q.Find trace5;
    Q.count_def Q.Union trace5;
    Q.trace_def 8Z account6 trace6;
    Q.final_account_def account6 trace6;
    Q.count_def Q.Allocate trace6;
    Q.count_def Q.Find trace6;
    Q.count_def Q.Union trace6;
    Q.trace_def 8Z account7 trace7;
    Q.final_account_def account7 trace7;
    Q.count_def Q.Allocate trace7;
    Q.count_def Q.Find trace7;
    Q.count_def Q.Union trace7;
    Q.trace_def 8Z account8 trace8;
    Q.final_account_def account8 trace8;
    Q.count_def Q.Allocate trace8;
    Q.count_def Q.Find trace8;
    Q.count_def Q.Union trace8;
    Q.trace_def 8Z account9 trace9;
    Q.final_account_def account9 trace9;
    Q.count_def Q.Allocate trace9;
    Q.count_def Q.Find trace9;
    Q.count_def Q.Union trace9;
    Q.trace_def 8Z account10 trace10;
    Q.final_account_def account10 trace10;
    Q.count_def Q.Allocate trace10;
    Q.count_def Q.Find trace10;
    Q.count_def Q.Union trace10;
    Q.trace_def 8Z account11 trace11;
    Q.final_account_def account11 trace11;
    Q.count_def Q.Allocate trace11;
    Q.count_def Q.Find trace11;
    Q.count_def Q.Union trace11;
    Q.trace_def 8Z account12 trace12;
    Q.final_account_def account12 trace12;
    Q.count_def Q.Allocate trace12;
    Q.count_def Q.Find trace12;
    Q.count_def Q.Union trace12;
    Q.fee_def 8Z Q.Allocate; Q.fee_def 8Z Q.Find; Q.fee_def 8Z Q.Union;
    Q.find_fee_def 8Z; Q.union_fee_def 8Z;
    Q.same_def Q.Allocate Q.Allocate;
    Q.same_def Q.Allocate Q.Find;
    Q.same_def Q.Allocate Q.Union;
    Q.same_def Q.Find Q.Allocate;
    Q.same_def Q.Find Q.Find;
    Q.same_def Q.Find Q.Union;
    Q.same_def Q.Union Q.Allocate;
    Q.same_def Q.Union Q.Find;
    Q.same_def Q.Union Q.Union;
    let u = () in
    let _checked = (u : {u : unit | Q.trace 8Z 1Z trace0}) in
    Q.sequence 8Z trace0 (U.ticks (borrow_ owned.#state));
    let u = () in
    let _proof = (u : {u : unit |
      Bigint.add 70Z (Bigint.mul 4Z work) <= Q.budget 8Z 5Z 3Z 4Z}) in ());
  print_endline "connectivity and paid-prefix bound: ok")
let () = run ()
