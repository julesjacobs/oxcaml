(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml vox_big_credits.mli vox_big_credits.ml vox_ackermann.ml vox_union_find_potential.ml vox_union_find_levels.ml vox_union_find_path_cost.ml vox_union_find_model.ml vox_union_find_forest.ml vox_union_find_rank.ml vox_union_find_mass.ml vox_union_find_link.ml vox_union_find_worker.ml vox_union_find_amortized.ml vox_union_find_bank.ml vox_union_find_spec.ml vox_union_find.mli vox_union_find.ml vox_union_find_online.mli vox_union_find_online.ml vox_connectivity.mli vox_connectivity.ml vox_union_find_online_cost.ml connectivity.ml";
 { bytecode; }
 { native; }
 { flags += " -principal"; bytecode; }
 { flags += " -principal"; native; }
*)

module C = Vox_big_credits.Make ()
module U = Vox_connectivity.Make (C)
module K = Vox_ackermann
module Q = Vox_union_find_online_cost

type funded = #{ state : U.t; wallet : C.token @@ ghost total }
type result = #{ value : U.elem @@ aliased; owned : funded }
let[@def] budget (s : funded @ local immutable total ghost forkable unyielding) =
  ghost_ (Bigint.add (U.account s.#state) (C.credits s.#wallet))
let[@def] available (s : funded @ local immutable total ghost forkable unyielding) =
  ghost_ (C.credits s.#wallet)

let (fee_bounds @ total) :
    (state : U.t) @ local immutable total ghost forkable unyielding ->
    {u : unit | if U.valid state && U.size state <= 8Z then
      U.find_fee state <= 44Z && U.union_fee state <= 132Z else true} @ ghost =
    fun state -> ghost_ (
  let population = 8Z in
  let alpha = K.inverse population in
  U.fee_bounds (borrow_ state) population alpha;
  ())

let create : (fee : {b : C.token | C.credits b >= 1Z}) @ unique total ghost ->
    {s : funded | let fee = fee in U.valid s.#state &&
      U.size s.#state = 0Z && budget s = C.credits fee &&
      available s = Bigint.sub (C.credits fee) 1Z} @ unique = fun fee ->
  let fee = fee in
  let split = C.split 1Z fee in
  let state = U.create split.C.left in
  let owned = #{state; wallet = split.C.right} in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  owned

let allocate :
    (owned : {s : funded | U.valid s.#state && U.size s.#state < Bigint.of_int max_int && available s >= 11Z})
      @ unique read_write total ->
    {r : result | let owned = owned in U.valid r.#owned.#state &&
      U.size r.#owned.#state = Bigint.add (U.size owned.#state) 1Z && U.member r.#value r.#owned.#state &&
      U.added (U.snapshot owned.#state) (U.snapshot r.#owned.#state) r.#value &&
      budget r.#owned = budget owned &&
      available r.#owned >= Bigint.sub (available owned) 11Z} @ unique =
    fun owned ->
  let owned = owned in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned);
    U.observations (borrow_ owned.#state); fee_bounds (borrow_ owned.#state));
  let amount = ghost_ (11Z) in
  let wallet = owned.#wallet in let state = owned.#state in
  let split = C.split amount wallet in
  let r = U.make_set state split.C.left in
  let #{U.value; state} = r in
  let owned = #{state; wallet = split.C.right} in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let r = #{value; owned} in r

let join : (x : U.elem) @ immutable -> (y : U.elem) @ immutable ->
    (owned : {s : funded | U.valid s.#state && U.size s.#state <= 8Z && U.member x s.#state && U.member y s.#state && available s >= 132Z})
      @ unique read_write total ->
    {r : result | let owned = owned in U.valid r.#owned.#state &&
      U.size r.#owned.#state = U.size owned.#state &&
      U.joined (U.snapshot owned.#state) (U.snapshot r.#owned.#state) x y r.#value &&
      budget r.#owned = budget owned &&
      available r.#owned >= Bigint.sub (available owned) 132Z} @ unique =
    fun x y owned ->
  let owned = owned in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned);
    U.observations (borrow_ owned.#state); fee_bounds (borrow_ owned.#state));
  let amount = ghost_ (U.union_fee (borrow_ owned.#state)) in
  let wallet = owned.#wallet in let state = owned.#state in
  let split = C.split amount wallet in
  let r = U.union x y state split.C.left in
  let #{U.value; state} = r in
  let owned = #{state; wallet = split.C.right} in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let r = #{value; owned} in r

let find : (x : U.elem) @ immutable ->
    (owned : {s : funded | U.valid s.#state && U.size s.#state <= 8Z && U.member x s.#state && available s >= 44Z})
      @ unique read_write total ->
    {r : result | let owned = owned in U.valid r.#owned.#state &&
      U.size r.#owned.#state = U.size owned.#state && r.#value === U.representative x owned.#state &&
      U.found (U.snapshot owned.#state) (U.snapshot r.#owned.#state) x &&
      budget r.#owned = budget owned &&
      available r.#owned >= Bigint.sub (available owned) 44Z} @ unique =
    fun x owned ->
  let owned = owned in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned);
    U.observations (borrow_ owned.#state); fee_bounds (borrow_ owned.#state));
  let amount = ghost_ (U.find_fee (borrow_ owned.#state)) in
  let wallet = owned.#wallet in let state = owned.#state in
  let split = C.split amount wallet in
  let r = U.find x state split.C.left in
  let #{U.value; state} = r in
  let owned = #{state; wallet = split.C.right} in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let r = #{value; owned} in r

let initially_empty (x : U.elem @ immutable) =
  let amount = ghost_ 1Z in
  let fee = C.Budget.create amount in
  let state = U.create fee in
  ghost_ (U.empty_law (borrow_ state) x);
  let proof : {u : unit | not (U.contains (U.snapshot state) x)} = () in
  let proof = proof in ()

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
    U.added_law before after x0 x0; U.observe x0 (borrow_ owned.#state);
    ());
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let r = allocate owned in
  let #{value = x1; owned} = r in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let account2 = ghost_ (U.account (borrow_ owned.#state)) in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.added_law before after x1 x0; U.observe x0 (borrow_ owned.#state);
    U.added_law before after x1 x1; U.observe x1 (borrow_ owned.#state);
    ());
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let r = allocate owned in
  let #{value = x2; owned} = r in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let account3 = ghost_ (U.account (borrow_ owned.#state)) in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.added_law before after x2 x0; U.observe x0 (borrow_ owned.#state);
    U.added_law before after x2 x1; U.observe x1 (borrow_ owned.#state);
    U.added_law before after x2 x2; U.observe x2 (borrow_ owned.#state);
    ());
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let r = allocate owned in
  let #{value = x3; owned} = r in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let account4 = ghost_ (U.account (borrow_ owned.#state)) in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.added_law before after x3 x0; U.observe x0 (borrow_ owned.#state);
    U.added_law before after x3 x1; U.observe x1 (borrow_ owned.#state);
    U.added_law before after x3 x2; U.observe x2 (borrow_ owned.#state);
    U.added_law before after x3 x3; U.observe x3 (borrow_ owned.#state);
    ());
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let r = allocate owned in
  let #{value = x4; owned} = r in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let account5 = ghost_ (U.account (borrow_ owned.#state)) in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.added_law before after x4 x0; U.observe x0 (borrow_ owned.#state);
    U.added_law before after x4 x1; U.observe x1 (borrow_ owned.#state);
    U.added_law before after x4 x2; U.observe x2 (borrow_ owned.#state);
    U.added_law before after x4 x3; U.observe x3 (borrow_ owned.#state);
    U.added_law before after x4 x4; U.observe x4 (borrow_ owned.#state);
    ());
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let r = join x0 x1 owned in
  let #{value = merged; owned} = r in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let account6 = ghost_ (U.account (borrow_ owned.#state)) in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.joined_law before after x0 x1 merged x0; U.observe x0 (borrow_ owned.#state);
    U.joined_law before after x0 x1 merged x1; U.observe x1 (borrow_ owned.#state);
    U.joined_law before after x0 x1 merged x2; U.observe x2 (borrow_ owned.#state);
    U.joined_law before after x0 x1 merged x3; U.observe x3 (borrow_ owned.#state);
    U.joined_law before after x0 x1 merged x4; U.observe x4 (borrow_ owned.#state);
    ());
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let r = join x2 x3 owned in
  let #{value = merged; owned} = r in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let account7 = ghost_ (U.account (borrow_ owned.#state)) in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.joined_law before after x2 x3 merged x0; U.observe x0 (borrow_ owned.#state);
    U.joined_law before after x2 x3 merged x1; U.observe x1 (borrow_ owned.#state);
    U.joined_law before after x2 x3 merged x2; U.observe x2 (borrow_ owned.#state);
    U.joined_law before after x2 x3 merged x3; U.observe x3 (borrow_ owned.#state);
    U.joined_law before after x2 x3 merged x4; U.observe x4 (borrow_ owned.#state);
    ());
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let r = join x1 x2 owned in
  let #{value = merged; owned} = r in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let account8 = ghost_ (U.account (borrow_ owned.#state)) in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.joined_law before after x1 x2 merged x0; U.observe x0 (borrow_ owned.#state);
    U.joined_law before after x1 x2 merged x1; U.observe x1 (borrow_ owned.#state);
    U.joined_law before after x1 x2 merged x2; U.observe x2 (borrow_ owned.#state);
    U.joined_law before after x1 x2 merged x3; U.observe x3 (borrow_ owned.#state);
    U.joined_law before after x1 x2 merged x4; U.observe x4 (borrow_ owned.#state);
    ());
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let r = join x0 x3 owned in
  let #{value = merged; owned} = r in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let account9 = ghost_ (U.account (borrow_ owned.#state)) in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.joined_law before after x0 x3 merged x0; U.observe x0 (borrow_ owned.#state);
    U.joined_law before after x0 x3 merged x1; U.observe x1 (borrow_ owned.#state);
    U.joined_law before after x0 x3 merged x2; U.observe x2 (borrow_ owned.#state);
    U.joined_law before after x0 x3 merged x3; U.observe x3 (borrow_ owned.#state);
    U.joined_law before after x0 x3 merged x4; U.observe x4 (borrow_ owned.#state);
    ());
  ghost_ (U.connected_def (U.snapshot (borrow_ owned.#state)) x0 x3;
    U.connected_def (U.snapshot (borrow_ owned.#state)) x0 x4);
  let proof : {u : unit | U.connected (U.snapshot owned.#state) x0 x3 &&
    not (U.connected (U.snapshot owned.#state) x0 x4)} = () in
  let proof = proof in
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let r = find x0 owned in
  let #{value = root0; owned} = r in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let account10 = ghost_ (U.account (borrow_ owned.#state)) in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.found_law before after x0 x0; U.observe x0 (borrow_ owned.#state);
    U.found_law before after x0 x3; U.observe x3 (borrow_ owned.#state);
    U.found_law before after x0 x4; U.observe x4 (borrow_ owned.#state);
    ());
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let r = find x3 owned in
  let #{value = root3; owned} = r in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  let account11 = ghost_ (U.account (borrow_ owned.#state)) in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.found_law before after x3 x0; U.observe x0 (borrow_ owned.#state);
    U.found_law before after x3 x3; U.observe x3 (borrow_ owned.#state);
    U.found_law before after x3 x4; U.observe x4 (borrow_ owned.#state);
    ());
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned);
    C.nonnegative (borrow_ owned.#wallet); U.account_bounds (borrow_ owned.#state);
    U.connected_def (U.snapshot (borrow_ owned.#state)) x0 x3;
    U.connected_def (U.snapshot (borrow_ owned.#state)) x0 x4);
  let proof : {u : unit | root0 === root3 &&
    U.connected (U.snapshot owned.#state) x0 x3 &&
    not (U.connected (U.snapshot owned.#state) x0 x4) &&
    U.ticks owned.#state <= initial} = () in
  let proof = proof in
  ghost_ (
    let trace11 : Q.step list = [] in
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
    let checked = (u : {u : unit | Q.trace 8Z 1Z trace0}) in
    Q.sequence 8Z trace0 (U.ticks (borrow_ owned.#state));
    let u = () in
    let proof = (u : {u : unit |
      U.ticks owned.#state <= Q.budget 8Z 5Z 2Z 4Z}) in ());
  print_endline "connectivity and paid-prefix bound: ok")
let () = run ()
