(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml vox_big_credits.mli vox_big_credits.ml vox_ackermann.ml vox_union_find_potential.ml vox_union_find_levels.ml vox_union_find_path_cost.ml vox_union_find_model.ml vox_union_find_forest.ml vox_union_find_rank.ml vox_union_find_mass.ml vox_union_find_link.ml vox_union_find_worker.ml vox_union_find_amortized.ml vox_union_find_bank.ml vox_union_find_spec.ml vox_union_find.mli vox_union_find.ml vox_union_find_online.mli vox_union_find_online.ml vox_connectivity.mli vox_connectivity.ml connectivity.ml";
 { bytecode; }
 { native; }
 { flags += " -principal"; bytecode; }
 { flags += " -principal"; native; }
*)

module C = Vox_big_credits.Make ()
module U = Vox_connectivity.Make (C)
module K = Vox_ackermann

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
  let u = () in u)

let create : (fee : {b : C.token | C.credits b >= 1Z}) @ unique total ghost ->
    {s : funded | U.valid s.#state &&
      U.size s.#state = 0Z && budget s = C.credits fee &&
      available s = Bigint.sub (C.credits fee) 1Z} @ unique = fun fee ->
  let split = C.split 1Z fee in
  let state = U.create split.C.left in
  let owned = #{state; wallet = split.C.right} in
  ghost_ (budget_def (borrow_ owned); available_def (borrow_ owned));
  owned

let allocate :
    (owned : {s : funded | U.valid s.#state && U.size s.#state < Bigint.of_int max_int && available s >= 11Z})
      @ unique read_write total ->
    {r : result | U.valid r.#owned.#state &&
      U.size r.#owned.#state = Bigint.add (U.size owned.#state) 1Z && U.member r.#value r.#owned.#state &&
      U.added (U.snapshot owned.#state) (U.snapshot r.#owned.#state) r.#value &&
      budget r.#owned = budget owned &&
      available r.#owned >= Bigint.sub (available owned) 11Z} @ unique =
    fun owned ->
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
    {r : result | U.valid r.#owned.#state &&
      U.size r.#owned.#state = U.size owned.#state &&
      U.joined (U.snapshot owned.#state) (U.snapshot r.#owned.#state) x y r.#value &&
      budget r.#owned = budget owned &&
      available r.#owned >= Bigint.sub (available owned) 132Z} @ unique =
    fun x y owned ->
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
    {r : result | U.valid r.#owned.#state &&
      U.size r.#owned.#state = U.size owned.#state && r.#value === U.representative x owned.#state &&
      U.found (U.snapshot owned.#state) (U.snapshot r.#owned.#state) x &&
      budget r.#owned = budget owned &&
      available r.#owned >= Bigint.sub (available owned) 44Z} @ unique =
    fun x owned ->
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

let run () =
  if max_int >= 8 then (
  let initial = ghost_ 1000Z in
  let wallet = C.Budget.create initial in
  let owned = create wallet in
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let r = allocate owned in
  let #{value = x0; owned} = r in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.added_law before after x0 x0; U.observe x0 (borrow_ owned.#state);
    ());
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let r = allocate owned in
  let #{value = x1; owned} = r in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.added_law before after x1 x0; U.observe x0 (borrow_ owned.#state);
    U.added_law before after x1 x1; U.observe x1 (borrow_ owned.#state);
    ());
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let r = allocate owned in
  let #{value = x2; owned} = r in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.added_law before after x2 x0; U.observe x0 (borrow_ owned.#state);
    U.added_law before after x2 x1; U.observe x1 (borrow_ owned.#state);
    U.added_law before after x2 x2; U.observe x2 (borrow_ owned.#state);
    ());
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let r = allocate owned in
  let #{value = x3; owned} = r in
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
  let _proof : {u : unit | U.connected (U.snapshot owned.#state) x0 x3 &&
    not (U.connected (U.snapshot owned.#state) x0 x4)} = () in
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let r = find x0 owned in
  let #{value = root0; owned} = r in
  let after = ghost_ (U.snapshot (borrow_ owned.#state)) in
  ghost_ (
    U.found_law before after x0 x0; U.observe x0 (borrow_ owned.#state);
    U.found_law before after x0 x3; U.observe x3 (borrow_ owned.#state);
    U.found_law before after x0 x4; U.observe x4 (borrow_ owned.#state);
    ());
  let before = ghost_ (U.snapshot (borrow_ owned.#state)) in
  let r = find x3 owned in
  let #{value = root3; owned} = r in
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
  let _proof : {u : unit | root0 === root3 &&
    U.connected (U.snapshot owned.#state) x0 x3 &&
    not (U.connected (U.snapshot owned.#state) x0 x4) &&
    U.ticks owned.#state <= initial} = () in
  print_endline "connectivity and paid-prefix bound: ok")
let () = run ()
