(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml vox_big_credits.mli vox_big_credits.ml vox_ackermann.ml vox_union_find_potential.ml vox_union_find_levels.ml vox_union_find_path_cost.ml vox_union_find_model.ml vox_union_find_forest.ml vox_union_find_rank.ml vox_union_find_mass.ml vox_union_find_link.ml vox_union_find_worker.ml vox_union_find_amortized.ml vox_union_find_bank.ml vox_union_find_spec.ml vox_union_find_events.mli vox_union_find_events.ml vox_union_find.mli vox_union_find.ml vox_union_find_complexity.ml union_find.ml";
 { native; }
*)

module C = Vox_big_credits.Make ()
module W = Vox_union_find_worker.Make (C)
module P = Ghost_pref
module H = P.Heap
module M = Vox_union_find_model

let () =
  let memory = P.empty () in
  let r = P.alloc (M.Root 3) memory in
  let root = r.P.value in
  let p = P.alloc (M.Link (2, root)) r.P.state in
  let parent = p.P.value in
  let x = P.alloc (M.Link (1, parent)) p.P.state in
  let start = x.P.value in
  let f = P.alloc (M.Root 0) x.P.state in
  let frame = f.P.value in
  let path = ghost_ (M.Step (start, M.Step (parent, M.Stop root))) in
  let before = ghost_ (P.own (borrow_ f.P.state)) in
  let amount = 10Z in
  let initial : {n : Bigint.t | n >= 0Z} = amount in
  let bank = C.Budget.create initial in
  let state = { W.memory = f.P.state; bank } in
  ghost_ (
    M.valid_def before path; M.head_def path; M.depth_def path;
    let tail = M.Step (parent, M.Stop root) in
    M.valid_def before tail; M.head_def tail; M.depth_def tail;
    M.valid_def before (M.Stop root); M.head_def (M.Stop root);
    M.depth_def (M.Stop root);
    W.heap_def (borrow_ state); W.balance_def (borrow_ state);
    Vox_union_find_worker.cost_def 2Z);
  let depth = 2Z in
  let input : {s : W.resource | M.valid (W.heap s) path &&
    start === M.head path && depth = M.depth path &&
    W.balance s >= Vox_union_find_worker.cost depth} = state in
  let result = W.find depth path start input in
  let #{ W.value; state } = result in
  ghost_ (
    M.root_def path; M.root_def (M.Step (parent, M.Stop root));
    M.root_def (M.Stop root);
    let u = () in
    (u : {u : unit | value === root && W.balance state = 0Z}));
  assert (Pref.equal value root);
  ghost_ (
    W.heap_def (borrow_ state);
    M.rank_def before start; M.rank_def before parent;
    M.compressed_def before path;
    M.compressed_def before (M.Step (parent, M.Stop root));
    M.compressed_def before (M.Stop root);
    M.compressed_mem before path start;
    M.compressed_mem before path frame;
    let u = () in
    (u : {u : unit | H.mem (P.own state.memory) start &&
      H.mem (P.own state.memory) frame}));
  let memory : {t : Vox_union_find_model.node P.token | H.mem (P.own t) start &&
    H.mem (P.own t) frame} = state.memory in
  let actual = P.read start (borrow_ memory) in
  let unchanged = P.read frame (borrow_ memory) in
  (match actual with M.Root _ -> assert false
    | M.Link (_, p) -> assert (Pref.equal p root));
  (match unchanged with M.Root rank -> assert (rank = 0)
    | M.Link _ -> assert false)

module U = Vox_union_find.Make (C)
module F = Vox_union_find_forest
module A = Vox_union_find_amortized
module Q = Vox_union_find_complexity

let () =
  let capacity = 4Z in let amount = 2000Z in
  let initial : {n : Bigint.t | n >= 0Z} = amount in
  let fee = C.Budget.create initial in
  let cap : {n : Bigint.t | 1Z <= n && n <= Bigint.of_int max_int} = assume_ capacity in
  let payment : {b : C.token | C.credits b >= 1Z} = fee in
  let initialized = U.create cap payment in
  let #{U.state; refund} = initialized in
  let proof_alpha = ghost_ (U.alpha (borrow_ state)) in
  ghost_ (U.alpha_def (borrow_ state); U.alpha_bounds (borrow_ state));
  ghost_ (F.size_def []);
  let old_paths = ghost_ (U.contents (borrow_ state)) in
  ghost_ (U.contents_def (borrow_ state));
  let input : {s : U.t | U.valid s && F.size s.#paths < s.#capacity} = state in
  let payment : {b : C.token | C.credits b >= 3Z} = refund in
  let added = U.make_set input payment in
  let #{U.value = x0; state; refund} = added in
  let account0 = ghost_ (U.account (borrow_ state)) in
  ghost_ (U.contents_def (borrow_ state); M.head_def (M.Stop x0);
    F.size_def (M.Stop x0 :: old_paths);
    F.member_def x0 (M.Stop x0 :: old_paths); U.member_def x0 (borrow_ state);
    ());
  let old_paths = ghost_ (U.contents (borrow_ state)) in
  ghost_ (U.contents_def (borrow_ state));
  let input : {s : U.t | U.valid s && F.size s.#paths < s.#capacity} = state in
  let payment : {b : C.token | C.credits b >= 3Z} = refund in
  let added = U.make_set input payment in
  let #{U.value = x1; state; refund} = added in
  let account1 = ghost_ (U.account (borrow_ state)) in
  ghost_ (U.contents_def (borrow_ state); M.head_def (M.Stop x1);
    F.size_def (M.Stop x1 :: old_paths);
    F.member_def x0 (M.Stop x1 :: old_paths); U.member_def x0 (borrow_ state);
    F.member_def x1 (M.Stop x1 :: old_paths); U.member_def x1 (borrow_ state);
    ());
  let old_paths = ghost_ (U.contents (borrow_ state)) in
  ghost_ (U.contents_def (borrow_ state));
  let input : {s : U.t | U.valid s && F.size s.#paths < s.#capacity} = state in
  let payment : {b : C.token | C.credits b >= 3Z} = refund in
  let added = U.make_set input payment in
  let #{U.value = x2; state; refund} = added in
  let account2 = ghost_ (U.account (borrow_ state)) in
  ghost_ (U.contents_def (borrow_ state); M.head_def (M.Stop x2);
    F.size_def (M.Stop x2 :: old_paths);
    F.member_def x0 (M.Stop x2 :: old_paths); U.member_def x0 (borrow_ state);
    F.member_def x1 (M.Stop x2 :: old_paths); U.member_def x1 (borrow_ state);
    F.member_def x2 (M.Stop x2 :: old_paths); U.member_def x2 (borrow_ state);
    ());
  let old_paths = ghost_ (U.contents (borrow_ state)) in
  ghost_ (U.contents_def (borrow_ state));
  let input : {s : U.t | U.valid s && F.size s.#paths < s.#capacity} = state in
  let payment : {b : C.token | C.credits b >= 3Z} = refund in
  let added = U.make_set input payment in
  let #{U.value = x3; state; refund} = added in
  let account3 = ghost_ (U.account (borrow_ state)) in
  ghost_ (U.contents_def (borrow_ state); M.head_def (M.Stop x3);
    F.size_def (M.Stop x3 :: old_paths);
    F.member_def x0 (M.Stop x3 :: old_paths); U.member_def x0 (borrow_ state);
    F.member_def x1 (M.Stop x3 :: old_paths); U.member_def x1 (borrow_ state);
    F.member_def x2 (M.Stop x3 :: old_paths); U.member_def x2 (borrow_ state);
    F.member_def x3 (M.Stop x3 :: old_paths); U.member_def x3 (borrow_ state);
    ());
  let old_paths = ghost_ (U.contents (borrow_ state)) in
  let alpha = ghost_ (U.alpha (borrow_ state)) in
  ghost_ (U.contents_def (borrow_ state); U.alpha_def (borrow_ state);
    A.union_fee_def alpha);
  let input : {s : U.t | U.valid s && U.member x0 s && U.member x1 s} = state in
  let payment : {b : C.token | let input = input in
    C.credits b >= A.union_fee input.#U.alpha} = refund in
  let before_x0 = ghost_ (U.representative x0 (borrow_ input)) in
  let before_x1 = ghost_ (U.representative x1 (borrow_ input)) in
  let before_x2 = ghost_ (U.representative x2 (borrow_ input)) in
  ghost_ (U.union_semantics x0 x1 x2 (borrow_ input));
  let joined = U.union x0 x1 input payment in
  let #{U.value = joined_root; state; refund} = joined in
  ghost_ (U.representative_def x2 (borrow_ state);
    let u = () in
    ignore (u : {u : unit | U.representative x2 state ===
      (if before_x2 === before_x0 || before_x2 === before_x1
       then joined_root else before_x2)}));
  let account4 = ghost_ (U.account (borrow_ state)) in
  let new_paths = ghost_ (U.contents (borrow_ state)) in
  ghost_ (U.contents_def (borrow_ state);
    F.member_same old_paths new_paths x0; U.member_def x0 (borrow_ state);
    F.member_same old_paths new_paths x1; U.member_def x1 (borrow_ state);
    F.member_same old_paths new_paths x2; U.member_def x2 (borrow_ state);
    F.member_same old_paths new_paths x3; U.member_def x3 (borrow_ state);
    ());
  let old_paths = ghost_ (U.contents (borrow_ state)) in
  let alpha = ghost_ (U.alpha (borrow_ state)) in
  ghost_ (U.contents_def (borrow_ state); U.alpha_def (borrow_ state);
    A.union_fee_def alpha);
  let input : {s : U.t | U.valid s && U.member x2 s && U.member x3 s} = state in
  let payment : {b : C.token | let input = input in
    C.credits b >= A.union_fee input.#U.alpha} = refund in
  let joined = U.union x2 x3 input payment in
  let #{U.value = _; state; refund} = joined in
  let account5 = ghost_ (U.account (borrow_ state)) in
  let new_paths = ghost_ (U.contents (borrow_ state)) in
  ghost_ (U.contents_def (borrow_ state);
    F.member_same old_paths new_paths x0; U.member_def x0 (borrow_ state);
    F.member_same old_paths new_paths x1; U.member_def x1 (borrow_ state);
    F.member_same old_paths new_paths x2; U.member_def x2 (borrow_ state);
    F.member_same old_paths new_paths x3; U.member_def x3 (borrow_ state);
    ());
  let old_paths = ghost_ (U.contents (borrow_ state)) in
  let alpha = ghost_ (U.alpha (borrow_ state)) in
  ghost_ (U.contents_def (borrow_ state); U.alpha_def (borrow_ state);
    A.union_fee_def alpha);
  let input : {s : U.t | U.valid s && U.member x0 s && U.member x2 s} = state in
  let payment : {b : C.token | let input = input in
    C.credits b >= A.union_fee input.#U.alpha} = refund in
  let joined = U.union x0 x2 input payment in
  let #{U.value = _; state; refund} = joined in
  let account6 = ghost_ (U.account (borrow_ state)) in
  let new_paths = ghost_ (U.contents (borrow_ state)) in
  ghost_ (U.contents_def (borrow_ state);
    F.member_same old_paths new_paths x0; U.member_def x0 (borrow_ state);
    F.member_same old_paths new_paths x1; U.member_def x1 (borrow_ state);
    F.member_same old_paths new_paths x2; U.member_def x2 (borrow_ state);
    F.member_same old_paths new_paths x3; U.member_def x3 (borrow_ state);
    ());
  let old_paths = ghost_ (U.contents (borrow_ state)) in
  let alpha = ghost_ (U.alpha (borrow_ state)) in
  ghost_ (U.contents_def (borrow_ state); U.alpha_def (borrow_ state);
    A.union_fee_def alpha);
  let input : {s : U.t | U.valid s && U.member x1 s && U.member x3 s} = state in
  let payment : {b : C.token | let input = input in
    C.credits b >= A.union_fee input.#U.alpha} = refund in
  let joined = U.union x1 x3 input payment in
  let #{U.value = _; state; refund} = joined in
  let account7 = ghost_ (U.account (borrow_ state)) in
  let new_paths = ghost_ (U.contents (borrow_ state)) in
  ghost_ (U.contents_def (borrow_ state);
    F.member_same old_paths new_paths x0; U.member_def x0 (borrow_ state);
    F.member_same old_paths new_paths x1; U.member_def x1 (borrow_ state);
    F.member_same old_paths new_paths x2; U.member_def x2 (borrow_ state);
    F.member_same old_paths new_paths x3; U.member_def x3 (borrow_ state);
    ());
  let old_paths = ghost_ (U.contents (borrow_ state)) in
  let alpha = ghost_ (U.alpha (borrow_ state)) in
  ghost_ (U.contents_def (borrow_ state); U.alpha_def (borrow_ state);
    A.find_fee_def alpha);
  let input : {s : U.t | U.valid s && U.member x0 s} = state in
  let payment : {b : C.token | let input = input in
    C.credits b >= A.find_fee input.#U.alpha} = refund in
  let before_x2 = ghost_ (U.representative x2 (borrow_ input)) in
  ghost_ (U.find_semantics x0 x2 (borrow_ input));
  let found = U.find x0 input payment in
  let #{U.value = r0; state; refund} = found in
  ghost_ (U.representative_def x2 (borrow_ state);
    Vox_union_find_spec.find_paths_def old_paths x0;
    let u = () in
    ignore (u : {u : unit | U.representative x2 state === before_x2}));
  let account8 = ghost_ (U.account (borrow_ state)) in
  let new_paths = ghost_ (U.contents (borrow_ state)) in
  ghost_ (U.contents_def (borrow_ state);
    F.member_same old_paths new_paths x0; U.member_def x0 (borrow_ state);
    F.member_same old_paths new_paths x1; U.member_def x1 (borrow_ state);
    F.member_same old_paths new_paths x2; U.member_def x2 (borrow_ state);
    F.member_same old_paths new_paths x3; U.member_def x3 (borrow_ state);
    ());
  let old_paths = ghost_ (U.contents (borrow_ state)) in
  let alpha = ghost_ (U.alpha (borrow_ state)) in
  ghost_ (U.contents_def (borrow_ state); U.alpha_def (borrow_ state);
    A.find_fee_def alpha);
  let input : {s : U.t | U.valid s && U.member x1 s} = state in
  let payment : {b : C.token | let input = input in
    C.credits b >= A.find_fee input.#U.alpha} = refund in
  let found = U.find x1 input payment in
  let #{U.value = r1; state; refund} = found in
  let account9 = ghost_ (U.account (borrow_ state)) in
  let new_paths = ghost_ (U.contents (borrow_ state)) in
  ghost_ (U.contents_def (borrow_ state);
    F.member_same old_paths new_paths x0; U.member_def x0 (borrow_ state);
    F.member_same old_paths new_paths x1; U.member_def x1 (borrow_ state);
    F.member_same old_paths new_paths x2; U.member_def x2 (borrow_ state);
    F.member_same old_paths new_paths x3; U.member_def x3 (borrow_ state);
    ());
  let old_paths = ghost_ (U.contents (borrow_ state)) in
  let alpha = ghost_ (U.alpha (borrow_ state)) in
  ghost_ (U.contents_def (borrow_ state); U.alpha_def (borrow_ state);
    A.find_fee_def alpha);
  let input : {s : U.t | U.valid s && U.member x2 s} = state in
  let payment : {b : C.token | let input = input in
    C.credits b >= A.find_fee input.#U.alpha} = refund in
  let found = U.find x2 input payment in
  let #{U.value = r2; state; refund} = found in
  let account10 = ghost_ (U.account (borrow_ state)) in
  let new_paths = ghost_ (U.contents (borrow_ state)) in
  ghost_ (U.contents_def (borrow_ state);
    F.member_same old_paths new_paths x0; U.member_def x0 (borrow_ state);
    F.member_same old_paths new_paths x1; U.member_def x1 (borrow_ state);
    F.member_same old_paths new_paths x2; U.member_def x2 (borrow_ state);
    F.member_same old_paths new_paths x3; U.member_def x3 (borrow_ state);
    ());
  let old_paths = ghost_ (U.contents (borrow_ state)) in
  let alpha = ghost_ (U.alpha (borrow_ state)) in
  ghost_ (U.contents_def (borrow_ state); U.alpha_def (borrow_ state);
    A.find_fee_def alpha);
  let input : {s : U.t | U.valid s && U.member x3 s} = state in
  let payment : {b : C.token | let input = input in
    C.credits b >= A.find_fee input.#U.alpha} = refund in
  let found = U.find x3 input payment in
  let #{U.value = r3; state; refund} = found in
  let account11 = ghost_ (U.account (borrow_ state)) in
  let new_paths = ghost_ (U.contents (borrow_ state)) in
  ghost_ (U.contents_def (borrow_ state);
    F.member_same old_paths new_paths x0; U.member_def x0 (borrow_ state);
    F.member_same old_paths new_paths x1; U.member_def x1 (borrow_ state);
    F.member_same old_paths new_paths x2; U.member_def x2 (borrow_ state);
    F.member_same old_paths new_paths x3; U.member_def x3 (borrow_ state);
    ());
  assert (Pref.equal r0 r1); assert (Pref.equal r0 r2); assert (Pref.equal r0 r3);
  ghost_ (U.account_bounds (borrow_ state); C.nonnegative (borrow_ refund);
    let u = () in (u : {u : unit | U.ticks state <= 2000Z &&
      Bigint.add (U.account state) (C.credits refund) = 2000Z}));
  let steps = ghost_ [
    {Q.operation = Q.Allocate; account = account0};
    {Q.operation = Q.Allocate; account = account1};
    {Q.operation = Q.Allocate; account = account2};
    {Q.operation = Q.Allocate; account = account3};
    {Q.operation = Q.Union; account = account4};
    {Q.operation = Q.Union; account = account5};
    {Q.operation = Q.Union; account = account6};
    {Q.operation = Q.Union; account = account7};
    {Q.operation = Q.Find; account = account8};
    {Q.operation = Q.Find; account = account9};
    {Q.operation = Q.Find; account = account10};
    {Q.operation = Q.Find; account = account11};
  ] in
  ghost_ (
    let tail12 = [] in
    let tail11 = {Q.operation = Q.Find; account = account11} :: tail12 in
    let tail10 = {Q.operation = Q.Find; account = account10} :: tail11 in
    let tail9 = {Q.operation = Q.Find; account = account9} :: tail10 in
    let tail8 = {Q.operation = Q.Find; account = account8} :: tail9 in
    let tail7 = {Q.operation = Q.Union; account = account7} :: tail8 in
    let tail6 = {Q.operation = Q.Union; account = account6} :: tail7 in
    let tail5 = {Q.operation = Q.Union; account = account5} :: tail6 in
    let tail4 = {Q.operation = Q.Union; account = account4} :: tail5 in
    let tail3 = {Q.operation = Q.Allocate; account = account3} :: tail4 in
    let tail2 = {Q.operation = Q.Allocate; account = account2} :: tail3 in
    let tail1 = {Q.operation = Q.Allocate; account = account1} :: tail2 in
    let tail0 = {Q.operation = Q.Allocate; account = account0} :: tail1 in
    Q.trace_def proof_alpha 1Z tail0; Q.final_account_def 1Z tail0;
    Q.count_def Q.Allocate tail0; Q.count_def Q.Find tail0; Q.count_def Q.Union tail0;
    Q.trace_def proof_alpha account0 tail1; Q.final_account_def account0 tail1;
    Q.count_def Q.Allocate tail1; Q.count_def Q.Find tail1; Q.count_def Q.Union tail1;
    Q.trace_def proof_alpha account1 tail2; Q.final_account_def account1 tail2;
    Q.count_def Q.Allocate tail2; Q.count_def Q.Find tail2; Q.count_def Q.Union tail2;
    Q.trace_def proof_alpha account2 tail3; Q.final_account_def account2 tail3;
    Q.count_def Q.Allocate tail3; Q.count_def Q.Find tail3; Q.count_def Q.Union tail3;
    Q.trace_def proof_alpha account3 tail4; Q.final_account_def account3 tail4;
    Q.count_def Q.Allocate tail4; Q.count_def Q.Find tail4; Q.count_def Q.Union tail4;
    Q.trace_def proof_alpha account4 tail5; Q.final_account_def account4 tail5;
    Q.count_def Q.Allocate tail5; Q.count_def Q.Find tail5; Q.count_def Q.Union tail5;
    Q.trace_def proof_alpha account5 tail6; Q.final_account_def account5 tail6;
    Q.count_def Q.Allocate tail6; Q.count_def Q.Find tail6; Q.count_def Q.Union tail6;
    Q.trace_def proof_alpha account6 tail7; Q.final_account_def account6 tail7;
    Q.count_def Q.Allocate tail7; Q.count_def Q.Find tail7; Q.count_def Q.Union tail7;
    Q.trace_def proof_alpha account7 tail8; Q.final_account_def account7 tail8;
    Q.count_def Q.Allocate tail8; Q.count_def Q.Find tail8; Q.count_def Q.Union tail8;
    Q.trace_def proof_alpha account8 tail9; Q.final_account_def account8 tail9;
    Q.count_def Q.Allocate tail9; Q.count_def Q.Find tail9; Q.count_def Q.Union tail9;
    Q.trace_def proof_alpha account9 tail10; Q.final_account_def account9 tail10;
    Q.count_def Q.Allocate tail10; Q.count_def Q.Find tail10; Q.count_def Q.Union tail10;
    Q.trace_def proof_alpha account10 tail11; Q.final_account_def account10 tail11;
    Q.count_def Q.Allocate tail11; Q.count_def Q.Find tail11; Q.count_def Q.Union tail11;
    Q.trace_def proof_alpha account11 tail12; Q.final_account_def account11 tail12;
    Q.count_def Q.Allocate tail12; Q.count_def Q.Find tail12; Q.count_def Q.Union tail12;
    Q.same_def Q.Allocate Q.Allocate;
    Q.same_def Q.Allocate Q.Find;
    Q.same_def Q.Allocate Q.Union;
    Q.same_def Q.Find Q.Allocate;
    Q.same_def Q.Find Q.Find;
    Q.same_def Q.Find Q.Union;
    Q.same_def Q.Union Q.Allocate;
    Q.same_def Q.Union Q.Find;
    Q.same_def Q.Union Q.Union;
    Q.fee_def proof_alpha Q.Allocate; Q.fee_def proof_alpha Q.Find;
    Q.fee_def proof_alpha Q.Union;
    U.account_bounds (borrow_ state);
    Q.sequence proof_alpha steps (U.ticks (borrow_ state));
    let u = () in (u : {u : unit |
      U.ticks state <= Q.budget proof_alpha 4Z 4Z 4Z}));
  ()
