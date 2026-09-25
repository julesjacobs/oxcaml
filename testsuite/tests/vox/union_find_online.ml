(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml vox_big_credits.mli vox_big_credits.ml vox_ackermann.ml vox_union_find_potential.ml vox_union_find_levels.ml vox_union_find_path_cost.ml vox_union_find_model.ml vox_union_find_forest.ml vox_union_find_rank.ml vox_union_find_mass.ml vox_union_find_link.ml vox_union_find_worker.ml vox_union_find_amortized.ml vox_union_find_bank.ml vox_union_find_spec.ml vox_union_find.mli vox_union_find.ml vox_union_find_complexity.ml vox_union_find_simple.mli vox_union_find_simple.ml vox_union_find_online.mli vox_union_find_online.ml vox_union_find_online_cost.ml union_find_online.ml";
 { bytecode; }
 { native; }
 { flags += " -principal"; bytecode; }
 { flags += " -principal"; native; }
*)

module C = Vox_big_credits.Make ()
module M = Vox_union_find_model
module F = Vox_union_find_forest
module K = Vox_ackermann
module S = Vox_union_find_spec
module Fixed = Vox_union_find_simple.Make (C)
module Online = Vox_union_find_online.Make (C)

module Fixed_client = struct
  module U = Fixed
  module Q = Vox_union_find_complexity
  let run () =
    if max_int >= 4 then (
    let amount = 1Z in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = 1Z} = fee in
    let limit = 4Z in
    let cap : {n : Bigint.t | 1Z <= n && n <= Bigint.of_int max_int} = limit in
    let state = U.create cap payment in
    let population = ghost_ 4Z in
    let cap : {n : Bigint.t | 1Z <= n} = population in
    let alpha = ghost_ (K.inverse cap) in
    let input : {s : U.t | U.valid s && U.size s < U.capacity s} = state in
    let amount = 3Z in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = 3Z} = fee in
    let added = U.make_set input payment in
    let #{U.value = x0; state} = added in
    let account0 = ghost_ (U.account (borrow_ state)) in
    let old_paths = ghost_ (U.contents (borrow_ state)) in
    ghost_ (U.member_def x0 (borrow_ state));
    let input : {s : U.t | U.valid s && U.size s < U.capacity s} = state in
    let amount = 3Z in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = 3Z} = fee in
    let added = U.make_set input payment in
    let #{U.value = x1; state} = added in
    let account1 = ghost_ (U.account (borrow_ state)) in
    ghost_ (F.member_def x0 (M.Stop x1 :: old_paths);
      M.head_def (M.Stop x1); U.member_def x0 (borrow_ state));
    let before = ghost_ (U.contents (borrow_ state)) in
    ghost_ (U.member_def x0 (borrow_ state); U.member_def x1 (borrow_ state);
      U.observations (borrow_ state);
      U.fee_bounds (borrow_ state) alpha);
    let input : {s : U.t | U.valid s && U.member x0 s && U.member x1 s} = state in
    let amount = ghost_ (U.union_fee (borrow_ input)) in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = U.union_fee input} = fee in
    ghost_ (U.union_semantics x0 x1 x0 (borrow_ input));
    let joined = U.union x0 x1 input payment in
    let #{U.value = root; state} = joined in
    ghost_ (F.member_same before (U.contents (borrow_ state)) x0;
      F.member_same before (U.contents (borrow_ state)) x1;
      U.member_def x0 (borrow_ state); U.member_def x1 (borrow_ state);
      U.representative_def x0 (borrow_ state));
    let before = ghost_ (U.contents (borrow_ state)) in
    let account2 = ghost_ (U.account (borrow_ state)) in
    ghost_ (U.observations (borrow_ state);
      U.fee_bounds (borrow_ state) alpha);
    let input : {s : U.t | U.valid s && U.member x0 s} = state in
    let amount = ghost_ (U.find_fee (borrow_ input)) in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = U.find_fee input} = fee in
    let found = U.find x0 input payment in
    let #{U.value; state} = found in
    assert (Pref.equal value root);
    ghost_ (F.member_same before (U.contents (borrow_ state)) x0;
      U.member_def x0 (borrow_ state));
    let account3 = ghost_ (U.account (borrow_ state)) in
    let old_paths = ghost_ (U.contents (borrow_ state)) in
    ghost_ (U.member_def x0 (borrow_ state));
    let input : {s : U.t | U.valid s && U.size s < U.capacity s} = state in
    let amount = 3Z in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = 3Z} = fee in
    let added = U.make_set input payment in
    let #{U.value = x2; state} = added in
    let account4 = ghost_ (U.account (borrow_ state)) in
    ghost_ (F.member_def x0 (M.Stop x2 :: old_paths);
      M.head_def (M.Stop x2); U.member_def x0 (borrow_ state));
    let before = ghost_ (U.contents (borrow_ state)) in
    ghost_ (U.member_def x0 (borrow_ state); U.member_def x2 (borrow_ state);
      U.observations (borrow_ state);
      U.fee_bounds (borrow_ state) alpha);
    let input : {s : U.t | U.valid s && U.member x0 s && U.member x2 s} = state in
    let amount = ghost_ (U.union_fee (borrow_ input)) in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = U.union_fee input} = fee in
    ghost_ (U.union_semantics x0 x2 x0 (borrow_ input));
    let joined = U.union x0 x2 input payment in
    let #{U.value = root; state} = joined in
    ghost_ (F.member_same before (U.contents (borrow_ state)) x0;
      F.member_same before (U.contents (borrow_ state)) x2;
      U.member_def x0 (borrow_ state); U.member_def x2 (borrow_ state);
      U.representative_def x0 (borrow_ state));
    let before = ghost_ (U.contents (borrow_ state)) in
    let account5 = ghost_ (U.account (borrow_ state)) in
    ghost_ (U.observations (borrow_ state);
      U.fee_bounds (borrow_ state) alpha);
    let input : {s : U.t | U.valid s && U.member x0 s} = state in
    let amount = ghost_ (U.find_fee (borrow_ input)) in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = U.find_fee input} = fee in
    let found = U.find x0 input payment in
    let #{U.value; state} = found in
    assert (Pref.equal value root);
    ghost_ (F.member_same before (U.contents (borrow_ state)) x0;
      U.member_def x0 (borrow_ state));
    let account6 = ghost_ (U.account (borrow_ state)) in
    let old_paths = ghost_ (U.contents (borrow_ state)) in
    ghost_ (U.member_def x0 (borrow_ state));
    let input : {s : U.t | U.valid s && U.size s < U.capacity s} = state in
    let amount = 3Z in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = 3Z} = fee in
    let added = U.make_set input payment in
    let #{U.value = x3; state} = added in
    let account7 = ghost_ (U.account (borrow_ state)) in
    ghost_ (F.member_def x0 (M.Stop x3 :: old_paths);
      M.head_def (M.Stop x3); U.member_def x0 (borrow_ state));
    let before = ghost_ (U.contents (borrow_ state)) in
    ghost_ (U.member_def x0 (borrow_ state); U.member_def x3 (borrow_ state);
      U.observations (borrow_ state);
      U.fee_bounds (borrow_ state) alpha);
    let input : {s : U.t | U.valid s && U.member x0 s && U.member x3 s} = state in
    let amount = ghost_ (U.union_fee (borrow_ input)) in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = U.union_fee input} = fee in
    ghost_ (U.union_semantics x0 x3 x0 (borrow_ input));
    let joined = U.union x0 x3 input payment in
    let #{U.value = root; state} = joined in
    ghost_ (F.member_same before (U.contents (borrow_ state)) x0;
      F.member_same before (U.contents (borrow_ state)) x3;
      U.member_def x0 (borrow_ state); U.member_def x3 (borrow_ state);
      U.representative_def x0 (borrow_ state));
    let before = ghost_ (U.contents (borrow_ state)) in
    let account8 = ghost_ (U.account (borrow_ state)) in
    ghost_ (U.observations (borrow_ state);
      U.fee_bounds (borrow_ state) alpha);
    let input : {s : U.t | U.valid s && U.member x0 s} = state in
    let amount = ghost_ (U.find_fee (borrow_ input)) in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = U.find_fee input} = fee in
    let found = U.find x0 input payment in
    let #{U.value; state} = found in
    assert (Pref.equal value root);
    ghost_ (F.member_same before (U.contents (borrow_ state)) x0;
      U.member_def x0 (borrow_ state));
    let account9 = ghost_ (U.account (borrow_ state)) in
    let steps10 = ghost_ [] in
    let steps9 = ghost_ ({Q.operation = Q.Find; account = account9} :: steps10) in
    let steps8 = ghost_ ({Q.operation = Q.Union; account = account8} :: steps9) in
    let steps7 = ghost_ ({Q.operation = Q.Allocate; account = account7} :: steps8) in
    let steps6 = ghost_ ({Q.operation = Q.Find; account = account6} :: steps7) in
    let steps5 = ghost_ ({Q.operation = Q.Union; account = account5} :: steps6) in
    let steps4 = ghost_ ({Q.operation = Q.Allocate; account = account4} :: steps5) in
    let steps3 = ghost_ ({Q.operation = Q.Find; account = account3} :: steps4) in
    let steps2 = ghost_ ({Q.operation = Q.Union; account = account2} :: steps3) in
    let steps1 = ghost_ ({Q.operation = Q.Allocate; account = account1} :: steps2) in
    let steps0 = ghost_ ({Q.operation = Q.Allocate; account = account0} :: steps1) in
    ghost_ (
      Q.trace_def alpha 1Z steps0;
      Q.trace_def alpha account0 steps1;
      Q.trace_def alpha account1 steps2;
      Q.trace_def alpha account2 steps3;
      Q.trace_def alpha account3 steps4;
      Q.trace_def alpha account4 steps5;
      Q.trace_def alpha account5 steps6;
      Q.trace_def alpha account6 steps7;
      Q.trace_def alpha account7 steps8;
      Q.trace_def alpha account8 steps9;
      Q.trace_def alpha account9 steps10;
      Q.fee_def alpha Q.Allocate; Q.fee_def alpha Q.Find; Q.fee_def alpha Q.Union;
      U.account_bounds (borrow_ state);
      let u = () in let _ : {u : unit | Q.trace alpha 1Z steps0} = u in
      Q.final_account_def 1Z steps0;
      Q.count_def Q.Allocate steps0;
      Q.count_def Q.Find steps0;
      Q.count_def Q.Union steps0;
      Q.final_account_def account0 steps1;
      Q.count_def Q.Allocate steps1;
      Q.count_def Q.Find steps1;
      Q.count_def Q.Union steps1;
      Q.final_account_def account1 steps2;
      Q.count_def Q.Allocate steps2;
      Q.count_def Q.Find steps2;
      Q.count_def Q.Union steps2;
      Q.final_account_def account2 steps3;
      Q.count_def Q.Allocate steps3;
      Q.count_def Q.Find steps3;
      Q.count_def Q.Union steps3;
      Q.final_account_def account3 steps4;
      Q.count_def Q.Allocate steps4;
      Q.count_def Q.Find steps4;
      Q.count_def Q.Union steps4;
      Q.final_account_def account4 steps5;
      Q.count_def Q.Allocate steps5;
      Q.count_def Q.Find steps5;
      Q.count_def Q.Union steps5;
      Q.final_account_def account5 steps6;
      Q.count_def Q.Allocate steps6;
      Q.count_def Q.Find steps6;
      Q.count_def Q.Union steps6;
      Q.final_account_def account6 steps7;
      Q.count_def Q.Allocate steps7;
      Q.count_def Q.Find steps7;
      Q.count_def Q.Union steps7;
      Q.final_account_def account7 steps8;
      Q.count_def Q.Allocate steps8;
      Q.count_def Q.Find steps8;
      Q.count_def Q.Union steps8;
      Q.final_account_def account8 steps9;
      Q.count_def Q.Allocate steps9;
      Q.count_def Q.Find steps9;
      Q.count_def Q.Union steps9;
      Q.final_account_def account9 steps10;
      Q.count_def Q.Allocate steps10;
      Q.count_def Q.Find steps10;
      Q.count_def Q.Union steps10;
      Q.same_def Q.Allocate Q.Allocate;
      Q.same_def Q.Allocate Q.Find;
      Q.same_def Q.Allocate Q.Union;
      Q.same_def Q.Find Q.Allocate;
      Q.same_def Q.Find Q.Find;
      Q.same_def Q.Find Q.Union;
      Q.same_def Q.Union Q.Allocate;
      Q.same_def Q.Union Q.Find;
      Q.same_def Q.Union Q.Union;
      Q.sequence alpha steps0 (U.ticks (borrow_ state));
      let u = () in
      let _ : {u : unit | U.ticks state <= Q.budget alpha 4Z 3Z 3Z} = u in ());
    ())
end

module Growing_client = struct
  module U = Online
  module Q = Vox_union_find_online_cost
  let run () =
    if max_int >= 9 then (
    let amount = 1Z in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = 1Z} = fee in
    let state = U.create payment in
    let population = ghost_ 9Z in
    let cap : {n : Bigint.t | 1Z <= n} = population in
    let alpha = ghost_ (K.inverse cap) in
    let input : {s : U.t | U.valid s && U.size s < Bigint.of_int max_int} = state in
    let amount = 11Z in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = 11Z} = fee in
    let added = U.make_set input payment in
    let #{U.value = x0; state} = added in
    let account0 = ghost_ (U.account (borrow_ state)) in
    let old_paths = ghost_ (U.contents (borrow_ state)) in
    ghost_ (U.member_def x0 (borrow_ state));
    let input : {s : U.t | U.valid s && U.size s < Bigint.of_int max_int} = state in
    let amount = 11Z in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = 11Z} = fee in
    let added = U.make_set input payment in
    let #{U.value = x1; state} = added in
    let account1 = ghost_ (U.account (borrow_ state)) in
    ghost_ (F.member_def x0 (M.Stop x1 :: old_paths);
      M.head_def (M.Stop x1); U.member_def x0 (borrow_ state));
    let before = ghost_ (U.contents (borrow_ state)) in
    ghost_ (U.member_def x0 (borrow_ state); U.member_def x1 (borrow_ state);
      U.observations (borrow_ state);
      U.fee_bounds (borrow_ state) population alpha);
    let input : {s : U.t | U.valid s && U.member x0 s && U.member x1 s} = state in
    let amount = ghost_ (U.union_fee (borrow_ input)) in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = U.union_fee input} = fee in
    ghost_ (U.union_semantics x0 x1 x0 (borrow_ input));
    let joined = U.union x0 x1 input payment in
    let #{U.value = root; state} = joined in
    ghost_ (F.member_same before (U.contents (borrow_ state)) x0;
      F.member_same before (U.contents (borrow_ state)) x1;
      U.member_def x0 (borrow_ state); U.member_def x1 (borrow_ state);
      U.representative_def x0 (borrow_ state));
    let before = ghost_ (U.contents (borrow_ state)) in
    let account2 = ghost_ (U.account (borrow_ state)) in
    ghost_ (U.observations (borrow_ state);
      U.fee_bounds (borrow_ state) population alpha);
    let input : {s : U.t | U.valid s && U.member x0 s} = state in
    let amount = ghost_ (U.find_fee (borrow_ input)) in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = U.find_fee input} = fee in
    let found = U.find x0 input payment in
    let #{U.value; state} = found in
    assert (Pref.equal value root);
    ghost_ (F.member_same before (U.contents (borrow_ state)) x0;
      U.member_def x0 (borrow_ state));
    let account3 = ghost_ (U.account (borrow_ state)) in
    let old_paths = ghost_ (U.contents (borrow_ state)) in
    ghost_ (U.member_def x0 (borrow_ state));
    let input : {s : U.t | U.valid s && U.size s < Bigint.of_int max_int} = state in
    let amount = 11Z in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = 11Z} = fee in
    let added = U.make_set input payment in
    let #{U.value = x2; state} = added in
    let account4 = ghost_ (U.account (borrow_ state)) in
    ghost_ (F.member_def x0 (M.Stop x2 :: old_paths);
      M.head_def (M.Stop x2); U.member_def x0 (borrow_ state));
    let before = ghost_ (U.contents (borrow_ state)) in
    ghost_ (U.member_def x0 (borrow_ state); U.member_def x2 (borrow_ state);
      U.observations (borrow_ state);
      U.fee_bounds (borrow_ state) population alpha);
    let input : {s : U.t | U.valid s && U.member x0 s && U.member x2 s} = state in
    let amount = ghost_ (U.union_fee (borrow_ input)) in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = U.union_fee input} = fee in
    ghost_ (U.union_semantics x0 x2 x0 (borrow_ input));
    let joined = U.union x0 x2 input payment in
    let #{U.value = root; state} = joined in
    ghost_ (F.member_same before (U.contents (borrow_ state)) x0;
      F.member_same before (U.contents (borrow_ state)) x2;
      U.member_def x0 (borrow_ state); U.member_def x2 (borrow_ state);
      U.representative_def x0 (borrow_ state));
    let before = ghost_ (U.contents (borrow_ state)) in
    let account5 = ghost_ (U.account (borrow_ state)) in
    ghost_ (U.observations (borrow_ state);
      U.fee_bounds (borrow_ state) population alpha);
    let input : {s : U.t | U.valid s && U.member x0 s} = state in
    let amount = ghost_ (U.find_fee (borrow_ input)) in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = U.find_fee input} = fee in
    let found = U.find x0 input payment in
    let #{U.value; state} = found in
    assert (Pref.equal value root);
    ghost_ (F.member_same before (U.contents (borrow_ state)) x0;
      U.member_def x0 (borrow_ state));
    let account6 = ghost_ (U.account (borrow_ state)) in
    let old_paths = ghost_ (U.contents (borrow_ state)) in
    ghost_ (U.member_def x0 (borrow_ state));
    let input : {s : U.t | U.valid s && U.size s < Bigint.of_int max_int} = state in
    let amount = 11Z in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = 11Z} = fee in
    let added = U.make_set input payment in
    let #{U.value = x3; state} = added in
    let account7 = ghost_ (U.account (borrow_ state)) in
    ghost_ (F.member_def x0 (M.Stop x3 :: old_paths);
      M.head_def (M.Stop x3); U.member_def x0 (borrow_ state));
    let old_paths = ghost_ (U.contents (borrow_ state)) in
    ghost_ (U.member_def x0 (borrow_ state));
    let input : {s : U.t | U.valid s && U.size s < Bigint.of_int max_int} = state in
    let amount = 11Z in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = 11Z} = fee in
    let added = U.make_set input payment in
    let #{U.value = x4; state} = added in
    let account8 = ghost_ (U.account (borrow_ state)) in
    ghost_ (F.member_def x0 (M.Stop x4 :: old_paths);
      M.head_def (M.Stop x4); U.member_def x0 (borrow_ state));
    let old_paths = ghost_ (U.contents (borrow_ state)) in
    ghost_ (U.member_def x0 (borrow_ state));
    let input : {s : U.t | U.valid s && U.size s < Bigint.of_int max_int} = state in
    let amount = 11Z in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = 11Z} = fee in
    let added = U.make_set input payment in
    let #{U.value = x5; state} = added in
    let account9 = ghost_ (U.account (borrow_ state)) in
    ghost_ (F.member_def x0 (M.Stop x5 :: old_paths);
      M.head_def (M.Stop x5); U.member_def x0 (borrow_ state));
    let old_paths = ghost_ (U.contents (borrow_ state)) in
    ghost_ (U.member_def x0 (borrow_ state));
    let input : {s : U.t | U.valid s && U.size s < Bigint.of_int max_int} = state in
    let amount = 11Z in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = 11Z} = fee in
    let added = U.make_set input payment in
    let #{U.value = x6; state} = added in
    let account10 = ghost_ (U.account (borrow_ state)) in
    ghost_ (F.member_def x0 (M.Stop x6 :: old_paths);
      M.head_def (M.Stop x6); U.member_def x0 (borrow_ state));
    let old_paths = ghost_ (U.contents (borrow_ state)) in
    ghost_ (U.member_def x0 (borrow_ state));
    let input : {s : U.t | U.valid s && U.size s < Bigint.of_int max_int} = state in
    let amount = 11Z in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = 11Z} = fee in
    let added = U.make_set input payment in
    let #{U.value = x7; state} = added in
    let account11 = ghost_ (U.account (borrow_ state)) in
    ghost_ (F.member_def x0 (M.Stop x7 :: old_paths);
      M.head_def (M.Stop x7); U.member_def x0 (borrow_ state));
    let old_paths = ghost_ (U.contents (borrow_ state)) in
    ghost_ (U.member_def x0 (borrow_ state));
    let input : {s : U.t | U.valid s && U.size s < Bigint.of_int max_int} = state in
    let amount = 11Z in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = 11Z} = fee in
    let added = U.make_set input payment in
    let #{U.value = x8; state} = added in
    let account12 = ghost_ (U.account (borrow_ state)) in
    ghost_ (F.member_def x0 (M.Stop x8 :: old_paths);
      M.head_def (M.Stop x8); U.member_def x0 (borrow_ state));
    let before = ghost_ (U.contents (borrow_ state)) in
    ghost_ (U.member_def x0 (borrow_ state); U.member_def x8 (borrow_ state);
      U.observations (borrow_ state);
      U.fee_bounds (borrow_ state) population alpha);
    let input : {s : U.t | U.valid s && U.member x0 s && U.member x8 s} = state in
    let amount = ghost_ (U.union_fee (borrow_ input)) in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = U.union_fee input} = fee in
    ghost_ (U.union_semantics x0 x8 x0 (borrow_ input));
    let joined = U.union x0 x8 input payment in
    let #{U.value = root; state} = joined in
    ghost_ (F.member_same before (U.contents (borrow_ state)) x0;
      F.member_same before (U.contents (borrow_ state)) x8;
      U.member_def x0 (borrow_ state); U.member_def x8 (borrow_ state);
      U.representative_def x0 (borrow_ state));
    let before = ghost_ (U.contents (borrow_ state)) in
    let account13 = ghost_ (U.account (borrow_ state)) in
    ghost_ (U.observations (borrow_ state);
      U.fee_bounds (borrow_ state) population alpha);
    let input : {s : U.t | U.valid s && U.member x0 s} = state in
    let amount = ghost_ (U.find_fee (borrow_ input)) in
    let issuance : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create issuance in
    let payment : {b : C.token | C.credits b = U.find_fee input} = fee in
    let found = U.find x0 input payment in
    let #{U.value; state} = found in
    assert (Pref.equal value root);
    ghost_ (F.member_same before (U.contents (borrow_ state)) x0;
      U.member_def x0 (borrow_ state));
    let account14 = ghost_ (U.account (borrow_ state)) in
    let steps15 = ghost_ [] in
    let steps14 = ghost_ ({Q.operation = Q.Find; account = account14} :: steps15) in
    let steps13 = ghost_ ({Q.operation = Q.Union; account = account13} :: steps14) in
    let steps12 = ghost_ ({Q.operation = Q.Allocate; account = account12} :: steps13) in
    let steps11 = ghost_ ({Q.operation = Q.Allocate; account = account11} :: steps12) in
    let steps10 = ghost_ ({Q.operation = Q.Allocate; account = account10} :: steps11) in
    let steps9 = ghost_ ({Q.operation = Q.Allocate; account = account9} :: steps10) in
    let steps8 = ghost_ ({Q.operation = Q.Allocate; account = account8} :: steps9) in
    let steps7 = ghost_ ({Q.operation = Q.Allocate; account = account7} :: steps8) in
    let steps6 = ghost_ ({Q.operation = Q.Find; account = account6} :: steps7) in
    let steps5 = ghost_ ({Q.operation = Q.Union; account = account5} :: steps6) in
    let steps4 = ghost_ ({Q.operation = Q.Allocate; account = account4} :: steps5) in
    let steps3 = ghost_ ({Q.operation = Q.Find; account = account3} :: steps4) in
    let steps2 = ghost_ ({Q.operation = Q.Union; account = account2} :: steps3) in
    let steps1 = ghost_ ({Q.operation = Q.Allocate; account = account1} :: steps2) in
    let steps0 = ghost_ ({Q.operation = Q.Allocate; account = account0} :: steps1) in
    ghost_ (
      Q.trace_def alpha 1Z steps0;
      Q.trace_def alpha account0 steps1;
      Q.trace_def alpha account1 steps2;
      Q.trace_def alpha account2 steps3;
      Q.trace_def alpha account3 steps4;
      Q.trace_def alpha account4 steps5;
      Q.trace_def alpha account5 steps6;
      Q.trace_def alpha account6 steps7;
      Q.trace_def alpha account7 steps8;
      Q.trace_def alpha account8 steps9;
      Q.trace_def alpha account9 steps10;
      Q.trace_def alpha account10 steps11;
      Q.trace_def alpha account11 steps12;
      Q.trace_def alpha account12 steps13;
      Q.trace_def alpha account13 steps14;
      Q.trace_def alpha account14 steps15;
      Q.fee_def alpha Q.Allocate; Q.fee_def alpha Q.Find; Q.fee_def alpha Q.Union;
      Q.find_fee_def alpha; Q.union_fee_def alpha;
      U.account_bounds (borrow_ state);
      let u = () in let _ : {u : unit | Q.trace alpha 1Z steps0} = u in
      Q.final_account_def 1Z steps0;
      Q.count_def Q.Allocate steps0;
      Q.count_def Q.Find steps0;
      Q.count_def Q.Union steps0;
      Q.final_account_def account0 steps1;
      Q.count_def Q.Allocate steps1;
      Q.count_def Q.Find steps1;
      Q.count_def Q.Union steps1;
      Q.final_account_def account1 steps2;
      Q.count_def Q.Allocate steps2;
      Q.count_def Q.Find steps2;
      Q.count_def Q.Union steps2;
      Q.final_account_def account2 steps3;
      Q.count_def Q.Allocate steps3;
      Q.count_def Q.Find steps3;
      Q.count_def Q.Union steps3;
      Q.final_account_def account3 steps4;
      Q.count_def Q.Allocate steps4;
      Q.count_def Q.Find steps4;
      Q.count_def Q.Union steps4;
      Q.final_account_def account4 steps5;
      Q.count_def Q.Allocate steps5;
      Q.count_def Q.Find steps5;
      Q.count_def Q.Union steps5;
      Q.final_account_def account5 steps6;
      Q.count_def Q.Allocate steps6;
      Q.count_def Q.Find steps6;
      Q.count_def Q.Union steps6;
      Q.final_account_def account6 steps7;
      Q.count_def Q.Allocate steps7;
      Q.count_def Q.Find steps7;
      Q.count_def Q.Union steps7;
      Q.final_account_def account7 steps8;
      Q.count_def Q.Allocate steps8;
      Q.count_def Q.Find steps8;
      Q.count_def Q.Union steps8;
      Q.final_account_def account8 steps9;
      Q.count_def Q.Allocate steps9;
      Q.count_def Q.Find steps9;
      Q.count_def Q.Union steps9;
      Q.final_account_def account9 steps10;
      Q.count_def Q.Allocate steps10;
      Q.count_def Q.Find steps10;
      Q.count_def Q.Union steps10;
      Q.final_account_def account10 steps11;
      Q.count_def Q.Allocate steps11;
      Q.count_def Q.Find steps11;
      Q.count_def Q.Union steps11;
      Q.final_account_def account11 steps12;
      Q.count_def Q.Allocate steps12;
      Q.count_def Q.Find steps12;
      Q.count_def Q.Union steps12;
      Q.final_account_def account12 steps13;
      Q.count_def Q.Allocate steps13;
      Q.count_def Q.Find steps13;
      Q.count_def Q.Union steps13;
      Q.final_account_def account13 steps14;
      Q.count_def Q.Allocate steps14;
      Q.count_def Q.Find steps14;
      Q.count_def Q.Union steps14;
      Q.final_account_def account14 steps15;
      Q.count_def Q.Allocate steps15;
      Q.count_def Q.Find steps15;
      Q.count_def Q.Union steps15;
      Q.same_def Q.Allocate Q.Allocate;
      Q.same_def Q.Allocate Q.Find;
      Q.same_def Q.Allocate Q.Union;
      Q.same_def Q.Find Q.Allocate;
      Q.same_def Q.Find Q.Find;
      Q.same_def Q.Find Q.Union;
      Q.same_def Q.Union Q.Allocate;
      Q.same_def Q.Union Q.Find;
      Q.same_def Q.Union Q.Union;
      Q.sequence alpha steps0 (U.ticks (borrow_ state));
      let u = () in
      let _ : {u : unit | U.ticks state <= Q.budget alpha 9Z 3Z 3Z} = u in ());
    ())
end

let () = Fixed_client.run (); Growing_client.run (); print_endline "simple and online union-find: ok"
