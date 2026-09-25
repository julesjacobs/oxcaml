(* TEST
 include ocamlcommon;
 include unix;
 native;
*)

let () =
  Unix.putenv "OCAML_EXPERIMENTAL_SYMBOLIC_MODES" "force";
  assert (Mode.For_testing.exact_no_constant_zap_is_rejected ());
  assert (Mode.For_testing.rigid_flexible_sequential_assertions ());
  assert (Mode.For_testing.rigid_flexible_copy_preserves_assertions ());
  assert (Mode.For_testing.nested_scope_pairs_match_oracle () = 0);
  assert (Mode.For_testing.scoped_conditional_residual_matches_oracle () = 0);
  assert (Mode.For_testing.nested_scopes_preserve_dependencies ());
  assert (Mode.For_testing.correlated_envelopes_preserve_bounds ());
  assert (Mode.For_testing.copied_hidden_witnesses_are_fresh ());
  print_endline "symbolic ownership preserved"
