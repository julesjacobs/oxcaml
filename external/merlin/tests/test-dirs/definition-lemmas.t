  $ cat >definitions.ml <<'EOF'
  > module M = struct
  >   let[@def] f x = x + 2
  > end
  > let x = 3
  > let proof = M.f_def x
  > EOF

  $ $MERLIN single errors -extension refinement_types -filename definitions.ml <definitions.ml | jq -c '.value | map(.message)'
  []

  $ $MERLIN single type-enclosing -position 5:17 -extension refinement_types -filename definitions.ml <definitions.ml | jq '[.value[].type | contains("{u : unit")] | any'
  true
