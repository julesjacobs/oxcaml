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

  $ cat >principal.ml <<'EOF'
  > module M = struct
  >   let[@def] clamp n = if n >= 0 then n else 0
  >   let[@def] normalize (b : bool) = if b = true then b else false
  > end
  > EOF

  $ $MERLIN single errors -principal -extension refinement_types -filename principal.ml <principal.ml | jq -c '.value | map(.message)'
  []
