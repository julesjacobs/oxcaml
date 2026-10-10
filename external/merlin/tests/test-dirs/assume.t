  $ cat >assume.ml <<'EOF'
  > type t = {v : int | v > 0}
  > let f x : t = assume_ x
  > let bad () : t = assume_ 1
  > let after = 42
  > EOF

  $ $MERLIN single errors -extension refinement_types -filename assume.ml <assume.ml | jq -c '.value | map(.message)'
  ["assume_ requires a plain local variable"]

  $ $MERLIN single type-enclosing -position 2:17 -extension refinement_types -filename assume.ml <assume.ml | jq -c '.value | map(.type)'
  ["t","int @ total -> t"]

  $ $MERLIN single locate -look-for implementation -position 2:22 -extension refinement_types -filename assume.ml <assume.ml | jq -c '.value.pos'
  {"line":2,"col":6}

  $ $MERLIN single type-enclosing -position 4:13 -extension refinement_types -filename assume.ml <assume.ml | jq -c '.value | map(.type)'
  ["int"]
