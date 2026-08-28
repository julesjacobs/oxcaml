  $ cat >numerical.ml <<'EOF'
  > let rec count (n : int) = if n <= 0 then 0 else count (n - 1)
  > [@@decreases n]
  > let after = 42
  > EOF

  $ $MERLIN single errors -extension refinement_types -filename numerical.ml <numerical.ml | jq -c '.value | map(.message)'
  ["Numerical termination verification is unavailable in this compiler"]

  $ $MERLIN single type-enclosing -position 3:13 -extension refinement_types -filename numerical.ml <numerical.ml | jq -c '.value | map(.type)'
  ["int"]
