  $ cat > test.ml <<'ML'
  > let f (x : int @ ghost) =
  >   let y = ghost_ (x + 1) in
  >   let use (_ : int @ ghost) = () in
  >   use y
  > ML

  $ $MERLIN single errors -filename test.ml < test.ml | jq '.value'
  []

  $ $MERLIN single mode-enclosing -position 4:7 -filename test.ml < test.ml | jq '.value[0].mode | contains("ghost")'
  true

  $ $MERLIN single type-enclosing -position 2:18 -filename test.ml < test.ml | jq '.value[0].type'
  "int"

  $ cat > test.ml <<'ML'
  > let bad (x : int @ ghost) = x + 1
  > ML

  $ $MERLIN single errors -filename test.ml < test.ml | jq '.value | map(.message)'
  [
    "This value is ghost but is expected to be real."
  ]
