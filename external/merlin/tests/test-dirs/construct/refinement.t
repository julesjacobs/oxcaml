  $ cat >refined.ml <<EOF
  > let x : { x : int | true } = _
  > EOF

  $ $MERLIN single construct -position 1:29 -depth 2 \
  > -extension refinement_types -filename refined.ml <refined.ml | jq '.value[1]'
  [
    "(let value = 0 in refine_ value)"
  ]

  $ cat >refined.ml <<EOF
  > let x : { x : int | true } = (let value = 0 in refine_ value)
  > EOF

  $ $MERLIN single errors -extension refinement_types \
  > -filename refined.ml <refined.ml | jq '.value'
  []

  $ cat >argument.ml <<EOF
  > let consume (_ : { x : int | true }) = ()
  > let result = consume _
  > EOF

  $ $MERLIN single construct -position 2:22 -depth 2 \
  > -extension refinement_types -filename argument.ml <argument.ml | jq '.value[1]'
  [
    "(let value = 0 in refine_ value)"
  ]

  $ cat >argument.ml <<EOF
  > let consume (_ : { x : int | true }) = ()
  > let result = consume (let value = 0 in refine_ value)
  > EOF

  $ $MERLIN single errors -extension refinement_types \
  > -filename argument.ml <argument.ml | jq '.value'
  []
