  $ cat >refined.ml <<EOF
  > let x : { x : int | true } = _
  > EOF

  $ $MERLIN single construct -position 1:29 -depth 2 \
  > -extension refinement_types -filename refined.ml <refined.ml | jq '.value[1]'
  [
    "refine_ 0"
  ]

  $ cat >refined.ml <<EOF
  > let x : { x : int | true } = refine_ 0
  > EOF

  $ $MERLIN single errors -extension refinement_types \
  > -filename refined.ml <refined.ml | jq '.value'
  []
