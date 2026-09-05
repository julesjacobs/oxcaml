  $ cat >refined.ml <<EOF
  > let x : { x : int | true } = _
  > EOF

  $ $MERLIN single construct -position 1:29 -depth 2 \
  > -extension refinement_types -filename refined.ml <refined.ml | jq '.value[1]'
  [
    "(let (value : int) = 0 in refine_ value)"
  ]

  $ cat >refined.ml <<EOF
  > let x : { x : int | true } = (let (value : int) = 0 in refine_ value)
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
    "(let (value : int) = 0 in refine_ value)"
  ]

  $ cat >argument.ml <<EOF
  > let consume (_ : { x : int | true }) = ()
  > let result = consume (let (value : int) = 0 in refine_ value)
  > EOF

  $ $MERLIN single errors -extension refinement_types \
  > -filename argument.ml <argument.ml | jq '.value'
  []

  $ cat >nested.ml <<EOF
  > let x : { x : { n : int | let eq (y : int) = y = y in eq n } | true } = _
  > EOF

  $ suggestion=$($MERLIN single construct -position 1:73 -depth 3 \
  > -extension refinement_types -filename nested.ml <nested.ml | revert-newlines | jq -r '.value[1][0]')
  $ printf 'let x : { x : { n : int | let eq (y : int) = y = y in eq n } | true } = %s\n' "$suggestion" >nested.ml
  $ $MERLIN single errors -extension refinement_types \
  > -filename nested.ml <nested.ml | jq '.value'
  []

  $ cat >optional.ml <<EOF
  > let x : { f : ?x:int -> unit -> int | true } = _
  > EOF

  $ suggestion=$($MERLIN single construct -position 1:48 -depth 3 \
  > -extension refinement_types -filename optional.ml <optional.ml | revert-newlines | jq -r '.value[1][0]')
  $ printf 'let x : { f : ?x:int -> unit -> int | true } = %s\n' "$suggestion" >optional.ml
  $ $MERLIN single errors -extension refinement_types \
  > -filename optional.ml <optional.ml | revert-newlines | jq '.value'
  []
