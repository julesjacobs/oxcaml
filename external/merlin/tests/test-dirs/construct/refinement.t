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

  $ cat >unboxed.ml <<EOF
  > type pair = #{ left : int; right : int }
  > let f (p : { r : pair | r.#left = r.#right }) = p
  > EOF

  $ inferred=$($MERLIN single type-enclosing -position 2:4 \
  > -extension refinement_types -filename unboxed.ml <unboxed.ml | revert-newlines | jq -r '.value[0].type')
  $ printf 'type pair = #{ left : int; right : int }\nexternal f : %s = "%%identity"\n' "$inferred" >unboxed.ml
  $ $MERLIN single errors -extension refinement_types \
  > -filename unboxed.ml <unboxed.ml | revert-newlines | jq '.value'
  [
    {
      "start": {
        "line": 2,
        "col": 0
      },
      "end": {
        "line": 2,
        "col": 66
      },
      "type": "warning",
      "sub": [],
      "valid": true,
      "message": "Warning 228: The verifier assumes this external's refinement;\n  nothing checks it."
    }
  ]

  $ cat >dependent_record.ml <<EOF
  > type interval = { lower : int; upper : {u : int | lower <= u} }
  > let (upper @ total) (r : interval) : {u : int | r.lower <= u} = r.upper
  > EOF

  $ $MERLIN single errors -extension refinement_types \
  > -filename dependent_record.ml <dependent_record.ml | jq '.value'
  []

  $ $MERLIN single type-enclosing -position 2:66 \
  > -extension refinement_types -filename dependent_record.ml \
  > <dependent_record.ml | jq -r '.value[1].type'
  {u : int | r.lower <= u}

  $ cat >dependent_constructor.ml <<EOF
  > type _ bounded =
  >   | Bounded : { lower : int; value : {v : int | lower <= v} } -> int bounded
  >   | Empty : unit bounded
  > let (ordered @ total) (Bounded {lower; value}) : {b : bool | b} = lower <= value
  > let (project @ total) (Bounded r) : int =
  >   let value : {v : int | r.lower <= v} = r.value in value
  > EOF

  $ $MERLIN single errors -extension refinement_types \
  > -filename dependent_constructor.ml <dependent_constructor.ml | jq '.value'
  []
