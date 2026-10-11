  $ cat >dependent.ml <<'ML'
  > external f : (x:int) -> (y:int) -> {r:int | r=x+y} = "f"
  > let y = 1
  > let partial = f y
  > external tuple : (p : (int * int)) -> {r:int | match p with a,b -> r=a+b} = "tuple"
  > let g (x:int) : {r:int | r=x} = refine_ x
  > module M = struct let x=1 let y=g x end
  > let exported : {r:int | r=M.x} = M.y
  > ML

  $ $MERLIN single errors -extension refinement_types \
  > -filename dependent.ml <dependent.ml | revert-newlines | jq '.value'
  [
    {
      "start": {
        "line": 1,
        "col": 0
      },
      "end": {
        "line": 1,
        "col": 56
      },
      "type": "warning",
      "sub": [],
      "valid": true,
      "message": "Warning 228: The verifier assumes this external's refinement;\n  nothing checks it."
    },
    {
      "start": {
        "line": 4,
        "col": 0
      },
      "end": {
        "line": 4,
        "col": 83
      },
      "type": "warning",
      "sub": [],
      "valid": true,
      "message": "Warning 228: The verifier assumes this external's refinement;\n  nothing checks it."
    }
  ]

  $ $MERLIN single type-enclosing -position 3:6 -extension refinement_types \
  > -filename dependent.ml <dependent.ml | jq '.value[0].type'
  "(y' : int) -> {r : int | r = (y + y')}"

  $ $MERLIN single type-enclosing -position 4:12 -extension refinement_types \
  > -filename dependent.ml <dependent.ml | jq '.value[0].type'
  "(p : (int * int)) -> {r : int | match p with | (a, b) -> r = (a + b)}"

  $ cat >escape.ml <<'ML'
  > external ( = ) : int -> int -> bool @@ total = "%equal"
  > external follow : (x : int) -> {y : int | y = x} -> int = "follow"
  > let escaped = follow (1 + 1)
  > let recovered = 42
  > ML

  $ $MERLIN single errors -extension refinement_types \
  > -filename escape.ml <escape.ml | revert-newlines | jq '.value'
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
    },
    {
      "start": {
        "line": 3,
        "col": 14
      },
      "end": {
        "line": 3,
        "col": 28
      },
      "type": "typer",
      "sub": [
        {
          "start": {
            "line": 3,
            "col": 14
          },
          "end": {
            "line": 3,
            "col": 28
          },
          "message": "This is the argument."
        },
        {
          "start": {
            "line": 3,
            "col": 14
          },
          "end": {
            "line": 3,
            "col": 28
          },
          "message": "Hint: bind the argument to a variable with a let outside this expression."
        }
      ],
      "valid": true,
      "message": "the refinement type of this expression mentions the argument\nfor parameter x, which is not a variable"
    }
  ]

  $ $MERLIN single type-enclosing -position 4:6 -extension refinement_types \
  > -filename escape.ml <escape.ml | revert-newlines | jq '.value[0].type'
  "int"

  $ cat >newtype.ml <<'ML'
  > external ( = ) : int -> int -> bool @@ total = "%equal"
  > external above : (x : int) -> {y : int | y = x} = "above"
  > let nested_newtype x (type a) (_ : a) : {y : int | y = x} = above x
  > let recovered = 42
  > ML

  $ $MERLIN single errors -extension refinement_types \
  > -filename newtype.ml <newtype.ml | revert-newlines | jq '.value'
  [
    {
      "start": {
        "line": 2,
        "col": 0
      },
      "end": {
        "line": 2,
        "col": 57
      },
      "type": "warning",
      "sub": [],
      "valid": true,
      "message": "Warning 228: The verifier assumes this external's refinement;\n  nothing checks it."
    }
  ]

  $ $MERLIN single type-enclosing -position 4:6 -extension refinement_types \
  > -filename newtype.ml <newtype.ml | revert-newlines | jq '.value[0].type'
  "int"
