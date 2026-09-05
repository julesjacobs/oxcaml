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
  > -filename dependent.ml <dependent.ml | jq '.value'
  []

  $ $MERLIN single type-enclosing -position 3:6 -extension refinement_types \
  > -filename dependent.ml <dependent.ml | jq '.value[0].type'
  "(y' : int) -> {r : int | r = (y + y')}"

  $ $MERLIN single type-enclosing -position 4:12 -extension refinement_types \
  > -filename dependent.ml <dependent.ml | jq '.value[0].type'
  "(p : (int * int)) -> {r : int | match p with | (a, b) -> r = (a + b)}"
