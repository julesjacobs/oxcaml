  $ cat >bigints.ml <<'EOF'
  > module Shadow = struct
  >   module Bigint = struct let of_int _ = false end
  >   let number = 123456789012345678901234567890Z
  > end
  > let result = Bigint.add Shadow.number 1Z
  > EOF

  $ $MERLIN single errors -filename bigints.ml <bigints.ml | jq -c '.value | map(.message)'
  []

  $ $MERLIN single type-enclosing -position 5:5 -filename bigints.ml <bigints.ml | jq -c '.value | map(.type)'
  ["Bigint.t"]
