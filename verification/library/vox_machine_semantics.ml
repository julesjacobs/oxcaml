let[@def] (signed @ total) (value : Bigint.t) =
  Bigint.sub
    (Bigint.modulo (Bigint.add value 4611686018427387904Z) 9223372036854775808Z)
    4611686018427387904Z

let (range @ total) (value : int) :
    {u : unit | -4611686018427387904Z <= Bigint.of_int value
      && Bigint.of_int value <= 4611686018427387903Z} @ ghost = ghost_ (())

let (add @ total) (left : int) (right : int) :
    {u : unit | Bigint.of_int (left + right) =
      signed (Bigint.add (Bigint.of_int left) (Bigint.of_int right))} @ ghost =
  ghost_ (
    signed_def (Bigint.add (Bigint.of_int left) (Bigint.of_int right));
    ())

let (sub @ total) (left : int) (right : int) :
    {u : unit | Bigint.of_int (left - right) =
      signed (Bigint.sub (Bigint.of_int left) (Bigint.of_int right))} @ ghost =
  ghost_ (
    signed_def (Bigint.sub (Bigint.of_int left) (Bigint.of_int right));
    ())

let (less_equal @ total) (left : int) (right : int) :
    {u : unit | (left <= right) = (Bigint.of_int left <= Bigint.of_int right)}
    @ ghost = ghost_ (())
