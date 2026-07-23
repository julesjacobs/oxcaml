type choice =
  | First of int
  | Second of int

let second (value : int) : choice{ _ = Second value } = Second value

let wrong_index (value : int) : choice{ _ = First value } = Second value
