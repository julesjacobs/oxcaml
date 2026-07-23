type point =
  { x : int;
    y : int
  }

let false_field (n : int) : point{ _.x = n + 1 } =
  { x = n;
    y = 0
  }
