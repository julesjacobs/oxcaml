type pair =
  { first : int;
    second : int
  }

let counter = ref 0

let next () =
  incr counter;
  !counter

let distinct_calls () : pair{ _.first = _.second } =
  { first = next ();
    second = next ()
  }
