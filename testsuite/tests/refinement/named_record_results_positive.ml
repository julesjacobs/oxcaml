type point =
  { x : int;
    y : int
  }

let make (n : int) : point{ _.x = n && _.y = n + 1 } =
  { x = n + 0;
    y = n + 1
  }

let reversed (n : int) : point{ _.x = n && _.y = n + 1 } =
  { y = n + 1;
    x = n + 0
  }

let counter = ref 0

let next () =
  incr counter;
  !counter

let make_with_effect (n : int) : point{ _.x = n } =
  { x = n;
    y = next ()
  }

let update (base : point) (n : int)
    : point{ _.x = n && _.y = base.y } =
  { base with x = n + 0 }

type triple =
  { first : int;
    second : int;
    kept : int
  }

let update_two (base : triple) (first : int) (second : int)
    : triple{
        _.first = first
        && _.second = second
        && _.kept = base.kept
      } =
  { base with
    second = second + 0;
    first = first + 0
  }

module Abstract_result : sig
  type t : immutable_data
  val witness : t
  val make : unit -> t{ _ = witness }
end = struct
  type t =
    { value : int;
      padding : int
    }

  let witness : t{ _.value = 0 } =
    { value = 0 + 0;
      padding = next ()
    }

  let make () : t{ _ = witness } = witness
end


module Abstract_field (Key : sig type t : immutable_data end) = struct
  type box =
    { payload : Key.t;
      tag : int
    }

  let make (payload : Key.t) (tag : int) : box{ _.tag = tag } =
    { payload;
      tag = tag + 0
    }
end
