(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-principal -c";
 ocamlc.byte;
*)

type _ gadt = Int : int gadt

let plain : type a. a gadt -> a = fun Int -> 3

let object_case =
  object (self)
    method private value = 3
    method get : type a. a gadt -> a = fun Int -> (self#value : int)
  end

let nested_capture (outer : int) =
  let inner (value : int) : unit{ outer = outer && value = value } = () in
  inner

let nested_capture_used = nested_capture 1 2

let nested_unreferenced (outer : int) =
  let inner (value : int) : unit{ value = value } = () in
  let _ = outer in
  inner

let nested_twice (outer : int) =
  let middle (middle_value : int) =
    let inner (value : int)
        : unit{
            outer = outer
            && middle_value = middle_value
            && value = value
          } =
      ()
    in
    inner
  in
  middle
