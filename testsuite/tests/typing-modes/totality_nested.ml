(* TEST
 expect;
*)

module Nested = struct
  type 'a box = Box of 'a

  let (unbox @ total) (Box x) = x
  let (unbox_nested @ total) (Box x : int box box) = x
  let (unbox_nested_generic @ total) (x : int box box) = unbox x

  type 'a record_box = { boxed : 'a }

  let (unbox_record @ total) ({ boxed } : int record_box record_box) = boxed
  let (project_record @ total) (x : int record_box record_box) = x.boxed

  let (flatten @ total) (x : int option option) =
    match x with None -> None | Some y -> y
end
[%%expect{|
module Nested :
  sig
    type 'a box = Box of 'a
    val unbox : 'a box -> 'a
    val unbox_nested : int box box -> int box
    val unbox_nested_generic : int box box -> int box
    type 'a record_box = { boxed : 'a; }
    val unbox_record : int record_box record_box -> int record_box
    val project_record : int record_box record_box -> int record_box
    val flatten : int option option -> int option
  end
|}]
