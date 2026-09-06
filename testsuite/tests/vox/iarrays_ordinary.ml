(* TEST
 { expect; }
 { expect.opt; }
*)

let (mutable_elements @ total) (values : int ref iarray @ read_write) =
  Iarray.length values
;;

let (contended_elements @ total) (values : int ref iarray @ contended) =
  Iarray.length values
;;

let local_length (local_ values : int iarray) =
  Iarray.length values
;;

let (higher_order @ total) values =
  Iarray.iter (fun _ -> ()) values;
  Iarray.fold_left (fun count _ -> count + 1) 0 values
;;

let fails _ = failwith "callback"
let partial_callback values = Iarray.iter fails values;;
[%%expect{|
val mutable_elements : int ref iarray -> int = <fun>
val contended_elements : int ref iarray @ contended -> int = <fun>
val local_length : int iarray @ local -> int = <fun>
val higher_order : ('a : value_maybe_null). 'a iarray -> int = <fun>
val fails : 'a -> 'b = <fun>
val partial_callback : ('a : value_maybe_null). 'a iarray -> unit = <fun>
|}]
