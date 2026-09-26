(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* [type a.] scopes over refinement predicates, and under -principal. *)

let (same2 @ total) : type a. (x : a list) -> {r : a list | r === x} =
  fun x -> x;;
[%%expect{|
val same2 : (x : 'a list) -> {r : 'a list | r === x} = <fun>
|}]

let (same3 @ total) : type a. (x : a list) -> {r : a list | r === (x : a list)} =
  fun x -> x;;
[%%expect{|
val same3 : (x : 'a list) -> {r : 'a list | r === (x : 'a list)} = <fun>
|}]

let (same4 @ total) : (x : 'a list) -> {r : 'a list | r === (x : 'a list)} =
  fun x -> x;;
[%%expect{|
val same4 : (x : 'a list) -> {r : 'a list | r === (x : 'a list)} = <fun>
|}]

module M = struct
  let (empty @ total) : {l : 'a list | l === []} = []

  let (empty_is_nil @ total) : type a. unit -> {u : unit | (empty : a list) === []} =
    fun () -> ()

  let (named_in_body @ total) :
      type a. unit -> {u : unit | (empty : a list) === []} =
    fun () -> let _ = (empty : a list) in ()

  let (explicit @ total) : 'a. unit -> {u : unit | (empty : 'a list) === []} =
    fun () -> ()
end;;
[%%expect{|
module M :
  sig
    val empty : {l : 'a list | l === []}
    val empty_is_nil : unit -> {u : unit | (empty : 'a list) === []}
    val named_in_body : unit -> {u : unit | (empty : 'a list) === []}
    val explicit : unit -> {u : unit | (empty : 'a list) === []}
  end
|}]
