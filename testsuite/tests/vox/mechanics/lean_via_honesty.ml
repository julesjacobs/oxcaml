(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* STAGE 3 fail-closed: the INCLUSION / honesty rule.  A via manifest
   denotes at its last map's target sort; it satisfies an interface's
   [refines (S)] claim ONLY when that sort equals [S].  Here the
   manifest is modelled at [iset] (via [elems]) but the signature claims
   [refines (ibag)] -- rejected, the message naming the manifest's OCaml
   target [via (elems : iset)] alongside the mismatched sorts. *)

type tree = Leaf | Node of tree * int * tree
type iset [@@vox.sort lean "ISet"]
type ibag [@@vox.sort lean "IBag"]

[%%vox.lean {lean|
inductive ISet where | nil : ISet
inductive IBag where | bnil : IBag
|lean}]

module M : sig
  type t : value refines (ibag)
  val mk : unit -> t
end = struct
  type t = tree{ bst _ } [@vox.via (elems : iset)]
  let mk : unit -> t = fun () -> assume_unchecked_ Leaf
end
[%%expect{|
type tree = Leaf | Node of tree * int * tree
type iset
type ibag
Lines 13-16, characters 6-3:
13 | ......struct
14 |   type t = tree{ bst _ } [@vox.via (elems : iset)]
15 |   let mk : unit -> t = fun () -> assume_unchecked_ Leaf
16 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = tree{ bst _ via (elems : iset) } val mk : unit -> t end
       is not included in
         sig type t val mk : unit -> t end
       Type declarations do not match:
         type t = tree{ bst _ via (elems : iset) }
       is not included in
         type t
       The second declares refines lean "IBag" where the first declares refines lean "ISet".
|}]
