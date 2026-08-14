(* TEST
 flags = "-dtypedtree -dno-locations -dno-unique-ids";
 expect;
*)

module X = struct end
[%%expect{|
[
  structure_item
    Tstr_module (Present)
    X
      module_expr
        Tmod_structure
        []
]

module X : sig end
|}]

module X = struct end [@foo]
[%%expect{|
[
  structure_item
    Tstr_module (Present)
    X
      module_expr
        attribute "foo"
          []
        Tmod_structure
        []
]

module X : sig end
|}]

module Y = X
[%%expect{|
[
  structure_item
    Tstr_module (Absent)
    Y
      module_expr
        Tmod_ident "X"
]

module Y = X
|}]

module type T = sig module Y = X end
[%%expect{|
[
  structure_item
    Tstr_modtype "T"
      module_type
        Tmty_signature
        [
          signature_item
            Tsig_module (Absent)
            Y
              module_type
                Tmty_alias "X"
        ]
        join_const(unique,uncontended,physical,read_write,static);meet_const(local,once,nonportable,partial,unforkable,yielding,stateful,erased)
        []
]

module type T = sig module Y = X end
|}]
