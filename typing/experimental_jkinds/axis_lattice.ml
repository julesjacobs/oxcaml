(* Axis lattice specialized from the generic product lattice. *)
module T = Product_lattice.Make (struct
  (* Align with all jkinds axes in OxCaml, in the same order
     as typing/jkind_axis.ml Axis_set.axis_index:
     0 Areality (Regionality): Global, Regional, Local -> 3
     1 Linearity: Many, Once -> 2
     2 Uniqueness (monadic): Unique, Aliased -> 2
     3 Portability: Portable, Nonportable -> 2
     4 Contention (monadic): Contended, Shared, Uncontended -> 3
     5 Yielding: Yielding, Unyielding -> 2
     6 Statefulness: Stateless, Observing, Stateful -> 3
     7 Visibility (monadic): Immutable, Read, Read_write -> 3
     8 Externality: External, External64, Internal -> 3
     9 Nullability: Non_null, Maybe_null -> 2
     10 Separability: Non_float, Separable, Maybe_separable -> 3
  *)
  let axis_sizes = [| 3; 2; 2; 2; 3; 2; 3; 3; 3; 2; 3 |]
end)

include T

let to_string = T.to_string
