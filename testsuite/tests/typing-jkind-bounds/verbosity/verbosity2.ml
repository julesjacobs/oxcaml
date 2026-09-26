(* TEST
 flags = "-kind-verbosity 2";
 expect;
*)

(* CR: We're failing to display top modal bounds and implied modal bounds. *)

(* CR: We should show non-top modal bounds first. *)

type t : value
[%%expect {|
type t
|}]

type t : immutable_data
[%%expect {|
type t
  : value non_float non_null
      mod forkable
          unyielding
          many
          total
          immutable
          stateless
          portable
          contended
          local
          unique
          static
          ghost
          internal
|}]

type t : immediate
[%%expect {|
type t
  : value non_pointer non_null
      mod global
          many
          total
          immutable
          forkable
          unyielding
          aliased
          stateless
          portable
          contended
          external_
          static
          ghost
|}]

type t : float64
[%%expect {|
type t
  : float64
      mod external_
          local
          unforkable
          yielding
          once
          unique
          partial
          stateful
          read_write
          nonportable
          uncontended
          static
          ghost
|}]

type t : any
[%%expect {|
type t
  : any
      mod local
          unforkable
          yielding
          once
          unique
          partial
          stateful
          read_write
          nonportable
          uncontended
          static
          ghost
          internal
|}]

type t : value mod portable
[%%expect {|
type t
  : value separable non_null
      mod portable
          local
          unforkable
          yielding
          once
          unique
          partial
          stateful
          read_write
          uncontended
          static
          ghost
          internal
|}]

type t : value mod stateless
[%%expect {|
type t
  : value separable non_null
      mod stateless
          portable
          local
          unforkable
          yielding
          once
          unique
          partial
          read_write
          uncontended
          static
          ghost
          internal
|}]

type 'a t : immutable_data with 'a
[%%expect {|
type 'a t
  : value non_float non_null
      mod forkable
          unyielding
          many
          total
          immutable
          stateless
          portable
          contended
          local
          unique
          static
          ghost
          internal
      with 'a
|}]

type ('a : immutable_data) t
[%%expect {|
type ('a
     : value non_float non_null
         mod forkable
             unyielding
             many
             total
             immutable
             stateless
             portable
             contended
             local
             unique
             static
             ghost
             internal)
     t
|}]

type ('a : value mod stateless) t
[%%expect {|
type ('a
     : value separable non_null
         mod stateless
             portable
             local
             unforkable
             yielding
             once
             unique
             partial
             read_write
             uncontended
             static
             ghost
             internal)
     t
|}]

type 'a t : value mod portable external_ with 'a @@ external_
[%%expect {|
type 'a t
  : value separable non_null
      mod portable
          external_
          local
          unforkable
          yielding
          once
          unique
          partial
          stateful
          read_write
          uncontended
          static
          ghost
      with 'a @@ external_
|}]

type 'a t : value mod external_ with 'a @@ external_
[%%expect {|
type 'a t
  : value separable non_null
      mod external_
          local
          unforkable
          yielding
          once
          unique
          partial
          stateful
          read_write
          nonportable
          uncontended
          static
          ghost
|}]

type 'a t : immutable_data with 'a @@ external_
[%%expect {|
type 'a t
  : value non_float non_null
      mod forkable
          unyielding
          many
          total
          immutable
          stateless
          portable
          contended
          local
          unique
          static
          ghost
          internal
      with 'a
|}]
