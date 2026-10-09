(* Library-private no-copy constructor (ADR-0003). Declared in the core library's dune
   [private_modules], so it is mechanically invisible to every downstream consumer: any
   reference from outside [oxsmt_core] fails to compile. Sound because [Iarr.t] is laid
   out as ['a array]; ownership of [a] transfers to the result and the caller must not
   retain or mutate [a] afterwards.

   Optional: the copying [Iarr.of_array]/[Iarr.of_list] are the default and the only thing
   the public interface promises. Nothing in core is required to use this; it exists only
   to skip a copy on a proven hot path. *)

external of_array : 'a array -> 'a Iarr.t = "%identity"
