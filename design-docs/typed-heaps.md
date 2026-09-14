# Typed heaps

`'a Pref.heap` is a finite map from locations to values of type `'a`.
`'a Pref.token` owns cells with that payload type. `Pref.own` preserves the
parameter; reading, writing, splitting and joining cannot change it.
An operation returning a value of type `'v` and ownership of `'a` cells returns
`('v, 'a) Pref.step`.

Keep ownership of different payload types in separate tokens. A list operation
can consume and return its `node option Pref.token` while its caller retains an
`int Pref.token`. Same-type frames still use `Pref.join` and `Pref.split`.
There is no universal payload type or unchecked conversion between heaps.

## Total elimination

Vox already makes projection and pattern matching on ordinary recursive types
partial. Checked `[@@inductive]` variants permit the restricted recursive
elimination needed for structural proofs; their values cannot be cyclically
initialized.

Abstract type constructors must preserve this boundary. Their arguments count
as possible recursive dependencies, even when their representation is hidden.
For example, projecting `run` from this record is partial:

```ocaml
type callback = {
  run : callback Pref.heap -> bool @@ total
}
```

The callback may exist as runtime data, but it cannot be extracted and applied
in a total proof of the heap diagonal. GADT arguments also count conservatively:
an existential equality must not hide a dependency on the enclosing type.

## Recursive initialization

An unfinished recursive value cannot certify its own totality. Total closures
and total record or constructor slots must be independent of the unfinished
recursive group. The check follows dependencies through local bindings,
containers and modules, including delayed dependencies. Total kind bounds on
existential constructor payloads count too.

Ordinary partial recursive closures and independent total callbacks remain
available. Single recursive functions retain their structural or explicit
termination check. Closure totality is fixed before the initialization check
uses it, so subsequent inference cannot turn an accepted partial closure into
a total closure.

## Passive handles

`Pref.t` carries a checked `[@@phantom_parameters]` guarantee. A handle's
logical identity does not contain its payload. Runtime reads require ownership
and are partial. This permits
total projection from node records containing handles to other nodes.

The guarantee is stored in compiled interfaces. The compiled-artifact format
version changes so older interfaces cannot be read as carrying this guarantee.
A concrete declaration may
claim it only when its parameters do not occur in its representation, except
under another constructor with the same guarantee. Signature matching and
`with type` constraints preserve the guarantee. Extensible types, GADTs and
recursive module signatures cannot claim it. Abstract primitive declarations
are part of the trusted interface, just like their external operations.

Heaps and ownership tokens do not have this guarantee: heap lookup and token
observation expose information about the payload. A wrapper cannot claim
phantom parameters merely because its runtime representation is erased.

Records containing independent total callbacks, such as `unit -> unit`, remain
valid heap payloads. Higher-order SMT encoding is separate from totality:
unsupported value equations cannot be assumed or used as goals. Allocation
exposes its membership fact explicitly so ordinary permission checks do not
depend on encoding the stored callback.
