---
layout: documentation-page
collectionName: Modes
title: Intro
---

<style>
.table {
    width: fit-content;
    margin-left: auto;
    margin-right: auto;
    margin-bottom: 20px;
    border-style: solid;
    border-color: blue;
    border-radius: 25px;
    padding: 15px;
    text-align: center;
}
.table td { padding: 0 5px; }
.table td:first-child, { padding-left: 0; }
.table td:last-child, { padding-right: 0; }
</style>

# Introduction to the Mode System

Modes are deep properties of values that are tracked by the OxCaml
compiler. Like types, they are inferred from definitions and checked for
consistency. (We use the term "type checking" to include both traditional type
checking/inference and mode checking/inference.) Modes have similarities and
relationships with types, but remain distinct: types are not modes, modes are
not types, types do not have modes, and modes do not have types. Types describe
what the data *is*, while modes describe how it is *used*.

Each mode is associated with a particular operation that may be performed on a
value. The mode may be a *past* mode, which tracks whether the operation has
happened to this value in the past; or a *future* mode, which tracks whether the
operation is allowed to happen to this value in the future. Modes are deep; when
attached to structured data, they apply to components, recursively. (OxCaml's
*modality* feature cuts off the deepness of a mode; modalities can be placed on
record and constructor fields.)

Just like a value has a type, a value in OxCaml also has a mode. Types do *not*
have a mode. That is, we do not have a type `string @ local` (say), but rather
if we have `(x : string @ local)`, then `x` has type `string` and is at mode
`local`. Modes also appear in the argument and return slots of a function type,
so we can have `string @ local -> string option @ global` to describe a function
whose argument will have the `local` mode and whose return will have the
`global` mode. These modes appear in the function's type but are associated with
the function's behavior, not the argument or result types (that is, there is
still no `string @ local` or `string option @ global`). Modes are considered
part of the type system; the type checker in OxCaml additionally checks for
correct usage of modes.

This page shows the modes that are currently supported. Each mode belongs to a
modal *axis* determined by the operation it tracks and whether it is a past or
future mode. The axes on this page are arranged with the least mode at the
bottom and the greatest mode at the top.

The type system supports *submoding*: values may move freely to greater modes
(which typically restrict what can be done with those values) but not to lesser
modes. Additionally, the type system knows that some types don't have
interesting interactions with some modes. Such types are said to *mode cross* on
those axes, which means values of these types may freely move in either
direction on the axis. The sections for each axis below describe which types can
mode cross on that axis.

Each axis has a *legacy* mode, shown in bold. This is the "default" mode, and is
chosen to make the modal type system backwards compatible with legacy OCaml
programs.

* [Modes for scope (locality)](#locality)
* [Modes for moving between threads (portability and
  contention)](#portability-contention)
* [Modes for aliasing (uniqueness and linearity)](#uniqueness-linearity)

# Modes for scope {#locality}

## Future modes: Locality

|------------|
| local      |
| `|`        |
| **global** |
{: .table }

Locality is a future axis that tracks whether a value is allowed to escape its
*region*. Regions are scopes created by standard OCaml language constructs: each
function's body is a region, as are loop bodies.

The type checker does not allow values that are *local* to escape their scope
(for example, by being returned from a function or stored in a global
ref). Values that are *global* may freely escape their scope. The compiler may
stack allocate values that are local (but does not guarantee to do so).

There is a caveat to the scoping restriction: for some types such as `int`, the
type system does allow *local* values to escape (i.e., they may be used as
global even when they are local). The basic idea is that if values of the type
are not allocated in memory at runtime (e.g., they are passed directly via
registers), then it is safe to let them escape their region. This behavior is
called *mode crossing* on the locality axis.

If mode crossing is not desired for a type even though it would typically mode
cross, then type abstraction can be used to prevent its values from escaping
(i.e., to prevent them from mode crossing the locality axis). For example, even
though a file descriptor is commonly represented as an `int` at runtime, type
abstraction can be used to hide this fact from the type system such that a
function that takes a file descriptor as an argument cannot let it escape.

See also the [documentation on locality and stack
allocation](../../stack-allocation/intro).

# Modes for moving between threads {#portability-contention}

## Past modes: Contention

|--------------------|
| contended          |
|       `/` | `\`    |
| corrupted | shared |
|       `\` | `/`    |
| **uncontended**    |
{: .table .merge-table-cells}

Contention is a past axis that tracks whether a value has been shared between
threads. A value is *contended* if another thread can both read and write to it,
*shared* if multiple threads have read-only access to it, *corrupted* if multiple
threads have write-only access to it, and *uncontended* otherwise. Note that *corrupted*
and *shared* are incomparable: neither is a submode of the other.

To enforce data race freedom, the typechecker does not permit reading or writing
unprotected mutable portions of contended values. (Types like `Atomic.t` protect
mutable values from data races and allow contended values to still retain
mutable components.) The unprotected mutable portions of shared values
may be read, but not written to. The unprotected mutable portions of corrupted
values may be written to, but not read from. Uncontended values may be accessed
and mutated freely.

Contention is irrelevant for types that are deeply immutable. Values of such
types *mode cross* on the contention axis; they may be used as uncontended even
when they are contended.

## Future modes: Portability

|-------------------------|
| **nonportable**         |
|       `/` | `\`         |
| shareable | corruptible |
|       `\` | `/`         |
| portable                |
{: .table .merge-table-cells}

Portability is a future axis that tracks whether a value is allowed to move across
thread boundaries. Functions that capture uncontended state are *nonportable*,
so cannot escape the current thread. Functions that capture shared state are
*shareable*, so may be executed in parallel. Functions that only close over
corrupted values are *corruptible*. Functions that capture all values at
contended are *portable*, so may execute concurrently. Note that *shareable* and
*corruptible* are incomparable: neither is a submode of the other.

Notably, it is generally safe to send mutable data *itself* to other threads,
because it will then be *contended*, so the mutable portions will be
inaccessible. What is scary is to send a function that *captures* uncontended
mutable data to another thread, because the captured data would remain
uncontended even when the function is shared. When the second thread runs the
function, both threads would be accessing the same uncontended mutable state (a
data race!).

Portability is irrelevant for types that do not contain functions. Values of
such types *mode cross* on the portability axis; they may be used as portable
even when they are nonportable.

## Future modes: Forkable

|----------------|
| unforkable     |
| `|`            |
| **forkable**   |
{: .table }

Forkable is a future axis that tracks whether a function is permitted to access
shared values in its parent stack. See [parallelism](../../parallelism/intro/).

Forkable has different defaults depending on the locality axis: *global* values are
defaulted to *forkable*, while *local* values are defaulted to *unforkable*.
More documentation on mode implications is available [here](../../kinds/syntax).

Forkable is irrelevant for types that do not contain functions, and values of such types
*mode cross* on the forkable axis; they may be used as forkable even when they are
unforkable.

# Modes for aliasing {#uniqueness-linearity}

## Past modes: Uniqueness

|-------------|
| **aliased** |
| `|`         |
| unique      |
{: .table }

Uniqueness is a past axis that tracks whether there are multiple references to a
value. A value is *unique* if there is only one reference to it, and *aliased*
otherwise.

A function that accepts a unique argument effectively consumes this argument,
since the caller will only be able to supply arguments they have no other
references to. This can be used to implement APIs like safe memory allocators
that ensure no use-after-free bugs.

In the future, we will implement *overwriting* on unique values. Overwriting is
an optimization that reuses the memory of a value in place, rather than
allocating a new copy. For example, we will be able to write a `List.map` that
reuses the memory in place, rather than allocating a new list, when we know no
other references to this list exist.

Uniqueness is irrelevant for types that don't contain any memory locations
subject to overwriting (even though we have not yet implemented overwriting).
Values of such types *mode cross* on the uniqueness axis; they may
be used as unique even when they are aliased.

For example, types which do not involve data allocated on the OCaml heap mode
cross on this axis, so all types that mode cross locality also mode cross
uniqueness. Some other types that we don't plan to support overwriting for can
also mode cross uniqueness, like functions.

See also the [documentation on uniqueness and
linearity](../../uniqueness/intro/).

## Future modes: Linearity

|----------|
| once     |
| `|`      |
| **many** |
{: .table }

Linearity is a future axis that tracks whether a function is permitted to be
aliased.  Values that are *many* may used multiple times, while values that are
*once* may only be used once.

Like portability, linearity is about functions: its purpose is to track unique
values in closures. A closure that captures a unique value is once, ensuring one
can not create multiple references to the unique value by using the function
multiple times.

Linearity is irrelevant for types that do not contain functions. Values of such
types *mode cross* on the linearity axis; they may be used as many even when
they are once.

See also the [documentation on uniqueness and
linearity](../../uniqueness/intro/).

# Modes for effects {#yielding}

## Future modes: Yielding

|----------------|
| yielding       |
| `|`            |
| **unyielding** |
{: .table }

Yielding is a future axis that tracks whether a function is permitted to perform
effects that will be handled in its parent stack. See [the OCaml Manual entry
for effect handlers](https://ocaml.org/manual/5.3/effects.html).

Yielding has different defaults depending on the locality axis: *global* values are
defaulted to *unyielding*, while *local* values are defaulted to *yielding*.
More documentation on mode implications is available [here](../../kinds/syntax).


Yielding is irrelevant for types that do not contain functions, and values of such types
*mode cross* on the yielding axis; they may be used as unyielding even
when they are yielding.

# Modes for purity {#visibility-statefulness}

## Past modes: Visibility

|----------------|
| immutable      |
|  `/` | `\`     |
| read | write   |
|  `\` | `/`     |
| **read_write** |
{: .table .merge-table-cells}

Visibility is a past axis that controls access to mutable portions of values.
It's similar to contention: the typechecker forbids accessing mutable fields of values
with *immutable* visiblity, forbids writing to mutable fields of values
with *read* visibility, and forbids reading from mutable fields of values with *write*
visibility. Unlike for contention, even thread-safe access is disallowed.

Visibility is irrelevant for types that are deeply immutable. Values of such
types *mode cross* on the visibility axis; they may be used as read_write even
when they are immutable.

## Future modes: Statefulness

|-------------------|
| **stateful**      |
|     `/` | `\`     |
| writing | reading |
|     `\` | `/`     |
| stateless         |
{: .table .merge-table-cells}

Statefulness is a future axis that tracks whether a function reads or writes to some
mutable state that it closes over (in other words, state that is not explicitly passed to it in an argument).

*Stateless* functions may not either read or write such state, *reading* functions can
only read it, and *writing* functions can only write it. *Stateful* functions have no
restrictions. Stateless closures capture all values at visibility *immutable*, while
reading closures capture all values at visibility *read*, and writing closures
capture all values at visibility *write*.

Statefulness is irrelevant for types that do not contain functions, and values of such
types *mode cross* on the statefulness axis; they may be used as stateless
even when they are stateful.

## Future modes: Totality

Totality is a two-point future axis, `total < partial`. A total closure cannot
call captured partial functions, recurse, loop, raise, use a non-exhaustive
pattern, or read or write mutable state. The restriction is relative to
function arguments: a total higher-order function may call its function
argument. Mutable allocation and returning mutable data are allowed.

### Inductive pattern matching

Pattern matching can itself introduce nontermination when a recursive type is
not inductive. For example, this definition has no syntactic recursion, but
evaluating `omega ()` repeatedly applies `delta` to `Roll delta`:

```ocaml
type knot = Roll of (knot -> int)

let (delta @ total) (Roll f as x) = f x
let (omega @ total) () = delta (Roll delta)
```

Constructor and record elimination in total code therefore requires either a
nonrecursive nominal type or a checked `[@@inductive]` declaration. Record
elimination includes record patterns and field projection. The checked
attribute allows only direct recursive fields and tuple components. It also
prevents recursive value definitions from constructing cycles:

```ocaml
type nat = Z | S of nat [@@inductive]

let (predecessor @ total) = function Z -> Z | S n -> n
let rec invalid = S invalid  (* rejected *)
```

As a conservative initial rule, ordinary recursive types without the attribute
cannot be destructured in total code. They can still be constructed, passed,
returned, or destructured by partial code. In particular, the rule prevents a
cyclic ordinary value such as `let rec ones = 1 :: ones` from being consumed by
a total pattern match.

Structural patterns have no declaration carrying the checked guarantee, so
they are also partial. This closes the specialization of a generic eliminator
to a recursive structural type:

```ocaml
let (unroll @ total) = function `Roll f -> f
type knot = [ `Roll of (knot -> int) ]
```

Pattern checks are deferred until all cases have fixed the matched type. Checks
with the same type and branch environment share one representation traversal,
so a match does not rescan an N-constructor declaration for each of its N
cases. Branches with distinct GADT environments remain separate.

The nonrecursive check follows transparent aliases and mutually recursive
module declarations. Neither an alias nor a module boundary may hide a cycle:

```ocaml
type alias = knot

module rec Left : sig type t = Left of Right.t end = Left
and Right : sig type t = Right of Left.t end = Right
```

Both `alias` and `Left.t` remain unavailable to constructor patterns in total
code. An `[@@inductive]` guarantee is also checked by signature inclusion, so a
module cannot claim the guarantee for an unchecked implementation type.

An abstract functor-parameter type is not evidence that a representation is
nonrecursive. A later application can identify that abstract type with the
datatype through a recursive-module constraint:

```ocaml
module type S = sig
  type payload
  type t = Roll of (payload -> int) [@@inductive]
end

module Eliminate (X : S) = struct
  let (unroll @ total) = function X.Roll f -> f  (* rejected *)
end
```

Without that rejection, a recursive-module constraint could close the abstract
dependency and recover the original counterexample:

```ocaml
module rec Closed :
  (S with type payload = Closed.t) = struct
  type payload = Closed.t
  type t = Roll of (payload -> int) [@@inductive]
end

module E = Eliminate (Closed)
let (delta @ total) x = E.unroll x x
let (omega @ total) () = delta (Closed.Roll delta)
```

The check therefore treats a reachable abstract functor-parameter type as an
unresolved cycle.

The declaration check also applies to recursive module signatures. The
recursive-signature path cannot justify an indirect occurrence in its own
datatype:

```ocaml
module rec Internal : sig
  type payload = Internal.t
  type t = Roll of (payload -> int) [@@inductive]  (* rejected *)
end = struct
  type payload = Internal.t
  type t = Roll of (payload -> int) [@@inductive]
end
```

Treating `Internal.t` as an unrelated nominal type would miss this equation.

The traversal also terminates when recursive modules transform type arguments
on each step. Such a cycle is rejected when the same declaration is reached
with different arguments:

```ocaml
module rec Left : sig
  type 'a t = Left of ('a * 'a) Right.t
end = Left
and Right : sig
  type 'a t = Right of 'a Left.t
end = Right

type root = Root of int Left.t
```

Following this representation produces `int Left.t`, then `(int * int)
Left.t`, and so on. Remembering only complete type expressions would never
reach a fixed point.

The cycle check follows instantiated representations rather than every type
argument. A phantom occurrence does not make the representation recursive:

```ocaml
type 'a phantom = Phantom
type t = Wrap of t phantom
```

Constructor patterns over `t` remain available in total code.

Likewise, a type parameter is a leaf of a nonrecursive representation:

```ocaml
type 'a box = Box of 'a
let (unbox @ total) = function Box x -> x
```

Destructuring the outer `box` takes one step for every instantiation of `'a`.
