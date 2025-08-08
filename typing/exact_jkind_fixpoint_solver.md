# Computing jkinds exactly

Observation by reisenberg:
    Any type constructor ('a, 'b) t acts like A * M1('a) * M2('b) where M1, M2 are modalities, and A is a type.
    In terms of lattices, it acts like A \/ (kind('a) /\ M1) \/ (kind('b) /\ M2) since all modalities are joins with a constant.
    In terms of OxCaml syntax, each type constructor's kind can be put in normal form  ('a, 'b) t : kind(A) with M1('a) with M2('b).

High level idea: find the actual A, M1, M2 for each type constructor, by setting up a system of equations and solving for the unknowns (least solution for concrete types, greatest solution for abstract types).

## Notation and terminology

I like to use notation x + y for x \/ y and x y for x /\ y.

Intuition:
  x + y means the worst kind of x and y
  x y means the best kind of x and y
  x <= y means that x is a better kind (fewer requirements) than y
  x >= y means that x is at least as bad as y

We will represent the kind of a type constructor ('a, 'b) t as three *coefficients* t0, t1, t2, such that the kind of ('a, 'b) t is t0 + t1 'a + t2 'b
An n-ary type constructor has n+1 coefficients. Coefficients range over the lattice in question.

We assume that the lattice is distributive.

## Type definitions as a system of equations

Consider the following type definitions:

type ('a, 'b) t = F(t, q, 'a, 'b)
type 'a q = G(t, q, 'a)

Where F,G are some formulas. By reisenbergs observation, we can assume that the kinds of t and q can be written as:

kind(('a,'b) t) = t0 + t1 kind('a) + t2 kind('b)
kind('a q) = q0 + q1 kind('a)

Let's use the notation a = kind('a), b = kind('b).

[That it is true that the kinds of mutually recursive types can be written like this if base type constructors can is not entirely obvious. Vaguely: this is true because there are only finitely many kinds, so in the end we can find solutions to equations by iterating a finite number of times, and that way we can write all kinds in terms of basic operations, so the assumption follows.]

Given this assumption, and using reisenberg's observation again, we can normalize the right hand sides F(t, q, 'a, 'b) and G(t, q, 'a) to formulas of the form:

kind(G(t, q, 'a)) = T0(t0,t1,t2,q0,q1) + T1(t0,t1,t2,q0,q1) a + T2(t0,t1,t2,q0,q1) b
kind(F(t, q, 'a, 'b)) = Q0(t0,t1,t2,q0,q1) + Q1(t0,t1,t2,q0,q1) a

Where T0, T1, T2, Q0, Q1 are lattice formulas over t0,t1,t2,q0,q1.

The mutual type definitions now entail the following system of equations:

forall a, b. t0 + t1 a + t2 b = T0(t0,t1,t2,q0,q1) + T1(t0,t1,t2,q0,q1) a + T2(t0,t1,t2,q0,q1) b
forall a. q0 + q1 a = Q0(t0,t1,t2,q0,q1) + Q1(t0,t1,t2,q0,q1) a

## Solving the system

We want to "solve" this system of equations in the unknowns t0,t1,t2,q0,q1, that is, find the best kinds such that this system is satisfied.

We have the following lemma:

Lemma: (forall a,b. t0 + t1 a + t2 b <= t0' + t1' a + t2' b) <=> (t0 <= t0') /\ (t1 <= t1') /\ (t2 <= t2'), provided t0 <= t1, t0 <= t2, t0' <= t1', t0' <= t2'
Proof. Put in a=b=bot, a=bot,b=top, a=top,b=bot. Qed

This means that we can rewrite the system of equations as follows, eliminating the foralls:

t0 = T0(t0,t1,t2,q0,q1)
t1 = T1(t0,t1,t2,q0,q1)
t2 = T2(t0,t1,t2,q0,q1)
q0 = Q0(t0,t1,t2,q0,q1)
q1 = Q1(t0,t1,t2,q0,q1)

This is a system purely in terms of lattice unknowns, and we can find the best solution by iterating, starting from t0=t1=t2=q0=q1=bot.
The iteration will terminate very quickly (in general I think the number of iterations will never exceed the number of equations).

## Abstract types

With abstract types we need to be careful to avoid Benjamin Peters' soundness bug. We need the greatest solution instead of the least solution for abstract types.
What this means is unclear, illustrated by the following example. Suppose our lattice is the booleans, and we have the following system of equations:
  x = y
  y = x
And suppose x comes from and abstract type and y comes from a concrete type. We want the least solution in y and greatest solution in x.
But we have two such solutions! (x,y) = (0,0) and (x,y) = (1,1). In both solutions, y cannot be decreased without violating the system, and x cannot be increased without violating the system.

To make it sound, we need to stage the solution process:
- Given a concrete y(x) of the concrete kinds as a function of the abstract kinds, we want the least solution x of the system {x >= y(x), y(x) >= x}.

The iterative algorithm to solve the system can operate as follows:
- Initialize all the concrete kinds with the best kind, abstract kinds with the worst kind.
- Now as long as there is an equation that is violated, update its left hand side to get the current value of its right hand side, but *prioritize solving the equations of concrete types first*.

This should give a sound solution.



## Lattice fixpoint solver

As a component, we will implement a lattice fixpoint solver that can create a graph representing a system of equations with formulas consisting of joins, meets, constants, and unknowns. Some of the unknowns are optimistic and some are pessimistic.

We need to design an API to create the graph that will let us do it for a given type that may be directly recursive (through cyclic types a la -rectypes), and because the type may involve constructors that are part of a mutually recursive block of concrete and abstract types.

For each type constructor ('a, 'b) t we create three unknows t0, t1, t2 and set up the system of equations for them.

We have a base lattice (which is a concrete point on the modality axes) and on top of it we build a new lattice that represents a "linear combination" over type variables.
We have combinations such as a0 + a1 'a + a2 'b, where 'a and 'b are type variables.
The lattice operations distribute in the usual way.

To compute the lattice value of a type, we recurse into it, but we already store the unknown for it in a cache so that we can look it up when we encounter the type again, and return it directly. When we encounter a type constructor call (A, B) t, then instead of unfolding t's definition, we just compute recursively [(A, B) t] = t0 + t1 [A] + t2 [B]. This does entail setting up the system of equations for the t constructor.

In summary:
- We have an operation [T] on types that computes the lattice vector of the type, with a constant and coefficients for each of the unsolved type variables that occur in T.
- The lattice vector of a type constructor ('a, 'b) t is [T] where [T] is t's definition (with 'a,'b inside it).
- Upon encountering a type constructor call (A, B) t, we don't unfold t's definition. Rather, we use the lattice vector of the constructor and only recurse into A,B using [(A, B) t] = t0 + t1 [A] + t2 [B] where (t0,t1,t2) is the lattice vector of t.
- We keep track of a mapping from types to unknowns, so that we can look them up when we encounter the type again.

At the end of the day we obtain a system of equations in the base lattice unknowns, which we can solve iteratively.



## Non-iterative solution (later)

We can also solve such systems in a non-iterative way by eliminating equations one by one.
The last equation of the system can be put in the form:

  q1 = constant(t1,t2,t3,q0) + coefficient(t1,t2,t3,q0) q1

Where constant(t1,t2,t3,q0) and coefficient(t1,t2,t3,q0) are constant with respect to q1, but may still be formulas with respect to t1,t2,t3,q0.

If q1 came from a concrete type, we solve q1 := constant(t1,t2,t3,q0), and if it came from an abstract type, we solve q1 := constant(t1,t2,t3,q0) + coefficient(t1,t2,t3,q0).
We can substitute this into the rest of the system to reduce the number of equations by 1.
