# Laws
[![Build Status](https://travis-ci.org/barambani/laws.svg?branch=master)](https://travis-ci.org/barambani/laws) [![codecov](https://codecov.io/gh/barambani/laws/branch/master/graph/badge.svg)](https://codecov.io/gh/barambani/laws)

Laws is a set of sample implementations of the fundamental functional programming abstractions, with the sole purpouse of demonstrating the definition and the property-based verification of their underlying algebraic structure's laws. They are implemented here as type classes for types of kind `*` and for higher order type operators of kind `* -> *` and, given the exemplificative nature of this work, they are strictly limited to the most basic, but still most important, ones. To notice that the implementations in this repository are not meant to be used in production code and, by no means, are intended as a state of the art in library design and modules composition. In more than one case, on the contrary, the semantic cohesion, the ease of understanding and the clarity, as reduction of boilerplate code, have been preferred to optimization and usability. For real life collections of production-ready functional programming abstractions, the suggestion is to have a look at libraries like [Scalaz](https://github.com/scalaz/scalaz) or [Cats](https://github.com/typelevel/cats).

## Structure
Currently the project includes the following type classes
```scala
- Semigroup
- Monoid
- Covariant Functor (Functor)
- Contravariant Functor (Contravariant)
- Invariant Functor (Invariant)
- Cartesian (*)
- Applicative Functor (Applicative)
- Monad

// (*) No laws implemented
```
Each type class lives in its own **_Scala_** file called **_Module_**. The module provides

  * the type class definition
  * a set of instances
  * a syntax extension
  * a definition of the laws using syntax extensions
  * a definition of the laws using the type class functions

An extra file containing the supporting **_Algebra_** is also given. For further details the best is to [navigate the code itself](https://github.com/barambani/laws/tree/master/src/main/scala).

## Implementation and Laws
### Semigroup
A *semigroup* consists of a *set* `A` (*type for us from now on*) and a binary operation (`combine`)
```scala
trait Semigroup[A] {
  def combine: (A, A) => A
}
```
where `combine` has to abide by the associativity law for every `a1`, `a2` and `a3` in `A`
```scala
(a1: A, a2: A, a3: A) => (a1 <> a2) <> a3 == a1 <> (a2 <> a3)

```
*Notice that the syntax extension allows us to use the notation `<>` for the `combine` operation. The right hand side of the expression above, in fact, is the actual body of the law definition in the code itself (see below)*
```scala
trait SemigroupLaws[A] {

  implicit def F: Semigroup[A]

  def combineAssociativity(a1: A, a2: A, a3: A): Boolean =
    (a1 <> a2) <> a3 == a1 <> (a2 <> a3)
}
```
[[Reference - Semigroup]](https://en.wikipedia.org/wiki/Semigroup)

### Monoid
A *monoid* is a specialization of a *semigroup*. To be a *monoid* any *semigroup* needs to define also an identity element 
```scala
trait Monoid[A] extends Semigroup[A] {
  val empty: A
}
```
that has to satisfy the identity law for every `a` in `A`
```scala
(a: A) => (a <> empty) == a && (empty <> a) == a
```
*Note that in the code, to access the `Monoid[A]` `empty` element we have to go through the implicit handle for the instance in use. This is not present in the expression above just to keep the read more fluent*
```scala
sealed trait MonoidLaws[A] extends SemigroupLaws[A] {

  implicit def F: Monoid[A]

  def emptyIdentity(a: A): Boolean =
    (a <> F.empty) == a && (F.empty <> a) == a
}
```
[[Reference - Monoid]](https://en.wikipedia.org/wiki/Monoid)

### Covariant Functor (Functor)
A *covariant functor* or *functor* is an abstract data type that has the capability for its vaules to be mapped over. More specifically, given a *functor* `fa` it's possible to obtain another *functor* `fb` with the same structure as `fa`, through the application of a *function* `f: a -> b` to every element in `fa`. To note, this is the first type class we meet that abstracts over an *higher order type operator* (or *type constructor*) and not over a *type*. This is because *functor* is an abstraction for *type containers* (in this case first order kinded types or types of kind `* -> *`) and not for regural types (or types of kind `*`). In Scala we can represent these concepts as
```scala
trait Functor[F[_]] {
  def map[A, B]: F[A] => (A => B) => F[B]
    
  def lift[A, B]: (A => B) => F[A] => F[B] =
    f => fa => map(fa)(f)
}
```
Notice that looking at the `lift` function (`fmap` in Haskell) we can see another important capability that the *functor* has. It allows, in fact, to create a morphism from a function from `A` to `B` to a function from `F[A]` to `F[B]`, that is the same as saying that it allows to *lift* a function into the context `F[_]`.

For an instance of `Functor[F[_]]` to be a valid *functor*, the `map` operation must preserve the identity morphism for every `F[A]` in `A` and the composition of morphisms for every `F[A]` in `A` and every `f` from `A` => `B` and `g` from `B` => `C`
```scala
(fa: F[A]) => (fa map identity[A]) == fa
(fa: F[A], f: A => B, g: B => C) => (fa map (g compose f)) == (fa map f map g)
```

[[Reference - Covariant Functor]](https://en.wikipedia.org/wiki/Functor)

### Contravariant Functor (Contravariant)
### Invariant Functor (Invariant)
### Cartesian
### Applicative Functor (Applicative)
### Monad
