# Laws
[![Build Status](https://travis-ci.org/barambani/laws.svg?branch=master)](https://travis-ci.org/barambani/laws) [![codecov](https://codecov.io/gh/barambani/laws/branch/master/graph/badge.svg)](https://codecov.io/gh/barambani/laws)

Laws is a set of sample implementations of the fundamental functional programming abstractions, with the sole purpouse of demonstrating the definition and the property-based verification of their underlying algebraic structure's laws. They are implemented here as type classes types of kind `*` and for higher order type operators of kind `* -> *` and, given the exemplificative nature of this work, they are strictly limited to the most basic, but still most important, ones. To notice that the implementations in this repository are not meant to be used in production code and, by no means, are intended as a state of the art in library design and modules composition. In more than one case, on the contrary, the semantic cohesion, the ease of understanding and the clarity, as reduction of boilerplate code, have been preferred to optimization and usability. For real life collections of production-ready functional programming abstractions, the suggestion is to have a look at libraries like [Scalaz](https://github.com/scalaz/scalaz) or [Cats](https://github.com/typelevel/cats).

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
*Notice that the syntax extension allows us to use the notation `<>` for the `combine` operation. The right hand side of the expression above, in fact, is the actual body of the law definition in the code (see below)*
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
### Contravariant Functor (Contravariant)
### Invariant Functor (Invariant)
### Cartesian
### Applicative Functor (Applicative)
### Monad
