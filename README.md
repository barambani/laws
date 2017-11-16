# Laws
[![Build Status](https://travis-ci.org/barambani/laws.svg?branch=master)](https://travis-ci.org/barambani/laws) [![codecov](https://codecov.io/gh/barambani/laws/branch/master/graph/badge.svg)](https://codecov.io/gh/barambani/laws)

Laws is a sample implementation of the fundamental functional programming abstractions, with the sole purpouse of allowing the definition and the property-based verification of their underlying algebraic structure's laws. Those abstractions are implemented as type classes for types of kind `*` and higher order type operators of kind `* -> *` and, given the exemplificative nature of this work, they are strictly limited to the most basic, but still, most important ones. To notice that the implementations in this repository are not meant to be used in production code and, by no means, are intended as a state of the art of library design and modules composition. In more than one case, on the contrary, the semantic cohesion, the ease of understanding and the clarity, intended as reduction of boilerplate code, have been preferred to optimization and usability. For real life collections of production ready functional programming abstractions, the suggestion is to have a look at libraries like [Scalaz](https://github.com/scalaz/scalaz) or [Cats](https://github.com/typelevel/cats).

## Structure
The type classes included are
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
and every one has its own **_Scala_** file called **_Module_**. As part of the implementation the following are provided:
```
- the type class definition
- a set of instances
- a syntax extension
- a definition of the laws that uses the syntax extension
- a definition of the laws that strictly uses the type class functions
```

## Laws
### Semigroup
A semigroup consists of a set (*type for us from now on*) and a binary operation ([see this for more](https://en.wikipedia.org/wiki/Semigroup)). Here is defined as
```scala
trait Semigroup[A] {
  def combine: (A, A) => A
}
```
and the `combine` operation has to abide by the associativity law as in
```scala
(a1: A, a2: A, a3: A) => (a1 <> a2) <> a3 == a1 <> (a2 <> a3)

```
Notice that the syntax extension allows us to use the notation `<>` for the combine operation.

### Monoid
A monoid is a specialization of a semigrup that adds to the structure and identity element ([more](https://en.wikipedia.org/wiki/Monoid)). Here is defined as 
```scala
trait Monoid[A] extends Semigroup[A] {
  def empty: A
}
```
and other than the operation associativity it has to respect also the identity law as in
```scala
(a: A) => (a <> empty) == a && (empty <> a) == a
```
