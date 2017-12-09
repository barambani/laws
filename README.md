# Laws
[![Build Status](https://travis-ci.org/barambani/laws.svg?branch=master)](https://travis-ci.org/barambani/laws) [![codecov](https://codecov.io/gh/barambani/laws/branch/master/graph/badge.svg)](https://codecov.io/gh/barambani/laws)

Laws is a set of sample implementations of the fundamental functional programming abstractions, with the sole purpouse of demonstrating the definition and the property-based verification of their underlying algebraic structure's laws. They are implemented here as type classes for types of kind `*` and for higher order type operators of kind `* -> *` and, given the exemplificative nature of this work, they are strictly limited to the most basic, but still most important, ones. To notice that the implementations in this repository are not meant to be used in production code and, by no means, are intended as a state of the art in library design and modules composition. In more than one case, on the contrary, the semantic cohesion, the ease of understanding and the clarity, as reduction of boilerplate code, have been preferred to optimization and usability. For real life collections of production-ready functional programming abstractions, the suggestion is to have a look at libraries like [Scalaz](https://github.com/scalaz/scalaz) or [Cats](https://github.com/typelevel/cats).

## Structure
Currently the project includes the following type classes
```
- Semigroup
- Monoid
- Covariant Functor (Functor)
- Contravariant Functor (Contravariant)
- Invariant Functor (Invariant)
- Cartesian (*)
- Applicative Functor (Applicative)
- Monad

(*) No laws implemented
```
Each type class lives in its own **_Scala_** file called **_Module_** and every module provides

```
- the type class definition
- a set of instances
- a syntax extension
- a definition of the laws using syntax extensions
- a definition of the laws using the type class functions
```

An extra file with the supporting **_Algebra_** completes the project. The structure of the source code, as can be seen, is very linear and easy to navigate so, for further details, the best advise is to [look into the the code itself](https://github.com/barambani/laws/tree/master/src/main/scala).

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
[ [Code](https://github.com/barambani/laws/blob/master/src/main/scala/SemigroupModule.scala), [Laws Check](https://github.com/barambani/laws/blob/master/src/test/scala/SemigroupLawsCheck.scala), [Reference](https://en.wikipedia.org/wiki/Semigroup) ]

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
[ [Code](https://github.com/barambani/laws/blob/master/src/main/scala/MonoidModule.scala), [Laws Check](https://github.com/barambani/laws/blob/master/src/test/scala/MonoidLawsCheck.scala), [Reference](https://en.wikipedia.org/wiki/Monoid) ]

### Covariant Functor (Functor)
A *covariant functor* or *functor* is an abstract data type that has the capability for its vaules to be mapped over. More specifically, given a *functor* `fa` it's possible to obtain another *functor* `fb` with the same structure as `fa`, through the application of a *function* `f: a -> b` to every element in `fa`. To note, this is the first type class we meet that abstracts over an *higher order type operator* (or *type constructor*) and not over a *type*. This is because *functor* is an abstraction for *type containers* (in this case *first order kinded types* or types of kind `* -> *`) and not for regural types (or types of kind `*`). In Scala we can represent these concepts as
```scala
trait Functor[F[_]] {
  def map[A, B]: F[A] => (A => B) => F[B]
    
  def lift[A, B]: (A => B) => F[A] => F[B] =
    f => fa => map(fa)(f)
}
```
Notice that looking at the `lift[A, B]` function (`fmap` in Haskell) we can see another important capability that the *functor* has. It allows, in fact, to create a morphism from a function from `A` to `B` to a function from `F[A]` to `F[B]`, that is the same as saying that it allows to *lift* a function into the context `F[_]`.

For an instance of `Functor[F[_]]` to be a valid *functor*, the `map` operation must preserve the identity morphism for every `F[A]` in `A` and the composition of morphisms for every `F[A]` in `A` and every `f` from `A` => `B` and `g` from `B` => `C`
```scala
(fa: F[A]) => (fa map identity[A]) == fa
(fa: F[A], f: A => B, g: B => C) => (fa map (g compose f)) == (fa map f map g)
```

[ [Code](https://github.com/barambani/laws/blob/master/src/main/scala/FunctorModule.scala), [Laws Check](https://github.com/barambani/laws/blob/master/src/test/scala/FunctorLawsCheck.scala), [Reference](https://en.wikipedia.org/wiki/Functor) ]

### Contravariant Functor (Contravariant)
If we focus on the adaptive semantic of the *functor* clearly visible in its *Function1* instance
```scala
implicit def functionFunctor[X]: Functor[Func[X, ?]] =
  new Functor[Func[X, ?]] {
    def map[A, B]: Func[X, A] => (A => B) => Func[X, B] =
      fa => f => Func[X, B](f compose fa.apply)
  }
```
where it allows to transform the **output** type inside the *Function1* context, and if we keep reasoning along the same line, we can start noticing that we might want to adapt also the **input** type of that context. We are saying this here because the *contravariant functor* allows us to do exactly that. This is in fact the *Function1* instance for it
```scala
implicit def funcContravariant[Y]: Contravariant[Func[?, Y]] =
  new Contravariant[Func[?, Y]] {
    def contramap[A, B]: Func[B, Y] => (A => B) => Func[A, Y] =
      fb => f => Func[A, Y](fb.apply _ compose f)
  }
```
The reader might notice that satisfying just this very specific need wouldn't be worth the troubles to formalize an abstraction, and that's correct. *Contravariant functor* in fact doesn't work only in this particular scenario, on the contrary it generalizes this behavior and applies the adaptation in any other context that models a form of processing. To give an example, if we consider a `Show[B]` abstraction defined as
```scala
trait Show[B] {
  def show: B => String
}
```
that converts to string a type `B`, a *contravariant functor* for `Show` can adapt the `show` function to accept any other type in **input** as long as we can provide a *morphism* from this other type to `B`. It can do that because it can generate a `Show[A]` given an `A => B` and we actually don't have to provide the implementation for `def show: A => String` at all. Instead, all we have to do is use the `contramap` function like in the example below
```scala
val fb: Show[B] = Show[B]
val f: A => B = ???

// this gives us a working instance of Show[A] fully implemented
val fa: Show[A] = fb contramap f

val anA: A = ???
fa.show(anA) // Works fine
```
The *contravariant functor type class* is defined as
```scala
trait Contravariant[F[_]] {
  def contramap[A, B]: F[B] => (A => B) => F[A]
  
  def lift[A, B]: (A => B) => F[B] => F[A] =
    f => fb => contramap(fb)(f)
}
```
where the `contramap` function does all the magic. Any instance of it, to be valid, needs to abide by the following laws (that are actually the dual of the *functor*'s laws)
```scala
(fa: F[A]) => (fa contramap identity[A]) == fa
(fc: F[C], f: A => B, g: B => C) => (fc contramap (g compose f)) == (fc contramap g contramap f)
```
Observe that also *contravariant functor* provides a `lift[A, B]` function (that's actually what's called `contramap` in Haskell), and it can be simply considered, like in the case of *functor*, a way to lift a function into a context, but in an inverted way. 

[ [Code](https://github.com/barambani/laws/blob/master/src/main/scala/ContravariantModule.scala), [Laws Check](https://github.com/barambani/laws/blob/master/src/test/scala/ContravariantLawsCheck.scala), [Reference](https://en.wikipedia.org/wiki/Functor#Covariance_and_contravariance) ]

### Invariant Functor (Invariant)
### Cartesian
### Applicative Functor (Applicative)
### Monad
