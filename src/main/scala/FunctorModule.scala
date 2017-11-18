import scala.language.higherKinds

import Algebra.{Tree, Branch, Leaf}
import Algebra.Func
import Algebra.Id

object FunctorModule {

  trait Functor[F[_]] {
    def map[A, B]: F[A] => (A => B) => F[B]
    
    def lift[A, B]: (A => B) => F[A] => F[B] =
      f => fa => map(fa)(f)
  }

  object Functor {
    def apply[F[_]](implicit F: Functor[F]): Functor[F] = F
  }

  implicit final class FunctorSyntax[F[_]: Functor, A](fa: F[A]) {
    def map[B]: (A => B) => F[B] =
      f => Functor[F].map(fa)(f)
  }

  sealed trait FunctorLaws[F[_]] {

    implicit def F: Functor[F]
    
    def mapPreservesIdentity[A]: F[A] => Boolean =
      fa => (fa map identity[A]) == fa

    def mapPreservesComposition[A, B, C]: F[A] => (A => B) => (B => C) => Boolean =
      fa => f => g => (fa map (g compose f)) == (fa map f map g)
  }

  sealed trait FunctorLawsNoInfix[F[_]] {

    implicit def F: Functor[F]
  
    def mapPreservesIdentity[A]: F[A] => Boolean =
      fa => F.map(fa)(identity[A]) == fa

    def mapPreservesComposition[A, B, C]: F[A] => (A => B) => (B => C) => Boolean =
      fa => f => g => F.map(fa)(g compose f) == F.map(F.map(fa)(f))(g)
  }

  object FunctorLaws {
    def apply[F[_]](implicit FI: Functor[F]): FunctorLaws[F] = 
      new FunctorLaws[F] { def F = FI }
  }
  
  object FunctorLawsNoInfix {
    def apply[F[_]](implicit FI: Functor[F]): FunctorLawsNoInfix[F] =
      new FunctorLawsNoInfix[F] { def F = FI }
  }

  object FunctorInstances {

    implicit val listFunctor: Functor[List] =
      new Functor[List] {
        def map[A, B]: List[A] => (A => B) => List[B] =
          fa => f => fa map f
      }

    implicit val optionFunctor: Functor[Option] =
      new Functor[Option] {
        def map[A, B]: Option[A] => (A => B) => Option[B] =
          fa => f => fa map f
      }

    implicit def eitherFunctor[E]: Functor[Either[E, ?]] =
      new Functor[Either[E, ?]] {
        def map[A, B]: Either[E, A] => (A => B) => Either[E, B] =
          fa => f => fa map f
      }

    implicit def functionFunctor[X]: Functor[Func[X, ?]] =
      new Functor[Func[X, ?]] {
        def map[A, B]: Func[X, A] => (A => B) => Func[X, B] =
          fa => f => Func[X, B](f compose fa.apply)
      }

    implicit val treeFunctor: Functor[Tree] =
      new Functor[Tree] {
        def map[A, B]: Tree[A] => (A => B) => Tree[B] =
          fa => f => fa match {
            case Branch(l, r) => Branch(map(l)(f), map(r)(f))
            case Leaf(v) => (Leaf.apply[B] _ compose f)(v)
          }
      }

    implicit val idFunctor: Functor[Id] =
      new Functor[Id] {
        def map[A, B]: Id[A] => (A => B) => Id[B] =
          fa => f => f(fa)
      }
  }
}
