import scala.language.higherKinds

import Algebra.{Tree, Branch, Leaf}
import Algebra.Func
import Algebra.Id

object FunctorModule {

  trait Functor[F[_]] {
    def fmap[A, B]: F[A] => (A => B) => F[B]
    
    def lift[A, B]: (A => B) => F[A] => F[B] =
      f => fa => fmap(fa)(f)
  }

  object Functor {
    def apply[F[_]](implicit F: Functor[F]): Functor[F] = F
  }

  implicit final class FunctorSyntax[F[_]: Functor, A](fa: F[A]) {
    def fmap[B]: (A => B) => F[B] =
      f => Functor[F].fmap(fa)(f)
  }

  sealed trait FunctorLaws[F[_]] {

    implicit def F: Functor[F]
    
    def fmapPreservesIdentity[A]: F[A] => Boolean =
      fa => (fa fmap identity[A]) == fa

    def fmapPreservesComposition[A, B, C]: F[A] => (A => B) => (B => C) => Boolean =
      fa => f => g => (fa fmap (g compose f)) == (fa fmap f fmap g)
  }

  sealed trait FunctorLawsNoInfix[F[_]] {

    implicit def F: Functor[F]
  
    def fmapPreservesIdentity[A]: F[A] => Boolean =
      fa => F.fmap(fa)(identity[A]) == fa

    def fmapPreservesComposition[A, B, C]: F[A] => (A => B) => (B => C) => Boolean =
      fa => f => g => F.fmap(fa)(g compose f) == F.fmap(F.fmap(fa)(f))(g)
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

    implicit val seqFunctor: Functor[Seq] = 
      new Functor[Seq] {
        def fmap[A, B]: Seq[A] => (A => B) => Seq[B] =
          fa => f => fa map f
      }

    implicit val optionFunctor: Functor[Option] =
      new Functor[Option] {
        def fmap[A, B]: Option[A] => (A => B) => Option[B] =
          fa => f => fa map f
      }

    implicit def functionFunctor[X]: Functor[Func[X, ?]] =
      new Functor[Func[X, ?]] {
        def fmap[A, B]: Func[X, A] => (A => B) => Func[X, B] =
          fa => f => Func[X, B](f compose fa.apply)
      }

    implicit val treeFunctor: Functor[Tree] =
      new Functor[Tree] {
        def fmap[A, B]: Tree[A] => (A => B) => Tree[B] =
          fa => f => fa match {
            case Branch(l, r) => Branch(fmap(l)(f), fmap(r)(f))
            case Leaf(v) => (Leaf.apply[B] _ compose f)(v)
          }
      }

    implicit val idFunctor: Functor[Id] =
      new Functor[Id] {
        def fmap[A, B]: Id[A] => (A => B) => Id[B] =
          fa => f => f(fa)
      }

    implicit val listFunctor: Functor[List] =
      new Functor[List] {
        def fmap[A, B]: List[A] => (A => B) => List[B] =
          fa => f => fa map f
      }
  }
}
