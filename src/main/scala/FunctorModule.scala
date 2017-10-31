import scala.language.higherKinds

import Algebra.{Tree, Branch, Leaf}
import Algebra.FuncFromIntTo
import Algebra.Id

object FunctorModule {

  trait Functor[F[_]] {
    
    def map[A, B]: F[A] => (A => B) => F[B]
    
    def lift[A, B]: (A => B) => F[A] => F[B] =
      f => fa => map(fa)(f)
  }

  object Functor {
    def apply[F[_]](implicit INST: Functor[F]): Functor[F] = INST
  }

  implicit final class FunctorSyntax[F[_]: Functor, A](fa: F[A]) {
    def map[B]: (A => B) => F[B] =
      f => Functor[F].map(fa)(f)
  }

  trait Laws {
    
    def mapPreservesIdentity[F[_]: Functor, A]: F[A] => Boolean =
      fa => (fa map identity[A]) == fa

    def mapPreservesComposition[F[_]: Functor, A, B, C]: F[A] => (A => B) => (B => C) => Boolean =
      fa => f => g => (fa map (g compose f)) == (fa map f map g)
  }

  trait LawsNoInfix {
  
    def mapPreservesIdentity[F[_], A](implicit FF: Functor[F]): F[A] => Boolean =
      fa => FF.map(fa)(identity[A]) == fa

    def mapPreservesComposition[F[_], A, B, C](implicit FF: Functor[F]): F[A] => (A => B) => (B => C) => Boolean =
      fa => f => g => FF.map(fa)(g compose f) == FF.map(FF.map(fa)(f))(g)
  }

  object Laws extends Laws
  object LawsNoInfix extends LawsNoInfix

  object FunctorInstances {

    implicit val seqFunctor: Functor[Seq] = 
      new Functor[Seq] {
        def map[A, B]: Seq[A] => (A => B) => Seq[B] =
          fa => f => fa map f
      }

    implicit val optionFunctor: Functor[Option] =
      new Functor[Option] {
        def map[A, B]: Option[A] => (A => B) => Option[B] =
          fa => f => fa map f
      }

    implicit val functionFromIntFunctor: Functor[FuncFromIntTo] =
      new Functor[FuncFromIntTo] {
        def map[A, B]: FuncFromIntTo[A] => (A => B) => FuncFromIntTo[B] =
          fa => f => FuncFromIntTo[B](f compose fa.apply)
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

    implicit val listFunctor: Functor[List] =
      new Functor[List] {
        def map[A, B]: List[A] => (A => B) => List[B] =
          fa => f => fa map f
      }
  }
}
