import scala.language.higherKinds

import Algebra.Show

object ContravariantLaws {

  trait Contravariant[F[_]] {
    
    def contramap[A, B]: F[B] => (A => B) => F[A]

    def lift[A, B]: (A => B) => F[B] => F[A] =
      f => fb => contramap(fb)(f)
  }

  object Contravariant {
    def apply[F[_]](implicit INST: Contravariant[F]): Contravariant[F] = INST
  }

  implicit final class ContravariantSyntax[F[_]: Contravariant, B](fb: F[B]) {
    def contramap[A]: (A => B) => F[A] =
      f => Contravariant[F].contramap(fb)(f)
  }

  sealed trait Laws {
  
    def contramapPreservesIdentity[F[_]: Contravariant, A]: F[A] => Boolean =
      fa => (fa contramap identity[A]) == fa

    def contramapPreservesComposition[F[_]: Contravariant, A, B, C]: F[C] => (A => B) => (B => C) => Boolean =
      fc => f => g => (fc contramap (g compose f)) == (fc contramap g contramap f)
  }

  sealed trait LawsNoInfix {
    
    def contramapPreservesIdentity[F[_], A](implicit CF: Contravariant[F]): F[A] => Boolean =
      fa => CF.contramap(fa)(identity[A]) == fa

    def contramapPreservesComposition[F[_], A, B, C](implicit CF: Contravariant[F]): F[C] => (A => B) => (B => C) => Boolean =
      fc => f => g => CF.contramap(fc)(g compose f) == CF.contramap(CF.contramap(fc)(g))(f)
  }

  object Laws extends Laws
  object LawsNoInfix extends LawsNoInfix

  object Instances {
  
    implicit lazy val showContravariant: Contravariant[Show] = 
      new Contravariant[Show] {
        def contramap[A, B]: Show[B] => (A => B) => Show[A] =
          sb => f => Show.newInstance[A] {
            a => (sb.show compose f)(a) 
          }
      }

  }
}
