import Algebra.{->, Func, Show}

import scala.language.higherKinds

object ContravariantModule {

  trait Contravariant[F[_]] {
    
    def contramap[A, B]: F[B] => (A => B) => F[A]

    def lift[A, B]: (A => B) => F[B] => F[A] =
      f => fb => contramap(fb)(f)
  }

  object Contravariant {
    def apply[F[_]](implicit F: Contravariant[F]): Contravariant[F] = F
  }

  implicit final class ContravariantSyntax[F[_]: Contravariant, B](fb: F[B]) {
    def contramap[A]: (A => B) => F[A] =
      f => Contravariant[F].contramap(fb)(f)
  }

  sealed trait ContravariantLaws[F[_]] {

    implicit def F: Contravariant[F]
  
    def contramapPreservesIdentity[A]: F[A] => Boolean =
      fa => (fa contramap identity[A]) == fa

    def contramapPreservesComposition[A, B, C]: F[C] => (A => B) => (B => C) => Boolean =
      fc => f => g => (fc contramap (g compose f)) == (fc contramap g contramap f)
  }

  sealed trait ContravariantLawsNoSyntax[F[_]] {

    implicit def F: Contravariant[F]

    def contramapPreservesIdentity[A]: F[A] => Boolean =
      fa => F.contramap(fa)(identity[A]) == fa

    def contramapPreservesComposition[A, B, C]: F[C] => (A => B) => (B => C) => Boolean =
      fc => f => g => F.contramap(fc)(g compose f) == F.contramap(F.contramap(fc)(g))(f)
  }

  object ContravariantLaws {
    def apply[F[_]](implicit FI: Contravariant[F]): ContravariantLaws[F] =
      new ContravariantLaws[F] { def F = FI }
  }

  object ContravariantLawsNoSyntax {
    def apply[F[_]](implicit FI: Contravariant[F]): ContravariantLawsNoSyntax[F] =
      new ContravariantLawsNoSyntax[F] { def F = FI }
  }

  object ContravariantInstances {
  
    implicit val showContravariant: Contravariant[Show] = 
      new Contravariant[Show] {
        def contramap[A, B]: Show[B] => (A => B) => Show[A] =
          sb => f => Show.newInstance[A] {
            a => (sb.show compose f)(a) 
          }
      }

    implicit def functionContravariant[Y]: Contravariant[? -> Y] =
      new Contravariant[? -> Y] {
        def contramap[A, B]: (B -> Y) => (A => B) => (A -> Y) =
          fb => f => Func(fb compose f)
      }
  }
}
