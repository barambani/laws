import Algebra.Codec
import ContravariantModule.Contravariant
import FunctorModule.Functor
import SemigroupModule.Semigroup

object InvariantModule {

  trait Invariant[F[_]] {
    
    def imap[A, B]: F[A] => (A => B) => (B => A) => F[B]

    def lift[A, B]: (A => B) => (B => A) => F[A] => F[B] =
      f => g => fa => imap(fa)(f)(g)
  }

  object Invariant {
    def apply[F[_]](implicit F: Invariant[F]): Invariant[F] = F
  }

  implicit final class InvariantSyntax[F[_]: Invariant, A](fa: F[A]) {
    def imap[B]: (A => B) => (B => A) => F[B] =
      f => g => Invariant[F].imap(fa)(f)(g)
  }

  sealed trait InvariantLaws[F[_]] {

    implicit def F: Invariant[F]

    def imapPreservesIdentity[A]: F[A] => Boolean =
      fa => fa.imap(identity[A])(identity[A]) == fa

    def imapPreservesComposition[A, B, C]: F[A] => (A => B) => (B => A) => (B => C) => (C => B) => Boolean =
      fa => f => f1 => g => g1 => 
        fa.imap(g compose f)(f1 compose g1) == fa.imap(f)(f1).imap(g)(g1)
  }

  sealed trait InvariantLawsNoSyntax[F[_]] {

    implicit def F: Invariant[F]

    def imapPreservesIdentity[A]: F[A] => Boolean =
      fa => F.imap(fa)(identity[A])(identity[A]) == fa

    def imapPreservesComposition[A, B, C]: F[A] => (A => B) => (B => A) => (B => C) => (C => B) => Boolean =
      fa => f => f1 => g => g1 =>
        F.imap(fa)(g compose f)(f1 compose g1) == F.imap(F.imap(fa)(f)(f1))(g)(g1)
  }

  object InvariantLaws {
    def apply[F[_]](implicit FI: Invariant[F]): InvariantLaws[F] =
      new InvariantLaws[F] { def F = FI }
  }

  object InvariantLawsNoSyntax {
    def apply[F[_]](implicit FI: Invariant[F]): InvariantLawsNoSyntax[F] =
      new InvariantLawsNoSyntax[F] { def F = FI }
  }

  object InvariantInstances extends LowPriorityInstances1 {

    implicit val codecInvariant: Invariant[Codec] =
      new Invariant[Codec] {
        def imap[A, B]: Codec[A] => (A => B) => (B => A) => Codec[B] =
          fa => f => g => Codec.newInstance(fa.encode compose g)(s => fa.decode(s) map f)
      }

    implicit val semigroupInvariant: Invariant[Semigroup] =
      new Invariant[Semigroup] {
        def imap[A, B]: Semigroup[A] => (A => B) => (B => A) => Semigroup[B] =
          sa => f => g => Semigroup.newInstance[B](
            (b1, b2) => {
              println(s"FILIPPO --> $b1")
              println(s"FILIPPO --> $b2")
              println
              println(s"FILIPPO --> ${g(b1)}")
              println(s"FILIPPO --> ${g(b2)}")
              println
              println(s"FILIPPO --> ${sa.combine(g(b1), g(b2))}")
              println
              println(s"FILIPPO --> ${f(sa.combine(g(b1), g(b2)))}")

              f(sa.combine(g(b1), g(b2)))
            }
          )
      }
  }

  sealed trait LowPriorityInstances1 extends LowPriorityInstances2 {

    implicit def functorInvariant[F[_]: Functor]: Invariant[F] =
      new Invariant[F] {
        def imap[A, B]: F[A] => (A => B) => (B => A) => F[B] =
          fa => f => _ => Functor[F].map(fa)(f)
      }
  }

  sealed trait LowPriorityInstances2 {

    implicit def contravariantInvariant[F[_]: Contravariant]: Invariant[F] =
      new Invariant[F] {
        def imap[A, B]: F[A] => (A => B) => (B => A) => F[B] =
          fa => _ => g => Contravariant[F].contramap(fa)(g)
      }
  }
}
