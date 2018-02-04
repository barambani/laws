import scala.language.higherKinds

import Algebra.Codec

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

  object InvariantInstances {
  
    implicit val codecInvarian: Invariant[Codec] = 
      new Invariant[Codec] {
        def imap[A, B]: Codec[A] => (A => B) => (B => A) => Codec[B] =
          fa => f => g => Codec.newInstance(fa.encode compose g)(s => fa.decode(s) map f)
      }
  }
}
