import scala.language.higherKinds

import Algebra.Codec

object InvariantLaws {

  trait Invariant[F[_]] {
    
    def imap[A, B]: F[A] => (A => B) => (B => A) => F[B]

    def lift[A, B]: (A => B) => (B => A) => F[A] => F[B] =
      f => g => fa => imap(fa)(f)(g)
  }

  object Invariant {
    def apply[F[_]](implicit INST: Invariant[F]): Invariant[F] = INST
  }

  implicit final class InvariantSyntax[F[_]: Invariant, A](fa: F[A]) {
    def imap[B]: (A => B) => (B => A) => F[B] =
      f => g => Invariant[F].imap(fa)(f)(g)
  }

  sealed trait Laws {

    def imapPreservesIdentity[F[_]: Invariant, A]: F[A] => Boolean =
      fa => fa.imap(identity[A])(identity[A]) == fa

    def imapPreservesComposition[F[_]: Invariant, A, B, C]: F[A] => (A => B) => (B => A) => (B => C) => (C => B) => Boolean =
      fa => f => f1 => g => g1 => 
        fa.imap(g compose f)(f1 compose g1) == fa.imap(f)(f1).imap(g)(g1)
  }

  sealed trait LawsNoInfix {
  
    def imapPreservesIdentity[F[_], A](implicit IV: Invariant[F]): F[A] => Boolean =
      fa => IV.imap(fa)(identity[A])(identity[A]) == fa

    def imapPreservesComposition[F[_], A, B, C](implicit IV: Invariant[F]): F[A] => (A => B) => (B => A) => (B => C) => (C => B) => Boolean =
      fa => f => f1 => g => g1 => 
        IV.imap(fa)(g compose f)(f1 compose g1) == IV.imap(IV.imap(fa)(f)(f1))(g)(g1)
  }

  object Laws extends Laws
  object LawsNoInfix extends LawsNoInfix

  object Instances {
  
    implicit lazy val codecInvarian: Invariant[Codec] = 
      new Invariant[Codec] {
        def imap[A, B]: Codec[A] => (A => B) => (B => A) => Codec[B] =
          fa => f => g => Codec.newInstance(fa.encode compose g)(s => fa.decode(s) map f)
      }
  }
}
