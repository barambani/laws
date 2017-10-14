import scala.language.higherKinds

import org.scalacheck._
import org.scalacheck.Prop.forAll
import InvariantLaws.Invariant
import InvariantLaws.Laws
import InvariantLaws.LawsNoInfix
import InvariantLaws.Instances._
import Algebra.Codec
import Algebra.Symbol
import ArbitraryImplicits._

sealed abstract class InvariantLawsCheck[F[_], A](name: String)(
  implicit
    IF:   Invariant[F],
    AFA:  Arbitrary[F[A]],
    AFF:  Arbitrary[A => Int],
    AFF1: Arbitrary[Int => A]
) extends Properties(s"$name Invariant Functor Laws Check") {

  property(" Invariant's Imap Preserves Identity") = forAll {
    (fa: F[A]) => Laws.imapPreservesIdentity(IF)(fa)
  }

  property(" Invariant's Imap Preserves Composition") = forAll {
    (fa: F[A], f: A => Int, f1: Int => A, g: Int => String, g1: String => Int) => Laws.imapPreservesComposition(IF)(fa)(f)(f1)(g)(g1)
  }

  property(" Invariant's Imap Preserves Identity (No Infix)") = forAll {
    (fa: F[A]) => LawsNoInfix.imapPreservesIdentity(IF)(fa)
  }

  property(" Invariant's Imap Preserves Composition (No Infix)") = forAll {
    (fa: F[A], f: A => Int, f1: Int => A, g: Int => String, g1: String => Int) => LawsNoInfix.imapPreservesComposition(IF)(fa)(f)(f1)(g)(g1)
  }
}

object CodecOfSymbolInvariantLawsCheck extends InvariantLawsCheck[Codec, Symbol]("Codec Of Symbol")
