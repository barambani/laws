import scala.language.higherKinds

import org.scalacheck._
import org.scalacheck.Prop.forAll
import InvariantModule.Invariant
import InvariantModule.InvariantLaws
import InvariantModule.InvariantLawsNoInfix
import InvariantModule.InvariantInstances._
import Algebra.Codec
import Algebra.Symbol
import ArbitraryImplicits._

sealed abstract class InvariantLawsCheck[F[_] : Invariant, A](name: String)(
  implicit
    AFA:  Arbitrary[F[A]],
    AFF:  Arbitrary[A => Int],
    AFF1: Arbitrary[Int => A]
) extends Properties(s"$name Invariant Functor Laws Check") {

  val laws        = InvariantLaws[F]
  val lawsNoInfix = InvariantLawsNoInfix[F]

  property(" Invariant's Imap Preserves Identity") = forAll {
    (fa: F[A]) => laws.imapPreservesIdentity(fa)
  }

  property(" Invariant's Imap Preserves Composition") = forAll {
    (fa: F[A], f: A => Int, f1: Int => A, g: Int => String, g1: String => Int) => laws.imapPreservesComposition(fa)(f)(f1)(g)(g1)
  }

  property(" Invariant's Imap Preserves Identity (No Infix)") = forAll {
    (fa: F[A]) => lawsNoInfix.imapPreservesIdentity(fa)
  }

  property(" Invariant's Imap Preserves Composition (No Infix)") = forAll {
    (fa: F[A], f: A => Int, f1: Int => A, g: Int => String, g1: String => Int) => lawsNoInfix.imapPreservesComposition(fa)(f)(f1)(g)(g1)
  }
}

object CodecOfSymbolInvariantLawsCheck extends InvariantLawsCheck[Codec, Symbol]("Codec Of Symbol")
