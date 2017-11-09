import scala.language.higherKinds

import Algebra.{Box, Show}
import Algebra.Show._
import ArbitraryImplicits._
import ContravariantModule.ContravariantLaws
import ContravariantModule.ContravariantLawsNoInfix
import ContravariantModule.Contravariant
import ContravariantModule.ContravariantInstances._
import org.scalacheck._
import org.scalacheck.Prop.forAll

sealed abstract class ContravariantLawsCheck[F[_] : Contravariant, A](name: String)(
  implicit
    AFA: Arbitrary[F[A]],
    AFS: Arbitrary[String => A],
    AFI: Arbitrary[Int => A]
) extends Properties(s"$name Contravariant Functor Laws Check") {

  val laws          = ContravariantLaws[F]
  val lawsNoInfix   = ContravariantLawsNoInfix[F]

  property(" Contravariant's Contramap Preserves Identity") = forAll {
    (fa: F[A]) => laws.contramapPreservesIdentity(fa)
  }

  property(" Contravariant's Contramap Preserves Composition") = forAll {
    (fa: F[A], f: Int => String, g: String => A) => laws.contramapPreservesComposition(fa)(f)(g)
  }

  property(" Contravariant's Contramap Preserves Identity (No Infix)") = forAll {
    (fa: F[A]) => lawsNoInfix.contramapPreservesIdentity(fa)
  }

  property(" Contravariant's Contramap Preserves Composition (No Infix)") = forAll {
    (fa: F[A], f: String => Int, g: Int => A) => lawsNoInfix.contramapPreservesComposition(fa)(f)(g)
  }

}

object ShowIntContravariantLawsCheck extends ContravariantLawsCheck[Show, Int]("Show of Int")
object ShowBoxContravariantLawsCheck extends ContravariantLawsCheck[Show, Box[Boolean]]("Show of Box")
