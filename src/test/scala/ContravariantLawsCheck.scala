import scala.language.higherKinds

import Algebra.{Box, Show}
import Algebra.Show._
import ArbitraryImplicits._
import ContravariantModule.{Laws, LawsNoInfix}
import ContravariantModule.Contravariant
import ContravariantModule.Instances._
import org.scalacheck._
import org.scalacheck.Prop.forAll

sealed abstract class ContravariantLawsCheck[F[_], A](name: String)(
  implicit
    CF:  Contravariant[F],
    AFA: Arbitrary[F[A]],
    AFS: Arbitrary[String => A],
    AFI: Arbitrary[Int => A]
) extends Properties(s"$name Contravariant Functor Laws Check") {

  property(" Contravariant's Contramap Preserves Identity") = forAll {
    (fa: F[A]) => Laws.contramapPreservesIdentity(CF)(fa)
  }

  property(" Contravariant's Contramap Preserves Composition") = forAll {
    (fa: F[A], f: Int => String, g: String => A) => Laws.contramapPreservesComposition(CF)(fa)(f)(g)
  }

  property(" Contravariant's Contramap Preserves Identity (No Infix)") = forAll {
    (fa: F[A]) => LawsNoInfix.contramapPreservesIdentity(CF)(fa)
  }

  property(" Contravariant's Contramap Preserves Composition (No Infix)") = forAll {
    (fa: F[A], f: String => Int, g: Int => A) => LawsNoInfix.contramapPreservesComposition(CF)(fa)(f)(g)
  }

}

object ShowIntContravariantLawsCheck extends ContravariantLawsCheck[Show, Int]("Show of Int")
object ShowBoxContravariantLawsCheck extends ContravariantLawsCheck[Show, Box[Boolean]]("Show of Box")
