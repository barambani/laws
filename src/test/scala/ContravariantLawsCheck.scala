import Algebra.Show._
import Algebra._
import ArbitraryImplicits._
import ContravariantModule.ContravariantInstances._
import ContravariantModule.{Contravariant, ContravariantLaws, ContravariantLawsNoSyntax}
import org.scalacheck.Prop.forAll
import org.scalacheck._

sealed abstract class ContravariantLawsCheck[F[_] : Contravariant, A, B, C](name: String)(
  implicit
    AFA: Arbitrary[F[A]],
    ACB: Arbitrary[C => B],
    ABA: Arbitrary[B => A]
) extends Properties(s"$name Contravariant Functor Laws Check") {

  val laws          = ContravariantLaws[F]
  val lawsNoSyntax  = ContravariantLawsNoSyntax[F]

  property(" Contravariant's Contramap Preserves Identity") = forAll {
    (fa: F[A]) => laws.contramapPreservesIdentity(fa)
  }

  property(" Contravariant's Contramap Preserves Composition") = forAll {
    (fa: F[A], f: C => B, g: B => A) => laws.contramapPreservesComposition(fa)(f)(g)
  }

  property(" Contravariant's Contramap Preserves Identity (No Syntax)") = forAll {
    (fa: F[A]) => lawsNoSyntax.contramapPreservesIdentity(fa)
  }

  property(" Contravariant's Contramap Preserves Composition (No Syntax)") = forAll {
    (fa: F[A], f: C => B, g: B => A) => lawsNoSyntax.contramapPreservesComposition(fa)(f)(g)
  }
}

object ShowIntContravariantLawsCheck extends ContravariantLawsCheck[Show, Int, String, Int]("Show of Int")
object ShowBoxContravariantLawsCheck extends ContravariantLawsCheck[Show, Box[Boolean], Int, Boolean]("Show of Box")
object IntFuncContravariantLawsCheck extends ContravariantLawsCheck[* -> Int, Box[Boolean], Tree[Double], String]("Function to Int")
