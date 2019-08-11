import Algebra.Show._
import Algebra.{->, Box, Id, Show, Symbol, Tree}
import ApplicativeModule.ApplicativeInstances._
import ApplicativeModule.{Applicative, ApplicativeLaws, ApplicativeLawsNoSyntax}
import ArbitraryImplicits._
import SemigroupModule.SemigroupInstances._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Properties, _}

sealed abstract class ApplicativeLawsCheck[F[_] : Applicative, A, B](name: String)(
  implicit
    AA : Arbitrary[A],
    ABA: Arbitrary[F[A]],
    AFB: Arbitrary[A => B],
    AFF: Arbitrary[F[A => B]]
) extends Properties(s"$name Applicative Laws Check") {

  val laws          = ApplicativeLaws[F]
  val lawsNoSyntax  = ApplicativeLawsNoSyntax[F]

  property(" Applicative Identity") = forAll {
    (fa: F[A]) => laws.applicativeIdentity(fa)
  }

  property(" Applicative Homomorphism") = forAll {
    (a: A, f: A => B) => laws.homomorphism(a)(f)
  }

  property(" Applicative Interchange") = forAll {
    (a: A, ff: F[A => B]) => laws.interchange(a)(ff)
  }

  property(" Applicative Map") = forAll {
    (fa: F[A], f: A => B) => laws.applicativeMap(fa)(f)
  }

  property(" Applicative Identity No Syntax") = forAll {
    (fa: F[A]) => lawsNoSyntax.applicativeIdentity(fa)
  }

  property(" Applicative Homomorphism No Syntax") = forAll {
    (a: A, f: A => B) => lawsNoSyntax.homomorphism(a)(f)
  }

  property(" Applicative Interchange No Syntax") = forAll {
    (a: A, ff: F[A => B]) => lawsNoSyntax.interchange(a)(ff)
  }

  property(" Applicative Map No Syntax") = forAll {
    (fa: F[A], f: A => B) => lawsNoSyntax.applicativeMap(fa)(f)
  }
}

object IdApplicativeLawsCheck             extends ApplicativeLawsCheck[Id, Int, String]("Id of Int and String")
object ListApplicativeLawsCheck           extends ApplicativeLawsCheck[List, Int, Symbol]("List of Int and Symbol")
object OptionApplicativeLawsCheck         extends ApplicativeLawsCheck[Option, String, Show[Box[Int]]]("Option of string and show of box")
object EitherApplicativeLawsCheck         extends ApplicativeLawsCheck[Either[String, ?], Symbol, Show[Box[Symbol]]]("Either of symbol and show of box")
object Either2ApplicativeLawsCheck        extends ApplicativeLawsCheck[Either[List[String], ?], Tree[Int], Show[Box[Symbol]]]("Either 2 of symbol and show of box")
object IntFunctionApplicativeLawsCheck    extends ApplicativeLawsCheck[Int -> ?, Tree[String], Boolean]("Function from int to tree and boolean")
object StringFunctionApplicativeLawsCheck extends ApplicativeLawsCheck[String -> ?, Box[Int], Box[String]]("Function from string to box of int and string")
object TreeApplicativeLawsCheck           extends ApplicativeLawsCheck[Tree, Symbol, Box[Boolean]]("Tree of symbol and box of boolean")
