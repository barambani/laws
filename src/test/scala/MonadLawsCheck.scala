import Algebra.Show._
import Algebra.{->, Box, Id, Show, Symbol, Tree}
import ArbitraryImplicits._
import MonadModule.MonadInstances._
import MonadModule.{Monad, MonadLaws, MonadLawsNoSyntax}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Properties, _}

sealed abstract class MonadLawsCheck[F[_] : Monad, A, B, C](name: String)(
  implicit
    AA : Arbitrary[A],
    AMI: Arbitrary[F[A]],
    AFA: Arbitrary[A => F[A]],
    AFB: Arbitrary[A => F[B]],
    BFC: Arbitrary[B => F[C]]
) extends Properties(s"$name Monad Laws Check") {

  val laws          = MonadLaws[F]
  val lawsNoSyntax  = MonadLawsNoSyntax[F]

  property(" Left identity") = forAll {
    (a: A, f: A => F[A]) => laws.leftIdentity(a)(f)
  }

  property(" Right identity") = forAll {
    (ma: F[A]) => laws.rightIdentity(ma)
  }

  property(" Associativity") = forAll {
    (ma: F[A], f: A => F[B], g: B => F[C]) => 
      laws.associativity(ma)(f)(g)
  }

  property(" Left identity No Syntax") = forAll {
    (a: A, f: A => F[A]) => lawsNoSyntax.leftIdentity(a)(f)
  }

  property(" Right identity No Syntax") = forAll {
    (ma: F[A]) => lawsNoSyntax.rightIdentity(ma)
  }

  property(" Associativity No Syntax") = forAll {
    (ma: F[A], f: A => F[B], g: B => F[C]) => 
      lawsNoSyntax.associativity(ma)(f)(g)
  }
}

object IdMonadLawsCheck             extends MonadLawsCheck[Id, Box[Int], String, Symbol]("Id")
object ListMonadLawsCheck           extends MonadLawsCheck[List, String, List[Int], Box[Int]]("List")
object OptionMonadLawsCheck         extends MonadLawsCheck[Option, Boolean, Tree[Boolean], Symbol]("Option")
object EitherMonadLawsCheck         extends MonadLawsCheck[Either[String, *], Symbol, Tree[String], Symbol]("Either")
object TreeMonadLawsCheck           extends MonadLawsCheck[Tree, Box[Int], Symbol, Show[Box[String]]]("Tree")
object IntFunctionMonadLawsCheck    extends MonadLawsCheck[Int -> *, Tree[String], Boolean, Symbol]("Function from int to tree and boolean")
object StringFunctionMonadLawsCheck extends MonadLawsCheck[String -> *, Box[Int], Box[String], Tree[String]]("Function from string to box of int and string")
