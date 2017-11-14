import scala.language.higherKinds

import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

import Algebra.Id
import Algebra.{Tree, Box, Symbol, Show}
import Algebra.Show._
import MonadModule.MonadInstances._
import MonadModule.Monad
import MonadModule.MonadLaws
import MonadModule.MonadLawsNoInfix
import ArbitraryImplicits._

sealed abstract class MonadLawsCheck[F[_] : Monad, A, B, C](name: String)(
  implicit
    AA : Arbitrary[A],
    AMI: Arbitrary[F[A]],
    AFA: Arbitrary[A => F[A]],
    AFB: Arbitrary[A => F[B]],
    BFC: Arbitrary[B => F[C]]
) extends Properties(s"$name Monad Laws Check") {

  val laws        = MonadLaws[F]
  val lawsNoInfix = MonadLawsNoInfix[F]

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

  property(" Left identity No Infix") = forAll {
    (a: A, f: A => F[A]) => lawsNoInfix.leftIdentity(a)(f)
  }

  property(" Right identity No Infix") = forAll {
    (ma: F[A]) => lawsNoInfix.rightIdentity(ma)
  }

  property(" Associativity No Infix") = forAll {
    (ma: F[A], f: A => F[B], g: B => F[C]) => 
      lawsNoInfix.associativity(ma)(f)(g)
  }
}

object IdMonadLawsCheck       extends MonadLawsCheck[Id, Box[Int], String, Symbol]("Id")
object ListMonadLawsCheck     extends MonadLawsCheck[List, String, List[Int], Box[Int]]("List")
object OptionMonadLawsCheck   extends MonadLawsCheck[Option, Boolean, Tree[Boolean], Symbol]("Option")
object EitherMonadLawsCheck   extends MonadLawsCheck[Either[String, ?], Symbol, Tree[String], Symbol]("Either")
object TreeMonadLawsCheck     extends MonadLawsCheck[Tree, Box[Int], Symbol, Show[Box[String]]]("Tree")
