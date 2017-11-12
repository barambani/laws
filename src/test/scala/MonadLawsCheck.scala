import scala.language.higherKinds

import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

import Algebra.Id
import Algebra.Tree
import MonadModule.MonadInstances._
import MonadModule.Monad
import MonadModule.MonadLaws
import MonadModule.MonadLawsNoInfix
import ArbitraryImplicits._

sealed abstract class MonadLawsCheck[F[_] : Monad](name: String)(
  implicit 
    AMI: Arbitrary[F[Int]],
    AMS: Arbitrary[F[String]],
    AMB: Arbitrary[F[Boolean]]
) extends Properties(s"$name Monad Laws Check") {

  val laws        = MonadLaws[F]
  val lawsNoInfix = MonadLawsNoInfix[F]

  property(" Left identity") = forAll {
    (a: Int, f: Int => F[Int]) => laws.leftIdentity(a)(f)
  }

  property(" Right identity") = forAll {
    (ma: F[Int]) => laws.rightIdentity(ma)
  }

  property(" Associativity") = forAll {
    (ma: F[Int], f: Int => F[String], g: String => F[Boolean]) => 
      laws.associativity(ma)(f)(g)
  }

  property(" Left identity No Infix") = forAll {
    (a: Int, f: Int => F[Int]) => lawsNoInfix.leftIdentity(a)(f)
  }

  property(" Right identity No Infix") = forAll {
    (ma: F[Int]) => lawsNoInfix.rightIdentity(ma)
  }

  property(" Associativity No Infix") = forAll {
    (ma: F[Int], f: Int => F[String], g: String => F[Boolean]) => 
      lawsNoInfix.associativity(ma)(f)(g)
  }
}

object IdMonadLawsCheck       extends MonadLawsCheck[Id]("Id")
object ListMonadLawsCheck     extends MonadLawsCheck[List]("List")
object OptionMonadLawsCheck   extends MonadLawsCheck[Option]("Option")
object EitherMonadLawsCheck   extends MonadLawsCheck[Either[String, ?]]("Either")
object TreeMonadLawsCheck     extends MonadLawsCheck[Tree]("Tree")
