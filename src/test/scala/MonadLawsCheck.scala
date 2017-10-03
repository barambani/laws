import scala.language.higherKinds

import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

import MonadLaws.MonadInstances._
import MonadLaws.Monad
import MonadLaws.Laws
import MonadLaws.LawsNoInfix

sealed abstract class MonadLawsCheck[M[_]](name: String)(
  implicit 
    MO: Monad[M],
    AMI: Arbitrary[M[Int]],
    AMS: Arbitrary[M[String]],
    AMB: Arbitrary[M[Boolean]]
) extends Properties(s"$name Monad Laws Check") {

  property(" Left identity") = forAll {
    (a: Int, f: Int => M[Int]) => Laws.leftIdentity(MO)(a)(f)
  }

  property(" Right identity") = forAll {
    (ma: M[Int]) => Laws.rightIdentity(MO)(ma)
  }

  property(" Associativity") = forAll {
    (ma: M[Int], f: Int => M[String], g: String => M[Boolean]) => Laws.associativity(MO)(ma)(f)(g)
  }

  property(" Left identity No Infix") = forAll {
    (a: Int, f: Int => M[Int]) => LawsNoInfix.leftIdentity(MO)(a)(f)
  }

  property(" Right identity No Infix") = forAll {
    (ma: M[Int]) => LawsNoInfix.rightIdentity(MO)(ma)
  }

  property(" Associativity No Infix") = forAll {
    (ma: M[Int], f: Int => M[String], g: String => M[Boolean]) => LawsNoInfix.associativity(MO)(ma)(f)(g)
  }
}

object IdMonadLawsCheck extends MonadLawsCheck[Id]("Id")
object ListMonadLawsCheck extends MonadLawsCheck[List]("List")
object OptionMonadLawsCheck extends MonadLawsCheck[Option]("Option")
