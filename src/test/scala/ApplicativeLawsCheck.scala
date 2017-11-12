import scala.language.higherKinds

import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

import ApplicativeModule.Applicative
import ApplicativeModule.ApplicativeLaws
import ApplicativeModule.ApplicativeLawsNoInfix

abstract class ApplicativeLawsCheck[F[_] : Applicative, A, B](name: String)(
  implicit
    AA : Arbitrary[A],
    AFI: Arbitrary[F[A]],
    AFB: Arbitrary[A => B],
    AFF: Arbitrary[F[A => B]]
) extends Properties(s"$name Applicative Laws Check") {

  val laws        = ApplicativeLaws[F]
  val lawsNoInfix = ApplicativeLawsNoInfix[F]

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
  
  property(" Applicative Identity No Infix") = forAll {
    (fa: F[A]) => lawsNoInfix.applicativeIdentity(fa)
  }

  property(" Applicative Homomorphism No Infix") = forAll {
    (a: A, f: A => B) => lawsNoInfix.homomorphism(a)(f)
  }

  property(" Applicative Interchange No Infix") = forAll {
    (a: A, ff: F[A => B]) => lawsNoInfix.interchange(a)(ff)
  }

  property(" Applicative Map No Infix") = forAll {
    (fa: F[A], f: A => B) => lawsNoInfix.applicativeMap(fa)(f)
  }
}
