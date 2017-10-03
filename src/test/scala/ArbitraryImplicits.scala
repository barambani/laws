import org.scalacheck.Arbitrary

import FunctorLaws.FunctorInstances.FuncFromIntTo

object ArbitraryImplicits {

  implicit def funcFromIntToArb[A](implicit AR: Arbitrary[A]): Arbitrary[FuncFromIntTo[A]] =
    Arbitrary[FuncFromIntTo[A]] {
      for(a <- Arbitrary.arbitrary[A]) yield FuncFromIntTo(_ => a)
    }
}
