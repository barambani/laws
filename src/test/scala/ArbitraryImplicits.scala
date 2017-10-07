import org.scalacheck.Arbitrary

import shapeless.Lazy

import Algebra.FuncFromIntTo
import Algebra.{Tree, Branch, Leaf}

object ArbitraryImplicits {

  implicit def funcFromIntToArb[A](implicit AR: Arbitrary[A]): Arbitrary[FuncFromIntTo[A]] =
    Arbitrary {
      AR.arbitrary map { a => FuncFromIntTo(_ => a) }
    }

  implicit def branchArb[A](
    implicit 
      TRAR: Lazy[Arbitrary[Tree[A]]]): Arbitrary[Branch[A]] =
    Arbitrary {
      for {
        l <- TRAR.value.arbitrary
        r <- TRAR.value.arbitrary
      } yield Branch(l, r)
    }

  implicit def leafArb[A](
    implicit 
      AR: Arbitrary[A]): Arbitrary[Leaf[A]] =
    Arbitrary {
      AR.arbitrary map Leaf.apply
    }

  implicit def treeArb[A](
    implicit
      AR: Arbitrary[Boolean],
      LAR: Lazy[Arbitrary[Leaf[A]]],
      BAR: Lazy[Arbitrary[Branch[A]]]): Arbitrary[Tree[A]] =
    Arbitrary {
      AR.arbitrary flatMap { 
        if(_) LAR.value.arbitrary 
        else  BAR.value.arbitrary 
      } 
    }
}
