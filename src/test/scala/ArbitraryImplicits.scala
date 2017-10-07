import org.scalacheck.Arbitrary

import org.scalacheck.Gen

import Algebra.FuncFromIntTo
import Algebra.{Tree, Branch, Leaf}

object ArbitraryImplicits {

  implicit def funcFromIntToArb[A](implicit AR: Arbitrary[A]): Arbitrary[FuncFromIntTo[A]] =
    Arbitrary[FuncFromIntTo[A]] {
      Arbitrary.arbitrary[A] map { a => FuncFromIntTo(_ => a) }
    }

  implicit def treeArb[A](implicit AR: Arbitrary[A]): Arbitrary[Tree[A]] =
    Arbitrary[Tree[A]] {

      def randomChoice: Gen[Boolean] =
        Gen.choose(1,10) map (_ <= 5)
      
      def branchGen: Gen[Tree[A]] =
        for {
          l <- treeGen
          r <- treeGen
        } yield Branch(l, r)

      def leafGen: Gen[Tree[A]] = 
        Arbitrary.arbitrary[A] map Leaf.apply

      def treeGen: Gen[Tree[A]] =
        randomChoice flatMap {
          if(_) leafGen else branchGen
        }

      treeGen
    }
}
