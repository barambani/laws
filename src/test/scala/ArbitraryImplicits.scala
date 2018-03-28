import Algebra.{->, Box, Branch, Codec, Func, Leaf, Show, Symbol, Tree}
import org.scalacheck.{Arbitrary, Cogen, Gen}
import shapeless.Lazy

object ArbitraryImplicits {

  implicit def funcToArb[X, R](
    implicit 
      AX: Arbitrary[X],
      AR: Arbitrary[R]): Arbitrary[X -> R] =
    Arbitrary {
      for {
        x <- AX.arbitrary
        r <- AR.arbitrary
      } yield Func(x => r)
    }

  implicit def treeChooser: Arbitrary[Boolean] =
    Arbitrary{ Gen.choose(1, 10) map (_ <= 6) }

  implicit def branchArb[A](
    implicit 
      LEFT : Lazy[Arbitrary[Tree[A]]],
      RIGHT: Lazy[Arbitrary[Tree[A]]]): Arbitrary[Branch[A]] =
    Arbitrary {
      for {
        l <- LEFT.value.arbitrary
        r <- RIGHT.value.arbitrary
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
      LAR: Arbitrary[Leaf[A]],
      BAR: Lazy[Arbitrary[Branch[A]]]): Arbitrary[Tree[A]] =
    Arbitrary {
      treeChooser.arbitrary flatMap { 
        if(_) LAR.arbitrary 
        else  BAR.value.arbitrary 
      } 
    }

  implicit def boxArb[A](
    implicit
      AA: Arbitrary[A]): Arbitrary[Box[A]] =
    Arbitrary {
      AA.arbitrary map (Box(_))
    }

  implicit def showArb[A](
    implicit 
      SH: Show[A]): Arbitrary[Show[A]] =
    Arbitrary(SH)

  implicit def boxToBox[A, B](
    implicit
      AB: Arbitrary[Box[B]]): Arbitrary[Box[A] => Box[B]] =
    Arbitrary {
      AB.arbitrary map { bb => (_: Box[A]) => bb }
    }

  implicit def aToBox[A, B](
    implicit
      AB: Arbitrary[B]): Arbitrary[A => Box[B]] =
    Arbitrary {
      AB.arbitrary map { b => (a: A) => Box(b) }
    }

  implicit def boxTo[A, B](
    implicit
      AB: Arbitrary[B]): Arbitrary[Box[A] => B] =
    Arbitrary {
      AB.arbitrary map { b => (_: Box[A]) => b }
    }

  implicit def treeToBox[A, B](
    implicit
      AB: Arbitrary[Box[B]]): Arbitrary[Tree[A] => Box[B]] =
    Arbitrary {
      AB.arbitrary map { b => (t: Tree[A]) => b }
    }

  implicit def treeTo[A, B](
    implicit
      AB: Arbitrary[B]): Arbitrary[Tree[A] => B] =
    Arbitrary {
      AB.arbitrary map { b => (t: Tree[A]) => b }
    }

  implicit def symbol(implicit AS: Arbitrary[String]): Arbitrary[Symbol] =
    Arbitrary { AS.arbitrary map Symbol.apply }

  implicit def symbolCodec(
    implicit
      CS: Codec[Symbol]): Arbitrary[Codec[Symbol]] =
    Arbitrary(CS)

  implicit def symbolCogen: Cogen[Symbol] =
    Cogen[String] contramap (_.name)

  implicit def symbolToInt(
    implicit
      AI: Arbitrary[Int]): Arbitrary[Symbol => Int] =
    Arbitrary.arbFunction1[Symbol, Int]

  implicit def intToSymbol: Arbitrary[Int => Symbol] =
    Arbitrary.arbFunction1[Int, Symbol]
}
