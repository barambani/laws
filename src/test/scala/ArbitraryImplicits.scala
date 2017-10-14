import org.scalacheck.Arbitrary
import org.scalacheck.Cogen

import shapeless.Lazy

import Algebra.FuncFromIntTo
import Algebra.{Tree, Branch, Leaf}
import Algebra.Show
import Algebra.Box
import Algebra.Symbol
import Algebra.Codec
import ContravariantLaws.Contravariant
import ContravariantLaws.ContravariantSyntax

object ArbitraryImplicits {

  implicit def funcFromIntToArb[A](
    implicit 
      AR: Arbitrary[A]): Arbitrary[FuncFromIntTo[A]] =
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
      LAR: Arbitrary[Leaf[A]],
      BAR: Lazy[Arbitrary[Branch[A]]]): Arbitrary[Tree[A]] =
    Arbitrary {
      AR.arbitrary flatMap { 
        if(_) LAR.arbitrary 
        else  BAR.value.arbitrary 
      } 
    }

  implicit def showArb[A](
    implicit 
      SH: Show[A]): Arbitrary[Show[A]] =
    Arbitrary(SH)

  implicit def boxArb[A](
    implicit
      AS: Arbitrary[Show[A]],
      CA: Contravariant[Show]): Arbitrary[Show[Box[A]]] =
    Arbitrary {
      AS.arbitrary map { sa => sa contramap (_.value) }
    }

  implicit def aToBox[A](
    implicit
      AB: Arbitrary[Boolean]): Arbitrary[A => Box[Boolean]] =
    Arbitrary {
      AB.arbitrary map { b => (a: A) => Box(b) }
    }

  implicit def symbol(implicit AS: Arbitrary[String]): Arbitrary[Symbol] =
    Arbitrary { AS.arbitrary map Symbol.apply }

  implicit def symbolCodec(
    implicit
      CS: Codec[Symbol]): Arbitrary[Codec[Symbol]] =
    Arbitrary(CS)

  implicit def symbolCogen: Cogen[Symbol] =
    Cogen[Symbol]((s: Symbol) => s.name.size.toLong)

  implicit def symbolToInt(
    implicit
      AI: Arbitrary[Int]): Arbitrary[Symbol => Int] =
    Arbitrary.arbFunction1[Symbol, Int]

  implicit def intToSymbol: Arbitrary[Int => Symbol] =
    Arbitrary.arbFunction1[Int, Symbol]
}
