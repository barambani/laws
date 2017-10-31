import scala.language.higherKinds

import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

import Algebra.Tree
import Algebra.FuncFromIntTo
import Algebra.Id
import FunctorModule.Functor
import FunctorModule.Laws
import FunctorModule.LawsNoInfix
import FunctorModule.FunctorInstances._
import ArbitraryImplicits._

abstract class FunctorLawsCheck[F[_]](name: String)(
  implicit
    FU: Functor[F],
    AFI: Arbitrary[F[Int]],
    AFS: Arbitrary[F[String]]
) extends Properties(s"$name Functor Laws Check") {

  property(" Functor's Map Preserves Identity") = forAll {
    (fa: F[Int]) => Laws.mapPreservesIdentity(FU)(fa)
  }

  property(" Functor's Map Preserves Composition") = forAll {
    (fa: F[Int], f: Int => String, g: String => Boolean) => 
      Laws.mapPreservesComposition(FU)(fa)(f)(g)
  }

  property(" Functor's Map Preserves Identity No Infix") = forAll {
    (fa: F[String]) => LawsNoInfix.mapPreservesIdentity(FU)(fa)
  }

  property(" Functor's Map Preserves Composition No Infix") = forAll {
    (fa: F[String], f: String => Int, g: Int => Boolean) => 
      LawsNoInfix.mapPreservesComposition(FU)(fa)(f)(g)
  }
}

object IdFunctorLawsCheck extends FunctorLawsCheck[Id]("Id")
object SequenceFunctorLawsCheck extends FunctorLawsCheck[Seq]("Sequence")
object ListFunctorLawsCheck extends FunctorLawsCheck[List]("List")
object OptionFunctorLawsCheck extends FunctorLawsCheck[Option]("Option")
object FunctionFromIntFunctorLawsCheck extends FunctorLawsCheck[FuncFromIntTo]("Function From Int")
object TreeFunctorLawsCheck extends FunctorLawsCheck[Tree]("Tree")
