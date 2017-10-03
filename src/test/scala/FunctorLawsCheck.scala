import scala.language.higherKinds

import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

import FunctorLaws.FunctorInstances.FuncFromIntTo
import FunctorLaws.Functor
import FunctorLaws.Laws
import FunctorLaws.LawsNoInfix
import FunctorLaws.FunctorInstances._
import ArbitraryImplicits._

sealed abstract class FunctorLawsCheck[F[_]](name: String)(
  implicit
    FU: Functor[F],
    AFI: Arbitrary[F[Int]],
    AFS: Arbitrary[F[String]]
) extends Properties(s"$name Functor Laws Check") {

  property(" Functor's Map Preserves Identity") = forAll {
    (fa: F[Int]) => Laws.mapPreservesIdentity(FU)(fa)
  }

  property(" Functor's Map Preserves Composition") = forAll {
    (fa: F[Int], f: Int => String, g: String => Boolean) => Laws.mapPreservesComposition(FU)(fa)(f)(g)
  }

  property(" Functor's Map Preserves Identity No Infix") = forAll {
    (fa: F[String]) => LawsNoInfix.mapPreservesIdentity(FU)(fa)
  }

  property(" Functor's Map Preserves Composition No Infix") = forAll {
    (fa: F[String], f: String => Int, g: Int => Boolean) => LawsNoInfix.mapPreservesComposition(FU)(fa)(f)(g)
  }
}

object SequenceFunctorLawsCheck extends FunctorLawsCheck[Seq]("Sequence")
object OptionFunctorLawsCheck extends FunctorLawsCheck[Option]("Option")
object FunctionFromIntFunctorLawsCheck extends FunctorLawsCheck[FuncFromIntTo]("Function From Int")
