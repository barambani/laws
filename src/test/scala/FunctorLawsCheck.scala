import scala.language.higherKinds

import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

import Algebra.Tree
import Algebra.Func
import Algebra.Id
import FunctorModule.Functor
import FunctorModule.FunctorLaws
import FunctorModule.FunctorLawsNoInfix
import FunctorModule.FunctorInstances._
import ArbitraryImplicits._

abstract class FunctorLawsCheck[F[_] : Functor](name: String)(
  implicit
    AFI: Arbitrary[F[Int]],
    AFS: Arbitrary[F[String]]
) extends Properties(s"$name Functor Laws Check") {

  val laws        = FunctorLaws[F]
  val lawsNoInfix = FunctorLawsNoInfix[F]

  property(" Functor's Map Preserves Identity") = forAll {
    (fa: F[Int]) => laws.fmapPreservesIdentity(fa)
  }

  property(" Functor's Map Preserves Composition") = forAll {
    (fa: F[Int], f: Int => String, g: String => Boolean) => 
      laws.fmapPreservesComposition(fa)(f)(g)
  }

  property(" Functor's Map Preserves Identity No Infix") = forAll {
    (fa: F[String]) => lawsNoInfix.fmapPreservesIdentity(fa)
  }

  property(" Functor's Map Preserves Composition No Infix") = forAll {
    (fa: F[String], f: String => Int, g: Int => Boolean) => 
      lawsNoInfix.fmapPreservesComposition(fa)(f)(g)
  }
}

object IdFunctorLawsCheck             extends FunctorLawsCheck[Id]("Id")
object ListFunctorLawsCheck           extends FunctorLawsCheck[List]("List")
object OptionFunctorLawsCheck         extends FunctorLawsCheck[Option]("Option")
object EitherFunctorLawsCheck         extends FunctorLawsCheck[Either[String, ?]]("Either")
object IntFunctionFunctorLawsCheck    extends FunctorLawsCheck[Func[Int, ?]]("Function From Int")
object StringFunctionFunctorLawsCheck extends FunctorLawsCheck[Func[String, ?]]("Function From String")
object TreeFunctorLawsCheck           extends FunctorLawsCheck[Tree]("Tree")
