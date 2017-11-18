import scala.language.higherKinds

import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

import Algebra.Tree
import Algebra.Func
import Algebra.Id
import Algebra.{Symbol, Box, Show}
import Algebra.Show._
import FunctorModule.Functor
import FunctorModule.FunctorLaws
import FunctorModule.FunctorLawsNoInfix
import FunctorModule.FunctorInstances._
import ArbitraryImplicits._

sealed abstract class FunctorLawsCheck[F[_] : Functor, A, B](name: String)(
  implicit
    AFI: Arbitrary[F[A]],
    AAB: Arbitrary[A => B],
    ABB: Arbitrary[B => Boolean]
) extends Properties(s"$name Functor Laws Check") {

  val laws        = FunctorLaws[F]
  val lawsNoInfix = FunctorLawsNoInfix[F]

  property(" Functor's Map Preserves Identity") = forAll {
    (fa: F[A]) => laws.mapPreservesIdentity(fa)
  }

  property(" Functor's Map Preserves Composition") = forAll {
    (fa: F[A], f: A => B, g: B => Boolean) => 
      laws.mapPreservesComposition(fa)(f)(g)
  }

  property(" Functor's Map Preserves Identity No Infix") = forAll {
    (fa: F[A]) => lawsNoInfix.mapPreservesIdentity(fa)
  }

  property(" Functor's Map Preserves Composition No Infix") = forAll {
    (fa: F[A], f: A => B, g: B => Boolean) => 
      lawsNoInfix.mapPreservesComposition(fa)(f)(g)
  }
}

object IdFunctorLawsCheck             extends FunctorLawsCheck[Id, String, Int]("Id")
object ListFunctorLawsCheck           extends FunctorLawsCheck[List, Show[Symbol], Box[String]]("List")
object OptionFunctorLawsCheck         extends FunctorLawsCheck[Option, Tree[Box[Int]], Boolean]("Option")
object EitherFunctorLawsCheck         extends FunctorLawsCheck[Either[String, ?], Box[Int], Symbol]("Either")
object IntFunctionFunctorLawsCheck    extends FunctorLawsCheck[Func[Int, ?], Box[String], Tree[Int]]("Function From Int")
object StringFunctionFunctorLawsCheck extends FunctorLawsCheck[Func[String, ?], Tree[String], Box[Int]]("Function From String")
object TreeFunctorLawsCheck           extends FunctorLawsCheck[Tree, String, Box[Box[Int]]]("Tree")
