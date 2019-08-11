import Algebra.Show._
import Algebra.{->, Box, Id, Show, Symbol, Tree}
import ArbitraryImplicits._
import FunctorModule.FunctorInstances._
import FunctorModule.{Functor, FunctorLaws, FunctorLawsNoSyntax}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Properties, _}

sealed abstract class FunctorLawsCheck[F[_] : Functor, A, B](name: String)(
  implicit
    AFI: Arbitrary[F[A]],
    AAB: Arbitrary[A => B],
    ABB: Arbitrary[B => Boolean]
) extends Properties(s"$name Functor Laws Check") {

  val laws          = FunctorLaws[F]
  val lawsNoSyntax  = FunctorLawsNoSyntax[F]

  property(" Functor's Map Preserves Identity") = forAll {
    (fa: F[A]) => laws.mapPreservesIdentity(fa)
  }

  property(" Functor's Map Preserves Composition") = forAll {
    (fa: F[A], f: A => B, g: B => Boolean) => 
      laws.mapPreservesComposition(fa)(f)(g)
  }

  property(" Functor's Map Preserves Identity No Syntax") = forAll {
    (fa: F[A]) => lawsNoSyntax.mapPreservesIdentity(fa)
  }

  property(" Functor's Map Preserves Composition No Syntax") = forAll {
    (fa: F[A], f: A => B, g: B => Boolean) => 
      lawsNoSyntax.mapPreservesComposition(fa)(f)(g)
  }
}

object IdFunctorLawsCheck             extends FunctorLawsCheck[Id, String, Int]("Id")
object ListFunctorLawsCheck           extends FunctorLawsCheck[List, Show[Symbol], Box[String]]("List")
object OptionFunctorLawsCheck         extends FunctorLawsCheck[Option, Tree[Box[Int]], Boolean]("Option")
object EitherFunctorLawsCheck         extends FunctorLawsCheck[Either[String, ?], Box[Int], Symbol]("Either")
object IntFunctionFunctorLawsCheck    extends FunctorLawsCheck[Int -> ?, Box[String], Tree[Int]]("Function From Int")
object StringFunctionFunctorLawsCheck extends FunctorLawsCheck[String -> ?, Tree[String], Box[Int]]("Function From String")
object TreeFunctorLawsCheck           extends FunctorLawsCheck[Tree, String, Box[Box[Int]]]("Tree")
