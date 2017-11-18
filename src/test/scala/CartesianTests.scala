import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

import Algebra.{Show, Tree, Symbol}
import Algebra.Show._
import CartesianModule.CartesianSyntax
import CartesianModule.CartesianInstances._
import CartesianModule.Cartesian
import ArbitraryImplicits._

sealed abstract class ListCartesianTests[A, B, C](name: String)(
  implicit
    LA: Arbitrary[List[A]],
    LB: Arbitrary[List[B]],
    FC: Arbitrary[List[C]]
) extends Properties(s"$name Cartesian Tests") {

  property(" Size of product is the product of sizes") = forAll {
    (fa: List[A], fb: List[B]) => (fa |*| fb).size == fa.size * fb.size
  }

  property(" Size of product3 is the product of sizes") = forAll {
    (fa: List[A], fb: List[B], fc: List[C]) => Cartesian.product3(fa, fb, fc).size == fa.size * fb.size * fc.size
  }
}

sealed abstract class OptionCartesianTests[A, B, C](name: String)(
  implicit
    LA: Arbitrary[Option[A]],
    LB: Arbitrary[Option[B]],
    FC: Arbitrary[Option[C]]
) extends Properties(s"$name Cartesian Tests") {

  property(" Product is empty if one of the terms is empty") = forAll {
    (fa: Option[A], fb: Option[B]) => 
      (fa |*| fb).isEmpty == fa.isEmpty || fb.isEmpty
  }

  property(" Product3 is empty if one of the terms is empty") = forAll {
    (fa: Option[A], fb: Option[B], fc: Option[C]) => Cartesian.product3(fa, fb, fc).isEmpty == fa.isEmpty || fb.isEmpty || fc.isEmpty
  }
}

object ListCartestianTest1    extends ListCartesianTests[Int, String, Boolean]("Value types Lists")
object ListCartestianTest2    extends ListCartesianTests[Show[Int], Symbol, Tree[Boolean]]("Classes Lists")
object OptionCartestianTest   extends OptionCartesianTests[Int, String, Boolean]("Value types Option")
object OptionCartestianTest2  extends OptionCartesianTests[Show[Int], Symbol, Tree[Boolean]]("Classes Option")
