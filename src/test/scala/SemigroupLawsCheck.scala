import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

import SemigroupModule.Semigroup
import SemigroupModule.SemigroupLaws
import SemigroupModule.SemigroupLawsNoInfix
import SemigroupModule.SemigroupInstances._

sealed abstract class SemigroupLawsCheck[A : Semigroup](name: String)(
  implicit
    AA: Arbitrary[A]
) extends Properties(s"$name Semigroup Laws Check") {

  val laws        = SemigroupLaws[A]
  val lawsNoInfix = SemigroupLawsNoInfix[A]

  property(" Combine Associativity") = forAll {
    (a1: A, a2: A, a3: A) => laws.combineAssociativity(a1, a2, a3)
  }

  property(" Combine Associativity No Infix") = forAll {
    (a1: A, a2: A, a3: A) => lawsNoInfix.combineAssociativity(a1, a2, a3)
  }
}

object IntWithAddSemigroupLawsCheck           extends SemigroupLawsCheck[Int]("Int With Addition")
object StringWithConcatSemigroupLawsCheck     extends SemigroupLawsCheck[String]("String With Concatenation")
object ListOfIntWithConcatSemigroupLawsCheck  extends SemigroupLawsCheck[List[Int]]("List Of Int With Concatenation")
