import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

import SemigroupModule.Semigroup
import SemigroupModule.Laws
import SemigroupModule.LawsNoInfix
import SemigroupModule.SemigroupInstances._

sealed abstract class SemigroupLawsCheck[A](name: String)(
  implicit
    SA: Semigroup[A],
    AA: Arbitrary[A]
) extends Properties(s"$name Semigroup Laws Check") {

  property(" Combine Associativity") = forAll {
    (a1: A, a2: A, a3: A) => Laws.combineAssociativity(a1, a2, a3)
  }

  property(" Combine Associativity No Infix") = forAll {
    (a1: A, a2: A, a3: A) => LawsNoInfix.combineAssociativity(a1, a2, a3)
  }
}

object IntWithAddSemigroupLawsCheck extends SemigroupLawsCheck[Int]("Int With Addition")
object StringWithConcatSemigroupLawsCheck extends SemigroupLawsCheck[String]("String With Concatenation")
object ListOfIntWithConcatSemigroupLawsCheck extends SemigroupLawsCheck[List[Int]]("List Of Int With Concatenation")
