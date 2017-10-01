import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

import MonoidLaws.Monoid
import MonoidLaws.Laws
import MonoidLaws.LawsNoInfix
import MonoidLaws.MonoidInstances._

sealed abstract class MonoidLawsCheck[A](name: String)(
  implicit
    MA: Monoid[A],
    AA: Arbitrary[A]
) extends Properties(s"$name Monoid Laws Check") {

  property(" Zero Identity") = forAll {
    (a: A) => Laws.zeroIdentity(a)
  }

  property(" Combine Associativity") = forAll {
    (a1: A, a2: A, a3: A) => Laws.combineAssociativity(a1, a2, a3)
  }

  property(" Zero Identity No Infix") = forAll {
    (a: A) => LawsNoInfix.zeroIdentity(a)
  }

  property(" Combine Associativity No Infix") = forAll {
    (a1: A, a2: A, a3: A) => LawsNoInfix.combineAssociativity(a1, a2, a3)
  }
}

object IntWithAddMonoidLawsCheck extends MonoidLawsCheck[Int]("Int With Addition")
object StringWithConcatMonoidLawsCheck extends MonoidLawsCheck[String]("String With Concatenation")
object ListOfIntWithConcatMonoidLawsCheck extends MonoidLawsCheck[List[Int]]("List Of Int With Concatenation")
