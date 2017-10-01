import SemigroupLaws.Semigroup
import SemigroupLaws.SemigroupSyntax
import SemigroupLaws.SemigroupInstances._

object MonoidLaws {

  trait Monoid[A] extends Semigroup[A] {
    def zero: A
  }

  object Monoid {
    
    def apply[A](implicit INST: Monoid[A]): Monoid[A] = INST

    def newInstance[A](z: A, SA: Semigroup[A]): Monoid[A] =
      new Monoid[A] {
        def append: (A, A) => A = SA.append
        def zero: A = z
      }
  }

  sealed trait Laws extends SemigroupLaws.Laws {
    def zeroIdentity[A](a: A)(implicit MA: Monoid[A]): Boolean =
      (a |+| MA.zero) == a && a == (MA.zero |+| a)
  }

  sealed trait LawsNoInfix extends SemigroupLaws.LawsNoInfix {
    def zeroIdentity[A](a: A)(implicit MA: Monoid[A]): Boolean =
      MA.append(a, MA.zero) == a && a == MA.append(MA.zero, a)
  }

  object Laws extends Laws
  object LawsNoInfix extends LawsNoInfix

  object MonoidInstances {
    
    implicit lazy val intWithAdditionM: Monoid[Int] =
      Monoid.newInstance(0, intWithAdditionS)

    implicit lazy val stringWithConcatM: Monoid[String] =
      Monoid.newInstance("", stringWithConcatS)

    implicit lazy val listOfIntWithAppendM: Monoid[List[Int]] =
      Monoid.newInstance(Nil: List[Int], listOfIntWithAppendS)
  }
}
