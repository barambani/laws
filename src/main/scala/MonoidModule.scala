import SemigroupModule.Semigroup
import SemigroupModule.SemigroupSyntax
import SemigroupModule.SemigroupInstances._

object MonoidModule {

  trait Monoid[A] extends Semigroup[A] {
    def zero: A
  }

  object Monoid {
    
    def apply[A](implicit F: Monoid[A]): Monoid[A] = F

    def newInstance[A](z: A, SA: Semigroup[A]): Monoid[A] =
      new Monoid[A] {
        def combine: (A, A) => A = SA.combine
        def zero: A = z
      }
  }

  sealed trait Laws extends SemigroupModule.Laws {
    def zeroIdentity[A: Monoid](a: A): Boolean =
      (a |+| Monoid[A].zero) == a && 
      (Monoid[A].zero |+| a) == a
  }

  sealed trait LawsNoInfix extends SemigroupModule.LawsNoInfix {
    def zeroIdentity[A: Monoid](a: A): Boolean =
      Monoid[A].combine(a, Monoid[A].zero) == a && 
      Monoid[A].combine(Monoid[A].zero, a) == a
  }

  object Laws extends Laws
  object LawsNoInfix extends LawsNoInfix

  object MonoidInstances {
    
    implicit val intWithAdditionM: Monoid[Int] =
      Monoid.newInstance(0, intWithAdditionS)

    implicit val stringWithConcatM: Monoid[String] =
      Monoid.newInstance("", stringWithConcatS)

    implicit val listOfIntWithAppendM: Monoid[List[Int]] =
      Monoid.newInstance(Nil: List[Int], listOfIntWithAppendS)
  }
}
