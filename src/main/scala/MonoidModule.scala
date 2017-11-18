import SemigroupModule.Semigroup
import SemigroupModule.SemigroupLaws
import SemigroupModule.SemigroupLawsNoInfix
import SemigroupModule.SemigroupSyntax
import SemigroupModule.SemigroupInstances._

object MonoidModule {

  trait Monoid[A] extends Semigroup[A] {
    val empty: A
  }

  object Monoid {
    
    def apply[A](implicit F: Monoid[A]): Monoid[A] = F

    def newInstance[A](z: A, SA: Semigroup[A]): Monoid[A] =
      new Monoid[A] {
        def combine: (A, A) => A = SA.combine
        val empty: A = z
      }
  }

  sealed trait MonoidLaws[A] extends SemigroupLaws[A] {

    implicit def F: Monoid[A]

    def emptyIdentity(a: A): Boolean =
      (a <> F.empty) == a && (F.empty <> a) == a
  }

  sealed trait MonoidLawsNoInfix[A] extends SemigroupLawsNoInfix[A] {

    implicit def F: Monoid[A]

    def emptyIdentity(a: A): Boolean =
      F.combine(a, F.empty) == a && F.combine(F.empty, a) == a
  }

  object MonoidLaws {
    def apply[A](implicit FI: Monoid[A]): MonoidLaws[A] =
      new MonoidLaws[A] { def F = FI }
  }

  object MonoidLawsNoInfix {
    def apply[A](implicit FI: Monoid[A]): MonoidLawsNoInfix[A] =
      new MonoidLawsNoInfix[A] { def F = FI }
  }

  object MonoidInstances {
    
    implicit val intWithAdditionM: Monoid[Int] =
      Monoid.newInstance(0, intWithAdditionS)

    implicit val stringWithConcatM: Monoid[String] =
      Monoid.newInstance("", stringWithConcatS)

    implicit val listOfIntWithAppendM: Monoid[List[Int]] =
      Monoid.newInstance(Nil: List[Int], listOfIntWithAppendS)
  }
}
