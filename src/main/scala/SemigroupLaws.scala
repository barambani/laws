import scala.language.higherKinds

import SemigroupLaws.SemigroupSyntax

object SemigroupLaws {

  trait Semigroup[A] {
    def append: (A, A) => A
  }

  object Semigroup {
    
    def apply[A](implicit INST: Semigroup[A]): Semigroup[A] = INST
    
    def newInstance[A]: ((A, A) => A) => Semigroup[A] = 
      f => new Semigroup[A] {
        def append: (A, A) => A = f
      }
    
    trait Laws {
      def appendAssociativity[A](implicit SA: Semigroup[A]): A => A => A => Boolean =
        a1 => a2 => a3 => (a1 |+| a2 |+| a3) == (a1 |+| (a2 |+| a3))
    }

    trait LawsNoInfix {
      def appendAssociativity[A](implicit SA: Semigroup[A]): A => A => A => Boolean =
        a1 => a2 => a3 => SA.append(SA.append(a1, a2), a3) == SA.append(a1, SA.append(a2, a3))
    }
  }

  implicit class SemigroupSyntax[A](a: A) {
    def |+|(a1: A)(implicit SA: Semigroup[A]): A = SA.append(a, a1)
  }
}
