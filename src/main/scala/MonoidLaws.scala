import scala.language.higherKinds

import SemigroupLaws.Semigroup
import SemigroupLaws.SemigroupSyntax

object MonoidLaws {

  trait Monoid[A] extends Semigroup[A] {
    def zero: A
  }

  object Monoid {
    
    def apply[A](implicit INST: Monoid[A]): Monoid[A] = INST

    def newInstance[A](implicit SA: Semigroup[A]): A => Monoid[A] =
      z => new Monoid[A] {
        def append: (A, A) => A = SA.append
        def zero: A = z
      }
  }

  sealed trait Laws extends Semigroup.Laws {
    def zeroIdentity[A](implicit MA: Monoid[A]): A => Boolean =
      a => (a |+| MA.zero) == (MA.zero |+| a) == a

  }

  sealed trait LawsNoInfix extends Semigroup.LawsNoInfix {
    def zeroIdentity[A](implicit MA: Monoid[A]): A => Boolean =
      a => MA.append(a, MA.zero) == MA.append(MA.zero, a) == a
  }
}
