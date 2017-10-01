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
  }

  implicit final class SemigroupSyntax[A](a: A) {
    def |+|(a1: A)(implicit SA: Semigroup[A]): A = SA.append(a, a1)
  }

  trait Laws {
    def appendAssociativity[A: Semigroup](a1: A, a2: A, a3: A): Boolean =
      ((a1 |+| a2) |+| a3) == (a1 |+| (a2 |+| a3))
  }

  trait LawsNoInfix {
    def appendAssociativity[A](a1: A, a2: A, a3: A)(implicit SA: Semigroup[A]): Boolean =
      SA.append(SA.append(a1, a2), a3) == SA.append(a1, SA.append(a2, a3))
  }

  object Laws extends Laws
  object LawsNoInfix extends LawsNoInfix

  object SemigroupInstances {
    
    implicit def intWithAdditionS: Semigroup[Int] =
      Semigroup.newInstance[Int](_ + _)

    implicit def stringWithConcatS: Semigroup[String] =
      Semigroup.newInstance[String](_ ++ _)

    implicit def listOfIntWithAppendS: Semigroup[List[Int]] =
      Semigroup.newInstance[List[Int]](_ ++ _)
  }
}
