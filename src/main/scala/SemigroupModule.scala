object SemigroupModule {

  trait Semigroup[A] {
    def combine: (A, A) => A
  }

  object Semigroup {
    
    def apply[A](implicit F: Semigroup[A]): Semigroup[A] = F
    
    def newInstance[A]: ((A, A) => A) => Semigroup[A] = 
      f => new Semigroup[A] {
        def combine: (A, A) => A = f
      }  
  }

  implicit final class SemigroupSyntax[A: Semigroup](a: A) {
    def |+|(a1: A): A = 
      Semigroup[A].combine(a, a1)
  }

  trait Laws {
    def combineAssociativity[A: Semigroup](a1: A, a2: A, a3: A): Boolean =
      ((a1 |+| a2) |+| a3) == (a1 |+| (a2 |+| a3))
  }

  trait LawsNoInfix {
    def combineAssociativity[A](a1: A, a2: A, a3: A)(implicit SA: Semigroup[A]): Boolean =
      SA.combine(SA.combine(a1, a2), a3) == SA.combine(a1, SA.combine(a2, a3))
  }

  object Laws extends Laws
  object LawsNoInfix extends LawsNoInfix

  object SemigroupInstances {
    
    implicit val intWithAdditionS: Semigroup[Int] =
      Semigroup.newInstance[Int](_ + _)

    implicit val stringWithConcatS: Semigroup[String] =
      Semigroup.newInstance[String](_ ++ _)

    implicit val listOfIntWithAppendS: Semigroup[List[Int]] =
      Semigroup.newInstance[List[Int]](_ ++ _)
  }
}
