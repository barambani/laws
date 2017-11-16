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
    def <>(a1: A): A = 
      Semigroup[A].combine(a, a1)
  }

  trait SemigroupLaws[A] {

    implicit def F: Semigroup[A]

    def combineAssociativity(a1: A, a2: A, a3: A): Boolean =
      (a1 <> a2) <> a3 == a1 <> (a2 <> a3)
  }

  trait SemigroupLawsNoInfix[A] {

    implicit def F: Semigroup[A]

    def combineAssociativity(a1: A, a2: A, a3: A): Boolean =
      F.combine(F.combine(a1, a2), a3) == F.combine(a1, F.combine(a2, a3))
  }

  object SemigroupLaws {
    def apply[A](implicit FI: Semigroup[A]): SemigroupLaws[A] =
      new SemigroupLaws[A] { def F = FI }
  }
  
  object SemigroupLawsNoInfix {
    def apply[A](implicit FI: Semigroup[A]): SemigroupLawsNoInfix[A] =
      new SemigroupLawsNoInfix[A] { def F = FI }
  }

  object SemigroupInstances {
    
    implicit val intWithAdditionS: Semigroup[Int] =
      Semigroup.newInstance[Int](_ + _)

    implicit val stringWithConcatS: Semigroup[String] =
      Semigroup.newInstance[String](_ ++ _)

    implicit val listOfIntWithAppendS: Semigroup[List[Int]] =
      Semigroup.newInstance[List[Int]](_ ++ _)

    implicit val listOfStringWithConcatS: Semigroup[List[String]] =
      Semigroup.newInstance[List[String]](_ ++ _)
  }
}
