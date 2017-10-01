import scala.language.higherKinds

object MonadLaws {

  type Id[A] = A 

  trait Monad[M[_]] {
    def unit[A]: A => M[A]
    def bind[A, B]: M[A] => (A => M[B]) => M[B]
  }

  object Monad {
    def apply[M[_]](implicit MON: Monad[M]): Monad[M] = MON
  }

  implicit class MonadSyntax[M[_]: Monad, A](ma: M[A]) {
    def >>=[B](f: A => M[B]): M[B] = 
      Monad[M].bind(ma) { f } 
  }

  sealed trait Laws {
  
    def leftIdentity[M[_], A](implicit MO: Monad[M]): A => (A => M[A]) => Boolean =
      a => f => (MO.unit(a) >>= f) == f(a)
    
    def rightIdentity[M[_], A](implicit MO: Monad[M]): M[A] => Boolean =
      ma => (ma >>= { a => MO.unit(a) }) == ma

    def associativity[M[_]: Monad, A, B, C]: M[A] => (A => M[B]) => (B => M[C]) => Boolean =
      ma => f => g => (ma >>= f >>= g) == (ma >>= (f(_) >>= g))
  }
  
  sealed trait LawsNoInfix {

    def leftIdentity[M[_], A](implicit MO: Monad[M]): A => (A => M[A]) => Boolean =
      a => f => MO.bind(MO.unit(a)) { f } == f(a)

    def rightIdentity[M[_], A](implicit MO: Monad[M]): M[A] => Boolean =
      ma => MO.bind(ma) { a => MO.unit(a) } == ma
    
    def associativity[M[_], A, B, C](implicit MO: Monad[M]): M[A] => (A => M[B]) => (B => M[C]) => Boolean =
      ma => f => g => MO.bind(MO.bind(ma) { f }) { g } == MO.bind(ma) { a => MO.bind(f(a)) { g } }
  }
  
  object Laws extends Laws
  object LawsNoInfix extends LawsNoInfix

  object MonadInstances {
  
    implicit def listMonad[A]: Monad[List] = new Monad[List] {
      def unit[A]: A => List[A] = 
        _ :: Nil
      def bind[A, B]: List[A] => (A => List[B]) => List[B] =
        ma => f => ma flatMap f
    }

    implicit def optionMonad[A]: Monad[Option] = new Monad[Option] {
      def unit[A]: A => Option[A] = 
        Some(_)
      def bind[A, B]: Option[A] => (A => Option[B]) => Option[B] = 
        ma => f => ma flatMap f
    }

    implicit def idMonad[A]: Monad[Id] = new Monad[Id] {
      def unit[A]: A => Id[A] = 
        a => a
      def bind[A, B]: Id[A] => (A => Id[B]) => Id[B] =
        ma => f => f(ma)
    }
  }
}
