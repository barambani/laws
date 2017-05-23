import scala.language.higherKinds

object MonadLaws {

  trait Monad[M[_]] {
    def unit[A]: A => M[A]
    def bind[A, B]: M[A] => (A => M[B]) => M[B]
  }

  object Monad {
    def apply[M[_]](implicit MON: Monad[M]): Monad[M] = MON
  }

  implicit class MonadOps[M[_]: Monad, A](ma: M[A]) {
    def >>=[B](f: A => M[B]): M[B] = 
      Monad[M].bind(ma) { f } 
  }

  trait Laws {

    def leftIdentity[M[_], A](implicit MO: Monad[M]): A => (A => M[A]) => Boolean =
      a => f => MO.bind(MO.unit(a)) { f } == f(a)

    def rightIdentity[M[_], A](implicit MO: Monad[M]): M[A] => Boolean =
      ma => MO.bind(ma) { a => MO.unit(a) } == ma
    
    def associativity[M[_], A, B, C](implicit MO: Monad[M]): M[A] => (A => M[B]) => (B => M[C]) => Boolean =
      ma => f => g => MO.bind(MO.bind(ma) { f }) { g } == MO.bind(ma) { a => MO.bind(f(a)) { g } }
    

    def leftIdentity1[M[_], A](implicit MO: Monad[M]): A => (A => M[A]) => Boolean =
      a => f => (MO.unit(a) >>= f) == f(a)
    
    def rightIdentity2[M[_], A](implicit MO: Monad[M]): M[A] => Boolean =
      ma => (ma >>= { a => MO.unit(a) }) == ma

    def associativity1[M[_], A, B, C](implicit MO: Monad[M]): M[A] => (A => M[B]) => (B => M[C]) => Boolean =
      ma => f => g => (ma >>= f >>= g) == (ma >>= (f(_) >>= g))
  }
}
