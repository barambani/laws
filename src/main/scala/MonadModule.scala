import Algebra._
import FunctorModule.Functor
import ApplicativeModule.Applicative

object MonadModule {

  trait Monad[F[_]] extends Applicative[F] with Functor[F] {
    def `return`[A]: A => F[A]
    def bind[A, B]: F[A] => (A => F[B]) => F[B]
 
    def pure[A]: A => F[A] =
      `return`
    
    def ap[A, B]: F[A] => F[A => B] => F[B] =
      fa => ff => bind(fa) { a => map(ff) { f => f(a) } }

    override def map[A, B]: F[A] => (A => B) => F[B] =
      ma => f => bind(ma) { a => (`return` compose f)(a) }
  }

  object Monad {
    def apply[F[_]](implicit F: Monad[F]): Monad[F] = F
  }

  implicit final class MonadSyntax[F[_] : Monad, A](fa: F[A]) {
    def >>=[B](f: A => F[B]): F[B] = 
      Monad[F].bind(fa) { f }

    def flatMap[B](f: A => F[B]): F[B] = Monad[F].bind(fa) { f }
  }

  sealed trait MonadLaws[F[_]] {

    implicit def F: Monad[F]
  
    def leftIdentity[A]: A => (A => F[A]) => Boolean =
      a => f => (F `return` a >>= f) == f(a)

    def rightIdentity[A]: F[A] => Boolean =
      ma => (ma >>= { a => F `return` a }) == ma

    def associativity[A, B, C]: F[A] => (A => F[B]) => (B => F[C]) => Boolean =
      ma => f => g => (ma >>= f >>= g) == (ma >>= (f(_) >>= g))
  }
  
  sealed trait MonadLawsNoSyntax[F[_]] {

    implicit def F: Monad[F]

    def leftIdentity[A]: A => (A => F[A]) => Boolean =
      a => f => F.bind(F.`return`(a)) { f } == f(a)

    def rightIdentity[A]: F[A] => Boolean =
      ma => F.bind(ma) { a => F.`return`(a) } == ma

    def associativity[A, B, C]: F[A] => (A => F[B]) => (B => F[C]) => Boolean =
      ma => f => g => F.bind(F.bind(ma){ f }){ g } == F.bind(ma){ a => F.bind(f(a)){ g } }
  }

  object MonadLaws {
    def apply[F[_]](implicit FI: Monad[F]): MonadLaws[F] =
      new MonadLaws[F] { def F = FI }
  }

  object MonadLawsNoSyntax {
    def apply[F[_]](implicit FI: Monad[F]): MonadLawsNoSyntax[F] =
      new MonadLawsNoSyntax[F] { def F = FI }
  }

  object MonadInstances {

    implicit val idMonad: Monad[Id] = new Monad[Id] {
      
      def `return`[A]: A => Id[A] = 
        a => a
      
      def bind[A, B]: Id[A] => (A => Id[B]) => Id[B] =
        ma => f => f(ma)
    }
    
    implicit val listMonad: Monad[List] = new Monad[List] {
      
      def `return`[A]: A => List[A] = 
        _ :: Nil
      
      def bind[A, B]: List[A] => (A => List[B]) => List[B] =
        ma => f => ma flatMap f
    }

    implicit val optionMonad: Monad[Option] = new Monad[Option] {
      
      def `return`[A]: A => Option[A] = Some(_)
      
      def bind[A, B]: Option[A] => (A => Option[B]) => Option[B] = 
        ma => f => ma flatMap f
    }

    implicit def eitherMonad[E]: Monad[Either[E, *]] = new Monad[Either[E, *]] {
    
      def `return`[A]: A => Either[E, A] = Right(_)

      def bind[A, B]: Either[E, A] => (A => Either[E, B]) => Either[E, B] =
        fa => f => fa flatMap f
    }

    implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    
      def `return`[A]: A => Tree[A] = Leaf[A]

      def bind[A, B]: Tree[A] => (A => Tree[B]) => Tree[B] =
        fa => f => fa match {
          case Branch(l, r) => Branch(bind(l)(f), bind(r)(f))
          case Leaf(a)      => f(a)
        }
    }

    implicit def functionApplicative[X]: Monad[X -> *] =
      new Monad[X -> *] {

        def `return`[A]: A => X -> A =
          a => Func(_ => a)

        def bind[A, B]: X -> A => (A => (X -> B)) => X -> B =
          fa => f => Func(x => (f compose fa)(x)(x))
      }
  }
}
