import Algebra._
import CartesianModule.Cartesian
import FunctorModule.{Functor, FunctorSyntax}
import SemigroupModule.{Semigroup, SemigroupSyntax}

object ApplicativeModule {

  trait Applicative[F[_]] extends Cartesian[F] with Functor[F] {
    def pure[A]: A => F[A]
    def ap[A, B]: F[A] => F[A => B] => F[B]

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      ap(fa)(map(fb) { b => a => f(a, b) })

    def product[A, B]: F[A] => F[B] => F[(A, B)] =
      fa => fb => ap(fa)(map(fb){ b => a => (a, b) })
    
    def map[A, B]: F[A] => (A => B) => F[B] =
      fa => f => ap(fa)(pure(f))
  }

  object Applicative {
    def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F
  }

  implicit final class ApplicativeSyntax[F[_] : Applicative, A, B](fab: F[A => B]) {
    def <**>(fa: F[A]): F[B] =
      Applicative[F].ap(fa)(fab)
  }
  
  implicit final class ApplicativeSyntax2[F[_] : Applicative, A](fa: F[A]) {
    def <*>[B]: F[A => B] => F[B] =
      ff => Applicative[F].ap(fa)(ff)
  }

  sealed trait ApplicativeLaws[F[_]] {
    
    implicit def F: Applicative[F]

    def applicativeIdentity[A]: F[A] => Boolean =
      fa => F.pure(identity[A] _) <**> fa == fa

    def homomorphism[A, B]: A => (A => B) => Boolean =
      a => f => F.pure(f) <**> F.pure(a) == F.pure(f(a))

    def interchange[A, B]: A => F[A => B] => Boolean =
      a => ff => (F.pure(a) <*> ff) == (ff <*> F.pure((f: A => B) => f(a)))
  
    def applicativeMap[A, B]: F[A] => (A => B) => Boolean =
      fa => f => (fa map f) == (fa <*> F.pure(f))
  }

  sealed trait ApplicativeLawsNoSyntax[F[_]] {

    implicit def F: Applicative[F]

    def applicativeIdentity[A]: F[A] => Boolean =
      fa => F.ap(fa)(F.pure(identity[A] _)) == fa

    def homomorphism[A, B]: A => (A => B) => Boolean =
      a => f => F.ap(F.pure(a))(F.pure(f)) == F.pure(f(a))

    def interchange[A, B]: A => F[A => B] => Boolean =
      a => ff => F.ap(F.pure(a))(ff) == F.ap[A => B, B](ff)(F.pure(f => f(a)))

    def applicativeMap[A, B]: F[A] => (A => B) => Boolean =
      fa => f => F.map(fa)(f) == F.ap(fa)(F.pure(f))
  }

  object ApplicativeLaws {
    def apply[F[_]](implicit FI: Applicative[F]): ApplicativeLaws[F] =
      new ApplicativeLaws[F] { def F = FI }
  }

  object ApplicativeLawsNoSyntax {
    def apply[F[_]](implicit FI: Applicative[F]): ApplicativeLawsNoSyntax[F] =
      new ApplicativeLawsNoSyntax[F] { def F = FI }
  }

  object ApplicativeInstances {
    
    implicit val listApplicative: Applicative[List] =
      new Applicative[List] {
        def pure[A]: A => List[A] =
          a => List(a)

        def ap[A, B]: List[A] => List[A => B] => List[B] =
          fa => ff => for {
            a <- fa
            f <- ff
          } yield f(a)
      }

    implicit val optionApplicative: Applicative[Option] =
      new Applicative[Option] {
        def pure[A]: A => Option[A] =
          a => Some(a)

        def ap[A, B]: Option[A] => Option[A => B] => Option[B] =
          fa => ff => (fa, ff) match {
            case (Some(a), Some(f)) => Some(f(a))
            case _                  => None
          }
      }

    implicit def eitherApplicative[E : Semigroup]: Applicative[Either[E, *]] =
      new Applicative[Either[E, *]] {
        def pure[A]: A => Either[E, A] =
          a => Right(a)

        def ap[A, B]: Either[E, A] => Either[E, A => B] => Either[E, B] =
          fa => ff => (fa, ff) match {
            case (Right(a), Right(f)) => Right(f(a))
            case (Left(ea), Left(ef)) => Left(ea <> ef)
            case (Left(ea), _)        => Left(ea)
            case (_, Left(ef))        => Left(ef)
          }
      }

    implicit def functionApplicative[X]: Applicative[X -> *] =
      new Applicative[X -> *] {
        def pure[A]: A => (X -> A) =
          a => Func(_ => a)

        def ap[A, B]: (X -> A) => (X -> (A => B)) => (X -> B) =
          fa => ff => Func(x => ff(x)(fa(x)))
      }

    implicit val treeApplicative: Applicative[Tree] =
      new Applicative[Tree] {
        def pure[A]: A => Tree[A] =
          a => Leaf(a)

        def ap[A, B]: Tree[A] => Tree[A => B] => Tree[B] =
          fa => ff => (fa, ff) match {
            case (Branch(fal, far), Branch(ffl, ffr)) => Branch(Branch(ap(fal)(ffl), ap(far)(ffl)), Branch(ap(fal)(ffr), ap(far)(ffr)))
            case (Branch(fal, far), Leaf(f))          => Branch(ap(fal)(pure(f)), ap(far)(pure(f)))
            case (Leaf(a), Branch(ffl, ffr))          => Branch(ap(pure(a))(ffl), ap(pure(a))(ffr))
            case (Leaf(a), Leaf(f))                   => Leaf(f(a))
          }
      }

    implicit val idApplicative: Applicative[Id] =
      new Applicative[Id] {
        def pure[A]: A => Id[A] =
          identity[A]

        def ap[A, B]: Id[A] => Id[A => B] => Id[B] =
          fa => ff => ff(fa)
      }
  }
}
