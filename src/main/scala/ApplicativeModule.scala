import FunctorModule.Functor
import CartesianModule.Cartesian

import scala.language.higherKinds

object ApplicativeModule {

  trait Applicative[F[_]] extends Cartesian[F] with Functor[F] {
    def pure[A]: A => F[A]
    def ap[A, B]: F[A] => F[A => B] => F[B]

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      ap(fa)(fmap(fb) { b => a => f(a, b) })

    def product[A, B]: F[A] => F[B] => F[(A, B)] =
      fa => fb => ap(fa)(fmap(fb){ b => a => (a, b) })
    
    def fmap[A, B]: F[A] => (A => B) => F[B] =
      fa => f => ap(fa)(pure(f))
  }
}
