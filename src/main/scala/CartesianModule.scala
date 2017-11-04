import scala.language.higherKinds

object CartesianModule {

  trait Cartesian[F[_]] {
     def product[A, B]: F[A] => F[B] => F[(A, B)]
  }

  object Cartesian {
    def apply[F[_]](implicit F: Cartesian[F]): Cartesian[F] = F
  }

  implicit final class CartesianSyntax[F[_]: Cartesian, A](fa: F[A]) {
    def |*|[B](fb: F[B]): F[(A, B)] =
      Cartesian[F].product(fa)(fb)
  }

  object CartesianInstances {
  
  }
}
