import scala.language.higherKinds

object CartesianModule {

  trait Cartesian[F[_]] {
    
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
    
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      map2(fa, fb) { (a, b) => (a, b) }
  }

  object Cartesian {
    def apply[F[_]](implicit F: Cartesian[F]): Cartesian[F] = F

    def map3[F[_]: Cartesian, A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
      Cartesian[F].map2(fa |*| fb, fc) { (ab, c) => f(ab._1, ab._2, c) }

    def product3[F[_]: Cartesian, A, B, C](fa: F[A], fb: F[B], fc: F[C]): F[(A, B, C)] =
      map3(fa, fb, fc) { (a, b, c) => (a, b, c) }
  }

  implicit final class CartesianSyntax[F[_]: Cartesian, A](fa: F[A]) {
    def |*|[B](fb: F[B]): F[(A, B)] =
      Cartesian[F].product(fa, fb)
  }

  object CartesianInstances {

    implicit val optionCartesian: Cartesian[Option] = new Cartesian[Option] {
      def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
        (fa, fb) match {
          case (Some(a), Some(b)) => Some(f(a, b))
          case _ => None
        }
    }

    implicit val listCartesian: Cartesian[List] = new Cartesian[List] {
      def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] =
        for {
          a <- fa
          b <- fb
        } yield f(a, b)
    }
  }
}
