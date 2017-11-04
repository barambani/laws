import scala.language.higherKinds

object CartesianModule {

  trait Cartesian[F[_]] {
    
    def map2[A, B, C]: F[A] => F[B] => ((A, B) => C) => F[C]

    def map3[A, B, C, D]: F[A] => F[B] => F[C] => ((A, B, C) => D) => F[D] =
      fa => fb => fc => f => map2(product(fa)(fb))(fc) { (ab, c) => f(ab._1, ab._2, c) }
    
    def product[A, B]: F[A] => F[B] => F[(A, B)] =
      fa => fb => map2(fa)(fb){ (a, b) => (a, b) }

    def product3[A, B, C]: F[A] => F[B] => F[C] => F[(A, B, C)] =
      fa => fb => fc => map3(fa)(fb)(fc){ (a, b, c) => (a, b, c) }
  }

  object Cartesian {
    def apply[F[_]](implicit F: Cartesian[F]): Cartesian[F] = F
  }

  implicit final class CartesianSyntax[F[_]: Cartesian, A](fa: F[A]) {
    def |*|[B](fb: F[B]): F[(A, B)] =
      Cartesian[F].product(fa)(fb)
  }

  object CartesianInstances {

    implicit val optionCartesian: Cartesian[Option] = new Cartesian[Option] {
      def map2[A, B, C]: Option[A] => Option[B] => ((A, B) => C) => Option[C] =
        fa => fb => f => (fa, fb) match {
          case (Some(a), Some(b)) => Some(f(a, b))
          case _ => None
        }
    }

    implicit val listCartesian: Cartesian[List] = new Cartesian[List] {
      def map2[A, B, C]: List[A] => List[B] => ((A, B) => C) => List[C] =
        fa => fb => f => for {
          a <- fa
          b <- fb
        } yield f(a, b)
    }
  }
}
