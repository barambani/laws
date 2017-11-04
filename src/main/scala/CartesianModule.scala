import scala.language.higherKinds

object CartesianModule {

  trait Cartesian[F[_]] {
     def product[A, B]: F[A] => F[B] => F[(A, B)]
  }


}
