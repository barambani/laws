object Algebra {

  type Id[A] = A
  type ->[A, B] = Func[A, B]

  final case class Func[-A, +R](private val f: A => R)
    extends (A => R) {
      override def apply(a: A): R = f(a)
      override def equals(other: Any): Boolean = 
        other match {
          case _: (A -> R)  => true
          case _            => false
        }
    }

  sealed trait Tree[+A] extends Product with Serializable
  final case class Branch[A](l: Tree[A], r: Tree[A]) extends Tree[A]
  final case class Leaf[A](v: A) extends Tree[A]

  trait Show[A] {
    def show: A => String
    override def equals(other: Any): Boolean = 
      other match {
        case _: Show[A] => true
        case _          => false
      }
  }
  
  object Show {
    
    def apply[A](implicit INST: Show[A]): Show[A] = INST
    def newInstance[A]: (A => String) => Show[A] =
      f => new Show[A] {
        def show: A => String = f
      }

    implicit val showInt: Show[Int] = 
      newInstance[Int](_.toString)

    implicit val showBoolean: Show[Boolean] =
      newInstance[Boolean](_.toString)

    implicit def showOfBox[A]: Show[Box[A]] = 
      newInstance[Box[A]](_.value.toString)

    implicit def showOfSymbol: Show[Symbol] =
      newInstance[Symbol](s => s"symbol ${ s.name }")
  }

  final case class Box[A](value: A)
  final case class Symbol(name: String)

  sealed trait Codec[A] {
    def encode: A => String
    def decode: String => Option[A]

    override def equals(other: Any): Boolean = 
      other match {
        case _: Codec[A]  => true
        case _            => false
      }
  }

  object Codec {
    def newInstance[A]: (A => String) => (String => Option[A]) => Codec[A] =
      f => g => new Codec[A] {
        def encode = f
        def decode = g
      }

    implicit lazy val symbolCodec: Codec[Symbol] =
      newInstance[Symbol](_.name)(n => Some(Symbol(n)))
  }
}
