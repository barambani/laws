object Algebra {

  type Id[A] = A

  final case class FuncFromIntTo[+R](private val f: Int => R) extends Function1[Int, R] {
    override def apply(i: Int): R = f(i)
    override def equals(other: Any): Boolean = 
      other match {
        case o: FuncFromIntTo[R] =>
          this.f(0) == o.f(0) &&
          this.f(Int.MaxValue) == o.f(Int.MaxValue) &&
          this.f(Int.MinValue) == o.f(Int.MinValue)
        case _ => false
      }
  }

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](v: A) extends Tree[A]
}
