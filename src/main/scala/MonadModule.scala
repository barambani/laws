import scala.language.higherKinds

import Algebra.Id
import Algebra.{Tree, Leaf, Branch}
import FunctorModule.Functor

object MonadModule {

  trait Monad[M[_]] extends Functor[M]  {
    def unit[A]: A => M[A]
    def bind[A, B]: M[A] => (A => M[B]) => M[B]
    def map[A, B]: M[A] => (A => B) => M[B] =
      ma => f => bind(ma) { a => (unit compose f)(a) }
  }

  object Monad {
    def apply[M[_]](implicit MON: Monad[M]): Monad[M] = MON
  }

  implicit final class MonadSyntax[M[_]: Monad, A](ma: M[A]) {
    def >>=[B](f: A => M[B]): M[B] = 
      Monad[M].bind(ma) { f } 
  }

  sealed trait Laws extends FunctorModule.Laws {
  
    def leftIdentity[M[_], A](implicit MO: Monad[M]): A => (A => M[A]) => Boolean =
      a => f => (MO.unit(a) >>= f) == f(a)
    
    def rightIdentity[M[_], A](implicit MO: Monad[M]): M[A] => Boolean =
      ma => (ma >>= { a => MO.unit(a) }) == ma

    def associativity[M[_]: Monad, A, B, C]: M[A] => (A => M[B]) => (B => M[C]) => Boolean =
      ma => f => g => (ma >>= f >>= g) == (ma >>= (f(_) >>= g))
  }
  
  sealed trait LawsNoInfix extends FunctorModule.LawsNoInfix {

    def leftIdentity[M[_], A](implicit MO: Monad[M]): A => (A => M[A]) => Boolean =
      a => f => MO.bind(MO.unit(a)) { f } == f(a)

    def rightIdentity[M[_], A](implicit MO: Monad[M]): M[A] => Boolean =
      ma => MO.bind(ma) { a => MO.unit(a) } == ma
    
    def associativity[M[_], A, B, C](implicit MO: Monad[M]): M[A] => (A => M[B]) => (B => M[C]) => Boolean =
      ma => f => g => MO.bind(MO.bind(ma) { f }) { g } == MO.bind(ma) { a => MO.bind(f(a)) { g } }
  }
  
  object Laws extends Laws
  object LawsNoInfix extends LawsNoInfix

  object MonadInstances {

    implicit val idMonad: Monad[Id] = new Monad[Id] {
      
      def unit[A]: A => Id[A] = 
        a => a
      
      def bind[A, B]: Id[A] => (A => Id[B]) => Id[B] =
        ma => f => f(ma)
    }
    
    implicit val seqMonad: Monad[Seq] = new Monad[Seq] {
      
      def unit[A]: A => Seq[A] = 
        _ :: Nil
      
      def bind[A, B]: Seq[A] => (A => Seq[B]) => Seq[B] =
        ma => f => ma flatMap f
    }
    
    implicit val listMonad: Monad[List] = new Monad[List] {
      
      def unit[A]: A => List[A] = 
        _ :: Nil
      
      def bind[A, B]: List[A] => (A => List[B]) => List[B] =
        ma => f => ma flatMap f
    }

    implicit val optionMonad: Monad[Option] = new Monad[Option] {
      
      def unit[A]: A => Option[A] = 
        Some(_)
      
      def bind[A, B]: Option[A] => (A => Option[B]) => Option[B] = 
        ma => f => ma flatMap f
    }

    implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    
      def unit[A]: A => Tree[A] =
        Leaf.apply

      def bind[A, B]: Tree[A] => (A => Tree[B]) => Tree[B] =
        fa => f => fa match {
          case Branch(l, r) => Branch(bind(l)(f), bind(r)(f))
          case Leaf(a)      => f(a)
        }
    }
  }
}
