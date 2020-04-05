package ru.ekuzmichev.chapter4

object `4.10.1 Exercise: Branching out Further with Monads` {
  import cats.Monad

  import scala.annotation.tailrec

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A)                        extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A]                        = Leaf(value)

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
      case Leaf(value)         => f(value)
    }

    override def tailRecM[A, B](a: A)(func: A => Tree[Either[A, B]]): Tree[B] = {
      @tailrec
      def loop(open: List[Tree[Either[A, B]]], closed: List[Option[Tree[B]]]): List[Tree[B]] = open match {
        case Branch(l, r) :: next =>
          loop(l :: r :: next, None :: closed)
        case Leaf(Left(value)) :: next =>
          loop(func(value) :: next, closed)
        case Leaf(Right(value)) :: next =>
          loop(next, Some(pure(value)) :: closed)
        case Nil =>
          closed.foldLeft(Nil: List[Tree[B]]) { (acc, maybeTree) =>
            maybeTree.map(_ :: acc).getOrElse {
              val left :: right :: tail = acc
              branch(left, right) :: tail
            }
          }
      }
      loop(List(func(a)), Nil).head
    }

  }
}

object BranchingOutFurtherWithMonadsTest extends App {
  import `4.10.1 Exercise: Branching out Further with Monads`._
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  val res1 = for {
    x <- branch(leaf(1), branch(leaf(2), leaf(3)))
    y <- branch(leaf(1), leaf(2))
  } yield x + y

  println(res1)

  val res2 = for {
    a <- branch(leaf(100), leaf(200))
    b <- branch(leaf(a - 10), leaf(a + 10))
    c <- branch(leaf(b - 1), leaf(b + 1))
  } yield c

  println(res2)

}
