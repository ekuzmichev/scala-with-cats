package ru.ekuzmichev.chapter3

object `3.5.4 Exercise: Branching out with Functors` {
  import cats.{ Functor, Show }

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A)                        extends Tree[A]

  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
    def leaf[A](value: A): Tree[A]                        = Leaf(value)
  }

  object FunctorInstances {
    implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
      override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
        case Leaf(value)         => Leaf(f(value))
      }
    }
  }

  object ConsoleUtils {
    def initColors: Iterator[String] = {
      import Console._
      Iterator.continually(List(BLUE, RED, GREEN, CYAN, YELLOW)).flatten
    }
    val defaultColor: String = Console.WHITE
  }

  object ShowInstances {
    import cats.syntax.show._
    implicit def treeShow[A: Show]: Show[Tree[A]] = new Show[Tree[A]] {
      import ConsoleUtils._
      val colors: Iterator[String] = initColors
      override def show(t: Tree[A]): String = {
        val lc = colors.next()
        val rc = colors.next()
        val dc = defaultColor
        t match {
          case Branch(left, right) => s"$lc[$dc${show(left)}$lc]$dc <- O -> $rc[$dc${show(right)}$rc]$dc"
          case Leaf(value)         => s"${value.show}"
        }
      }
    }
  }
}

object BranchingOutWithFunctorsTest extends App {
  import `3.5.4 Exercise: Branching out with Functors`._
  import FunctorInstances._
  import ShowInstances._
  import cats.instances.int._
  import cats.syntax.functor._
  import cats.syntax.show._

  val tree: Tree[Int] = Branch(Branch(Leaf(2), Branch(Leaf(3), Leaf(4))), Leaf(1))
  println(tree.map(_ * 2).show)

  import Tree._

  println(
    branch(
      branch(
        branch(leaf(5), leaf(6)),
        branch(leaf(3), leaf(4))
      ),
      branch(leaf(8), leaf(7))
    ).map(_ * 2).show
  )
}
