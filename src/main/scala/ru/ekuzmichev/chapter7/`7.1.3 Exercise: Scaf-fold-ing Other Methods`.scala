package ru.ekuzmichev.chapter7

import cats.kernel.Monoid
import org.scalatest.matchers.should.Matchers

object `7.1.3 Exercise: Scaf-fold-ing Other Methods` {
  def map[A, B](list: List[A])(f: A => B): List[B] =
    list.foldRight(List.empty[B])((elem, accum) => f(elem) :: accum)

  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
    list.foldRight(List.empty[B])((elem, accum) => f(elem) ::: accum)

  def filter[A](list: List[A])(p: A => Boolean): List[A] =
    list.foldRight(List.empty[A])((elem, accum) => if (p(elem)) elem :: accum else accum)

  def sum[A](list: List[A])(implicit m: Monoid[A]): A =
    list.foldRight(m.empty)(m.combine)
}

object ScafFoldingOtherMethodsTest extends App with Matchers {
  import `7.1.3 Exercise: Scaf-fold-ing Other Methods`._

  map(List(1, 2, 3))(_ * 2) should be(List(2, 4, 6))
  map(List("a", "b", "c"))(_.toUpperCase) should be(List("A", "B", "C"))

  flatMap(List(1, 2, 3))(i => List(i, i)) should be(List(1, 1, 2, 2, 3, 3))

  filter(List(1, 2, 3))(_ % 2 == 0) should be(List(2))
  filter(List("a", "b", "c"))(_ != "b") should be(List("a", "c"))

  import cats.instances.int._
  import cats.instances.string._
  
  sum(List(1, 2, 3)) should be(6)
  sum(List("a", "b")) should be("ab")
}
