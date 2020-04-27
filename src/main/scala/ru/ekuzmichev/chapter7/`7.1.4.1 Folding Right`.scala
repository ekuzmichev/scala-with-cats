package ru.ekuzmichev.chapter7

import org.scalatest.matchers.should.Matchers

object `7.1.4.1 Folding Right`

object FoldingRightTest extends App with Matchers {
  def bigData = (1 to 100000).toList

  val resStandard = bigData.foldRight(0L)(_ + _)
  resStandard should be(5000050000L)

  import cats.Foldable
  import cats.Eval
  import cats.instances.list._

  val resFoldable: Eval[Long] = Foldable[List].foldRight(bigData, Eval.now(0L))((num, eval) => eval.map(_ + num))

  resFoldable.value should be(5000050000L)
}
