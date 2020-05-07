package ru.ekuzmichev.chapter8

import cats.Monoid

import scala.concurrent.{Await, Future}
import scala.language.postfixOps

object `Case Study: Map-Reduce` {
  //  Associative
  //   reduce(a1, reduce(a2, a3)) == reduce(reduce(a1, a2), a3)
  //  Identity
  //   reduce(seed, a1) == reduce(a1, seed) == a1

  def foldMap[A, B: Monoid](seq: Vector[A])(func: A => B): B =
    seq.map(func).fold(Monoid[B].empty)(Monoid[B].combine)

  import cats.instances.future._
  import cats.instances.vector._
  import cats.syntax.traverse._

  import scala.concurrent.ExecutionContext.Implicits.global

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val numCores: Int = Runtime.getRuntime.availableProcessors
    val groupSize = (values.size / numCores).ceil.toInt
    println(s"Data size: ${values.size}, num of cores: $numCores, group size: $groupSize")
    values
      .grouped(groupSize)
      .map(grp => Future(foldMap(grp)(func)))
      .toVector
      .sequence
      .map(foldMap(_)(identity))
  }

  import cats.syntax.foldable._

  def foldMapCats[A, B: Monoid](seq: Vector[A])(func: A => B): B = seq.foldMap(func)
}

object MapReduceTest extends App {
  import `Case Study: Map-Reduce`._
  import cats.instances.int._
  import cats.instances.string._

  assert(foldMap(Vector(1, 2, 3))(_.toString) == "123")
  assert(foldMap(Vector(1, 2, 3))(identity) == 6)
  assert(foldMap("Hello world!".toVector)(_.toString.toUpperCase) == "HELLO WORLD!")

  import scala.concurrent.duration._
  assert(Await.result(parallelFoldMap("Hello world!!!".toVector)(_.toString.toUpperCase), 2 seconds) == "HELLO WORLD!!!")

  assert(foldMapCats(Vector(1, 2, 3))(_.toString) == "123")
  assert(foldMapCats(Vector(1, 2, 3))(identity) == 6)
  assert(foldMapCats("Hello world!".toVector)(_.toString.toUpperCase) == "HELLO WORLD!")
}
