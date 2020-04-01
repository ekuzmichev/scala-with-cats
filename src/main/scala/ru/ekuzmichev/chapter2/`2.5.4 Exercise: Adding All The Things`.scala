package ru.ekuzmichev.chapter2

import ru.ekuzmichev.util

object `2.5.4 Exercise: Adding All The Things` {
  import cats.Monoid

  class SuperAdder {
    def add[A](items: List[A])(implicit m: Monoid[A]): A =
      items.foldLeft(m.empty)(m.combine)
  }

  case class Order(totalCost: Double, quantity: Double) {
    import util.MathOps.DoubleOps
    def ~=(other: Order): Boolean = (totalCost ~= other.totalCost) && (quantity ~= other.quantity)
  }

  object MonoidInstances {
    import cats.instances.double._
    import cats.syntax.semigroup._

    implicit val orderMonoid: Monoid[Order] = {
      val dm = Monoid[Double]
      Monoid.instance[Order](
        Order(dm.empty, dm.empty),
        (o1, o2) => Order(o1.totalCost |+| o2.totalCost, o1.totalCost |+| o2.quantity)
      )
    }
  }
}

object AddingAllTheThingsTest extends App {
  import `2.5.4 Exercise: Adding All The Things`._
  import MonoidInstances._
  import cats.instances.int._
  import cats.instances.option._
  import cats.syntax.option._

  val superAdder = new SuperAdder
  assert(superAdder.add(List(1, 2, 3, 4, 5)) == 15)
  assert(superAdder.add(List(1.some, 2.some, 3.some, none, 5.some)) == 11.some)
  assert(superAdder.add(List(Order(0, 0), Order(1.1, 1.1), Order(2.2, 2.2))) ~= Order(3.3, 3.3))
}
