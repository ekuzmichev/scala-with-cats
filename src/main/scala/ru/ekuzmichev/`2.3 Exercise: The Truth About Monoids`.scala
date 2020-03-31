package ru.ekuzmichev

object `2.3 Exercise: The Truth About Monoids` {
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }
  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }
  object Monoid {
    def apply[A](implicit instance: Monoid[A]): Monoid[A] = instance
  }
  object MonoidLaws {
    def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean =
      m.combine(m.combine(x, y), z) == m.combine(x, m.combine(y, z))

    def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean =
      m.combine(x, m.empty) == x &&
        m.combine(m.empty, x) == x
  }
  object MonoidInstances {
    val booleanAndMonoid: Monoid[Boolean] = new Monoid[Boolean] {
      override def empty: Boolean                           = true
      override def combine(x: Boolean, y: Boolean): Boolean = x && y
    }
    val booleanOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
      override def empty: Boolean                           = false
      override def combine(x: Boolean, y: Boolean): Boolean = x || y
    }
    val booleanEitherMonoid: Monoid[Boolean] = new Monoid[Boolean] {
      override def empty: Boolean                           = false
      override def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)
    }
    val booleanXnorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
      override def empty: Boolean                           = true
      override def combine(x: Boolean, y: Boolean): Boolean = (!x || y) && (x || !y)
    }
  }

}

object TheTruthAboutMonoidsTest extends App {
  import `2.3 Exercise: The Truth About Monoids`._
  import MonoidLaws._
  import MonoidInstances._

  def testLaws()(implicit monoid: Monoid[Boolean]): Unit = {
    assert(associativeLaw(true, false, true))
    assert(associativeLaw(false, false, false))
    assert(associativeLaw(true, true, true))
    assert(identityLaw(true))
    assert(identityLaw(false))
  }

  testLaws()(booleanAndMonoid)
  testLaws()(booleanOrMonoid)
  testLaws()(booleanEitherMonoid)
  testLaws()(booleanXnorMonoid)

}
