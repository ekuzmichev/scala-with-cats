package ru.ekuzmichev.chapter11


object `Case Study: CRDTs` {
  object Step1 {
    final case class GCounter(counters: Map[String, Int]) {
      def increment(machine: String, amount: Int): GCounter =
        GCounter(counters + (machine -> (counters.getOrElse(machine, 0) + amount)))

      def merge(that: GCounter): GCounter =
        GCounter(that.counters ++ this.counters.map { case (k, v) => (k, v max that.counters.getOrElse(k, 0)) })

      def total: Int = counters.values.sum
    }

  }

  object Step2 {

    import cats.kernel.CommutativeMonoid

    trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
      def combine(a1: A, a2: A): A

      def empty: A
    }

    object BoundedSemiLattice {
      implicit val intBoundedSemiLattice: BoundedSemiLattice[Int] = new BoundedSemiLattice[Int] {
        override def empty: Int = 0

        override def combine(a1: Int, a2: Int): Int = a1 max a2
      }

      implicit def setBoundedSemiLattice[A](): BoundedSemiLattice[Set[A]] = new BoundedSemiLattice[Set[A]] {
        override def empty: Set[A] = Set.empty

        override def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 union a2
      }

      def apply[A](implicit instance: BoundedSemiLattice[A]): BoundedSemiLattice[A] = instance
    }

    final case class GCounter[A](counters: Map[String, A]) {
      import cats.instances.list._
      import cats.instances.map._
      import cats.syntax.foldable._
      import cats.syntax.semigroup._

      def increment(machine: String, amount: A)(implicit m: CommutativeMonoid[A]): GCounter[A] = {
        val value = counters.getOrElse(machine, m.empty) |+| amount
        GCounter(counters + (machine -> value))
      }

      def merge(that: GCounter[A])(implicit b: BoundedSemiLattice[A]): GCounter[A] =
        GCounter(that.counters |+| this.counters)

      def total(implicit m: CommutativeMonoid[A]): A = counters.values.toList.combineAll
    }

  }

  object Step3 {
    import Step2.BoundedSemiLattice
    import cats.kernel.CommutativeMonoid

    import scala.language.higherKinds

    trait GCounter[F[_, _], K, V] {
      def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]
      def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]
      def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
    }
    object GCounter {
      def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]): GCounter[F, K, V] = counter

      implicit def mapGCounter[K, V]: GCounter[Map, K, V] = new GCounter[Map, K, V] {
        import cats.instances.list._
        import cats.instances.map._
        import cats.syntax.foldable._
        import cats.syntax.semigroup._

        override def increment(f: Map[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): Map[K, V] = {
          val value = f.getOrElse(k, m.empty) |+| v
          f + (k -> value)
        }

        override def merge(f1: Map[K, V], f2: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] = f1 |+| f2
        override def total(f: Map[K, V])(implicit m: CommutativeMonoid[V]): V                          = f.values.toList.combineAll
      }
    }
  }

  object Step4 {
    import scala.language.higherKinds
    import cats.kernel.CommutativeMonoid
    import Step3.GCounter
    import Step2.BoundedSemiLattice

    trait KeyValueStore[F[_, _]] {
      def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]
      def get[K, V](f: F[K, V])(k: K): Option[V]
      def getOrElse[K, V](f: F[K, V])(k: K, default: V): V = get(f)(k).getOrElse(default)
      def values[K, V](f: F[K, V]): List[V]
    }

    object KeyValueStore {
      implicit val mapKeyValueStore: KeyValueStore[Map] = new KeyValueStore[Map] {
        override def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f + (k -> v)
        override def get[K, V](f: Map[K, V])(k: K): Option[V]       = f.get(k)
        override def values[K, V](f: Map[K, V]): List[V]            = f.values.toList
      }

      def apply[F[_, _]](implicit instance: KeyValueStore[F]): KeyValueStore[F] = instance
    }

    implicit class KvsOps[F[_, _], K, V](f: F[K, V]) {
      def put(key: K, value: V)(implicit kvs: KeyValueStore[F]): F[K, V]   = kvs.put(f)(key, value)
      def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V]           = kvs.get(f)(key)
      def getOrElse(key: K, default: V)(implicit kvs: KeyValueStore[F]): V = kvs.getOrElse(f)(key, default)
      def values(implicit kvs: KeyValueStore[F]): List[V]                  = kvs.values(f)
    }

    import cats.instances.list._
    import cats.syntax.foldable._
    import cats.syntax.semigroup._

    implicit def gCounterInstance[F[_, _], K, V](
      implicit kvs: KeyValueStore[F],
      km: CommutativeMonoid[F[K, V]]
    ): GCounter[F, K, V] =
      new GCounter[F, K, V] {
        def increment(f: F[K, V])(key: K, value: V)(implicit m: CommutativeMonoid[V]): F[K, V] = {
          val total = f.getOrElse(key, m.empty) |+| value
          f.put(key, total)
        }
        def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V] = f1 |+| f2
        def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V                      = f.values.combineAll
      }
  }
}

object CRDTsTest extends App {
  import ru.ekuzmichev.chapter11.`Case Study: CRDTs`._

  {
    import Step1._
    val c1 = GCounter(Map("a" -> 1, "b" -> 2, "c" -> 3))
    val c2 = GCounter(Map("a" -> 0, "b" -> 3))

    assert(c1.increment("a", 1) == GCounter(Map("a" -> 2, "b" -> 2, "c" -> 3)))
    assert(c2.increment("c", 1) == GCounter(Map("a" -> 0, "b" -> 3, "c" -> 1)))
    assert(c1.merge(c2) == GCounter(Map("a"         -> 1, "b" -> 3, "c" -> 3)))
    assert(c1.total == 6)
  }

  {
    import Step2._

    {
      import BoundedSemiLattice._

      assert(BoundedSemiLattice[Int].empty == 0)
      assert(BoundedSemiLattice[Int].combine(42, 21) == BoundedSemiLattice[Int].combine(21, 42))
    }

    val c1 = GCounter(Map("a" -> 1, "b" -> 2, "c" -> 3))
    val c2 = GCounter(Map("a" -> 0, "b" -> 3))

    import cats.instances.int._

    assert(c1.increment("a", 1) == GCounter(Map("a" -> 2, "b" -> 2, "c" -> 3)))
    assert(c2.increment("c", 1) == GCounter(Map("a" -> 0, "b" -> 3, "c" -> 1)))
    assert(c1.total == 6)

    {
      import BoundedSemiLattice._
      assert(c1.merge(c2) == GCounter(Map("a" -> 1, "b" -> 3, "c" -> 3)))
    }
  }

  {
    import Step3._
    import cats.instances.int._

    val m1 = Map("a" -> 1, "b" -> 2, "c" -> 3)
    val m2 = Map("a" -> 0, "b" -> 3)
    assert(GCounter[Map, String, Int].merge(m1, m2) == Map("a" -> 1, "b" -> 3, "c" -> 3))
    assert(GCounter[Map, String, Int].total(m1) == 6)
  }

  {
    import Step4._
    import Step3.GCounter
    val m1 = Map("a" -> 1, "b" -> 2, "c" -> 3)
    val m2 = Map("a" -> 0, "b" -> 3)
    assert(KeyValueStore[Map].put(Map("a" -> 0, "b" -> 3))("c", 3) == Map("a" -> 0, "b" -> 3, "c" -> 3))
    assert(
      GCounter[Map, String, Int].merge(m1,m2) ==  Map("a" -> 1, "b" -> 3, "c" -> 3)
    )
  }
}
