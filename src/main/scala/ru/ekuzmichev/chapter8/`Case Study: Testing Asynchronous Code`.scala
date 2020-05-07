package ru.ekuzmichev.chapter8

import cats.{Applicative, Functor, Id}

import scala.language.higherKinds

object `Case Study: Testing Asynchronous Code` {
  import scala.concurrent.Future

  trait UptimeClient[F[_]] {
    def getUptime(hostName: String): F[Int]
  }

  import cats.instances.list._
  import cats.syntax.functor._
  import cats.syntax.traverse._

  class UptimeService[F[_] : Applicative](client: UptimeClient[F]) {
    def getTotalUptime(hostNames: List[String]): F[Int] = hostNames.traverse(client.getUptime).map(_.sum)
  }

  trait RealUptimeClient extends UptimeClient[Future] {
    def getUptime(hostname: String): Future[Int]
  }

  trait TestUptimeClient extends UptimeClient[Id] {
    def getUptime(hostname: String): Int
  }

  class TestUptimeClientImpl(hostnames:Map[String, Int]) extends TestUptimeClient {
    override def getUptime(hostname: String): Int = hostnames.getOrElse(hostname, 0)
  }

  def testTotalUptime(): Unit = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClientImpl(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }
}

object TestingAsynchronousCodeTest extends App {
   import `Case Study: Testing Asynchronous Code`._

  testTotalUptime()
}
