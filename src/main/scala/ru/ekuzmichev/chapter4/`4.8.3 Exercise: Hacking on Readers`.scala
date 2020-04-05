package ru.ekuzmichev.chapter4

object `4.8.3 Exercise: Hacking on Readers` {
  import cats.data.Reader
  import cats.syntax.applicative._

  final case class Db(
    usernames: Map[Int, String],
    passwords: Map[String, String]
  )

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] = Reader(_.usernames.get(userId))
  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))
  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      username   <- findUsername(userId)
      passwordOk <- username.map(checkPassword(_, password)).getOrElse(false.pure[DbReader])
    } yield passwordOk
}

object HackingOnReadersTest extends App {
  import `4.8.3 Exercise: Hacking on Readers`._

  val users     = Map(1 -> "dade", 2 -> "kate", 3 -> "margo")
  val passwords = Map("dade" -> "zerocool", "kate" -> "acidburn", "margo" -> "secret")
  val db        = Db(users, passwords)
  assert(checkLogin(1, "zerocool").run(db))
  assert(!checkLogin(4, "davinci").run(db))
}
